const std = @import("std");
const ArrayList = std.ArrayList;
const alignForward = std.mem.alignForward;
const assert = std.debug.assert;
const panic = std.debug.panic;
const Allocator = std.mem.Allocator;
const max = std.math.maxInt;

const Type = @import("../../type.zig");
const Parser = @import("../../parser.zig");
const IR = @import("../../ir.zig");
const Compiler = @import("../../compiler.zig");
const Codegen = @import("../../codegen.zig");
const PE = @import("../pe.zig");
const Import = Codegen.Import;
const Encoding = @import("encoding.zig");
const Semantics = @import("../../semantics.zig");

pub fn log(comptime format: []const u8, arguments: anytype) void
{
    Compiler.log(.x86_64, "[CODEGEN] " ++ format, arguments);
}

fn codegen_error(comptime format: []const u8, arguments: anytype) noreturn
{
    panic(format, arguments);
}

fn get_alloca_size(program: *const IR.Program, instruction_ref: IR.Reference) u64
{
    assert(instruction_ref.get_ID() == .instruction);
    assert(IR.Instruction.get_ID(instruction_ref) == .alloca);
    const instruction_index = instruction_ref.get_index();
    const alloca = program.instructions.alloca[instruction_index];
    const alloca_type = alloca.alloca_type;
    const alloca_size = alloca_type.get_size();
    return alloca_size;
}

const system_v_argument_registers = [_]Encoding.Register
{
    .DI,
    .SI,
    .C,
    .r8,
    .r9,
};

const msvc_argument_registers = [_]Encoding.Register
{
    .C,
    .D,
    .r8,
    .r9,
};

const OperandKind = enum
{
    none,
    immediate,
    register,
    stack,
    relative_global,
    relative_external,
    relative_label,
};

const mov_rbp_rsp_bytes = [_]u8 { 0x48, 0x89, 0xe5 };
const pop_rbp_bytes = [_]u8 { 0x5d };
const systemv_abi_prologue = push_rbp_bytes ++ mov_rbp_rsp_bytes;
const push_rbp_bytes = [_]u8 { 0x55 };
const ret_bytes = [_]u8 { 0xc3 };

fn add_rsp_s8(n: i8) [4]u8
{
    assert(n > 0);
    var bytes = [_]u8 { 0x48, 0x83, 0xc4, undefined };
    @ptrCast(*i8, &bytes[3]).* = n;

    return bytes;
}

fn add_rsp_s32(n: i32) [7]u8
{
    assert(n > 0);
    var bytes = [_]u8 { 0x48, 0x81, 0xc4, undefined, undefined, undefined, undefined };

    @ptrCast(* align(1) i32, &bytes[3]).* = n;

    return bytes;
}

fn call_rel32(operand_index: u32) Instruction
{
    const bytes = [_]u8 { 0xe8 };
    return Instruction.Unresolved.new(bytes[0..], operand_index, .relative_global, 4, bytes.len);
}

fn call_rip_rel32(operand_index: u32) Instruction
{
    const bytes = [_]u8 { 0x48, 0xff, 0x15 };
    return Instruction.Unresolved.new(bytes[0..], operand_index, .relative_external, 4, bytes.len);
}

fn jcc_rel32(jmp: JmpOpcode, operand_index: u32) Instruction
{
    log("Encoding conditional jmp {}\n", .{jmp});
    const bytes = [_]u8 { 0x0f, @enumToInt(jmp) + 0x10 };
    return Instruction.Unresolved.new(bytes[0..], operand_index, .relative_label, 4, bytes.len);
}

fn jmp_rel32(operand_index: u32, operand_kind: Instruction.Unresolved.UnknownOperand.Kind) Instruction
{
    const bytes = [_]u8 { 0xe9 };
    // @TODO: take into account jumps to functions
    return Instruction.Unresolved.new(bytes[0..], operand_index, operand_kind, 4, bytes.len);
}

fn sub_rsp_s8(n: i8) [4]u8
{
    assert(n > 0);
    var bytes = [_]u8 { 0x48, 0x83, 0xec, undefined };
    @ptrCast(*i8, &bytes[3]).* = n;

    return bytes;
}

fn sub_rsp_s32(n: i32) [7]u8
{
    assert(n > 0);
    var bytes = [_]u8 { 0x48, 0x81, 0xec, undefined, undefined, undefined, undefined };

    @ptrCast(* align(1) i32, &bytes[3]).* = n;

    return bytes;
}

fn cmp_indirect_immediate(indirect_register: Encoding.Register, indirect_offset: i32, indirect_size: u8, integer_literal: Parser.IntegerLiteral) Instruction
{
    _ = indirect_size;
    _ = indirect_offset;
    _ = indirect_register;
    _ = integer_literal;

    const integer_byte_count: u8 = blk:
    {
        if (integer_literal.value > max(u32))
        {
            codegen_error("64-bit immediates are not supported for cmp\n", .{});
        }
        
        if (integer_literal.value > max(u16))
        {
            assert(integer_literal.value <= max(u32));
            break :blk 4;
        }
        if (integer_literal.value > max(u8))
        {
            assert(integer_literal.value <= max(u16));
            break :blk 2;
        }

        break :blk 1;
    };

    const integer_bytes = std.mem.asBytes(&integer_literal.value)[0..integer_byte_count];
    const opcode: u8 = if (integer_byte_count > 1 or indirect_size == max(u8)) 0x81 else 0x83;

    return encode_indirect_instruction_opcode_plus_register_indirect_offset(null, opcode, 0x38, indirect_register, indirect_offset, integer_bytes);
}

fn mov_register_literal(register: Encoding.Register, number: u64, byte_count: u16) Instruction
{
    const number_bytes = std.mem.asBytes(&number);

    const byte_slice = switch (byte_count)
    {
        1 => blk1:
        {
            const bytes = [_]u8 { 0xb0 + @enumToInt(register), @truncate(u8, number) };
            break :blk1 bytes[0..];
        },
        2 => blk2:
        {
            const bytes = [_]u8 { 0x66, 0xb8 + @enumToInt(register),
                number_bytes[0], number_bytes[1] };
            break :blk2 bytes[0..];
        },
        4 => blk4:
        {
            const bytes = [_]u8 { 0xb8 + @enumToInt(register),
                number_bytes[0], number_bytes[1], number_bytes[2], number_bytes[3] };
            break :blk4 bytes[0..];
        },
        8 => blk8:
        {
            const bytes = [_]u8 { 0x48, 0xb8 + @enumToInt(register),
                number_bytes[0], number_bytes[1], number_bytes[2], number_bytes[3], number_bytes[4], number_bytes[5], number_bytes[6], number_bytes[7] };
            break :blk8 bytes[0..];
        },
        else => unreachable,
    };

    return Instruction.Resolved.new(byte_slice);
}

fn encode_indirect_instruction_opcode_plus_register_indirect_offset(rex: ?u8, opcode: u8, register_byte_start: u8, indirect_register: Encoding.Register, indirect_offset: i32, after_common_bytes: []const u8) Instruction
{
    log("Encoding mov indirect...\n", .{});
    var bytes: [Instruction.max_bytes]u8 = undefined;
    var encoded_byte_count: u8 = 0;

    var indirect_offset_byte_count: u8 = undefined;
    const indirect_offset_i8 = @intCast(i8, indirect_offset);

    const indirect_offset_bytes = blk:
    {
        if (indirect_offset < std.math.minInt(i8) or indirect_offset > max(i8))
        {
            indirect_offset_byte_count = 4;
            break :blk @intToPtr([*]u8, @ptrToInt(&indirect_offset))[0..indirect_offset_byte_count];
        }
        else
        {
            indirect_offset_byte_count = 1;
            break :blk std.mem.asBytes(@ptrCast(* align(1) const i8, &indirect_offset_i8))[0..];
        }
    };

    if (rex) |rex_byte|
    {
        log("Appending rex byte: {x:0>2}\n", .{rex_byte});
        bytes[encoded_byte_count] = rex_byte;
        encoded_byte_count += 1;
    }

    log("Appending opcode: {x:0>2}\n", .{opcode});
    bytes[encoded_byte_count] = opcode;
    encoded_byte_count += 1;

    const indirect_offset_not_zero = indirect_offset != 0;
    const indirect_offset_dependent_register_byte_part = (@as(u8, 0x40) * ((@as(u8, @boolToInt(indirect_offset_byte_count == 4)) << 1) | 1)) * @as(u8, @boolToInt(indirect_offset_not_zero));

    log("Register byte start: {x:0>2}\n", .{register_byte_start});
    log("Register: {x:0>2}\n", .{@enumToInt(indirect_register)});
    log("indirect_offset_dependent_register_byte_part: {x:0>2}\n", .{indirect_offset_dependent_register_byte_part});
    const register_byte = register_byte_start |
        @enumToInt(indirect_register) |
        indirect_offset_dependent_register_byte_part;

    log("Appending register byte: {x:0>2}\n", .{register_byte});

    bytes[encoded_byte_count] = register_byte;
    encoded_byte_count += 1;

    if (indirect_register == .SP)
    {
        log("Appending SIB byte: {x:0>2}\n", .{0x24});
        bytes[encoded_byte_count] = 0x24;
        encoded_byte_count += 1;
    }

    if (indirect_offset_not_zero)
    {
        for (indirect_offset_bytes) |byte|
        {
            log("Appending stack offset byte: {x:0>2}\n", .{byte});
            bytes[encoded_byte_count] = byte;
            encoded_byte_count += 1;
        }
    }

    for (after_common_bytes) |byte|
    {
        log("Appending byte after indirect bytes: {x:0>2}\n", .{byte});
        bytes[encoded_byte_count] = byte;
        encoded_byte_count += 1;
    }

    return Instruction.Resolved.new_bytes(bytes, encoded_byte_count);
}

fn mov_indirect_immediate_unsigned(indirect_register: Encoding.Register, indirect_offset: i32, immediate: u64, allocation_size: u16) Instruction
{
    // @TODO: assert that for 64-bit register you cant use 64-bit immediate
    const immediate_bytes = std.mem.asBytes(&immediate)[0..allocation_size];
    return encode_indirect_instruction_opcode_plus_register_indirect_offset(null, 0xc7, 0, indirect_register, indirect_offset, immediate_bytes);
}

fn mov_register_indirect(dst_register: Encoding.Register, allocation_size: u8, indirect_register: Encoding.Register, indirect_offset: i32) callconv(.Inline) Instruction
{
    return switch (allocation_size)
    {
        1,2,8 => unreachable,
        4 => encode_indirect_instruction_opcode_plus_register_indirect_offset(null, 0x8b, @enumToInt(dst_register), indirect_register, indirect_offset, std.mem.zeroes([]const u8)),
        else => unreachable,
    };
}

fn mov_indirect_register(indirect_register: Encoding.Register, indirect_offset: i32, indirect_size: u8, dst_register: Encoding.Register, dst_register_size: u8) Instruction
{
    assert(dst_register_size == indirect_size);
    const opcode: u8 = if (indirect_size > 1) 0x89 else 0x88;

    return encode_indirect_instruction_opcode_plus_register_indirect_offset(null, opcode, @enumToInt(dst_register), indirect_register, indirect_offset, std.mem.zeroes([]u8));

}

fn add_register_indirect(dst_register: Encoding.Register, allocation_size: u8, indirect_register: Encoding.Register, indirect_offset: i32) Instruction
{
    var indirect_offset_byte_count: u8 = 0;
    if (indirect_offset != 0)
    {
        if (indirect_offset > max(i8))
        {
            indirect_offset_byte_count = 4;
        }
        else 
        {
            indirect_offset_byte_count = 1;
        }
    }

    var bytes: [15]u8 = undefined;
    var byte_count: u8 = 2 + @intCast(u8, @boolToInt(indirect_register == .SP)) + indirect_offset_byte_count;

    const opcode: u8 = if (allocation_size == 1) 0x02 else 0x03;
    bytes[0] = opcode;

    const register_byte = blk:
    {
        const base: u8 = 
            if (indirect_offset != 0)
                0x40
            else
                0x00;
        const result = base | (@enumToInt(dst_register) << 3) | @enumToInt(indirect_register);
        break :blk result;
    };
    bytes[1] = register_byte;

    var offset: u8 = 2;

    if (indirect_register == .SP)
    {
        bytes[offset] = 0x24;
        offset += 1;
    }

    if (indirect_offset_byte_count > 0)
    {
        for (std.mem.asBytes(&indirect_offset)[0..indirect_offset_byte_count]) |indirect_offset_byte|
        {
            bytes[offset] = indirect_offset_byte;
            offset += 1;
        }
    }

    return Instruction.Resolved.new(bytes[0..byte_count]);
}

fn add_register_immediate(register: Encoding.Register, register_size: u8, immediate: Parser.IntegerLiteral) Instruction
{
    const value = immediate.value;
    var immediate_byte_count: u8 = 0;
    if (value > max(u32)) immediate_byte_count = 8
    else if (value > max(u16)) immediate_byte_count = 4
    else if (value > max(u8)) immediate_byte_count = 2
    else immediate_byte_count = 1;

    assert(immediate_byte_count <= register_size);

    if (register == .A)
    {
        const opcode: u8 = 0x05 - @as(u8, @boolToInt(register_size == 1));
        // @TODO: work around not to deal with another encoding
        if (immediate_byte_count == register_size)
        {
            immediate_byte_count = register_size;
        }
        const immediate_bytes = std.mem.asBytes(&immediate.value)[0..immediate_byte_count];
        const array_of_slices = [_][]const u8 { std.mem.asBytes(&opcode), immediate_bytes };
        return Instruction.Resolved.new_by_components(array_of_slices[0..]);
    }
    else
    {
        const opcode: u8 = 0x81 - @as(u8, @boolToInt(register_size == 1));
        _ = opcode;
        if (immediate_byte_count == 1)
        {
            if (register_size > immediate_byte_count)
            {
                unreachable;
            }
            else
            {
                unreachable;
            }
        }
        else
        {
            assert(register_size == immediate_byte_count);
            unreachable;
        }
    }
}

fn inc_register(register: Encoding.Register, register_size: u8) Instruction
{
    const opcode = 0xff - @as(u8, @boolToInt(register_size == 1));
    const register_byte = 0xc0 | @enumToInt(register);
    var bytes = [_]u8 { opcode, register_byte };

    return Instruction.Resolved.new(bytes[0..]);
}

fn xor_register(register: Encoding.Register, size: u8) Instruction
{
    const mod = 0b11;
    const r_m = @enumToInt(register);

    const byte_slice = switch (size)
    {
        1 => unreachable,
        2 => unreachable,
        4 => blk4: 
        {
            const bytes = [_]u8 { 0x31, (mod << 6) | ((@enumToInt(register) & 0b111) << 3) | (r_m & 0b111)};
            break :blk4 bytes[0..];
        },
        8 => unreachable,
        else => unreachable,
    };

    return Instruction.Resolved.new(byte_slice);
}

const Instruction = struct
{
    const Self = @This();
    const max_bytes = 15;
    const max_bytes_before_operand_resolution = 3;

    resolution: Resolution,
    status: Resolution.Status,

    const Resolved = struct
    {
        bytes: [max_bytes]u8,
        size: u8,

        fn new(bytes: []const u8) callconv(.Inline) Instruction
        {
            assert(bytes.len <= max_bytes);

            return .{
                .resolution = .{
                    .resolved = .{
                        .bytes = blk: {
                            var result: [max_bytes]u8 = undefined;
                            std.mem.copy(u8, result[0..], bytes);
                            break :blk result;
                        },
                        .size = @intCast(u8, bytes.len),
                    }
                },
                .status = .resolved,
            };
        }

        fn new_bytes(bytes: [max_bytes]u8, byte_count: u8) callconv(.Inline) Instruction
        {
            assert(bytes.len <= max_bytes);

            return .{
                .resolution = .{
                    .resolved = .{
                        .bytes = bytes,
                        .size = byte_count,
                    }
                },
                .status = .resolved,
            };
        }

        pub fn new_by_components(byte_slices: []const []const u8) callconv(.Inline) Instruction
        {
            var offset: u8 = 0;
            
            return .{
                .resolution = .{
                    .resolved = .{
                        .bytes = blk: {
                            var result: [max_bytes]u8 = undefined;

                            for (byte_slices) |byte_slice|
                            {
                                std.mem.copy(u8, result[offset..], byte_slice);
                                offset += @intCast(u8, byte_slice.len);
                            }
                            assert(result.len <= max_bytes);

                            break :blk result;
                        },
                        .size = offset,
                    }
                },
                .status = .resolved,
            };
        }
    };

    const Unresolved = struct
    {
        bytes: [max_bytes_before_operand_resolution]u8,
        unknown_operand: UnknownOperand,

        const UnknownOperand = extern struct
        {
            index: u32,
            flags: u8,

            const Kind = enum(u8)
            {
                immediate,
                relative_global,
                relative_external,
                relative_label,
            };

            pub fn get_size(self: UnknownOperand) u8
            {
                return self.flags & 0xf;
            }
            
            pub fn set_size(self: *UnknownOperand, size: u8) void
            {
                self.flags = (self.flags & 0xf0) | size;
            }

            pub fn get_offset(self: UnknownOperand) u8
            {
                return (self.flags & 0x30) >> 4;
            }

            pub fn set_offset(self: *UnknownOperand, offset: u8) void
            {
                self.flags = (self.flags & 0xcf) | (offset << 4);
            }

            pub fn get_kind(self: UnknownOperand) Kind
            {
                return @intToEnum(Kind, (self.flags & 0xc0) >> 6);
            }

            pub fn set_kind(self: *UnknownOperand, kind: Kind) void
            {
                self.flags = (self.flags & 0x3f) | (@enumToInt(kind) << 6);
            }
        };

        fn new(known_bytes: []const u8, operand_index: u32, operand_kind: UnknownOperand.Kind, operand_size: u8, operand_offset: u8) callconv(.Inline) Instruction
        {
            assert(operand_offset <= max_bytes);
            assert(operand_size <= 8);

            return .  {
                .resolution = .{
                    .unresolved = .{
                        .bytes = blk: {
                            var result: [max_bytes_before_operand_resolution]u8 = undefined;
                            std.mem.copy(u8, result[0..], known_bytes);
                            break :blk result;
                        },
                        .unknown_operand = blk: {
                            var uk_op = Unresolved.UnknownOperand {
                                .index = operand_index,
                                .flags = undefined,
                            };
                            uk_op.set_kind(operand_kind);
                            uk_op.set_size(operand_size);
                            uk_op.set_offset(operand_offset);

                            break :blk uk_op;
                        },
                    }
                },
                .status = .unresolved,
            };
        }
    };

    const Resolution = extern union
    {
        resolved: Resolved,
        unresolved: Unresolved,

        const Status = enum(u8)
        {
            resolved,
            unresolved,
        };
    };


};

//const Instruction = struct
//{
    //const Self = @This();

    //bytes: []const u8,
    //id: ID,
    //resolved: bool,
    //operand: Operand, 

    //const ID = enum(u16)
    //{
        //call,
        //mov,
        //ret,
        //xor,
    //};

    //const Operand = struct
    //{
        //index: u32,
        //kind: Kind,
        //offset: u8,

        //const Kind = enum(u8)
        //{
            //none,
            //immediate,
            //relative_global,
            //relative_external,
        //};
    //};

    //fn create_resolved(id: ID, bytes: []const u8) Self
    //{
        //log("Appending instruction {}:\n", .{id});
        //for (bytes) |byte| log("{x:0>2} ", .{byte});
        //log("\n", .{});

        //return .
        //{
            //.id = id,
            //.resolved = true,
            //.bytes = bytes,
            //.operand = .
            //{
                //.offset = 0,
                //.kind = .none,
                //.index = 0,
            //},
        //};
    //}

    //fn create_unresolved_operand(id: ID, bytes: []const u8, operand_offset: u8, operand_kind: Operand.Kind, operand_index: u32) Self
    //{
        //return .
        //{
            //.id = id,
            //.resolved = false,
            //.bytes = bytes,
            //.operand = .
            //{
                //.offset = operand_offset,
                //.kind = operand_kind,
                //.index = operand_index
            //},
        //};
    //}
//};

const Label = union(enum)
{
    resolved: struct
    {
        target: u64,
    },
    unresolved: struct
    {
        locations: ArrayList(PatchLocation),
        instruction_index: u32,
    },

    const PatchLocation = struct
    {
        const BufferType = enum(u8)
        {
            code,
            data,
            rdata,
        };

        buffer_index: u32,
        next_instruction_offset: u16,
        buffer_type: BufferType,
    };
};

const BackwardPatch = struct
{
    code_buffer_offset: u32,
};

pub const Program = struct
{
    functions: []Function,
    data_buffer: ArrayList(u8),

    const Function = struct
    {
        instructions: ArrayList(Instruction),
        basic_block_instruction_offsets: ArrayList(u64),
        basic_block_buffer_offsets: ArrayList(u64),
        previous_patches: ArrayList(BackwardPatch),
        code_buffer_offset: u32,
        max_call_parameter_size: i32,
        terminator: Terminator,
        register_allocator: Register.Manager,
        stack_allocator: Stack.Manager,

        const Terminator = enum
        {
            noreturn,
            ret,
        };

        fn append_instruction(self: *Function, instruction: Instruction) void
        {
            log("Instruction index: {}\n", .{self.instructions.items.len});
            self.instructions.append(instruction) catch unreachable;
        }
    };

    fn estimate_max_code_size(self: *Program) u64
    {
        var total_instruction_count: u64 = 0;
        const aprox_fixed_instruction_count_per_function = 5;

        for (self.functions) |function|
        {
            total_instruction_count += aprox_fixed_instruction_count_per_function + function.instructions.items.len;
        }

        return total_instruction_count * Instruction.max_bytes;
    }

    pub fn encode_text_section_pe(self: *Program, allocator: *Allocator, text: *PE.Section, text_out: *PE.Section.Text.EncodingOutput, offset: *PE.Offset) void
    {
        text_out.patches = ArrayList(PE.Patch).init(allocator);

        const aproximate_code_size = std.mem.alignForward(self.estimate_max_code_size(), PE.file_alignment);
        text.buffer.ensureTotalCapacity(aproximate_code_size) catch unreachable;

        for (self.functions) |*function, function_i|
        {
            log("Encoding bytes for function #{}...\n", .{function_i});
            function.code_buffer_offset = @intCast(u32, text.buffer.items.len);

            for (function.previous_patches.items) |patch|
            {
                var pointer_writer = @ptrCast(* align(1) i32, &text.buffer.items[patch.code_buffer_offset]);
                pointer_writer.* = @intCast(i32, @intCast(i64, function.code_buffer_offset) - @intCast(i64, patch.code_buffer_offset + @sizeOf(i32)));
            }
           
            var add_rsp_at_epilogue = false;

            if (encode_frame_pointer)
            {
                switch (abi)
                {
                    .msvc =>
                    {
                        const stack_offset = function.stack_allocator.offset;
                        if (stack_offset > max(i8))
                        {
                            const sub_rsp = sub_rsp_s32(stack_offset)[0..];
                            std.debug.print("[{}] ", .{text.buffer.items.len});
                            for (sub_rsp) |byte|
                            {
                                std.debug.print("{x:0>2} ", .{byte});
                            }
                            std.debug.print("\n", .{});

                            text.buffer.appendSlice(sub_rsp) catch unreachable;
                            add_rsp_at_epilogue = true;
                        }
                        else if (stack_offset > 0)
                        {
                            const sub_rsp = sub_rsp_s8(@intCast(i8, stack_offset))[0..];
                            std.debug.print("[{}] ", .{text.buffer.items.len});
                            for (sub_rsp) |byte|
                            {
                                std.debug.print("{x:0>2} ", .{byte});
                            }
                            std.debug.print("\n", .{});

                            text.buffer.appendSlice(sub_rsp) catch unreachable;
                            add_rsp_at_epilogue = true;
                        }
                    },
                    .gnu =>
                    {
                        text.buffer.appendSlice(systemv_abi_prologue[0..]) catch unreachable;
                    },
                    else => panic("not implemented: {}\n", .{abi}),
                }
            }

            var current_block_offset: u32 = 0;

            const basic_block_count = function.basic_block_instruction_offsets.items.len;
            var post_function_patches_arraylist = ArrayList(ArrayList(u32)).initCapacity(allocator, basic_block_count) catch unreachable;
            post_function_patches_arraylist.items.len = basic_block_count;

            for (post_function_patches_arraylist.items) |*patch|
            {
                patch.* = ArrayList(u32).init(allocator);
            }

            var post_function_patches = post_function_patches_arraylist.items;

            var post_function_patch_count: u32 = 0;
            for (function.basic_block_instruction_offsets.items) |instruction_offset, i|
            {
                log("#{}: {}\n", .{i, instruction_offset});
            }
            log("Instruction count: {}\n", .{function.instructions.items.len});

            for (function.instructions.items) |instruction, instruction_i|
            {
                log("Writing instruction #{}\n", .{instruction_i});
                log("Current block offset: {}\n", .{current_block_offset});

                if (current_block_offset < function.basic_block_instruction_offsets.items.len)
                {
                    const instruction_offsets = function.basic_block_instruction_offsets.items[current_block_offset..];
                    const code_buffer_offset = text.buffer.items.len;

                    for (instruction_offsets) |current_block_instruction_offset|
                    {
                        if (current_block_instruction_offset > instruction_i) break;

                        assert(current_block_instruction_offset == instruction_i);
                        log("Appending block code buffer offset {} to block {}\n", .{code_buffer_offset, current_block_offset});
                        function.basic_block_buffer_offsets.append(code_buffer_offset) catch unreachable;

                        current_block_offset += 1;
                    }

                    //var current_block_instruction_offset = function.basic_block_instruction_offsets.items[current_block_offset];
                    //log("Current block instruction offset: {}\n", .{current_block_instruction_offset});

                    //while (current_block_instruction_offset == instruction_i)
                    //{
                        //const code_buffer_offset = text.buffer.items.len;
                        //current_block_offset += 1;
                        //if (current_block_offset < function.basic_block_instruction_offsets.items.len)
                        //{
                            //current_block_instruction_offset = function.basic_block_instruction_offsets.items[current_block_offset];
                        //}
                    //}
                }

                if (instruction.status == .resolved)
                {
                    const instruction_bytes = instruction.resolution.resolved.bytes[0..instruction.resolution.resolved.size];
                    //log("Resolved instruction. Appending:\n", .{});
                    //std.debug.print("[{}] ", .{text.buffer.items.len});
                    //for (instruction_bytes) |byte|
                    //{
                        //std.debug.print("{x:0>2} ", .{byte});
                    //}
                    //std.debug.print("\n", .{});
                    text.buffer.appendSlice(instruction_bytes) catch unreachable;
                }
                else
                {
                    const unresolved = instruction.resolution.unresolved;
                    const operand = unresolved.unknown_operand;
                    const operand_offset = operand.get_offset();
                    //log("Operand offset: {}\n", .{operand_offset});
                    const operand_size = operand.get_size();
                    const instruction_length = operand_offset + operand_size;
                    const from = text.buffer.items.len + instruction_length;
                    const code_buffer_offset = @intCast(u32, text.buffer.items.len + operand_offset);
                    const known_bytes = unresolved.bytes[0..operand_offset];
                    //std.debug.print("Unresolved instruction. Appending known bytes:\n", .{});
                    //std.debug.print("[{}] ", .{text.buffer.items.len});
                    //for (known_bytes) |byte|
                    //{
                        //std.debug.print("{x:0>2} ", .{byte});
                    //}

                    //std.debug.print("\n", .{});
                    text.buffer.appendSlice(known_bytes) catch unreachable;

                    const operand_kind = operand.get_kind();
                    log("Operand kind: {}\n", .{operand_kind});

                    switch (operand_kind)
                    {
                        .relative_global =>
                        {
                            if (operand.index <= function_i)
                            {
                                const target = self.functions[operand.index].code_buffer_offset;
                                const rel32 = @intCast(i32, @intCast(i64, target) - @intCast(i64, from));
                                text.buffer.appendSlice(std.mem.asBytes(&rel32)) catch unreachable;
                            }
                            else
                            {
                                text.buffer.appendNTimes(0xcc, operand_size) catch unreachable;
                                self.functions[operand.index].previous_patches.append(.{ .code_buffer_offset = code_buffer_offset }) catch unreachable;
                            }
                        },
                        .relative_external =>
                        {
                            text_out.patches.append(.
                                {
                                    .section_buffer_index_to = code_buffer_offset,
                                    .section_buffer_index_from = operand.index,
                                    .section_to_write_to = .@".text",
                                    .section_to_read_from = .@".rdata",
                                }) catch unreachable;
                            text.buffer.appendNTimes(0xcc, operand_size) catch unreachable;
                        },
                        .relative_label =>
                        {
                            const basic_block_index = operand.index;

                            if (basic_block_index < current_block_offset)
                            {
                                //log("Address of the jump is already known\n", .{});
                                const target = function.basic_block_buffer_offsets.items[basic_block_index];
                                const rel32 = @intCast(i32, @intCast(i64, target) - @intCast(i64, from));
                                const rel32_bytes = std.mem.asBytes(&rel32);
                                //log("Resolved 4-byte relative:\n", .{});
                                //for (rel32_bytes) |byte| 
                                //{
                                    //log("{x}\n", .{byte});
                                //}
                                text.buffer.appendSlice(rel32_bytes) catch unreachable;
                            }
                            else
                            {
                                log("Address of the jump is not yet known. Appending a patch to block #{}. Offset to be patched: 0x{x}\n", .{basic_block_index, code_buffer_offset});
                                text.buffer.appendNTimes(0xcc, operand_size) catch unreachable;
                                post_function_patches[basic_block_index].append(code_buffer_offset) catch unreachable;
                                post_function_patch_count += 1;
                            }
                        },
                        else => unreachable,
                    }
                }
            }

            log("{}\n", .{current_block_offset});

            log("\n\n\nResolving function patches for {} blocks\n", .{basic_block_count});
            var resolution_count: u32 = 0;
            for (function.basic_block_buffer_offsets.items) |block_buffer_offset, block_index|
            {
                log("Basic block #{}\n", .{block_index});

                for (post_function_patches[block_index].items) |patch_buffer_index|
                {
                    log("Resolving jump to basic block... Patch offset 0x{x}\n", .{patch_buffer_index});
                    resolution_count += 1;
                    var pointer_writer = @ptrCast(* align(1) i32, &text.buffer.items[patch_buffer_index]);
                    const value_to_write = @intCast(i32, @intCast(i64, block_buffer_offset) - @intCast(i64, patch_buffer_index + @sizeOf(i32)));
                    log("Value to write:\n", .{});
                    for (std.mem.asBytes(&value_to_write)) |byte|
                    {
                        log("0x{x}\n", .{byte});
                    }
                    pointer_writer.* = value_to_write;
                }
            }

            assert(resolution_count == post_function_patch_count);
            log("Resolution count for this function: {}\n", .{resolution_count});

            if (add_rsp_at_epilogue)
            {
                if (function.terminator == .ret)
                {
                    switch (abi)
                    {
                        .msvc =>
                        {
                            const stack_offset = function.stack_allocator.offset;
                            if (stack_offset > max(i8))
                            {
                                text.buffer.appendSlice(add_rsp_s32(stack_offset)[0..]) catch unreachable;
                            }
                            else if (stack_offset > 0)
                            {
                                text.buffer.appendSlice(add_rsp_s8(@intCast(i8, stack_offset))[0..]) catch unreachable;
                            }
                        },
                        .gnu =>
                        {
                            unreachable;
                        },
                        else => panic("not implemented: {}\n", .{abi}),
                    }
                }
            }

            if (function.terminator == .ret)
            {
                std.debug.print("[{}] {x:0>2}\n", .{text.buffer.items.len, ret_bytes[0]});
                text.buffer.append(ret_bytes[0]) catch unreachable;
            }
            else if (function.terminator == .noreturn)
            {
                // Append Int3
                const int3 = 0xcc;
                std.debug.print("[{}] {x:0>2}\n", .{text.buffer.items.len, int3});
                text.buffer.append(int3) catch unreachable;
            }
            else unreachable;
        }

        const code_size = @intCast(u32, text.buffer.items.len);
        log("Code size: {} bytes. Aproximation: {} bytes\n", .{code_size, aproximate_code_size});
        assert(aproximate_code_size >= code_size);
        text.header.misc.virtual_size = code_size;
        text.header.size_of_raw_data = @intCast(u32, alignForward(code_size, PE.file_alignment));

        offset.after_size(text.header.size_of_raw_data);
    }
};

var abi: std.Target.Abi = undefined;
const encode_frame_pointer = true;

const Labels = struct
{
    function_index: u32,
    instruction_index: u16,
    byte_offset: u8,
    operand_size: u8,

    fn from_instruction(function_index: u32, block_index: u16, byte_offset: u8, operand_size: u8) Label
    {
        return Label
        {
            .function_index = function_index,
            .block_index = block_index,
            .byte_offset = byte_offset,
            .operand_size = operand_size,
        };
    }

    fn from_function(function_index: u32) Label
    {
        return Label.from_instruction(function_index, 0, 0, 0);
    }
};

fn get_type_size(T: Type) u64
{
    return switch (T.get_ID())
    {
        .integer => Type.Integer.get_bit_count(T) >> 3,
        else => panic("{}\n", .{T.get_ID()}),
    };
}

const Stack = struct
{
    const Store = struct
    {
        const ArrayType = [Register.count]Stack.Store;
    };

    // @TODO: figure out proper signed/unsigned types
    const Manager = struct
    {
        allocations: ArrayList(Allocation),
        offset: i32,

        fn new(allocator: *Allocator) Stack.Manager
        {
            return .
            {
                .allocations = ArrayList(Allocation).init(allocator),
                .offset = 0,
            };
        }

        // @TODO: work on alignment
        fn allocate(self: *Stack.Manager, size: u32, alloca_i: ?u32) i32
        {
            const allocation_offset = @intCast(i32, alignForward(@intCast(u32, self.offset), size));
            self.offset += @intCast(i32, size);
            self.allocations.append(.
                {
                    .size = size,
                    .alignment = size,
                    .offset = allocation_offset,
                    .alloca = alloca_i,
                }) catch unreachable;

            return -self.offset;
        }
        
        fn get_allocation(self: *Stack.Manager, alloca_i: u32) Allocation
        {
            log("Searching for alloca #{}\n", .{alloca_i});

            for (self.allocations.items) |allocation|
            {
                if (allocation.alloca) |stack_allocator_alloca|
                {
                    if (stack_allocator_alloca == alloca_i)
                    {
                        return allocation;
                    }
                }
            }

            panic("Not found\n", .{});
        }

        const Allocation = struct
        {
            size: u32,
            alignment: u32,
            offset: i32,
            alloca: ?u32,
        };
    };
};

const Register = struct
{
    value: IR.Reference,
    size: u8,

    const count = 16;
    const ArrayType = [Register.count]Register; 

    const Manager = struct
    {
        registers: Register.ArrayType, 
        argument_registers: []const Encoding.Register,

        const Self = @This();

        fn new(argument_registers: []const Encoding.Register) Self
        {
            const result = Self
            {
                .registers = std.mem.zeroes(Register.ArrayType),
                .argument_registers = argument_registers,
            };

            return result;
        }

        fn register_allocation(self: *Register.Manager, dst_register: u64, value: IR.Reference, byte_count: u8) void
        {
            log("\n\n\n\nAllocating {} bytes in {}\n\n\n\n\n", .{byte_count, @intToEnum(Encoding.Register, dst_register)});
            var reg = &self.registers[dst_register];
            reg.size = byte_count;
            reg.value = value;
        }

        fn allocate(self: *Register.Manager, value: IR.Reference, byte_count: u8) Encoding.Register
        {
            for (self.registers) |*reg, reg_i|
            {
                if (reg.size == 0)
                {
                    self.register_allocation(reg_i, value, byte_count);
                    return @intToEnum(Encoding.Register, @intCast(u8, reg_i));
                }
            }

            panic("Registers are full\n", .{});
        }

        fn allocate_argument(self: *Register.Manager, argument_reference: IR.Reference, byte_count: u8) Encoding.Register
        {
            for (self.argument_registers) |r|
            {
                const register_index = @enumToInt(r);

                var register = &self.registers[register_index];
                if (register.size == 0)
                {
                    self.register_allocation(register_index, argument_reference, byte_count);
                    return r;
                }
            }

            panic("No argument register ready\n", .{});
        }

        fn allocate_return(self: *Register.Manager, reference: IR.Reference, byte_count: u8) Encoding.Register
        {
            assert(byte_count <= 8);
            const register_a = @enumToInt(Encoding.Register.A);
            var register = &self.registers[register_a];
            if (register.size == 0)
            {
                self.register_allocation(register_a, reference, byte_count);

                return Encoding.Register.A;
            }

            panic("Return register A is busy\n", .{});
        }

        const Result = struct
        {
            value: Encoding.Register,
            size: u8,
        };

        fn get_register(self: *Register.Manager, reference: IR.Reference) ?Result
        {
            for (self.registers) |reg, reg_i|
            {
                if (reg.value.value == reference.value and reg.size > 0)
                {
                    const result = Result { .value = @intToEnum(Encoding.Register, @intCast(u8, reg_i)), .size = reg.size };
                    return result;
                }
            }

            return null;
        }

        fn spill_registers_before_call(self: *Register.Manager) State
        {
            log("Saving registers before call...\n", .{});

            var saved_register_count: u32 = 0;

            var registers = std.mem.zeroes(Register.ArrayType);

            for (self.registers) |register, register_i|
            {
                if (register.size > 0 and must_save(@intToEnum(Encoding.Register, @intCast(u8, register_i))))
                {
                    registers[register_i] = register;
                    saved_register_count += 1;
                }
            }

            log("Registers to be saved: {}\n", .{saved_register_count});

            if (saved_register_count > 0)
            {
                for (registers) |register|
                {
                    if (register.size > 0)
                    {
                        panic("IR value: {}\n", .{register.value});
                    }
                }
            }

            if (saved_register_count > 0) unreachable;
            return std.mem.zeroes(State);
        }

        fn alter_allocation(self: *Self, register: Encoding.Register, new_reference: IR.Reference) void
        {
            var dst_register = &self.registers[@enumToInt(register)];
            assert(dst_register.size > 0);
            dst_register.value = new_reference;
        }

        fn reset(self: *Self) void
        {
            self.registers = std.mem.zeroes(Register.ArrayType);
        }

        const State = struct
        {
            registers: Register.ArrayType,
            stack_stores: Stack.Store.ArrayType,
            saved_register_count: u32,
        };


        fn must_save(register: Encoding.Register) bool
        {
            return switch (register)
            {
                .A => true,
                .SP => false,
                else => panic("ni: {}\n", .{register}),
            };
        }
    };

    const ID = enum(u8)
    {
        A = 0,
        C = 1,
        D = 2,
        B = 3,
        // SP = 4,
        // BP = 5,
        // SI = 6,
        // DI = 7,

        r8 = 8,
        r9 = 9,
        r10 = 10,
        r11 = 11,
        r12 = 12,
        r13 = 13,
        r14 = 14,
        r15 = 15,

        pub const AH: u8 = 4;
        pub const CH: u8 = 5;
        pub const DH: u8 = 6;
        pub const BH: u8 = 7;
    };
};

fn process_load(program: *const IR.Program, function: *Program.Function, load_reference: IR.Reference) void
{
    const load_index = load_reference.get_index(); 
    const load = program.instructions.load[load_index];
    assert(IR.Instruction.get_ID(load.pointer) == .alloca);

    const uses = program.get_uses(.load, load_index);
    log("Load use count: {}\n", .{uses.len});
    assert(uses.len == 1);

    var return_register = false;

    const use = uses[0];
    switch (use.get_ID())
    {
        .instruction =>
        {
            const id = IR.Instruction.get_ID(use);
            switch (id)
            {
                // Do nothing
                .icmp =>
                {
                    log("Load use: {}. Skipping...\n", .{id});
                    return;
                },
                .add =>
                {
                    const add = &program.instructions.add[use.get_index()];
                    if (add.left.value == use.value)
                    {
                        panic("not implemented\n", .{});
                    }
                    else
                    {
                        return;
                    }
                },
                .ret =>
                {
                    return_register = true;
                },
                else => panic("{}\n", .{IR.Instruction.get_ID(use)}),
            }
        },
        else => panic("{}\n", .{use.get_ID()}),
    }

    const allocation = function.stack_allocator.get_allocation(load.pointer.get_index());
    assert(allocation.size <= 8);
    const register_size = @intCast(u8, allocation.size);

    const load_register = 
        if (return_register)
            function.register_allocator.allocate_return(load_reference, register_size)
        else
            function.register_allocator.allocate(load_reference, register_size);

    const mov_register_stack = mov_register_indirect(load_register, @intCast(u8, allocation.size), stack_register, allocation.offset);
    function.append_instruction(mov_register_stack);
}

fn process_icmp(program: *const IR.Program, function: *Program.Function, icmp_reference: IR.Reference) void
{
    const icmp = program.instructions.icmp[icmp_reference.get_index()];
    var should_load_first_argument = false;

    var first_operand_kind: OperandKind = undefined;
    var second_operand_kind: OperandKind = undefined;

    const first_operand_allocation = blk:
    {
        switch (icmp.left.get_ID())
        {
            .instruction =>
            {
                const instr_id = IR.Instruction.get_ID(icmp.left);
                switch (instr_id)
                {
                    .load =>
                    {
                        first_operand_kind = .stack;
                        const load = program.instructions.load[icmp.left.get_index()];
                        const allocation = function.stack_allocator.get_allocation(load.pointer.get_index());
                        break :blk allocation;
                    },
                    else => panic("ni: {}\n", .{instr_id}),
                }
            },
            else => panic("ni: {}\n", .{icmp.left.get_ID()}),
        }
    };

    switch (icmp.right.get_ID())
    {
        .constant =>
        {
            switch (IR.Constant.get_ID(icmp.right))
            {
                .int =>
                {
                    second_operand_kind = .immediate;

                    const integer_literal = program.integer_literals[icmp.right.get_index()];
                    assert(first_operand_allocation.size == @sizeOf(i32));

                    const cmp = blk:
                    {
                        if (first_operand_kind == .stack)
                        {
                            break :blk cmp_indirect_immediate(stack_register, first_operand_allocation.offset, @intCast(u8, first_operand_allocation.size), integer_literal);
                        }
                        else unreachable;
                    };

                    function.append_instruction(cmp);
                },
                else => panic("ni: {}\n", .{IR.Constant.get_ID(icmp.right)}),
            }
        },
        else => panic("ni: {}\n", .{icmp.right.get_ID()}),
    }

    if (should_load_first_argument)
    {
        unreachable;
    }
}

fn process_add(program: *const IR.Program, function: *Program.Function, add_reference: IR.Reference) void
{
    const add = program.instructions.add[add_reference.get_index()];
    var should_load_first_argument = false;

    var first_operand_kind: OperandKind = undefined;
    var second_operand_kind: OperandKind = undefined;

    const first_operand_allocation = blk:
    {
        switch (add.left.get_ID())
        {
            .instruction =>
            {
                const instr_id = IR.Instruction.get_ID(add.left);
                switch (instr_id)
                {
                    .load =>
                    {
                        first_operand_kind = .stack;
                        const load = program.instructions.load[add.left.get_index()];
                        const allocation = function.stack_allocator.get_allocation(load.pointer.get_index());
                        break :blk allocation;
                    },
                    else => panic("ni: {}\n", .{instr_id}),
                }
            },
            else => panic("ni: {}\n", .{add.left.get_ID()}),
        }
    };

    log("First operand: {}\n", .{first_operand_kind});

    switch (add.right.get_ID())
    {
        .constant =>
        {
            switch (IR.Constant.get_ID(add.right))
            {
                .int =>
                {
                    second_operand_kind = .immediate;

                    const integer_literal = program.integer_literals[add.right.get_index()];
                    assert(first_operand_allocation.size == @sizeOf(i32));
                    if (integer_literal.value == 0) return;

                    // @TODO: be more subtle about register allocating the first operand
                    if (first_operand_kind == .stack)
                    {
                        const register_size = @intCast(u8, first_operand_allocation.size);
                        const load_register = function.register_allocator.allocate(add.right, register_size);
                        _ = mov_register_indirect(load_register, @intCast(u8, first_operand_allocation.size), stack_register, first_operand_allocation.offset);
                        function.register_allocator.alter_allocation(load_register, add_reference);

                        if (integer_literal.value == 1)
                        {
                            const inc_reg = inc_register(load_register, register_size);
                            function.append_instruction(inc_reg);
                        }
                        else
                        {
                            log("Add register immediate\n", .{});
                            const add_register_literal = add_register_immediate(load_register, register_size, integer_literal);
                            function.append_instruction(add_register_literal);
                        }
                    }
                    else unreachable;
                },
                else => panic("ni: {}\n", .{IR.Constant.get_ID(add.right)}),
            }
        },
        .instruction =>
        {
            switch (IR.Instruction.get_ID(add.right))
            {
                .load =>
                {
                    second_operand_kind = .stack;

                    const load = program.instructions.load[add.right.get_index()];
                    const second_operand_allocation = function.stack_allocator.get_allocation(load.pointer.get_index());

                    if (first_operand_kind == .stack)
                    {
                        // move to a register first operand if it is also a stack operand
                        log("First operand is also stack, we have to allocate\n", .{});
                        const load_register = function.register_allocator.allocate(add.right, @intCast(u8, first_operand_allocation.size));
                        const mov_register_stack = mov_register_indirect(load_register, @intCast(u8, first_operand_allocation.size), stack_register, first_operand_allocation.offset);
                        function.append_instruction(mov_register_stack);
                        const add_register_stack = add_register_indirect(load_register, @intCast(u8, first_operand_allocation.size), stack_register, second_operand_allocation.offset);
                        function.register_allocator.alter_allocation(load_register, add_reference);
                        function.append_instruction(add_register_stack);
                    }
                    else
                    {
                        unreachable;
                    }
                },
                else => panic("{}\n", .{IR.Instruction.get_ID(add.right)}),
            }
        },
        else => panic("ni: {}\n", .{add.right.get_ID()}),
    }

    log("Second operand kind: {}\n", .{second_operand_kind});

    if (should_load_first_argument)
    {
        unreachable;
    }
}

const JmpOpcode = enum(u8)
{
    jge = 0x7d,
    jne = 0x75,
    jle = 0x7e,

    pub fn get(compare_id: IR.Instruction.ICmp.ICmpID) JmpOpcode
    {
        const result = switch (compare_id)
        {
            .slt => JmpOpcode.jge,
            .eq => JmpOpcode.jne,
            .sgt => JmpOpcode.jle,
            else => panic("ni: {}\n", .{compare_id}),
        };

        log("For compare instruction {}, we should use jcc opcode {}\n", .{compare_id, result});

        return result;
    }
};

var stack_register: Encoding.Register = undefined;

pub fn encode(allocator: *Allocator, program: *const IR.Program, executable_filename: []const u8, target: std.Target) void
{
    abi = target.abi;
    const os = target.os.tag;
    if (os == .windows) abi = .msvc;
    const argument_registers =
        if (abi == .msvc)
            msvc_argument_registers[0..]
        else if (abi == .gnu)
            system_v_argument_registers[0..]
        else
            panic("Not implemented: {}\n", .{abi});

    stack_register =
        if (abi == .msvc) Encoding.Register.SP
        else if (abi == .gnu) Encoding.Register.BP
        else panic("NI: {}\n", .{abi});

    const function_count = program.functions.len;
    assert(function_count > 0);

    //assert(std.mem.eql(program.functions[0].declaration.name, "entry"));

    var functions = ArrayList(Program.Function).initCapacity(allocator, function_count) catch unreachable;
    var data_buffer = ArrayList(u8).init(allocator);

    for (program.functions) |*function|
    {
        var stack_allocator = Stack.Manager.new(allocator);
        const basic_block_count = function.basic_blocks.len;
        var labels = ArrayList(Label).initCapacity(allocator, basic_block_count) catch unreachable;
        labels.resize(basic_block_count) catch unreachable;

        const entry_block_index = function.basic_blocks[0];
        const entry_block = program.basic_blocks[entry_block_index];

        for (entry_block.instructions.items) |instruction|
        {
            if (IR.Instruction.get_ID(instruction) == .alloca)
            {
                const alloca_size = @intCast(u32, get_alloca_size(program, instruction));
                _ = stack_allocator.allocate(alloca_size, instruction.get_index());
            }
        }

        for (labels.items) |*label|
        {
            label.* = Label
            {
                .unresolved = .
                {
                    .locations = ArrayList(Label.PatchLocation).init(allocator),
                    .instruction_index = 0,
                },
            };
        }

        functions.append(.
            {
                .instructions = ArrayList(Instruction).init(allocator),
                .basic_block_instruction_offsets  = ArrayList(u64).initCapacity(allocator, basic_block_count) catch unreachable,
                .basic_block_buffer_offsets  = ArrayList(u64).initCapacity(allocator, basic_block_count) catch unreachable,
                .max_call_parameter_size = 0,
                .previous_patches = ArrayList(BackwardPatch).init(allocator),
                .code_buffer_offset = 0,
                .terminator = .noreturn,
                .register_allocator = Register.Manager.new(argument_registers),
                .stack_allocator = stack_allocator,
            }) catch unreachable;

        // @TODO:
        // resize and initialize labels
        //   unreachable;
    }

    for (program.functions) |*ir_function, ir_function_i|
    {
        log("\n\n======================\n[FUNCTION #{}]\n======================\n\n", .{ir_function_i});
        var function = &functions.items[ir_function_i];

        log("Basic blocks:\n", .{});
        for (ir_function.basic_blocks) |basic_block, basic_block_i|
        {
            log("[{}]: #{}\n", .{basic_block_i, basic_block});
        }
        std.debug.print("\n", .{});


        for (ir_function.basic_blocks) |basic_block_index, basic_block_i|
        {
            log("Processing basic block #{} (global index {})\n", .{basic_block_i, basic_block_index});
            const basic_block = program.basic_blocks[basic_block_index];
            assert(basic_block.instructions.items.len > 0);
            // @TODO:
            // reset register allocator
            //const function_offset = @intCast(u32, function.instructions.items.len);

            // Reset each time a basic block is processed
            function.register_allocator.reset();

            const instruction_offset = function.instructions.items.len;
            function.basic_block_instruction_offsets.appendAssumeCapacity(instruction_offset);

            for (basic_block.instructions.items) |instruction|
            {
                const instruction_id = IR.Instruction.get_ID(instruction);
                const instruction_index = instruction.get_index();

                log("\n\nProcessing {}...\n\n", .{instruction_id});

                switch (instruction_id)
                {
                    .call =>
                    {
                        const call = program.instructions.call[instruction_index];
                        const callee = call.callee;
                        const callee_id = callee.get_ID();
                        var callee_index = callee.get_index();

                        const argument_count = call.arguments.len;

                        var function_type: Type.Function = undefined;
                        const call_instruction = blk:
                        {
                            switch (callee_id)
                            {
                                .global_function =>
                                {
                                    assert(callee_index <= max(i32));
                                    function_type = program.functions[callee_index].declaration.type;
                                    break :blk call_rel32(callee_index);
                                },
                                .external_function =>
                                {
                                    const external_function = program.external.functions[callee_index];
                                    function_type = external_function.declaration.type;
                                    callee_index = external_function.index.to_u32();
                                    break :blk call_rip_rel32(callee_index);
                                },
                                else => unreachable,
                            }
                        };

                        assert(argument_count <= function.register_allocator.argument_registers.len);
                        var register_allocator_state = function.register_allocator.spill_registers_before_call();
                        for (call.arguments) |argument, argument_i|
                        {
                            const argument_id = argument.get_ID();
                            const argument_array_index = argument.get_index();
                            const argument_type = function_type.argument_types[argument_i];

                            switch (argument_id)
                            {
                                .constant =>
                                {
                                    const constant_id = IR.Constant.get_ID(argument);

                                    switch (constant_id)
                                    {
                                        .int =>
                                        {
                                            if (argument_type.get_ID() != .integer)
                                            {
                                                codegen_error("Expected integer\n", .{});
                                            }

                                            const integer_literal = program.integer_literals[argument_array_index];
                                            //@TODO: typecheck signedness
                                            assert(!integer_literal.signed);
                                            const literal = integer_literal.value;

                                            const integer_bit_count = Type.Integer.get_bit_count(argument_type);
                                            // assert n % 8 == 0
                                            assert((integer_bit_count & 0x7) == 0);
                                            const integer_byte_count = @truncate(u8, integer_bit_count >> 3);
                                            const argument_register = function.register_allocator.allocate_argument(argument, integer_byte_count);

                                            if (literal == 0)
                                            {
                                                // do xor
                                                function.append_instruction(xor_register(argument_register, integer_byte_count));
                                            }
                                            else
                                            {
                                                // do mov
                                                unreachable;
                                            }
                                        },
                                        else => panic("Constant id: {}\n", .{constant_id}),
                                    }
                                },
                                else => panic("Arg id: {}\n", .{argument_id}),
                            }
                        }

                        function.append_instruction(call_instruction);

                        // @TODO: do we need to do this?
                        // @TODO: resolve callee
                        //var callee_function = functions[callee_index];

                        var parameter_stack_size = @intCast(i32, std.math.max(4, argument_count) * 8);

                        // @TODO: use count
                        if (call.type.value != Type.Builtin.void_type.value and call.type.value != Type.Builtin.noreturn_type.value)
                        {
                            const return_size = @intCast(i32, get_type_size(call.type));
                            if (abi == .msvc and return_size > 8)
                            {
                                parameter_stack_size += return_size;
                                unreachable;
                            }
                        }
                        else
                        {
                            // @TODO: assert use count == 0
                        }

                        if (abi == .msvc)
                        {
                            function.max_call_parameter_size = std.math.max(function.max_call_parameter_size, parameter_stack_size);
                        }

                        if (register_allocator_state.saved_register_count > 0 )
                        {
                            // @TODO: restore registers
                            unreachable;
                        }
                    },
                    .ret =>
                    {
                        const ret = program.instructions.ret[instruction_index];

                        if (ret.type.value == Type.Builtin.noreturn_type.value)
                        {
                            codegen_error("Noreturn type should not return ever\n", .{});
                        }

                        if (ret.type.value != Type.Builtin.void_type.value)
                        {
                            const return_expr = ret.value;
                            const return_expr_id = return_expr.get_ID();
                            const return_expr_index = return_expr.get_index();
                            switch (return_expr_id)
                            {
                                .constant =>
                                {
                                    const constant_id = IR.Constant.get_ID(return_expr);
                                    switch (constant_id)
                                    {
                                        .int =>
                                        {
                                            if (ret.type.get_ID() != .integer)
                                            {
                                                codegen_error("Expected type: {}\n", .{ret.type.get_ID()});
                                            }
                                            const bit_count = Type.Integer.get_bit_count(ret.type);
                                            assert((bit_count & 0b111) == 0);
                                            const byte_count = bit_count >> 3;
                                            const integer_literal = program.integer_literals[return_expr_index];
                                            if (integer_literal.value != 0)
                                            {
                                                if (!integer_literal.signed)
                                                {
                                                    const register = function.register_allocator.allocate_return(return_expr, @intCast(u8, byte_count));
                                                    const ret_mov = mov_register_literal(register, integer_literal.value, byte_count);
                                                    function.append_instruction(ret_mov);
                                                }
                                                else
                                                {
                                                    unreachable;
                                                }
                                            }
                                            else
                                            {
                                                unreachable;
                                            }
                                        },
                                        else => panic("Constant ID: {}\n", .{constant_id}),
                                    }
                                },
                                .instruction =>
                                {
                                    const ret_expr_instruction_id = IR.Instruction.get_ID(return_expr);
                                    assert(ret_expr_instruction_id != .alloca);
                                    switch (ret_expr_instruction_id)
                                    {
                                        .load =>
                                        {
                                            const allocated_register = function.register_allocator.get_register(return_expr) orelse unreachable;
                                            
                                            if (allocated_register.value != .A)
                                            {
                                                panic("Badly allocated register\n", .{});
                                            }
                                        },
                                        else => panic("Instruction: {}\n", .{ret_expr_instruction_id}),
                                    }
                                },
                                else => panic("RE: {}\n", .{return_expr_id}),
                            }
                        }

                        function.terminator = .ret;
                    },
                    .alloca =>
                    {
                        log("Alloca. Ignoring...\n", .{});
                    },
                    .store =>
                    {
                        const store = program.instructions.store[instruction_index];
                        const store_uses = program.instruction_uses[@enumToInt(IR.Instruction.ID.store)][instruction_index];
                        const store_use_count = store_uses.items.len;
                        log("Store use count: {}\n", .{store_use_count});

                        // assert this is an alloca
                        assert(store.pointer.get_ID() == .instruction);
                        assert(IR.Instruction.get_ID(store.pointer) == .alloca);
                        const alloca_i = store.pointer.get_index(); 
                        const alloca = program.instructions.alloca[alloca_i];
                        const stack_allocation = function.stack_allocator.get_allocation(alloca_i);

                        const store_value_id = store.value.get_ID();
                        switch (store_value_id)
                        {
                            .constant =>
                            {
                                const constant_id = IR.Constant.get_ID(store.value);
                                switch (constant_id)
                                {
                                    .int =>
                                    {
                                        const integer_literal = program.integer_literals[store.value.get_index()];
                                        const alloca_type = alloca.alloca_type;
                                        if (alloca_type.get_ID() != .integer)
                                        {
                                            codegen_error("Expected type {}\n", .{alloca_type.get_ID()});
                                        }

                                        const bit_count = Type.Integer.get_bit_count(alloca_type);
                                        assert((bit_count & 0b111) == 0);
                                        const byte_count = bit_count >> 3;

                                        if (!integer_literal.signed)
                                        {
                                            log("Allocation offset: {}\n", .{stack_allocation.offset});
                                            const mov_stack_literal = mov_indirect_immediate_unsigned(stack_register, stack_allocation.offset, integer_literal.value, byte_count);
                                            function.append_instruction(mov_stack_literal);
                                        }
                                        else
                                        {
                                            unreachable;
                                        }
                                    },
                                    else => panic("Constant ID: {}\n", .{constant_id}),
                                }
                            },
                            .instruction =>
                            {
                                const store_instruction_id = IR.Instruction.get_ID(store.value);
                                const register_allocation = switch (store_instruction_id)
                                {
                                    .add => blk:
                                    {
                                        break :blk function.register_allocator.get_register(store.value) orelse unreachable;
                                    },
                                    else => panic("{}\n", .{store_instruction_id}),
                                };
                                const mov_stack_register = mov_indirect_register(stack_register, stack_allocation.offset, @intCast(u8, stack_allocation.size), register_allocation.value, register_allocation.size);
                                function.append_instruction(mov_stack_register);
                            },
                            else => panic("Store value id: {}\n", .{store_value_id}),
                        }
                    },
                    .load =>
                    {
                        process_load(program, function, instruction);
                    },
                    .br =>
                    {
                        const br = program.instructions.br[instruction_index];
                        // is conditional
                        if (br.condition) |branch_condition|
                        {
                            const branch_condition_id = IR.Instruction.get_ID(branch_condition);
                            switch (branch_condition_id)
                            {
                                .icmp =>
                                {
                                    const icmp_id = program.instructions.icmp[branch_condition.get_index()].id;
                                    const jmp_opcode = JmpOpcode.get(icmp_id);
                                    if (program.get_block_function_index(br.dst_basic_block) != program.get_block_function_index(ir_function.basic_blocks[basic_block_i + 1]))
                                    {
                                        unreachable;
                                    }

                                    assert(br.dst_basic_block_false != null);
                                    const conditional_jmp = jcc_rel32(jmp_opcode, program.get_block_function_index(br.dst_basic_block_false.?));
                                    function.append_instruction(conditional_jmp);
                                },
                                else => panic("NI: {}\n", .{branch_condition_id}),
                            }
                        }
                        else
                        {
                            const br_function_basic_block = program.get_block_function_index(br.dst_basic_block);
                            const next_function_basic_block = program.get_block_function_index(ir_function.basic_blocks[basic_block_i + 1]);

                            if (br_function_basic_block != next_function_basic_block)
                            {
                                const unconditional_jmp = jmp_rel32(br_function_basic_block, .relative_label);
                                function.append_instruction(unconditional_jmp);
                            }
                            else
                            {
                                log("We are not encoding the jump because the target block and the next block are the same\n", .{});
                            }
                        }
                    },
                    .icmp =>
                    {
                        process_icmp(program, function, instruction);
                    },
                    .add =>
                    {
                        process_add(program, function, instruction);
                    },
                    else => panic("Not implemented: {}\n", .{instruction_id}),
                }
            }
        }

        // function end
        switch (abi)
        {
            .msvc =>
            {
                log("Initial stack offset: {}\n", .{function.stack_allocator.offset});
                log("Max call parameter stack size: {}\n", .{function.max_call_parameter_size});
                const alignment = 0x8;
                function.stack_allocator.offset += function.max_call_parameter_size;
                // @TODO: this can cause bugs
                function.stack_allocator.offset = @boolToInt(function.stack_allocator.offset > 0) * (@intCast(i32, alignForward(@intCast(u32, function.stack_allocator.offset), alignment)) + 0x10);
                log("Stack offset: 0x{x:0>2}\n", .{function.stack_allocator.offset});
            },
            else => panic("not implemented: {}\n", .{abi}),
        }
    }

    var executable = Program
    {
        .functions = functions.items,
        .data_buffer = data_buffer,
    };

    switch (os)
    {
        .windows =>
        {
            PE.write(allocator, &executable, executable_filename, program.external, target);
        },
        else => panic("OS {} not implemented\n", .{os}),
    }
}
