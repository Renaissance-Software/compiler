const std = @import("std");
const ArrayList = std.ArrayList;
const alignForward = std.mem.alignForward;
const assert = std.debug.assert;
const panic = std.debug.panic;
const max = std.math.maxInt;

const Type = @import("../../type.zig");
const Parser = @import("../../parser.zig");
const IR = @import("../../ir.zig");
const Compiler = @import("../../compiler.zig");
const Codegen = @import("../../codegen.zig");
const PE = @import("../pe.zig");
const Import = Codegen.Import;
const Semantics = @import("../../semantics.zig");

pub fn log(comptime format: []const u8, arguments: anytype) void
{
    Compiler.log(.x86_64, "[CODEGEN] " ++ format, arguments);
}

fn codegen_error(comptime format: []const u8, arguments: anytype) noreturn
{
    panic(format, arguments);
}

const system_v_argument_registers = [_]Register.ID
{
    .DI,
    .SI,
    .C,
    .r8,
    .r9,
};

const msvc_argument_registers = [_]Register.ID
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

pub fn lea_indirect(register: Register.ID, size: u8, indirect_register: Register.ID, indirect_offset: i32) Instruction
{
    assert(size == 8);
    const rex_byte = 0x48;
    const opcode = 0x8d;
    const indirect_offset_not_zero = indirect_offset != 0;
    const indirect_offset_32_bit = indirect_offset > std.math.maxInt(i8) and indirect_offset < std.math.minInt(i8);
    const register_byte: u8 = (@as(u8, @boolToInt(indirect_offset_not_zero and indirect_offset_32_bit)) << 7) | (@as(u8, @boolToInt(indirect_offset_not_zero and !indirect_offset_32_bit)) << 6) | (@enumToInt(register) << 3) | @enumToInt(indirect_register);
    const sib_byte = 0x24;

    const bytes = blk:
    {
        if (indirect_offset_not_zero)
        {
            if (indirect_offset_32_bit)
            {
                const indirect_offset_bytes = std.mem.asBytes(&indirect_offset);
                if (indirect_register == .SP)
                {
                    break :blk &[_]u8 { rex_byte, opcode, register_byte, sib_byte, indirect_offset_bytes[0], indirect_offset_bytes[1], indirect_offset_bytes[2], indirect_offset_bytes[3] };
                }
                else
                {
                    break :blk &[_]u8 { rex_byte, opcode, register_byte, indirect_offset_bytes[0], indirect_offset_bytes[1], indirect_offset_bytes[2], indirect_offset_bytes[3] };
                }
            }
            else
            {
                const indirect_offset_byte : i8 = @intCast(i8, indirect_offset);
                const indirect_offset_bytes = std.mem.asBytes(&indirect_offset_byte);
                if (indirect_register == .SP)
                {
                    break :blk &[_]u8 { rex_byte, opcode, register_byte, sib_byte, indirect_offset_bytes[0] };
                }
                else
                {
                    break :blk &[_]u8 { rex_byte, opcode, register_byte, indirect_offset_bytes[0] };
                }
            }
        }
        else
        {
            if (indirect_register == .SP)
            {
                break :blk &[_]u8 { rex_byte, opcode, register_byte, sib_byte };
            }
            else
            {
                break :blk &[_]u8 { rex_byte, opcode, register_byte };
            }
        }
    };

    return Instruction.Resolved.new(bytes);
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

pub fn cmp_indirect_immediate(indirect_register: Register.ID, indirect_offset: i32, indirect_size: u8, integer_literal: Parser.IntegerLiteral) Instruction
{
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
            if (indirect_size == 2) break :blk 2
            else break :blk 4;
        }
        break :blk 1;
    };

    const integer_bytes = std.mem.asBytes(&integer_literal.value)[0..integer_byte_count];
    const opcode = if (integer_byte_count > 1 or indirect_size == max(u8)) &[_]u8 { 0x81 } else &[_]u8 { 0x83 };

    return encode_indirect_instruction_opcode_plus_register_indirect_offset(null, opcode, 0x38, indirect_register, indirect_offset, integer_bytes);
}

fn cmp_register_indirect(register: Register.ID, register_size: u8, indirect_register: Register.ID, indirect_offset: i32) Instruction
{
    const opcode = [_]u8 { 0x3b - @as(u8, @boolToInt(register_size == 1)) };

    return encode_indirect_instruction_opcode_plus_register_indirect_offset(null, opcode[0..], @enumToInt(register) << 3, indirect_register, indirect_offset, std.mem.zeroes([]const u8));
}

fn mov_register_literal(register: Register.ID, number: u64, byte_count: u16) Instruction
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

fn mov_register_register(dst: Register.ID, src: Register.ID, register_size: u8) Instruction
{
    const opcode: u8 = 0x89 - @as(u8, @boolToInt(register_size == 1));
    const register_byte: u8 = 0xc0 | (@enumToInt(src) << 3) | @enumToInt(dst);

    const bytes = [_]u8 { opcode, register_byte };

    return Instruction.Resolved.new(bytes[0..]);
}

fn encode_indirect_instruction_opcode_plus_register_indirect_offset(rex: ?u8, opcode: []const u8, register_byte_start: u8, indirect_register: Register.ID, indirect_offset: i32, after_common_bytes: []const u8) Instruction
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

    for (opcode) |opcode_byte|
    {
        log("{x:0>2}\n", .{opcode_byte});
        bytes[encoded_byte_count] = opcode_byte;
        encoded_byte_count += 1;
    }

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

    log("Indirect register: {s}\n", .{@tagName(indirect_register)});
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

fn mov_indirect_immediate_unsigned(indirect_register: Register.ID, indirect_offset: i32, immediate: u64, allocation_size: u16) Instruction
{
    // @TODO: assert that for 64-bit register you cant use 64-bit immediate
    const immediate_bytes = std.mem.asBytes(&immediate)[0..allocation_size];
    return encode_indirect_instruction_opcode_plus_register_indirect_offset(null, &[_]u8 { 0xc7 }, 0, indirect_register, indirect_offset, immediate_bytes);
}

pub fn mov_register_indirect(dst_register: Register.ID, allocation_size: u8, indirect_register: Register.ID, indirect_offset: i32) callconv(.Inline) Instruction
{
    log("Mov register indirect. Dst register: {}, size: {}. Indirect: {}, size: {}\n", .{dst_register, allocation_size, indirect_register, indirect_offset});
    return switch (allocation_size)
    {
        1, 2 => unreachable,
        4 => encode_indirect_instruction_opcode_plus_register_indirect_offset(null, &[_]u8 { 0x8b }, @enumToInt(dst_register) << 3, indirect_register, indirect_offset, std.mem.zeroes([]const u8)),
        8 => encode_indirect_instruction_opcode_plus_register_indirect_offset(0x48, &[_]u8 { 0x8b }, @enumToInt(dst_register) << 3, indirect_register, indirect_offset, std.mem.zeroes([]const u8)),
        else => unreachable,
    };
}

pub fn mov_indirect_register(indirect_register: Register.ID, indirect_offset: i32, indirect_size: u8, dst_register: Register.ID, dst_register_size: u8) Instruction
{
    assert(dst_register_size == indirect_size);

    log("Mov indirect register\n", .{});
    log("Size: {}\n", .{indirect_size});
    const opcode: []const u8 = if (indirect_size > 1) &[_]u8 { 0x89 } else &[_]u8 { 0x88 };

    return encode_indirect_instruction_opcode_plus_register_indirect_offset(if (dst_register_size == 8) @enumToInt(Rex.W) else null, opcode, @enumToInt(dst_register) << 3, indirect_register, indirect_offset, std.mem.zeroes([]u8));
}

pub fn add_register_indirect(dst_register: Register.ID, allocation_size: u8, indirect_register: Register.ID, indirect_offset: i32) Instruction
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

pub fn add_register_register(dst: Register.ID, other: Register.ID, register_size: u8
) Instruction
{
    const opcode = 0x01 - @boolToInt(register_size == 1);
    const register_byte = 0xc0 | @intCast(u8, (@enumToInt(other)) << 3) | @enumToInt(dst);
    const bytes = [_]u8 { opcode, register_byte };

    return Instruction.Resolved.new(bytes[0..]);
}

pub fn add_register_immediate(register: Register.ID, register_size: u8, immediate: Parser.IntegerLiteral) Instruction
{
    const value = immediate.value;
    var immediate_byte_count: u8 = 0;
    if (value > max(u32)) immediate_byte_count = 8
    else if (value > max(u16)) immediate_byte_count = 4
    else if (value > max(u8) and register_size > 2) immediate_byte_count = 4
    else if (value > max(u8)) immediate_byte_count = 2
    else immediate_byte_count = 1;

    assert(immediate_byte_count <= register_size);

    if (register == .A and !(immediate_byte_count == 1 and register_size != immediate_byte_count))
    {
        const opcode: u8 = 0x05 - @as(u8, @boolToInt(register_size == 1));
        const immediate_bytes = std.mem.asBytes(&immediate.value)[0..immediate_byte_count];
        const array_of_slices = [_][]const u8 { std.mem.asBytes(&opcode), immediate_bytes };
        return Instruction.Resolved.new_by_components(array_of_slices[0..]);
    }
    else
    {
        if (immediate_byte_count == 1)
        {
            if (register_size > immediate_byte_count)
            {
                const opcode = 0x83;
                const register_byte = 0xc0 | @enumToInt(register);
                const byte_prologue = [_]u8 { opcode, register_byte };
                const immediate_bytes = std.mem.asBytes(&immediate.value)[0..immediate_byte_count];
                const array_of_slices = [_][]const u8 { byte_prologue[0..], immediate_bytes };
                return Instruction.Resolved.new_by_components(array_of_slices[0..]);
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

pub fn sub_register_immediate(register: Register.ID, register_size: u8, immediate: Parser.IntegerLiteral) Instruction
{
    const value = immediate.value;
    var immediate_byte_count: u8 = 0;
    if (value > max(u32)) immediate_byte_count = 8
    else if (value > max(u16)) immediate_byte_count = 4
    else if (value > max(u8)) immediate_byte_count = 2
    else immediate_byte_count = 1;

    assert(immediate_byte_count <= register_size);

    if (register == .A and !(immediate_byte_count == 1 and register_size != immediate_byte_count))
    {
        const opcode: u8 = 0x2d - @as(u8, @boolToInt(register_size == 1));
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
        if (immediate_byte_count == 1)
        {
            if (register_size > immediate_byte_count)
            {
                const opcode = 0x83;
                const register_byte = 0xe8 | @enumToInt(register);
                const byte_prologue = [_]u8 { opcode, register_byte };
                const immediate_bytes = std.mem.asBytes(&immediate.value)[0..immediate_byte_count];
                const array_of_slices = [_][]const u8 { byte_prologue[0..], immediate_bytes };

                return Instruction.Resolved.new_by_components(array_of_slices[0..]);
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

fn dec_register(register: Register.ID, register_size: u8) Instruction
{
    const opcode = 0xff - @as(u8, @boolToInt(register_size == 1));
    const register_byte = 0xc8 | @enumToInt(register);
    var bytes = [_]u8 { opcode, register_byte };

    return Instruction.Resolved.new(bytes[0..]);
}

fn inc_register(register: Register.ID, register_size: u8) Instruction
{
    const opcode = 0xff - @as(u8, @boolToInt(register_size == 1));
    const register_byte = 0xc0 | @enumToInt(register);
    var bytes = [_]u8 { opcode, register_byte };

    return Instruction.Resolved.new(bytes[0..]);
}

fn imul_register_indirect(register: Register.ID, operand_size: u8, indirect_register: Register.ID, indirect_offset: i32) Instruction
{
    const opcode = [_]u8 { 0x0f, 0xaf };
    _ = operand_size;

    return encode_indirect_instruction_opcode_plus_register_indirect_offset(null, opcode[0..], @enumToInt(register) << 3, indirect_register, indirect_offset, std.mem.zeroes([]const u8));
}

fn xor_register(register: Register.ID, size: u8) Instruction
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

pub const Instruction = struct
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
            log("Instruction bytes:\n", .{});
            for (bytes) |byte|
            {
                log("0x{x:0>2}\n", .{byte});
            }
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

            const instruction = Instruction
            {
                .resolution = .{
                    .resolved = .{
                        .bytes = bytes,
                        .size = byte_count,
                    }
                },
                .status = .resolved,
            };

            log ("Instruction bytes:\n", .{});
            for (bytes[0..byte_count]) |byte|
            {
                log("0x{x:0>2}\n", .{byte});
            }

            return instruction;
        }

        pub fn new_by_components(byte_slices: []const []const u8) callconv(.Inline) Instruction
        {
            var offset: u8 = 0;
            
            const instruction = Instruction
            {
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
            log("Instruction bytes:\n", .{});
            const instruction_bytes = instruction.resolution.resolved.bytes[0..instruction.resolution.resolved.size];
            for (instruction_bytes) |byte|
            {
                log("0x{x:0>2}\n", .{byte});
            }

            return instruction;
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
        register_allocator: Register.Allocator,
        stack_allocator: Stack.Allocator,

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

    pub fn encode_text_section_pe(self: *Program, allocator: *std.mem.Allocator, text: *PE.Section, text_out: *PE.Section.Text.EncodingOutput, offset: *PE.Offset) void
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
                            log("[{}] ", .{text.buffer.items.len});
                            for (sub_rsp) |byte|
                            {
                                log("{x:0>2} ", .{byte});
                            }
                            log("\n", .{});

                            text.buffer.appendSlice(sub_rsp) catch unreachable;
                            add_rsp_at_epilogue = true;
                        }
                        else if (stack_offset > 0)
                        {
                            const sub_rsp = sub_rsp_s8(@intCast(i8, stack_offset))[0..];
                            log("[{}] ", .{text.buffer.items.len});
                            for (sub_rsp) |byte|
                            {
                                log("{x:0>2} ", .{byte});
                            }
                            log("\n", .{});

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
                                log("Address of the jump is not yet known. Appending a patch to block #{} from #{}. Offset to be patched: 0x{x}\n", .{basic_block_index, current_block_offset - 1, code_buffer_offset});
                                text.buffer.appendNTimes(0xcc, operand_size) catch unreachable;
                                post_function_patches[basic_block_index].append(code_buffer_offset) catch unreachable;
                                post_function_patch_count += 1;
                            }
                        },
                        else => unreachable,
                    }
                }
            }

            // end collecting block offsets
            if (current_block_offset < function.basic_block_instruction_offsets.items.len)
            {
                const code_buffer_offset = text.buffer.items.len;

                log("Appending block code buffer offset {} to block {}\n", .{code_buffer_offset, current_block_offset});
                function.basic_block_buffer_offsets.append(code_buffer_offset) catch unreachable;
                current_block_offset += 1;
            }

            assert(current_block_offset == function.basic_block_instruction_offsets.items.len);

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

            log("Resolution count for this function: {}. Post function patch count: {}\n", .{resolution_count, post_function_patch_count});
            assert(resolution_count == post_function_patch_count);

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
                log("[{}] {x:0>2}\n", .{text.buffer.items.len, ret_bytes[0]});
                text.buffer.append(ret_bytes[0]) catch unreachable;
            }
            else if (function.terminator == .noreturn)
            {
                // Append Int3
                const int3 = 0xcc;
                log("[{}] {x:0>2}\n", .{text.buffer.items.len, int3});
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

const Stack = struct
{
    const Store = struct
    {
        const ArrayType = [Register.count]Stack.Store;
    };

    // @TODO: figure out proper signed/unsigned types
    const Allocator = struct
    {
        allocations: ArrayList(Indirect),
        offset: i32,

        const Self = @This();

        fn new(allocator: *std.mem.Allocator) Stack.Allocator
        {
            return .
            {
                .allocations = ArrayList(Indirect).init(allocator),
                .offset = 0,
            };
        }

        // @TODO: work on alignment
        fn allocate(self: *Self, size: u32, alloca: IR.Reference) void
        {
            const allocation_offset = @intCast(i32, alignForward(@intCast(u32, self.offset), size));
            log("Allocating in stack {} bytes for alloca #{} at [R{s} + 0x{x}]\n", .{size, alloca.get_index(), @tagName(stack_register), self.offset});

            self.offset += @intCast(i32, size);
            const indirect = Indirect
            {
                .size = size,
                .alignment = size,
                .offset = allocation_offset,
                .reference = alloca,
                .register = stack_register,
            };

            self.allocations.append(indirect) catch unreachable;
        }
        
        fn get_allocation(self: *Self, alloca: IR.Reference) Indirect
        {
            log("Searching for alloca #{}\n", .{alloca.get_index()});

            for (self.allocations.items) |allocation|
            {
                if (allocation.reference.value == alloca.value) return allocation;
            }

            panic("Not found\n", .{});
        }
    };
};

const Indirect = struct
{
    reference: IR.Reference,
    offset: i32,
    alignment: u32,
    size: u32,
    register: Register.ID,
};

const Direct = struct
{
    reference: IR.Reference,
    size: u8,
    modifier: u8,
    register: Register.ID,
};


const Register = extern union
{
    direct: Direct,
    indirect: Indirect,

    const count = 16;
    const ArrayType = [Register.count]Register; 

    pub const ID = enum(u8)
    {
        A = 0,
        C = 1,
        D = 2,
        B = 3,

        SP = 4,
        BP = 5,
        SI = 6,
        DI = 7,

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

        pub const NFlag: u8 = 0b1000;
    };

    // @TODO: register number is already store in index. With modifier, we can access every register

    const OccupationKind = enum(u8)
    {
        none,
        direct,
        indirect,
    };

    const Saver = struct
    {
        state: State,
        saved_register_count: u8,
    };

    const LegacyGPR = enum(u8)
    {
        a = 0,
        c = 1,
        d = 2,
        b = 3,
        const count = std.enums.values(LegacyGPR).len;
    };

    const NewRegister = enum(u8)
    {
        r8 = 8,
        r9 = 9,
        r10 = 10,
        r11 = 11,
        r12 = 12,
        r13 = 13,
        r14 = 14,
        r15 = 15,

        const count = std.enums.values(NewRegister).len;
    };

    const SpecializedRegister = enum(u8)
    {
        sp = 4,
        bp = 5,
        si = 6,
        di = 7,

        const count = std.enums.values(SpecializedRegister).len;
    };

    const AccessType = enum
    {
        general,
        legacy_gp,
        specialized,
        arguments
    };

    const Registers = extern union
    {
        by_type: struct
        {
            legacy: [LegacyGPR.count]Register,
            specialized: [SpecializedRegister.count]Register,
            new: [NewRegister.count]Register,
        },
        array: [Register.count]Register,
    };

    comptime
    {
        assert(@sizeOf(Registers) == @sizeOf([Register.count]Register));
        assert(@sizeOf(Occupation) == @sizeOf([Register.count]OccupationKind));
    }

    const Occupation = extern union
    {
        by_type: struct
        {
            legacy: [LegacyGPR.count]OccupationKind,
            specialized: [SpecializedRegister.count]OccupationKind,
            new: [NewRegister.count]OccupationKind,
        },
        array: [Register.count]OccupationKind,
    };

    const State = struct
    {
        occupation: Occupation,
        registers: Registers,
    };

    const Allocator = struct
    {
        state: State,
        argument_registers: []const Register.ID,

        const Self = @This();

        fn new(argument_registers: []const Register.ID) Self
        {
            var result: Self = undefined;
            result.argument_registers = argument_registers;
            result.state.occupation.array = std.mem.zeroes([Register.count]OccupationKind);

            return result;
        }

        fn free(self: *Self, register: ID) void
        {
            self.state.occupation.array[@enumToInt(register)] = .none;
        }

        fn internal_get_register(self: *Self, register: ID, comptime register_type: AccessType) callconv(.Inline) *Register
        {
            const register_int = @enumToInt(register);
            if (register_int < @enumToInt(ID.SP))
            {
                return self.legacy_gp_registers[register_int];
            }
            else if (register_int < @enumToInt(ID.r8))
            {
                comptime assert(register_type == .arguments);
                assert(register == .DI or register == .SI);
                return self.specialized_registers[register_int - @enumToInt(std.enums.values(SpecializedRegister)[0])];
            }
            // @TODO: implement extended registers
            else
            {
                comptime assert(register_type == .arguments);
                assert(register == .r8 or register == .r9);
                return self.new_registers[register_int - @enumToInt(std.enums.values(NewRegister)[0])];
            }
        }

        fn allocate_direct(self: *Self, value: IR.Reference, register_size: u8) Direct
        {
            for (self.state.occupation.by_type.legacy) |*reg_occupation, reg_i|
            {
                if (reg_occupation.* == .none)
                {
                    var register = &self.state.registers.by_type.legacy[reg_i];
                    reg_occupation.* = .direct;

                    register.* = Register
                    {
                        .direct = .
                        {
                            .reference = value,
                            .size = register_size,
                            .modifier = 0,
                            .register = @intToEnum(ID, reg_i),
                        }
                    };

                    return register.direct;
                }
            }

            panic("Couldn't allocate register\n", .{});
        }

        fn allocate_indirect(self: *Self, reference: IR.Reference, indirect_offset: i32, indirect_size: u32) Indirect
        {
            for (self.state.occupation.by_type.legacy) |*reg_occupation, reg_i|
            {
                if (reg_occupation.* == .none)
                {
                    const indirect = Indirect
                    {
                        .reference = reference,
                        .offset = indirect_offset,
                        .alignment = indirect_size,
                        .size = indirect_size,
                        .register = @intToEnum(Register.ID, reg_i),
                    };

                    reg_occupation.* = .indirect;

                    var register = &self.state.registers.by_type.legacy[reg_i];

                    register.* = .
                    {
                        .indirect = indirect,
                    };

                    return indirect;
                }
            }

            panic("No free registers\n", .{});
        }

        // @TODO: is this correct? can cause bugs to manage code encoding from here
        // @TODO: delete this and properly organize it in the code
        //fn receive_return_in_backup_register(self: *Self, function: *Program.Function, reference: IR.Reference) struct { OccupationKindRegister
        //{
            //for (self.registers) |reg, reg_i|
            //{
                //if (reg.value.value == reference.value and reg.size > 0)
                //{
                    //const callee_allocated_register = @intToEnum(Register.ID, reg_i);
                    //const saved_callee_allocated_register = reg;
                    //const backup_register = self.allocate(saved_callee_allocated_register.value, saved_callee_allocated_register.size);

                    //self.internal_free_from_register(callee_allocated_register);
                    //self.internal_free_from_register(backup_register);

                    //const mov_reg_reg = mov_register_register(backup_register, callee_allocated_register, saved_callee_allocated_register.size);
                    //function.append_instruction(mov_reg_reg);

                    //return backup_register;
                //}
            //}

            //panic("No value wanted in registers\n", .{});
        //}

        // @TODO: improve
        fn allocate_argument(self: *Self, argument_index: u32, size: u8) Direct
        {
            const target_register = self.argument_registers[argument_index];

            log("Allocating argument #{} ({} bytes) in {s}\n", .{argument_index, size, @tagName(target_register)});
            var reg_occupation = &self.state.occupation.array[@enumToInt(target_register)];

            if (reg_occupation.* == .none)
            {
                reg_occupation.* = .direct;

                var register = &self.state.registers.array[@enumToInt(target_register)];
                register.* = .
                {
                    .direct = .
                    {
                        .reference = IR.Function.Argument.new(argument_index),
                        .size = size,
                        .modifier = 0,
                        .register = target_register,
                    },
                };

                return register.direct;
            }

            panic("No argument register ready\n", .{});
        }

        fn allocate_call_argument(self: *Self, argument_index: u32, argument: IR.Reference, size: u8) Direct
        {
            const target_register = self.argument_registers[argument_index];

            log("Allocating argument #{} ({} bytes) in {}\n", .{argument_index, size, target_register});
            var reg_occupation = &self.state.occupation.array[@enumToInt(target_register)];

            if (reg_occupation.* == .none)
            {
                var register = &self.state.registers.array[@enumToInt(target_register)];
                reg_occupation.* = .direct;

                register.* = .
                {
                    .direct = .
                    {
                        .reference = argument,
                        .size = size,
                        .modifier = 0,
                        .register = target_register,
                    },
                };

                return register.direct;
            }

            panic("No argument register ready\n", .{});
        }

        // @TODO: improve
        fn allocate_return(self: *Self, reference: IR.Reference, size: u8) Direct
        {
            assert(size <= 8);
            const register_a = @enumToInt(Register.ID.A);
            var reg_occupation = &self.state.occupation.by_type.legacy[register_a];

            if (reg_occupation.* == .none)
            {
                var register = &self.state.registers.by_type.legacy[register_a];
                reg_occupation.* = .direct;
                register.direct = .
                {
                    .reference = reference,
                    .size = size,
                    .modifier = 0,
                    .register = .A,
                };

                return register.direct;
            }

            panic("Return register A is busy\n", .{});
        }

        fn allocate_return_indirect(self: *Self, reference: IR.Reference, size: u8) Indirect
        {
            _ = self; _ = reference; _ = size;
            unreachable;
        }

        fn fetch_direct(self: *Self, program: *const IR.Program, reference: IR.Reference, use: IR.Reference) ?Direct
        {
            for (self.state.occupation.by_type.legacy) |*reg_occupation, reg_i|
            {
                if (reg_occupation.* == .direct)
                {
                    var register = &self.state.registers.by_type.legacy[reg_i];

                    if (register.direct.reference.value == reference.value)
                    {
                        assert(register.direct.size > 0);
                        const uses = program.get_uses(IR.Instruction.get_ID(reference), reference.get_index());
                        assert(uses.len > 0);

                        if (uses[uses.len - 1].value == use.value)
                        {
                            assert(uses.len == 1);
                            reg_occupation.* = .none;
                        }

                        return register.direct;
                    }
                }
            }

            log("Couldn't find value in a register\n", .{});
            return null;
        }

        fn fetch_indirect(self: *Self, reference: IR.Reference) Indirect
        {
            for (self.legacy_gp_registers) |*reg|
            {
                if (reg == .indirect and reg.indirect.reference.value == reference.value and reg.indirect.size > 0)
                {
                    return reg.indirect;
                }
            }
        }

        fn fetch_argument(self: *Self, argument_index: u32) Direct
        {
            const argument_register = self.argument_registers[argument_index];
            const register_occupation = self.state.occupation.array[@enumToInt(argument_register)];

            if (register_occupation == .none or register_occupation == .indirect)
            {
                codegen_error("Register {s} is not occupied with the wanted value\n", .{@tagName(argument_register)});
            }

            const result = self.state.registers.array[@enumToInt(argument_register)];
            if (result.direct.reference.get_ID() != .argument) codegen_error("Register {s} is not occupied by an argument\n", .{@tagName(argument_register)});
            if (result.direct.reference.get_index() != argument_index) codegen_error("Register {s} is not occupied by the correct argument\n", .{@tagName(argument_register)});

            return result.direct;
        }

        fn spill_registers_before_call(self: *Self) Saver
        {
            // @TODO: implement
            _ = self;
            var saver: Saver = undefined;
            saver.state.occupation.array = std.mem.zeroes([Register.count]OccupationKind);
            saver.saved_register_count = 0;
            return saver;
        }

        fn alter_allocation_direct(self: *Self, register: Register.ID, new_reference: IR.Reference) void
        {
            var dst_register_occupation = &self.state.occupation.array[@enumToInt(register)];
            if (dst_register_occupation.* == .none) codegen_error("Register is expected to be busy\n", .{});
            assert(dst_register_occupation.* == .direct);
            var dst_register = &self.state.registers.array[@enumToInt(register)];
            dst_register.direct.reference = new_reference;
        }

        fn reset(self: *Self) void
        {
            std.mem.set(OccupationKind, self.state.occupation.array[0..], .none);
        }

        fn must_save(register: Register.ID) bool
        {
            const register_int = @enumToInt(register);
            return register_int <= @enumToInt(Register.ID.D) or (register_int >= @enumToInt(Register.ID.r8) and register_int <= @enumToInt(Register.ID.r11));
        }
    };

};

fn process_load_for_ret(program: *const IR.Program, function: *Program.Function, load_reference: IR.Reference) Direct
{
    const load_index = load_reference.get_index(); 
    const load = program.instructions.load[load_index];

    const load_pointer_id = IR.Instruction.get_ID(load.pointer);
    switch (load_pointer_id)
    {
        .alloca =>
        {
            const allocation = function.stack_allocator.get_allocation(load.pointer);
            assert(allocation.size <= 8);
            const register_size = @intCast(u8, allocation.size);

            const load_register = function.register_allocator.allocate_return(load_reference, register_size);

            const mov_register_stack = mov_register_indirect(load_register.register, @intCast(u8, allocation.size), allocation.register, allocation.offset);
            function.append_instruction(mov_register_stack);
            return load_register;
        },
        .load =>
        {
            const resulting_size = @intCast(u8, load.type.get_size());
            assert(resulting_size == 4);
            const load_s_load = program.instructions.load[load.pointer.get_index()];
            const alloca_ref = load_s_load.pointer;
            assert(IR.Instruction.get_ID(alloca_ref) == .alloca);
            const allocation = function.stack_allocator.get_allocation(alloca_ref);
            const direct = function.register_allocator.allocate_direct(load.pointer, @intCast(u8, allocation.size));
            function.register_allocator.free(direct.register);
            const mov_reg_stack = mov_register_indirect(direct.register, direct.size, allocation.register, allocation.offset);
            function.append_instruction(mov_reg_stack);
            const indirect = function.register_allocator.allocate_indirect(load_reference, 0,  resulting_size);
            function.register_allocator.free(indirect.register);
            const allocated_return = function.register_allocator.allocate_return(load_reference, resulting_size);
            const mov_to_return_register = mov_register_indirect(allocated_return.register, allocated_return.size, indirect.register, indirect.offset);
            function.append_instruction(mov_to_return_register);

            return allocated_return;
        },
        else => panic("NI: {}\n", .{load_pointer_id}),
    }
}

fn process_add(program: *const IR.Program, function: *Program.Function, add_reference: IR.Reference) void
{
    const add = program.instructions.add[add_reference.get_index()];

    switch (add.left.get_ID())
    {
        .instruction =>
        {
            const instr_id = IR.Instruction.get_ID(add.left);
            switch (instr_id)
            {
                .load =>
                {
                    const first_operand_allocation = fetch_load(program, function, add.left, add_reference, true);

                    switch (add.right.get_ID())
                    {
                        .constant =>
                        {
                            switch (IR.Constant.get_ID(add.right))
                            {
                                .int =>
                                {
                                    const second_operand_integer_literal = program.integer_literals[add.right.get_index()];
                                    assert(first_operand_allocation.size == @sizeOf(i32));
                                    if (second_operand_integer_literal.value == 0) return;

                                    // @TODO: be more subtle about register allocating the first operand
                                    const register_size = @intCast(u8, first_operand_allocation.size);
                                    const load_register = function.register_allocator.allocate_direct(add.left, register_size);
                                    const mov_reg_indirect = mov_register_indirect(load_register.register, @intCast(u8, first_operand_allocation.size), first_operand_allocation.register, first_operand_allocation.offset);
                                    // @TODO: implement this in a better way
                                    if (first_operand_allocation.register != stack_register)
                                    {
                                        function.register_allocator.free(first_operand_allocation.register);
                                    }

                                    function.register_allocator.alter_allocation_direct(load_register.register, add_reference);
                                    function.append_instruction(mov_reg_indirect);

                                    if (second_operand_integer_literal.value == 1)
                                    {
                                        log("Encoding inc instead of add\n", .{});
                                        const inc_reg = inc_register(load_register.register, register_size);
                                        function.append_instruction(inc_reg);
                                    }
                                    else
                                    {
                                        log("Encoding add register immediate {}\n", .{second_operand_integer_literal.value});
                                        const add_register_literal = add_register_immediate(load_register.register, register_size, second_operand_integer_literal);
                                        function.append_instruction(add_register_literal);
                                    }
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
                                    const second_operand_load = program.instructions.load[add.right.get_index()];
                                    const second_operand_allocation = function.stack_allocator.get_allocation(second_operand_load.pointer);

                                    // move to a register first operand if it is also a stack operand
                                    log("First operand is also stack, we have to allocate it into a register\n", .{});
                                    const load_register = function.register_allocator.allocate_direct(add.right, @intCast(u8, first_operand_allocation.size));
                                    const mov_register_stack = mov_register_indirect(load_register.register, @intCast(u8, first_operand_allocation.size), first_operand_allocation.register, first_operand_allocation.offset);
                                    function.append_instruction(mov_register_stack);
                                    const add_register_stack = add_register_indirect(load_register.register, @intCast(u8, first_operand_allocation.size), second_operand_allocation.register, second_operand_allocation.offset);
                                    function.register_allocator.alter_allocation_direct(load_register.register, add_reference);
                                    function.append_instruction(add_register_stack);
                                },
                                .call =>
                                {
                                    // @TODO: return direct
                                    // @TODO: this code is managed and can cause problems
                                    //const backup_call_register = function.register_allocator.receive_return_in_backup_register(function, add.right);
                                    //assert(backup_call_register.occupation_kind == .direct);
                                    //const load_register = function.register_allocator.allocate_direct(add.left, @intCast(u8, first_operand_allocation.size));
                                    //const mov_register_stack = mov_register_indirect(load_register.register, @intCast(u8, first_operand_allocation.size), first_operand_allocation.register, first_operand_allocation.offset);
                                    //function.append_instruction(mov_register_stack);

                                    //const add_reg_reg = add_register_register(load_register.register, backup_call_register.occupated.direct.register, @intCast(u8, first_operand_allocation.size));
                                    //function.register_allocator.alter_allocation(load_register.register, add_reference);
                                    //function.append_instruction(add_reg_reg);
                                    unreachable;
                                },
                                else => panic("{}\n", .{IR.Instruction.get_ID(add.right)}),
                            }
                        },
                        else => panic("ni: {}\n", .{add.right.get_ID()}),
                    }
                },
                .call =>
                {
                    // @TODO: here we are assuming than the integer can't be over 8 bytes
                    const first_operand_call = program.instructions.call[add.left.get_index()];
                    const returned_call_register = function.register_allocator.fetch_direct(program, add.left, add_reference) orelse unreachable;

                    _ = first_operand_call;

                    switch (add.right.get_ID())
                    {
                        .constant =>
                        {
                            switch (IR.Constant.get_ID(add.right))
                            {
                                .int =>
                                {
                                    const second_operand_integer_literal = program.integer_literals[add.right.get_index()];
                                    log("Call register size: {}\n", .{returned_call_register.size});
                                    assert(returned_call_register.size == @sizeOf(i32));

                                    if (second_operand_integer_literal.value == 0) return;

                                    // @TODO: here we need to assert that the register size is the same than before
                                    const allocation_direct = function.register_allocator.allocate_return(add_reference, returned_call_register.size);
                                    assert(allocation_direct.register == returned_call_register.register);
                                    assert(allocation_direct.size == returned_call_register.size);
                                    if (second_operand_integer_literal.value == 1)
                                    {
                                        log("Encoding inc instead of add\n", .{});
                                        const inc_reg = inc_register(returned_call_register.register, returned_call_register.size);
                                        function.append_instruction(inc_reg);
                                    }
                                    else
                                    {
                                        log("Encoding add register immediate {}\n", .{second_operand_integer_literal.value});
                                        const add_register_literal = add_register_immediate(returned_call_register.register, returned_call_register.size, second_operand_integer_literal);
                                        function.append_instruction(add_register_literal);
                                    }
                                },
                                else => panic("ni: {}\n", .{IR.Constant.get_ID(add.right)}),
                            }
                        },
                        else => panic("ni: {}\n", .{add.right.get_ID()}),
                    }
                },
                else => panic("ni: {}\n", .{instr_id}),
            }
        },
        else => panic("ni: {}\n", .{add.left.get_ID()}),
    }
}

fn process_icmp(program: *const IR.Program, function: *Program.Function, icmp_reference: IR.Reference) void
{
    const icmp = program.instructions.icmp[icmp_reference.get_index()];

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
                        // @TODO: figure out this false
                        break :blk fetch_load(program, function, icmp.left, icmp_reference, true);
                    },
                    else => panic("ni: {}\n", .{instr_id}),
                }
            },
            else => panic("ni: {}\n", .{icmp.left.get_ID()}),
        }
    };

    log("First operand allocation: {}\n", .{first_operand_allocation});

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
                            break :blk cmp_indirect_immediate(first_operand_allocation.register, first_operand_allocation.offset, @intCast(u8, first_operand_allocation.size), integer_literal);
                        }
                        else unreachable;
                    };

                    function.append_instruction(cmp);
                },
                else => panic("ni: {}\n", .{IR.Constant.get_ID(icmp.right)}),
            }
        },
        .instruction =>
        {
            const instruction_id = IR.Instruction.get_ID(icmp.right);
            switch (instruction_id)
            {
                .load =>
                {
                    if (first_operand_kind == .stack)
                    {
                        const register_size = @intCast(u8, first_operand_allocation.size);
                        const load_register = function.register_allocator.allocate_direct(icmp.right, register_size);
                        const mov_register_stack = mov_register_indirect(load_register.register, @intCast(u8, first_operand_allocation.size), first_operand_allocation.register, first_operand_allocation.offset);
                        function.register_allocator.alter_allocation_direct(load_register.register, icmp_reference);
                        function.append_instruction(mov_register_stack);

                        const second_operand = fetch_load(program, function, icmp.right, icmp_reference, false);
                        const cmp_register_stack = cmp_register_indirect(load_register.register, register_size, second_operand.register, second_operand.offset);
                        function.append_instruction(cmp_register_stack);
                    }
                    else
                    {
                        unreachable;
                    }
                },
                else => panic("ni: {}\n", .{instruction_id}),
            }
        },
        else => panic("ni: {}\n", .{icmp.right.get_ID()}),
    }
}

fn fetch_load(program: *const IR.Program, function: *Program.Function, load_ref: IR.Reference, load_use: IR.Reference, comptime needs_to_be_register_loaded: bool) Indirect
{
    // @TODO: implement
    _ = load_use;

    assert(load_ref.get_ID() == .instruction);
    assert(IR.Instruction.get_ID(load_ref) == .load);

    const load = program.instructions.load[load_ref.get_index()];
    const load_pointer_id = IR.Instruction.get_ID(load.pointer);

    log("Load pointer ID: {}\n", .{load_pointer_id});
    log("Looking for alloca in registers\n", .{});

    switch (load_pointer_id)
    {
        .alloca =>
        {
            const load_use_id = IR.Instruction.get_ID(load_use);
            switch (load_use_id)
            {
                .store =>
                {
                    const store = program.instructions.store[load_use.get_index()];
                    const resulting_size = @intCast(u8, store.type.get_size());
                    log("Resulting size: {}\n", .{resulting_size});
                    assert(resulting_size == 4);

                    const allocation = function.stack_allocator.get_allocation(load.pointer);
                    assert(allocation.size <= 8);
                    const register_size = @intCast(u8, allocation.size);

                    const load_register = function.register_allocator.allocate_direct(load.pointer, register_size);
                    function.register_allocator.free(load_register.register);
                    const mov_register_stack = mov_register_indirect(load_register.register, @intCast(u8, allocation.size), allocation.register, allocation.offset);
                    function.append_instruction(mov_register_stack);
                    const indirect = function.register_allocator.allocate_indirect(load_ref, 0, resulting_size);
                    return indirect;
                },
                .load => return function.stack_allocator.get_allocation(load.pointer),
                .icmp => return function.stack_allocator.get_allocation(load.pointer),
                else => panic("NI: {}\n", .{load_use_id}),
            }
        },
        .load =>
        {
            if (false)
            {
                // @TODO: needs to be register loaded?
                const stack_indirect = fetch_load(program, function, load.pointer, load_ref, needs_to_be_register_loaded);
                log("Stack indirect: {}\n", .{stack_indirect});
                // @TODO: avoid hardcoded values
                const register_indirect = function.register_allocator.allocate_indirect(load_ref, 0, @intCast(u8, stack_indirect.size));
                const load_size = load.type.get_size();
                log("Load size: {}\n", .{load_size});
                assert(load_size == 4);
                const mov_register_stack = mov_register_indirect(register_indirect.register, @intCast(u8, register_indirect.size), stack_indirect.register, stack_indirect.offset);
                function.append_instruction(mov_register_stack);
                return register_indirect;
            }
            else
            {
                const load_size = @intCast(u8, load.type.get_size());
                log("Load size: {}\n", .{load_size});

                const stack_indirect = fetch_load(program, function, load.pointer, load_ref, needs_to_be_register_loaded);
                const register = function.register_allocator.allocate_direct(load.pointer, @intCast(u8, stack_indirect.size));
                const mov_register_stack = mov_register_indirect(register.register, @intCast(u8, register.size), stack_indirect.register, stack_indirect.offset);
                function.append_instruction(mov_register_stack);

                function.register_allocator.free(register.register);
                const register_indirect = function.register_allocator.allocate_indirect(load_ref, 0, load_size);

                return register_indirect;
            }
        },
        //.get_element_ptr => get_mc_value_from_ir_value(function, executable, constant_data_list, data_buffer, mc_function, first_operand, ir_value),
        else => panic("load ptr id: {}\n", .{load_pointer_id}),
    }

    //if (function.register_allocator.get_allocation(program, load_ref, load_use)) |load_pointer|
    //{
        //_ = load_pointer;
        //unreachable;
    //}
    //else
    //{
        //assert(load.pointer.get_ID() == .instruction);
        //log("Load pointer id: {}\n", .{load_pointer_id});

    //}
}


fn process_sub(program: *const IR.Program, function: *Program.Function, sub_reference: IR.Reference) void
{
    const sub = program.instructions.sub[sub_reference.get_index()];
    var should_load_first_argument = false;

    var first_operand_kind: OperandKind = undefined;
    var second_operand_kind: OperandKind = undefined;

    const first_operand_allocation = blk:
    {
        switch (sub.left.get_ID())
        {
            .instruction =>
            {
                const instr_id = IR.Instruction.get_ID(sub.left);
                switch (instr_id)
                {
                    .load =>
                    {
                        break :blk fetch_load(program, function, sub.left, sub_reference, true);
                    },
                    else => panic("ni: {}\n", .{instr_id}),
                }
            },
            else => panic("ni: {}\n", .{sub.left.get_ID()}),
        }
    };

    log("First operand allocation: {}\n", .{first_operand_allocation});

    switch (sub.right.get_ID())
    {
        .constant =>
        {
            switch (IR.Constant.get_ID(sub.right))
            {
                .int =>
                {
                    second_operand_kind = .immediate;

                    const integer_literal = program.integer_literals[sub.right.get_index()];
                    assert(first_operand_allocation.size == @sizeOf(i32));
                    if (integer_literal.value == 0) return;

                    // @TODO: be more subtle about register allocating the first operand
                    const register_size = @intCast(u8, first_operand_allocation.size);
                    const load_register = function.register_allocator.allocate_direct(sub.right, register_size);
                    const mov_reg_indirect = mov_register_indirect(load_register.register, @intCast(u8, first_operand_allocation.size), first_operand_allocation.register, first_operand_allocation.offset);
                    function.register_allocator.alter_allocation_direct(load_register.register, sub_reference);
                    function.append_instruction(mov_reg_indirect);
                    // @TODO: implement this in a better way
                    if (first_operand_allocation.register != stack_register)
                    {
                        function.register_allocator.free(first_operand_allocation.register);
                    }

                    if (integer_literal.value == 1)
                    {
                        const dec_reg = dec_register(load_register.register, register_size);
                        function.append_instruction(dec_reg);
                    }
                    else
                    {
                        log("Sub register immediate\n", .{});
                        const sub_register_literal = sub_register_immediate(load_register.register, register_size, integer_literal);
                        function.append_instruction(sub_register_literal);
                    }
                },
                else => panic("ni: {}\n", .{IR.Constant.get_ID(sub.right)}),
            }
        },
        .instruction =>
        {
            switch (IR.Instruction.get_ID(sub.right))
            {
                .load =>
                {
                    second_operand_kind = .stack;

                    //const load = program.instructions.load[sub.right.get_index()];
                    //const second_operand_allocation = function.stack_allocator.get_allocation(load.pointer.get_index());

                    if (first_operand_kind == .stack)
                    {
                        // move to a register first operand if it is also a stack operand
                        //log("First operand is also stack, we have to allocate\n", .{});
                        //const load_register = function.register_allocator.allocate(sub.right, @intCast(u8, first_operand_allocation.size));
                        //const mov_register_stack = mov_register_indirect(load_register, @intCast(u8, first_operand_allocation.size), first_operand_allocation.register, first_operand_allocation.offset);
                        //function.append_instruction(mov_register_stack);
                        //const sub_register_stack = sub_register_indirect(load_register, @intCast(u8, first_operand_allocation.size), first_operand_allocation.register, second_operand_allocation.offset);
                        //function.register_allocator.alter_allocation_direct(load_register, sub_reference);
                        //function.append_instruction(sub_register_stack);
                        unreachable;
                    }
                    else
                    {
                        unreachable;
                    }
                },
                else => panic("{}\n", .{IR.Instruction.get_ID(sub.right)}),
            }
        },
        else => panic("ni: {}\n", .{sub.right.get_ID()}),
    }

    log("First operand: {}\n", .{first_operand_kind});


    log("Second operand kind: {}\n", .{second_operand_kind});

    if (should_load_first_argument)
    {
        unreachable;
    }
}

fn process_mul(program: *const IR.Program, function: *Program.Function, mul_reference: IR.Reference) void
{
    const mul = program.instructions.mul[mul_reference.get_index()];
    var should_load_first_argument = false;

    var first_operand_kind: OperandKind = undefined;
    var second_operand_kind: OperandKind = undefined;

    var signedness: Type.Integer.Signedness = undefined;

    const first_operand_allocation = blk:
    {
        switch (mul.left.get_ID())
        {
            .instruction =>
            {
                const instr_id = IR.Instruction.get_ID(mul.left);
                switch (instr_id)
                {
                    .load =>
                    {
                        first_operand_kind = .stack;
                        const load = program.instructions.load[mul.left.get_index()];
                        signedness = Type.Integer.get_signedness(load.type);
                        const allocation = function.stack_allocator.get_allocation(load.pointer);
                        break :blk allocation;
                    },
                    else => panic("ni: {}\n", .{instr_id}),
                }
            },
            else => panic("ni: {}\n", .{mul.left.get_ID()}),
        }
    };

    log("First operand: {}\n", .{first_operand_kind});

    switch (mul.right.get_ID())
    {
        .constant =>
        {
            switch (IR.Constant.get_ID(mul.right))
            {
                .int =>
                {
                    second_operand_kind = .immediate;

                    const integer_literal = program.integer_literals[mul.right.get_index()];
                    assert(first_operand_allocation.size == @sizeOf(i32));
                    if (integer_literal.value == 0) return;

                    // @TODO: be more subtle about register allocating the first operand
                    if (first_operand_kind == .stack)
                    {
                        //const register_size = @intCast(u8, first_operand_allocation.size);
                        //const load_register = function.register_allocator.allocate(mul.right, register_size);
                        //const mov_register_stack = mov_register_indirect(load_register, @intCast(u8, first_operand_allocation.size), first_operand_allocation.register, first_operand_allocation.offset);
                        //function.register_allocator.alter_allocation_direct(load_register, mul_reference);
                        //function.append_instruction(mov_register_stack);

                        //log("Mul register immediate\n", .{});
                        //const mul_register_literal = mul_register_immediate(load_register, register_size, integer_literal);
                        //function.append_instruction(mul_register_literal);
                        if (signedness == .signed)
                        {
                            unreachable;
                        }
                        else
                        {
                            unreachable;
                        }
                        unreachable;
                    }
                    else unreachable;
                },
                else => panic("ni: {}\n", .{IR.Constant.get_ID(mul.right)}),
            }
        },
        .instruction =>
        {
            switch (IR.Instruction.get_ID(mul.right))
            {
                .load =>
                {
                    second_operand_kind = .stack;

                    const load = program.instructions.load[mul.right.get_index()];
                    const second_operand_allocation = function.stack_allocator.get_allocation(load.pointer);

                    if (first_operand_kind == .stack)
                    {
                        // move to a register first operand if it is also a stack operand
                        log("First operand is also stack, we have to allocate\n", .{});
                        const load_register = function.register_allocator.allocate_direct(mul.right, @intCast(u8, first_operand_allocation.size));
                        const mov_register_stack = mov_register_indirect(load_register.register, @intCast(u8, first_operand_allocation.size), first_operand_allocation.register, first_operand_allocation.offset);
                        function.append_instruction(mov_register_stack);
                        function.register_allocator.alter_allocation_direct(load_register.register, mul_reference);
                        if (signedness == .signed)
                        {
                            const imul_register_stack = imul_register_indirect(load_register.register, @intCast(u8, first_operand_allocation.size), first_operand_allocation.register, second_operand_allocation.offset);
                            function.append_instruction(imul_register_stack);
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
                else => panic("{}\n", .{IR.Instruction.get_ID(mul.right)}),
            }
        },
        else => panic("ni: {}\n", .{mul.right.get_ID()}),
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

var stack_register: Register.ID = undefined;

pub fn encode(allocator: *std.mem.Allocator, program: *const IR.Program, executable_filename: []const u8, target: std.Target) void
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
        if (abi == .msvc) Register.ID.SP
        else if (abi == .gnu) Register.ID.BP
        else panic("NI: {}\n", .{abi});

    const function_count = program.functions.len;
    assert(function_count > 0);

    //assert(std.mem.eql(program.functions[0].declaration.name, "entry"));

    var functions = ArrayList(Program.Function).initCapacity(allocator, function_count) catch unreachable;
    var data_buffer = ArrayList(u8).init(allocator);

    for (program.functions) |*function|
    {
        var stack_allocator = Stack.Allocator.new(allocator);
        var register_allocator = Register.Allocator.new(argument_registers);

        for (function.declaration.type.argument_types) |argument_type, argument_i|
        {
            assert(argument_i < register_allocator.argument_registers.len);
            const argument_size = argument_type.get_size();
            _ = register_allocator.allocate_argument(@intCast(u32, argument_i), @intCast(u8, argument_size));
        }
        const basic_block_count = function.basic_blocks.len;

        const entry_block_index = function.basic_blocks[0];
        const entry_block = program.basic_blocks[entry_block_index];

        for (entry_block.instructions.items) |instruction|
        {
            if (IR.Instruction.get_ID(instruction) == .alloca)
            {
                const alloca_size = @intCast(u32, instruction.get_size(program));
                _ = stack_allocator.allocate(alloca_size, instruction);
            }
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
                .register_allocator = register_allocator,
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
        var occupied_register_count: u32 = 0;
        const argument_count = ir_function.declaration.type.argument_types.len;

        for (function.register_allocator.state.occupation.array) |occupation|
        {
            occupied_register_count += @boolToInt(occupation != .none);
        }
        assert(occupied_register_count == argument_count);

        log("Basic blocks:\n", .{});
        for (ir_function.basic_blocks) |basic_block, basic_block_i|
        {
            log("[{}]: #{}\n", .{basic_block_i, basic_block});
        }
        log("\n", .{});

        log("Alloca count: {}\n", .{function.stack_allocator.allocations.items.len});

        for (ir_function.basic_blocks) |basic_block_index, basic_block_i|
        {
            log("Processing basic block #{} (global index {})\n", .{basic_block_i, basic_block_index});
            const basic_block = program.basic_blocks[basic_block_index];
            assert(basic_block.instructions.items.len > 0);
            // @TODO:
            // reset register allocator
            //const function_offset = @intCast(u32, function.instructions.items.len);

            // Reset each time a basic block is processed
            if (basic_block_i != 0) function.register_allocator.reset();

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

                        const call_argument_count = call.arguments.len;

                        var function_type: Type.Function = undefined;

                        assert(call_argument_count <= function.register_allocator.argument_registers.len);
                        var register_allocator_saver = function.register_allocator.spill_registers_before_call();

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

                        log("Call argument count: {}\n", .{call_argument_count});

                        for (call.arguments) |argument, argument_i|
                        {
                            const argument_id = argument.get_ID();
                            const argument_element_index = argument.get_index();
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

                                            const integer_literal = program.integer_literals[argument_element_index];
                                            //@TODO: typecheck signedness
                                            assert(!integer_literal.signed);
                                            const literal = integer_literal.value;

                                            const integer_bit_count = Type.Integer.get_bit_count(argument_type);
                                            // assert n % 8 == 0
                                            assert((integer_bit_count & 0x7) == 0);
                                            const integer_byte_count = @truncate(u8, integer_bit_count >> 3);
                                            const argument_register = function.register_allocator.allocate_call_argument(@intCast(u32, argument_i), argument, integer_byte_count);

                                            if (literal == 0)
                                            {
                                                const xor_reg = xor_register(argument_register.register, integer_byte_count);
                                                function.append_instruction(xor_reg);
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
                                .instruction =>
                                {
                                    const arg_instruction_id = IR.Instruction.get_ID(argument);
                                    
                                    switch (arg_instruction_id)
                                    {
                                        .load =>
                                        {
                                            const argument_load = program.instructions.load[argument.get_index()];
                                            const argument_load_size = argument_load.type.get_size();
                                            const argument_direct = function.register_allocator.allocate_call_argument(@intCast(u32, argument_i), argument, @intCast(u8, argument_load_size));
                                            function.register_allocator.free(argument_direct.register);
                                            assert(IR.Instruction.get_ID(argument_load.pointer) == .alloca);
                                            const stack_indirect = function.stack_allocator.get_allocation(argument_load.pointer);

                                            const argument_from_stack_to_reg = mov_register_indirect(argument_direct.register, argument_direct.size, stack_indirect.register, stack_indirect.offset);
                                            function.append_instruction(argument_from_stack_to_reg);
                                        },
                                        .alloca =>
                                        {
                                            const address_of_variable = function.stack_allocator.get_allocation(argument);
                                            const argument_register = function.register_allocator.allocate_call_argument(@intCast(u32, argument_i), argument, Type.Pointer.size);
                                            function.register_allocator.free(argument_register.register);
                                            const argument_pointer_from_stack_to_reg = lea_indirect(argument_register.register, argument_register.size, address_of_variable.register, address_of_variable.offset);
                                            function.append_instruction(argument_pointer_from_stack_to_reg);
                                        },
                                        else => panic("Instruction id: {}\n", .{arg_instruction_id}),
                                    }
                                },
                                else => panic("Arg id: {}\n", .{argument_id}),
                            }
                        }

                        //if (call.arguments.len == 2) @breakpoint();

                        function.append_instruction(call_instruction);

                        // @TODO: do we need to do this?
                        // @TODO: resolve callee
                        //var callee_function = functions[callee_index];

                        var parameter_stack_size = @intCast(i32, std.math.max(4, call_argument_count) * 8);

                        // @TODO: use count
                        if (call.type.value != Type.Builtin.void_type.value and call.type.value != Type.Builtin.noreturn_type.value)
                        {
                            const return_size = @intCast(u8, call.type.get_size());
                            assert(return_size > 0 and return_size <= 8);

                            assert(register_allocator_saver.state.occupation.by_type.legacy[@enumToInt(Register.ID.A)] == .none);
                            if (program.get_uses(.call, instruction.get_index()).len > 0)
                            {
                                _ = function.register_allocator.allocate_return(instruction, return_size);
                            }

                            if (abi == .msvc and return_size > 8)
                            {
                                parameter_stack_size += return_size;
                                unreachable;
                            }
                        }
                        //else
                        //{
                            //// @TODO: assert use count == 0
                        //}

                        if (abi == .msvc)
                        {
                            function.max_call_parameter_size = std.math.max(function.max_call_parameter_size, parameter_stack_size);
                        }

                        if (register_allocator_saver.saved_register_count > 0)
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
                                                    const ret_mov = mov_register_literal(register.register, integer_literal.value, byte_count);
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
                                            const load_processed = process_load_for_ret(program, function, return_expr);
                                            _ = load_processed;
                                        },
                                        .add =>
                                        {
                                            const allocated_register = function.register_allocator.fetch_direct(program, return_expr, instruction) orelse unreachable;
                                            if (allocated_register.register != .A)
                                            {
                                                panic("Badly allocated register\n", .{});
                                            }
                                        },
                                        .call =>
                                        {
                                            const allocated_register = function.register_allocator.fetch_direct(program, return_expr, instruction) orelse unreachable;
                                            if (allocated_register.register != .A)
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
                        const store_pointer_id = IR.Instruction.get_ID(store.pointer);
                        log("Store pointer ID: {}\n", .{store_pointer_id});


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
                                        if (store.type.get_ID() != .integer)
                                        {
                                            codegen_error("Expected type {}\n", .{store.type.get_ID()});
                                        }

                                        const bit_count = Type.Integer.get_bit_count(store.type);
                                        assert((bit_count & 0b111) == 0);
                                        const byte_count = bit_count >> 3;

                                        // @TODO: delay this
                                        const stack_allocation = switch (store_pointer_id)
                                        {
                                            .alloca => blk:
                                            {
                                                break :blk function.stack_allocator.get_allocation(store.pointer);
                                            },
                                            .load => blk:
                                            {
                                                break :blk fetch_load(program, function, store.pointer, instruction, false);
                                            },
                                            else => panic("{}\n", .{store_pointer_id}),
                                        };

                                        log("Allocation: {}\n", .{stack_allocation});

                                        if (!integer_literal.signed)
                                        {
                                            log("Allocation offset: {}\n", .{stack_allocation.offset});
                                            const mov_stack_literal = mov_indirect_immediate_unsigned(stack_allocation.register, stack_allocation.offset, integer_literal.value, byte_count);
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

                                // @TODO: delay this
                                const stack_allocation = switch (store_pointer_id)
                                {
                                    .alloca => blk:
                                    {
                                        break :blk function.stack_allocator.get_allocation(store.pointer);
                                    },
                                    .load => blk:
                                    {
                                        break :blk fetch_load(program, function, store.pointer, instruction, false);
                                    },
                                    else => panic("{}\n", .{store_pointer_id}),
                                };
                                log("Allocation: {}\n", .{stack_allocation});

                                switch (store_instruction_id)
                                {
                                    .add, .sub, .mul =>
                                    {
                                        const register_allocation = function.register_allocator.fetch_direct(program, store.value, instruction) orelse unreachable;
                                        const mov_stack_register = mov_indirect_register(stack_allocation.register, stack_allocation.offset, @intCast(u8, stack_allocation.size), register_allocation.register, register_allocation.size);
                                        function.append_instruction(mov_stack_register);
                                    },
                                    .alloca =>
                                    {
                                        assert(stack_allocation.size == 8);
                                        assert(store.type.get_size() == 8);

                                        const register_allocation = function.register_allocator.fetch_direct(program, store.value, instruction) orelse blk:
                                        {
                                            log("Emitting lea\n", .{});

                                            const alloca_in_stack = function.stack_allocator.get_allocation(store.value);
                                            assert(alloca_in_stack.size == 4);
                                            const register_size = Type.Pointer.size;
                                            const register = function.register_allocator.allocate_direct(store.pointer, register_size);
                                            log("Lea register: {}, size: {}. Lea indirect register: {}. Lea indirect offset: {}\n", .{register.register, register.size, alloca_in_stack.register, alloca_in_stack.offset});
                                            const lea_stack = lea_indirect(register.register, register_size, alloca_in_stack.register, alloca_in_stack.offset);
                                            function.append_instruction(lea_stack);
                                            break :blk register;
                                        };

                                        const mov_stack_register = mov_indirect_register(stack_allocation.register, stack_allocation.offset, register_allocation.size, register_allocation.register, register_allocation.size);
                                        function.append_instruction(mov_stack_register);
                                        function.register_allocator.free(register_allocation.register);
                                    },
                                    .call =>
                                    {
                                        const store_size = store.type.get_size();
                                        log("Store size: {}\n", .{store_size});
                                        const register_allocation = function.register_allocator.fetch_direct(program, store.value, instruction) orelse unreachable;
                                        log("RA: {}\n", .{register_allocation});

                                        const mov_stack_register = mov_indirect_register(stack_allocation.register, stack_allocation.offset, register_allocation.size, register_allocation.register, register_allocation.size);
                                        function.append_instruction(mov_stack_register);
                                        function.register_allocator.free(register_allocation.register);
                                    },
                                    else => panic("{}\n", .{store_instruction_id}),
                                }
                            },
                            .argument =>
                            {
                                const argument_index = store.value.get_index();
                                const argument_alloca = ir_function.argument_allocas[argument_index];
                                const stack_allocation = function.stack_allocator.get_allocation(argument_alloca);
                                const argument_register = function.register_allocator.fetch_argument(argument_index);

                                const mov_arg_to_stack = mov_indirect_register(stack_allocation.register, stack_allocation.offset, @intCast(u8, stack_allocation.size), argument_register.register, argument_register.size);
                                function.append_instruction(mov_arg_to_stack);
                            },
                            else => panic("Store value id: {}\n", .{store_value_id}),
                        }
                    },
                    .load =>
                    {
                        log("Skipping loads\n", .{});
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
                    .sub =>
                    {
                        process_sub(program, function, instruction);
                    },
                    .mul =>
                    {
                        process_mul(program, function, instruction);
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

const Encoding = struct
{
    pub const OldInstruction = struct
    {
        //op_code: [4]u8,
        //options: Options,
        //operand_combinations: [4]Operand.Combination,
        //operand_combination_count: u64,

        pub const Options = struct
        {
            option: Option,
            digit: u8,
            explicit_byte_size: u8,

            pub const Option = enum
            {
                None,
                Digit,
                Reg,
                OpCodePlusReg,
                ExplicitByteSize,
            };
        };

        pub const ID = enum
        {
            adc,
            adcx,
            add,
            adox,
            @"and",
            andn,
            bextr,
            blsi,
            blsmsk,
            blsr,
            bndcl,
            bndcu,
            bndcn,
            bndldx,
            bndmk,
            bndmov,
            bndstx,
            bsf,
            bsr,
            bswap,
            bt,
            btc,
            btr,
            bts,
            bzhi,
            call,
            cbw,
            cwde,
            cdqe,
            clac,
            clc,
            cld,
            cldemote,
            clflush,
            clflushopt,
            cli,
            clrssbsy,
            clts,
            clwb,
            cmc,
            cmov,
            cmp,
            cmpxchg,
            cmpxchg8,
            cmpxchg16,
            cpuid,
            crc32,
            cwd,
            cdq,
            cqo,
            dec,
            div,
            endbr32,
            endbr64,
            enter,
            hlt,
            idiv,
            imul,
            in,
            inc,
            incssp,
            ins,
            int3,
            int,
            invd,
            invlpg,
            invpcid,
            iret,
            ja,
            jae,
            jb,
            jbe,
            jc,
            jecxz,
            jrcxz,
            je,
            jg,
            jge,
            jl,
            jle,
            jna,
            jnae,
            jnb,
            jnbe,
            jnc,
            jne,
            jng,
            jnge,
            jnl,
            jnle,
            jno,
            jnp,
            jns,
            jnz,
            jo,
            jp,
            jpe,
            jpo,
            js,
            jz,
            jmp,
            lar,
            lds,
            lss,
            les,
            lfs,
            lgs,
            lea,
            leave,
            lfence,
            lgdt,
            lidt,
            lldt,
            lmsw,
            lock,
            lods,
            lodsb,
            lodsw,
            lodsd,
            lodsq,
            loop,
            loope,
            loopne,
            lsl,
            ltr,
            lzcnt,
            mfence,
            mov,
            movcr,
            movdbg,
            movbe,
            movdq,
            movdiri,
            movdir64,
            movq,
            movs,
            movsx,
            movzx,
            mul,
            mulx,
            mwait,
            neg,
            nop,
            not,
            @"or",
            out,
            outs,
            pause,
            pdep,
            pext,
            pop,
            popcnt,
            popf,
            por,
            prefetch,
            prefetchw,
            ptwrite,
            push,
            pushf,
            rotate,
            rdfsbase,
            rdgsbase,
            rdmsr,
            rdpid,
            rdpmc,
            rdrand,
            rdseed,
            rdssp,
            rdtsc,
            rdtscp,
            rep,
            ret,
            rsm,
            rstorssp,
            sahf,
            sal,
            sar,
            shl,
            shr,
            sarx,
            shlx,
            shrx,
            saveprevssp,
            sbb,
            scas,
            seta,
            setae,
            setb,
            setbe,
            setc,
            sete,
            setg,
            setge,
            setl,
            setle,
            setna,
            setnae,
            setnb,
            setnbe,
            setnc,
            setne,
            setng,
            setnge,
            setnl,
            setnle,
            setno,
            setnp,
            setns,
            setnz,
            seto,
            setp,
            setpe,
            setpo,
            sets,
            setssbsy,
            sfence,
            sgdt,
            shld,
            shrd,
            sidt,
            sldt,
            smsw,
            stac,
            stc,
            std,
            sti,
            stos,
            str,
            sub,
            swapgs,
            syscall,
            sysenter,
            sysexit,
            sysret,
            @"test",
            tpause,
            tzcnt,
            ud,
            umonitor,
            umwait,
            wait,
            wbinvd,
            wbnoinvd,
            wrfsbase,
            wrgsbase,
            wrmsr,
            wrss,
            wruss,
            xacquire,
            xrelease,
            xabort,
            xadd,
            xbegin,
            xchg,
            xend,
            xgetbv,
            xlat,
            xor,
            xrstor,
            xrstors,
            xsave,
            xsavec,
            xsaveopt,
            xsaves,
            xsetbv,
            xtest,
        };
    };
};
    pub const Rex = enum(u8)
    {
        None = 0,
        Rex = 0x40,
        B = 0x41,
        X = 0x42,
        R = 0x44,
        W = 0x48,
    };

    pub const SIB = enum(u8)
    {
        scale1 = 0b00,
        scale2 = 0b01,
        scale3 = 0b10,
        scale4 = 0b11,
    };

    pub const Mod = enum(u8)
    {
        no_displacement = 0b00,
        displacement8 = 0b01,
        displacement32 = 0b10,
        register = 0b11,
    };
