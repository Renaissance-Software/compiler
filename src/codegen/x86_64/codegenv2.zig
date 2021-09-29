const std = @import("std");
const ArrayList = std.ArrayList;
const alignForward = std.mem.alignForward;
const assert = std.debug.assert;
const panic = std.debug.panic;
const Allocator = std.mem.Allocator;

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
    Compiler.log(.x86_64, format, arguments);
}

fn codegen_error(comptime format: []const u8, arguments: anytype) noreturn
{
    panic(format, arguments);
}

fn get_alloca_size(function: *IR.Function, instruction_ref: IR.Reference) u64
{
    assert(instruction_ref.get_ID() == .instruction);
    assert(IR.Instruction.get_ID(instruction_ref) == .alloca);
    const instruction = function.instructions[instruction_ref.get_index()];
    panic("Instruction: {}\n", .{instruction});
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

const call_rel32_bytes = [_]u8 { 0xe8, 0xcc, 0xcc, 0xcc, 0xcc };
fn call_rel32(rel32: i32) [5]u8
{
    var bytes = call_rel32_bytes;
    const offset = 1;
    for (std.mem.asBytes(&rel32)) |byte, i|
    {
        bytes[offset + i] = byte;
    }

    return bytes;
}

const call_rip_rel32_bytes = [_]u8 { 0x48, 0xff, 0x15, 0xcc, 0xcc, 0xcc, 0xcc };

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

fn mov_register_literal_1_bytes(register: Encoding.Register, number: u8) [2]u8
{
    return [_]u8 { 0xb0 + @enumToInt(register), number };
}

fn mov_register_literal_2_bytes(register: Encoding.Register, number: u16) [4]u8
{
    return [_]u8 { 0x66, 0xb8 + @enumToInt(register), @truncate(u8, number), @truncate(u8, (number & 0xff00) >> 8) };
}

fn mov_register_literal_4_bytes(register: Encoding.Register, number: u32) [5]u8
{
    return [_]u8 { 0xb8 + @enumToInt(register), @truncate(u8, number), @truncate(u8, (number & 0xff00) >> 8), @truncate(u8, (number & 0xff0000) >> 16), @truncate(u8, (number & 0xff000000) >> 24) };
}

fn mov_register_literal_8_bytes(register: Encoding.Register, number: u64) [10]u8
{
    return [_]u8 { 0x48, 0xb8 + @enumToInt(register), @truncate(u8, number), @truncate(u8, (number & 0xff00) >> 8), @truncate(u8, (number & 0xff0000) >> 16), @truncate(u8, (number & 0xff000000) >> 24), @truncate(u8, (number & 0xff00000000) >> 32), @truncate(u8, (number & 0xff0000000000) >> 40), @truncate(u8, (number & 0xff000000000000) >> 48), @truncate(u8, (number & 0xff00000000000000) >> 56) };
}
//fn xor_register_immediate(register: Encoding.Register, register_byte: u8, comptime ImmediateType: type, immediate: ImmediateType) void
//{
    //if (register == .A)
    //{
        //unreachable;
    //}
    //else
    //{
        //unreachable;
    //}
//}

fn xor_register(register: Encoding.Register, size: u8) [2]u8
{
    const mod = 0b11;
    const r_m = @enumToInt(register);

    return switch (size)
    {
        1 => unreachable,
        2 => unreachable,
        4 => [_]u8 { 0x31, (mod << 6) | ((@enumToInt(register) & 0b111) << 3) | (r_m & 0b111)},
        8 => unreachable,
        else => unreachable,
    };
}

const Instruction = struct
{
    const Self = @This();

    bytes: []const u8,
    id: ID,
    resolved: bool,
    operand: Operand, 

    const ID = enum(u16)
    {
        call,
        mov,
        ret,
        xor,
    };

    const Operand = struct
    {
        index: u32,
        kind: Kind,
        offset: u8,

        const Kind = enum(u8)
        {
            none,
            immediate,
            relative_global,
            relative_external,
        };
    };

    fn create_resolved(id: ID, bytes: []const u8) Self
    {
        return .
        {
            .id = id,
            .resolved = true,
            .bytes = bytes,
            .operand = .
            {
                .offset = 0,
                .kind = .none,
                .index = 0,
            },
        };
    }

    fn create_unresolved_operand(id: ID, bytes: []const u8, operand_offset: u8, operand_kind: Operand.Kind, operand_index: u32) Self
    {
        return .
        {
            .id = id,
            .resolved = false,
            .bytes = bytes,
            .operand = .
            {
                .offset = operand_offset,
                .kind = operand_kind,
                .index = operand_index
            },
        };
    }
};

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
        labels: ArrayList(Label),
        previous_patches: ArrayList(BackwardPatch),
        rsp: i32,
        code_buffer_offset: u32,
        max_call_parameter_size: i32,
        terminator: Terminator,
        register_allocator: Register.Allocator,

        const Terminator = enum
        {
            noreturn,
            ret,
        };

    };

    const max_bytes_per_instruction = 15;

    fn estimate_max_code_size(self: *Program) u64
    {
        var total_instruction_count: u64 = 0;
        const aprox_fixed_instruction_count_per_function = 5;

        for (self.functions) |function|
        {
            total_instruction_count += aprox_fixed_instruction_count_per_function + function.instructions.items.len;
        }

        return total_instruction_count * max_bytes_per_instruction;
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
                        log("Function RSP: {}\n", .{function.rsp});
                        if (function.rsp > std.math.maxInt(i8))
                        {
                            text.buffer.appendSlice(sub_rsp_s32(function.rsp)[0..]) catch unreachable;
                            add_rsp_at_epilogue = true;
                        }
                        else if (function.rsp > 0)
                        {
                            text.buffer.appendSlice(sub_rsp_s8(@intCast(i8, function.rsp))[0..]) catch unreachable;
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

            const instruction_count = function.instructions.items.len;
            log("Instruction count: {}\n", .{instruction_count});

            for (function.instructions.items) |instruction|
            {
                if (instruction.resolved)
                {
                    text.buffer.appendSlice(instruction.bytes) catch unreachable;
                }
                else
                {
                    const instruction_id = instruction.id;
                    switch (instruction_id)
                    {
                        .call =>
                        {
                            const operand_index = instruction.operand.index;
                            const from = text.buffer.items.len + instruction.bytes.len;

                            switch (instruction.operand.kind)
                            {
                                .relative_global =>
                                {
                                    if (operand_index <= function_i)
                                    {
                                        const target = self.functions[operand_index].code_buffer_offset;
                                        const rel32 = @intCast(i32, @intCast(i64, target) - @intCast(i64, from));
                                        text.buffer.appendSlice(call_rel32(rel32)[0..]) catch unreachable;
                                    }
                                    else
                                    {
                                        const code_buffer_offset = @intCast(u32, text.buffer.items.len + instruction.operand.offset);
                                        self.functions[operand_index].previous_patches.append(.{ .code_buffer_offset = code_buffer_offset }) catch unreachable;
                                        text.buffer.appendSlice(call_rel32_bytes[0..]) catch unreachable;
                                    }
                                },
                                .relative_external =>
                                {
                                    std.debug.print("operand offset: {}\n", .{instruction.operand.offset});
                                    const external_function_index = Parser.Function.External.Index.from_u32(operand_index);
                                    std.debug.print("Library index: {}. Function index: {}\n", .{external_function_index.library, external_function_index.function});
                                    const target_index = @intCast(u32, text.buffer.items.len + instruction.operand.offset);
                                    std.debug.print("Adding patch with rip index: {}\n", .{target_index});
                                    text_out.patches.append(.
                                    {
                                        .section_buffer_index_to = target_index,
                                        .section_buffer_index_from = operand_index,
                                        .section_to_write_to = .@".text",
                                        .section_to_read_from = .@".rdata",
                                    }) catch unreachable;

                                    text.buffer.appendSlice(call_rip_rel32_bytes[0..]) catch unreachable;
                                },
                                else => unreachable,
                            }
                        },
                        else => unreachable,
                    }
                }
            }

            if (add_rsp_at_epilogue)
            {
                if (function.terminator == .ret)
                {
                    switch (abi)
                    {
                        .msvc =>
                        {
                            if (function.rsp > std.math.maxInt(i8))
                            {
                                text.buffer.appendSlice(add_rsp_s32(function.rsp)[0..]) catch unreachable;
                            }
                            else if (function.rsp > 0)
                            {
                                text.buffer.appendSlice(add_rsp_s8(@intCast(i8, function.rsp))[0..]) catch unreachable;
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
                text.buffer.append(ret_bytes[0]) catch unreachable;
            }
            else if (function.terminator == .noreturn)
            {
                // Append Int3
                text.buffer.append(0xcc) catch unreachable;
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
};

const Register = struct
{
    value: ?IR.Reference,
    size: u8,

    const count = 16;
    const ArrayType = [Register.count]Register; 

    const Allocator = struct
    {
        registers: Register.ArrayType, 
        argument_registers: []const Encoding.Register,

        const Self = @This();

        fn new(argument_registers: []const Encoding.Register) Self
        {
            const result = Self
            {
                .registers = std.enums.directEnumArray(Register.ID, Register, 4,.{
                    .A   =  .{ .size = 0, .value = null, },
                    .C   =  .{ .size = 0, .value = null, },
                    .D   =  .{ .size = 0, .value = null, },
                    .B   =  .{ .size = 0, .value = null, },
                    .r8  =  .{ .size = 0, .value = null, },
                    .r9  =  .{ .size = 0, .value = null, },
                    .r10 =  .{ .size = 0, .value = null, },
                    .r11 =  .{ .size = 0, .value = null, },
                    .r12 =  .{ .size = 0, .value = null, },
                    .r13 =  .{ .size = 0, .value = null, },
                    .r14 =  .{ .size = 0, .value = null, },
                    .r15 =  .{ .size = 0, .value = null, },
                }),
                .argument_registers = argument_registers,
            };

            return result;
        }

        fn spill_registers_before_call(self: *Register.Allocator) State
        {
            log("Saving registers before call...\n", .{});

            var saved_register_count: u32 = 0;

            var registers = std.mem.zeroes(Register.ArrayType);

            for (self.registers) |register, register_i|
            {
                if (register.value != null and must_save(@intToEnum(Encoding.Register, register_i)))
                {
                    registers[register_i] = register;
                    saved_register_count += 1;
                }
            }

            log("Registers to be saved: {}\n", .{saved_register_count});

            if (saved_register_count > 0)
            {
                for (registers) |register, register_i|
                {
                    _ = register_i;
                    if (register.value != null)
                    {
                        panic("IR value: {}\n", .{register.value.?});
                    }
                }
            }

            if (saved_register_count > 0) unreachable;
            return std.mem.zeroes(State);
        }

        fn allocate_argument(self: *Register.Allocator, argument_reference: IR.Reference, byte_count: u8) Encoding.Register
        {
            for (self.argument_registers) |r|
            {
                const register_index = @enumToInt(r);

                var register = &self.registers[register_index];
                if (register.value == null)
                {
                    register.value = argument_reference;
                    register.size = byte_count;

                    return r;
                }
            }

            panic("No argument register ready\n", .{});
        }

        fn allocate_return(self: *Register.Allocator, reference: IR.Reference, byte_count: u16) Encoding.Register
        {
            assert(byte_count <= 8);
            var register = &self.registers[@enumToInt(Encoding.Register.A)];
            if (register.value == null)
            {
                register.value = reference;
                register.size = @intCast(u8, byte_count);

                return Encoding.Register.A;
            }

            panic("Return register A is busy\n", .{});
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

    const function_count = program.functions.len;
    assert(function_count > 0);

    //assert(std.mem.eql(program.functions[0].declaration.name, "entry"));

    var functions = ArrayList(Program.Function).initCapacity(allocator, function_count) catch unreachable;
    var data_buffer = ArrayList(u8).init(allocator);

    for (program.functions) |*function|
    {
        const basic_block_count = function.basic_blocks.len;
        var labels = ArrayList(Label).initCapacity(allocator, basic_block_count) catch unreachable;
        labels.resize(basic_block_count) catch unreachable;
        log("Creating {} labels...\n", .{basic_block_count});

        for (function.basic_blocks[0].instructions) |instruction|
        {
            if (IR.Instruction.get_ID(instruction) == .alloca)
            {
                const alloca_size = get_alloca_size(function, instruction);
                _ = alloca_size;
                // stack allocate this
                unreachable;
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
                .labels = labels,
                .rsp = 0,
                .max_call_parameter_size = 0,
                .previous_patches = ArrayList(BackwardPatch).init(allocator),
                .code_buffer_offset = 0,
                .terminator = .noreturn,
                .register_allocator = Register.Allocator.new(argument_registers),
            }) catch unreachable;

        // @TODO:
        // resize and initialize labels
        //   unreachable;
    }

    for (program.functions) |*ir_function, ir_function_i|
    {
        var function = &functions.items[ir_function_i];

        for (ir_function.basic_blocks) |*basic_block, basic_block_i|
        {
            assert(basic_block.instructions.len > 0);
            // @TODO:
            // reset register allocator
            const function_offset = @intCast(u32, function.instructions.items.len);
            function.labels.items[basic_block_i].unresolved.instruction_index = function_offset;

            for (basic_block.instructions) |instruction|
            {
                const instruction_id = IR.Instruction.get_ID(instruction);
                const instruction_index = instruction.get_index();

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
                        var call_operand_kind: Instruction.Operand.Kind = undefined;
                        var operand_offset: u8 = undefined;

                        switch (callee_id)
                        {
                            .global_function =>
                            {
                                assert(callee_index <= std.math.maxInt(i32));
                                function_type = program.functions[callee_index].type;
                                call_operand_kind = .relative_global;
                                operand_offset = 1;
                            },
                            .external_function =>
                            {
                                const external_function = program.external.functions[callee_index];
                                function_type = external_function.declaration.type;
                                callee_index = external_function.index.to_u32();
                                call_operand_kind = .relative_external;
                                operand_offset = 3;
                            },
                            else => unreachable,
                        }

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

                                            log("Integer literal index: {}\n", .{argument_array_index});
                                            const integer_literal = program.integer_literals[argument_array_index];
                                            //@TODO: typecheck signedness
                                            assert(!integer_literal.signed);
                                            const literal = integer_literal.value;

                                            const integer_bit_count = Type.Integer.get_bit_count(argument_type);
                                            // assert n % 8 == 0
                                            assert((integer_bit_count & 0x7) == 0);
                                            const integer_byte_count = @truncate(u8, integer_bit_count >> 3);
                                            const argument_register = function.register_allocator.allocate_argument(argument, integer_byte_count);
                                            log("Argument register: {}\n", .{argument_register});
                                            log("Byte count: {}\n", .{integer_byte_count});

                                            if (literal == 0)
                                            {
                                                // do xor
                                                function.instructions.append(Instruction.create_resolved(.xor, xor_register(argument_register, integer_byte_count)[0..])) catch unreachable;
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

                        const call_instruction = Instruction.create_unresolved_operand(.call, call_rel32_bytes[0..], operand_offset, call_operand_kind, callee_index);
                        function.instructions.append(call_instruction) catch unreachable;

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
                                                    const register = function.register_allocator.allocate_return(return_expr, byte_count);
                                                    const ret_mov = switch (byte_count)
                                                    {
                                                        1 => Instruction.create_resolved(.mov, mov_register_literal_1_bytes(register, @truncate(u8, integer_literal.value))[0..]),
                                                        2 => Instruction.create_resolved(.mov, mov_register_literal_2_bytes(register, @truncate(u16, integer_literal.value))[0..]),
                                                        4 => Instruction.create_resolved(.mov, mov_register_literal_4_bytes(register, @truncate(u32, integer_literal.value))[0..]),
                                                        8 => Instruction.create_resolved(.mov, mov_register_literal_8_bytes(register, @truncate(u64, integer_literal.value))[0..]),
                                                        else => unreachable,
                                                    };

                                                    function.instructions.append(ret_mov) catch unreachable;
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
                                else => panic("RE: {}\n", .{return_expr_id}),
                            }
                        }

                        function.terminator = .ret;
                        //function.instructions.append(Instruction.create_resolved(.ret, ret_bytes[0..])) catch unreachable;
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
                const alignment = 0x8;
                function.rsp += function.max_call_parameter_size;
                function.rsp = @boolToInt(function.rsp > 0) * (@intCast(i32, alignForward(@intCast(u32, function.rsp), 0x10)) + alignment);
            },
            else => panic("not implemented: {}\n", .{abi}),
        }
    }

    for (functions.items) |function, function_i|
    {
        log("\nFunction #{}\n", .{function_i});
        for (function.instructions.items) |instruction, instruction_i|
        {
            log("#{} {}\n", .{instruction_i, instruction.id});
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
