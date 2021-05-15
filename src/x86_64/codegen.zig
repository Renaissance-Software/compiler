const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const assert = std.debug.assert;
const panic = std.debug.panic;
const print = std.debug.print;

const IR = @import("../bytecode.zig");

const Encoding = @import("encoding.zig");
const Mnemonic = Encoding.Instruction.ID;

const encode_frame_pointer = true;

const log = std.log.scoped(.x86_64_codegen);

const c = @cImport(
{
    @cInclude("unistd.h");
    @cInclude("sys/mman.h");
});

const CallingConvention = enum
{
    MSVC,
    SystemV,
};

const calling_convention = CallingConvention.SystemV;

const Operand = struct
{
    value: Value,
    size: u32,

    const Value = union(ID)
    {
        none,
        immediate: Immediate,
        // @Info: This is an index to the label
        relative: Relative,
        register: Encoding.Register,
        indirect: Indirect,
        rip_relative,
        import_rip_relative,
    };

    const ID = enum
    {
        none,
        immediate,
        relative,
        register,
        indirect,
        rip_relative,
        import_rip_relative,
    };

    const Size = enum(u8)
    {
        any = 0,
        bits8 = 1,
        bits16 = 2,
        bits32 = 4,
        bits48 = 6,
        bits64 = 8,
        bits80 = 10,
    };

    const Relative = struct
    {
        label: *Label,

        fn create(value: *IR.Value, module: *IR.Module, current_function: *IR.Function, function_index: u64, executable: *Executable) Operand
        {
            var label: *Label = undefined;

            switch (value.id)
            {
                IR.Value.ID.BasicBlock =>
                {
                    const basic_block = @ptrCast(*IR.BasicBlock, value);
                    for (current_function.basic_blocks.items) |bb, i|
                    {
                        if (basic_block == bb)
                        {
                            label = &executable.functions.items[function_index].labels.items[i];
                            break;
                        }
                    }
                },
                else => panic("ni: {}\n", .{value.id}),
            }

            return Operand
            {
                .value = Operand.Value {
                    .relative = Operand.Relative {
                        .label = label,
                    },
                },
                // @Info: all relatives should be treated with  4-byte size
                .size = 4,
            };
        }
    };

    const Immediate = extern union
    {
        imm8: u8,
        imm16: u16,
        imm32: u32,
        imm64: u64,
        imm_arr: [8]u8,
    };

    const Indirect = struct
    {
        displacement: i32,
        register: Encoding.Register,
    };

    pub fn format(self: Operand, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void
    {
        switch (self.value)
        {
            Operand.ID.register =>
            {
                switch (self.size)
                {
                    8 =>
                    {
                        switch (self.value.register)
                        {
                            Encoding.Register.A => try std.fmt.format(writer, "rax", .{}),
                            Encoding.Register.C => try std.fmt.format(writer, "rcx", .{}),
                            Encoding.Register.D => try std.fmt.format(writer, "rdx", .{}),
                            Encoding.Register.B => try std.fmt.format(writer, "rbx", .{}),
                            Encoding.Register.SP => try std.fmt.format(writer, "rsp", .{}),
                            Encoding.Register.BP => try std.fmt.format(writer, "rbp", .{}),
                            Encoding.Register.SI => try std.fmt.format(writer, "rsi", .{}),
                            Encoding.Register.DI => try std.fmt.format(writer, "rdi", .{}),

                            Encoding.Register.r8 => try std.fmt.format(writer, "r8", .{}),
                            Encoding.Register.r9 => try std.fmt.format(writer, "r9", .{}),
                            Encoding.Register.r10 => try std.fmt.format(writer, "r10", .{}),
                            Encoding.Register.r11 => try std.fmt.format(writer, "r11", .{}),
                            Encoding.Register.r12 => try std.fmt.format(writer, "r12", .{}),
                            Encoding.Register.r13 => try std.fmt.format(writer, "r13", .{}),
                            Encoding.Register.r14 => try std.fmt.format(writer, "r14", .{}),
                            Encoding.Register.r15 => try std.fmt.format(writer, "r15", .{}),
                        }
                    },
                    4 =>
                    {
                        switch (self.value.register)
                        {
                            Encoding.Register.A => try std.fmt.format(writer, "eax", .{}),
                            Encoding.Register.C => try std.fmt.format(writer, "ecx", .{}),
                            Encoding.Register.D => try std.fmt.format(writer, "edx", .{}),
                            Encoding.Register.B => try std.fmt.format(writer, "ebx", .{}),
                            Encoding.Register.SP => try std.fmt.format(writer, "esp", .{}),
                            Encoding.Register.BP => try std.fmt.format(writer, "ebp", .{}),
                            Encoding.Register.SI => try std.fmt.format(writer, "esi", .{}),
                            Encoding.Register.DI => try std.fmt.format(writer, "edi", .{}),

                            else => unreachable,
                        }
                    },
                    2 =>
                    {
                        switch (self.value.register)
                        {
                            Encoding.Register.A => try std.fmt.format(writer, "ax", .{}),
                            Encoding.Register.C => try std.fmt.format(writer, "cx", .{}),
                            Encoding.Register.D => try std.fmt.format(writer, "dx", .{}),
                            Encoding.Register.B => try std.fmt.format(writer, "bx", .{}),
                            Encoding.Register.SP => try std.fmt.format(writer, "sp", .{}),
                            Encoding.Register.BP => try std.fmt.format(writer, "bp", .{}),
                            Encoding.Register.SI => try std.fmt.format(writer, "si", .{}),
                            Encoding.Register.DI => try std.fmt.format(writer, "di", .{}),
                            else => unreachable,
                        }
                    },
                    1 =>
                    {
                        panic("ni\n", .{});
                    },
                    else => panic("unreachable\n", .{}),
                }
            },
            Operand.ID.immediate =>
            {
                switch (self.size)
                {
                    8 => try std.fmt.format(writer, "0x{x}", .{self.value.immediate.imm64}),
                    4 => try std.fmt.format(writer, "0x{x}", .{self.value.immediate.imm32}),
                    2 => try std.fmt.format(writer, "0x{x}", .{self.value.immediate.imm16}),
                    1 => try std.fmt.format(writer, "0x{x}", .{self.value.immediate.imm8}),
                    else => unreachable,
                }
            },
            Operand.ID.indirect =>
            {
                const displacement = self.value.indirect.displacement;

                const sign: u8 = blk: {
                    if (displacement < 0)
                    {
                        break :blk '-';
                    }
                    else
                    {
                        break :blk '+';
                    }
                };

                const word_str = blk: {
                    switch (self.size)
                    {
                        8 => break :blk "QWORD",
                        4 => break :blk "DWORD",
                        2 => break :blk "WORD",
                        1 => break :blk "BYTE",
                        else => panic("ni: {}\n", .{self.size}),
                    }
                };

                const reg_str = blk:
                {
                    const reg = self.value.indirect.register;
                    switch (reg)
                    {
                        Encoding.Register.BP => break :blk "rbp",
                        else => panic("ni: {}\n", .{reg}),
                    }
                };

                if (displacement != 0)
                {
                    const displacement_abs = std.math.absCast(displacement);

                    try std.fmt.format(writer, "{s} PTR [{s}{c}0x{x}]", .{word_str, reg_str, sign, displacement_abs});
                }
                else
                {
                    panic("displacement is zero: implement\n", .{});
                }
            },
            Operand.ID.relative =>
            {
                const label = self.value.relative.label;
                try std.fmt.format(writer, "relative(unresolved)", .{});
            },
            else => panic("ni: {}\n", .{self.value}),
        }
    }
};

const Label = struct
{
    instruction_index: u64,
    target: u64,
    locations: ArrayList(Location),

    const Location = struct
    {
        relative_to_be_written_address: u64, // @Info: target
        address_after_instruction: u64, // @Info: offset
    };
};

const Instruction = struct
{
    id: Mnemonic,
    operands: [4]Operand,
    operand_count: u64,

    fn add_operand(self: *Instruction, operand: Operand) void
    {
        const operand_count = self.operand_count;
        assert(operand_count < self.operands.len);
        self.operands[operand_count] = operand;
        self.operand_count += 1;
    }

    pub fn format(self: Instruction, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void
    {
        try std.fmt.format(writer, "{s}", .{@tagName(self.id)});
        if (self.operand_count > 0)
        {
            try writer.writeAll(" ");

            var i: u64 = 0;
            while (i < self.operand_count - 1) : (i += 1)
            {
                try std.fmt.format(writer, "{}, ", .{self.operands[i]});
            }
            try std.fmt.format(writer, "{}", .{self.operands[i]});
        }
    }
};

fn create_instruction(instruction_id: Mnemonic, operands: ?[]const Operand) Instruction
{
    var instruction = Instruction
    {
        .id = instruction_id,
        .operands = undefined,
        //.label = undefined,
        .operand_count = 0,
    };

    if (operands) |operand_slice|
    {
        for (operand_slice) |operand|
        {
            instruction.add_operand(operand);
        }
    }

    return instruction;
}

const Function = struct
{
    instructions: InstructionBuffer,
    labels: LabelBuffer,

    fn append(self: *Function, instruction: Instruction) void
    {
        log.debug("Appending instruction: {}\n", .{instruction});
        self.instructions.append(instruction) catch |err| {
            panic("Error allocating memory for instruction\n", .{});
        };
    }
};

const InstructionBuffer = ArrayList(Instruction);
const FunctionBuffer = ArrayList(Function);
const LabelBuffer = ArrayList(Label);

const Executable = struct
{
    functions: FunctionBuffer,
    code_buffer: std.ArrayList(u8),
    code_base_RVA: u64,
};

fn get_stack_register() Encoding.Register
{
    const register = switch (calling_convention)
    {
        CallingConvention.SystemV => Encoding.Register.BP,
        CallingConvention.MSVC => Encoding.Register.SP,
        //else => panic("ni: {}\n", .{calling_convention}),
    };

    return register;
}

fn encode_instruction(allocator: *Allocator, executable: *Executable, instruction: Instruction) void
{
    log.debug("\nEncoding instruction: {}...\n", .{instruction.id});
    const encodings = Encoding.instructions[@enumToInt(instruction.id)];
    if (encodings.len == 0)
    {
        panic("Instruction not implemented yet: {}\n", .{instruction.id});
    }

    //log.debug("Encoding count: {}\n", .{encodings.len});
    const operand_count = instruction.operand_count;

    for (encodings) |encoding|
    {
        var combination_index: u64 = 0;
        if (encoding.operand_combination_count == 0 and operand_count == 0)
        {
            panic("reached here\n", .{});
        }

        while (combination_index < encoding.operand_combination_count) : (combination_index += 1)
        {
            const combination = encoding.operand_combinations[combination_index];
            var matched = true;

            //log.debug("---Combination---\n", .{});
            //log.debug("Operand count. Required: {}. Current: {}\n", .{operand_count, combination.count});
            if (operand_count != combination.count)
            {
                log.debug("Operand count is different. Skipping...\n", .{});
                continue;
            }

            var operand_index: u64 = 0;

            while (operand_index < operand_count) : (operand_index += 1)
            {
                const operand = instruction.operands[operand_index];
                const operand_encoding = combination.operands[operand_index];
                //log.debug("Operand ID: {s}. Operand encoding ID: {}\n", .{@tagName(operand.value), operand_encoding.id});
                //log.debug("Operand size: {}. Encoding size: {}\n", .{operand.size, operand_encoding.size});

                switch (operand.value)
                {
                    Operand.ID.register =>
                    {
                        if (operand_encoding.id == Encoding.Operand.ID.register and operand_encoding.size == operand.size)
                        {
                            continue;
                        }
                        if (operand_encoding.id == Encoding.Operand.ID.register_A and operand.value.register == Encoding.Register.A and operand_encoding.size == operand.size)
                        {
                            continue;
                        }
                        if (operand_encoding.id == Encoding.Operand.ID.register_or_memory and operand_encoding.size == operand.size)
                        {
                            continue;
                        }
                    },
                    Operand.ID.immediate =>
                    {
                        if (operand_encoding.id == Encoding.Operand.ID.immediate and operand_encoding.size == operand.size)
                        {
                            continue;
                        }
                    },
                    Operand.ID.indirect =>
                    {
                        if (operand_encoding.id == Encoding.Operand.ID.register_or_memory)
                        {
                            continue;
                        }
                        if (operand_encoding.id == Encoding.Operand.ID.memory)
                        {
                            continue;
                        }
                    },
                    Operand.ID.relative =>
                    {
                        // @TODO: size is 0 here, so it's unimportant to compare them to the encoding sizes
                        if (operand_encoding.id == Encoding.Operand.ID.relative and operand_encoding.size == 4)
                        {
                            continue;
                        }
                        if (operand_encoding.id == Encoding.Operand.ID.memory)
                        {
                            continue;
                        }
                    },
                    else => panic("not implemented: {}\n", .{operand.value}),
                }

                matched = false;
                break;
            }
            
            if (matched)
            {
                log.debug("Matched instruction\n", .{});
                const instruction_offset = executable.code_buffer.items.len; 
                //const instruction_offset_address = @ptrToInt(executable.code_buffer.items.ptr) + instruction_offset;
                //log.debug("Instruction offset address: 0x{x}\n", .{instruction_offset_address});

                // @TODO: label
                //

                var rex_byte: u8 = @enumToInt(combination.rex);
                log.debug("Initial REX byte: 0x{x}\n", .{rex_byte});
                var memory_encoding = false;
                var r_m_encoding = false;

                operand_index = 0;
                while (operand_index < operand_count) : (operand_index += 1)
                {
                    const operand = instruction.operands[operand_index];
                    const operand_encoding = combination.operands[operand_index];
                    log.debug("Operand encoding: {}\n", .{operand_encoding.id});

                    if (operand.value == Operand.ID.register and (@enumToInt(operand.value.register) & Encoding.Register.NFlag) != 0)
                    {
                        if (encoding.options.option == Encoding.Instruction.Options.Option.Digit)
                        {
                            rex_byte |= @enumToInt(Encoding.Rex.R);
                        }
                        else if (encoding.options.option == Encoding.Instruction.Options.Option.OpCodePlusReg)
                        {
                            rex_byte |= @enumToInt(Encoding.Rex.B);
                        }
                    }
                    else if (operand_encoding.id == Encoding.Operand.ID.register_or_memory)
                    {
                        r_m_encoding = true;
                    }
                    else if (operand_encoding.id == Encoding.Operand.ID.memory)
                    {
                        memory_encoding = true;
                    }
                }

                var reg_code: u8 = undefined;
                var op_code: [encoding.op_code.len]u8 = encoding.op_code;

                if (encoding.options.option == Encoding.Instruction.Options.Option.OpCodePlusReg)
                {
                    var plus_reg_op_code = op_code[0];
                    assert(op_code[1] == 0);
                    assert(op_code[2] == 0);
                    assert(op_code[3] == 0);

                    reg_code = @enumToInt(instruction.operands[0].value.register);
                    const d = plus_reg_op_code & 0b10 != 0;
                    const s = plus_reg_op_code & 0b1 != 0;
                    plus_reg_op_code = (plus_reg_op_code & 0b11111000) | (reg_code & 0b111);
                    op_code[0] = plus_reg_op_code;
                }

                // MOD RM
                var need_sib = false;
                var sib_byte: u8 = 0;
                const is_digit = encoding.options.option == Encoding.Instruction.Options.Option.Digit;
                const is_reg = encoding.options.option == Encoding.Instruction.Options.Option.Reg;

                const need_mod_rm = is_digit or is_reg or r_m_encoding or memory_encoding;
                var register_or_digit: u8 = 0;
                var r_m: u8 = 0;
                var mod: u8 = 0;
                var mod_r_m: u8 = 0;

                if (need_mod_rm)
                {
                    log.debug("It needs MOD RM. Doing operations...\n", .{});

                    operand_index = 0;

                    while (operand_index < operand_count) : (operand_index += 1)
                    {
                        const operand = instruction.operands[operand_index];
                        log.debug("Operand {}. ID: {s}\n", .{operand_index, @tagName(operand.value)});

                        switch (operand.value)
                        {
                            Operand.ID.register =>
                            {
                                const register_value = @enumToInt(operand.value.register);
                                if (register_value & Encoding.Register.NFlag != 0)
                                {
                                    rex_byte |= @enumToInt(Encoding.Rex.R);
                                }

                                if (operand_index == 0)
                                {
                                    mod = @enumToInt(Encoding.Mod.register);
                                    r_m = register_value;
                                    reg_code = register_value;
                                    if (is_reg)
                                    {
                                        register_or_digit = register_value;
                                    }
                                }
                                else if (operand_index == 1)
                                {
                                    if (is_reg)
                                    {
                                        register_or_digit = register_value;
                                    }
                                }
                            },
                            Operand.ID.indirect =>
                            {
                                const indirect_register = @enumToInt(operand.value.indirect.register);
                                const displacement = operand.value.indirect.displacement;
                                const abs = std.math.absCast(displacement);
                                if (abs > std.math.maxInt(u8))
                                {
                                    mod = @enumToInt(Encoding.Mod.displacement32);
                                }
                                else
                                {
                                    mod = @enumToInt(Encoding.Mod.displacement8);
                                }
                                r_m = indirect_register;
                                need_sib = indirect_register == @enumToInt(Encoding.Register.SP);

                                if (need_sib)
                                {
                                    sib_byte = (@enumToInt(Encoding.SIB.scale1) << 6) | (r_m << 3) | r_m;
                                }
                            },
                            Operand.ID.immediate => { },
                            else => panic("ni: {}\n", .{operand.value}),
                        }
                    }

                    if (is_digit)
                    {
                        register_or_digit = encoding.options.digit;
                    }

                    mod_r_m = ((mod & 0b11) << 6) | ((register_or_digit & 0b111) << 3) | (r_m & 0b111);
                }

                if (rex_byte != 0)
                {
                    log.debug("Writing rex byte: 0x{x}\n", .{rex_byte});

                    executable.code_buffer.append(rex_byte) catch unreachable;
                }
                else if ((instruction.operands[0].value == Operand.ID.register and instruction.operands[0].size == 2) or (instruction.operands[1].value == Operand.ID.register and instruction.operands[1].size == 2) or  encoding.options.explicit_byte_size == 2)
                {
                    executable.code_buffer.append(Encoding.Operand.size_override) catch unreachable;
                }

                // @TODO:
                const op_code_byte = op_code[0];
                log.debug("Writing op code byte {}: 0x{x}\n", .{0, op_code_byte});
                executable.code_buffer.append(op_code_byte) catch unreachable;
                assert(op_code[1] == 0);
                assert(op_code[2] == 0);
                assert(op_code[3] == 0);

                if (need_mod_rm)
                {
                    log.debug("Writing Mod RM: 0x{x}\n", .{mod_r_m});

                    executable.code_buffer.append(mod_r_m) catch unreachable;
                }

                if (need_sib)
                {
                    log.debug("Writing SIB byte: 0x{x}\n", .{sib_byte});

                    executable.code_buffer.append(sib_byte) catch unreachable;
                }

                // Displacement
                if (need_mod_rm and mod != @enumToInt(Encoding.Mod.register))
                {
                    operand_index = 0;

                    while (operand_index < operand_count) : (operand_index += 1)
                    {
                        const operand = instruction.operands[operand_index];

                        switch (operand.value)
                        {
                            Operand.ID.indirect =>
                            {
                                const displacement = operand.value.indirect.displacement;

                                switch (@intToEnum(Encoding.Mod, mod))
                                {
                                    Encoding.Mod.displacement8 =>
                                    {
                                        const displacement8 = @intCast(i8, displacement);
                                        executable.code_buffer.appendSlice(std.mem.asBytes(&displacement8)) catch |err| {
                                            panic("Error appending the displacement bytes\n", .{});
                                        };
                                    },
                                    Encoding.Mod.displacement32 =>
                                    {
                                        log.debug("Writing displacement: 4 bytes\n", .{});
                                        executable.code_buffer.appendSlice(std.mem.asBytes(&displacement)) catch |err| {
                                            panic("Error appending the displacement bytes\n", .{});
                                        };
                                    },
                                    else => {},
                                }
                            },
                            Operand.ID.import_rip_relative =>
                            {
                                panic("ni\n", .{});
                            },
                            Operand.ID.rip_relative =>
                            {
                                panic("ni\n", .{});
                            },
                            else => {},
                        }
                    }
                }

                operand_index = 0;
                log.debug("Operand count: {}\n", .{operand_count});

                // Immediate, relatives
                while (operand_index < operand_count) : (operand_index += 1)
                {
                    const operand = instruction.operands[operand_index];

                    switch (operand.value)
                    {
                        Operand.ID.immediate =>
                        {
                            const operand_size = operand.size;
                            log.debug("Writing immediate operand of size: {}\n", .{operand_size});
                            var byte: u64 = 0;
                            while (byte < operand_size) : (byte += 1)
                            {
                                executable.code_buffer.append(operand.value.immediate.imm_arr[byte]) catch |err| {
                                    panic("Error encoding immediate\n", .{});
                                };
                            }
                        },
                        Operand.ID.relative =>
                        {
                            const jump_size: u64 = 4;
                            const label = operand.value.relative.label;
                            if (label.target != 0xcccccccccccccccc)
                            {
                                log.debug("Label is resolved: 0x{x}\n", .{label.target});
                                const patch_target = @intToPtr(*align(1) i32, @ptrToInt(executable.code_buffer.items.ptr) + executable.code_buffer.items.len);
                                const instruction_after_address = @ptrToInt(patch_target) + jump_size;
                                const target = @intCast(i64, label.target); 
                                const address_after_instruction = @intCast(i64, instruction_after_address);
                                const sub_result = target - address_after_instruction;
                                assert(sub_result >= std.math.minInt(i32) and sub_result <= std.math.maxInt(i32));
                                const difference = @intCast(i32, sub_result);
                                log.debug("Difference: {}\n", .{difference});
                                log.debug("To be patch content: {}\n", .{patch_target.*});
                                executable.code_buffer.appendSlice(std.mem.asBytes(&difference)) catch |err| {
                                    panic("Error appending relative operand bytes\n", .{});
                                };
                            }
                            else
                            {
                                log.debug("Label is not resolved, adding a patch location to the label\n", .{});
                                const patch_target = @ptrToInt(executable.code_buffer.items.ptr) + executable.code_buffer.items.len;
                                const unresolved_jump_value: u32 = 0xcccccccc;
                                assert(@sizeOf(@TypeOf(unresolved_jump_value)) == jump_size);
                                executable.code_buffer.appendSlice(std.mem.asBytes(&unresolved_jump_value)) catch |err| {
                                    panic("Error appending unresolved relative operand\n", .{});
                                };
                                label.locations.append(Label.Location {
                                    .relative_to_be_written_address = patch_target,
                                    .address_after_instruction = patch_target + jump_size,
                                }) catch |err| {
                                    panic("Error adding a patch location to the label\n", .{});
                                };
                            }
                        },
                        Operand.ID.register, Operand.ID.indirect => {},
                        else => panic("ni: {}\n", .{operand.value}),
                    }
                }

                const instruction_slice = executable.code_buffer.items[instruction_offset..];

                log.debug("Encoded instruction: {}:\n", .{instruction.id});
                for (instruction_slice) |byte|
                {
                    log.debug("0x{x}|", .{byte});
                }
                log.debug("\n", .{});

                log.debug("The code buffer has so far:\n", .{});

                for (executable.code_buffer.items) |byte|
                {
                    log.debug("0x{x}|", .{byte});
                }

                log.debug("\n", .{});
                return;
            }
        }
    }

    panic("Unable to find a fitting instruction: {}", .{instruction.id});
}

//Fix the bad encoding on stack movs

pub const ExecutionBuffer = struct
{
    fn allocFn(allocator: *Allocator, len: usize, ptr_align: u29, len_align: u29, ret_addr: usize) std.mem.Allocator.Error![]u8
    {
        const mmap_result = c.mmap(null, len, c.PROT_READ |c.PROT_WRITE | c.PROT_EXEC, c.MAP_PRIVATE | c.MAP_ANON, -1, 0);
        if (@ptrToInt(mmap_result) != 0)
        {
            return @ptrCast([*]u8, mmap_result)[0..len];
        }
        else
        {
            return error.OutOfMemory;
        }
    }
    fn resizeFn(allocator: *Allocator, buf: []u8, buf_align: u29, new_len: usize, len_align: u29, ret_addr: usize) std.mem.Allocator.Error!usize
    {
        return error.OutOfMemory;
    }
};

fn stack_operand(offset: i32, size: u32) Operand
{
    const operand = Operand
    {
        .value = Operand.Value {
            .indirect = Operand.Indirect {
                .displacement = -offset,
                .register = get_stack_register(),
            },
        },
        .size = size,
    };

    return operand;
}

fn constant_int_operand(constant_int: *IR.ConstantInt) Operand
{
    switch (constant_int.bit_count)
    {
        32 =>
        {
            const value: u32 = blk: {
                if (constant_int.is_signed)
                {
                    break :blk @intCast(u32, - @intCast(i32, constant_int.int_value));
                }
                else
                {
                    break :blk @intCast(u32, constant_int.int_value);
                }
            };

            return Operand 
            {
                .value = Operand.Value {
                    .immediate = Operand.Immediate {
                        .imm32 = value,
                    },
                    },
                .size = @enumToInt(Operand.Size.bits32),
            };
        },
        else => panic("ni: {}\n", .{constant_int.bit_count}),
    }
}

fn get_stack_offset(ir_function: *IR.Function, alloca: *IR.Instruction) i32
{
    var stack_offset: i32 = 0;
    var index: u64 = 0;

    while (true)
    {
        const alloca_instruction = ir_function.basic_blocks.items[0].instructions.items[index];
        stack_offset += @intCast(i32, alloca_instruction.value.alloca.type.size);

        if (alloca_instruction == alloca)
        {
            break;
        }

        index += 1;
    }

    return stack_offset;
}

const RegisterAllocator = struct
{
    registers: [16]AllocatedRegister,

    fn allocate(self: *RegisterAllocator, value: *IR.Value, return_register: bool) Operand
    {
        const size = value.type.size;
        assert(size > 0 and size <= 8);
        if (return_register)
        {
            var register_ptr = &self.registers[@enumToInt(AllocatedRegister.ID.A)];
            if (register_ptr.value != null)
            {
                panic("Register A is already allocated with size: {}\n", .{register_ptr.size});
            }

            register_ptr.value = value;
            register_ptr.size = @intCast(u8, size);

            return Operand {
                .value = Operand.Value {
                    .register = Encoding.Register.A,
                },
                .size = size,
            };
        }
        else
        {
            for (self.registers) |*register, i|
            {
                if (i == 4 or i == 5 or i == 6 or i == 7)
                {
                    continue;
                }

                if (register.value == null)
                {
                    register.value = value;
                    register.size = @intCast(u8, size);

                    return Operand {
                        .value = Operand.Value {
                            .register = @intToEnum(Encoding.Register, @intCast(u8, i)),
                        },
                        .size = size,
                    };
                }

                log.debug("Register {} is busy\n", .{@intToEnum(Encoding.Register, @intCast(u8, i))});
            }

            panic("All registers are busy\n", .{});
        }
    }

    fn get_allocation(self: *RegisterAllocator, value: *IR.Value) ?Operand
    {
        for (self.registers) |*register, i|
        {
            if (i == 4 or i == 5 or i == 6 or i == 7)
            {
                continue;
            }

            if (register.value) |ir_value|
            {
                if (ir_value == value)
                {
                    return Operand {
                        .value = Operand.Value {
                            .register = @intToEnum(Encoding.Register, @intCast(u8, i)),
                        },
                        .size = ir_value.type.size,
                    };
                }
            }

            log.debug("Register {} is not allocated with the value\n", .{@intToEnum(Encoding.Register, @intCast(u8, i))});
        }

        log.debug("Value is not register allocated\n", .{});
        return null;
    }

    fn free(self: *RegisterAllocator, register: Encoding.Register) void
    {
        const register_index = @enumToInt(register);
        self.registers[register_index].size = 0;
        self.registers[register_index].value = null;
    }

    fn create() RegisterAllocator
    {
        const result = RegisterAllocator
        {
            .registers = std.enums.directEnumArray(AllocatedRegister.ID, AllocatedRegister, 4, .
            {
                .A   = AllocatedRegister { .size = 0, .value = null, },
                .C   = AllocatedRegister { .size = 0, .value = null, },
                .D   = AllocatedRegister { .size = 0, .value = null, },
                .B   = AllocatedRegister { .size = 0, .value = null, },
                .r8  = AllocatedRegister { .size = 0, .value = null, },
                .r9  = AllocatedRegister { .size = 0, .value = null, },
                .r10 = AllocatedRegister { .size = 0, .value = null, },
                .r11 = AllocatedRegister { .size = 0, .value = null, },
                .r12 = AllocatedRegister { .size = 0, .value = null, },
                .r13 = AllocatedRegister { .size = 0, .value = null, },
                .r14 = AllocatedRegister { .size = 0, .value = null, },
                .r15 = AllocatedRegister { .size = 0, .value = null, },
            }),
        };

        return result;
    }

    const AllocatedRegister = struct
    {
        value: ?*IR.Value,
        size: u8,

        const ID = enum
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
};

pub fn ensure_return_value(mc_function: *Function, function: *IR.Function, register_allocator: *RegisterAllocator, operand: Operand) void
{
    const register_a = Operand 
    {
        .value = Operand.Value {
            .register = Encoding.Register.A,
        },
        .size = operand.size,
    };

    const operands = [2]Operand { register_a, operand };
    switch (operand.value)
    {
        Operand.ID.indirect =>
        {
            const register = operand.value.indirect.register;
            assert(register == get_stack_register());

            const mov = create_instruction(Mnemonic.mov, operands[0..]);
            mc_function.append(mov);
        },
        Operand.ID.immediate =>
        {
            const mov = create_instruction(Mnemonic.mov, operands[0..]);
            mc_function.append(mov);
        },
        else => panic("ni: {}\n", .{operand.value}),
    }
}

pub fn get_mc_value_from_ir_value(function: *IR.Function, register_allocator: *RegisterAllocator, ir_value: *IR.Value) Operand
{
    switch (ir_value.id)
    {
        IR.Value.ID.Instruction =>
        {
            const instruction = @ptrCast(*IR.Instruction, ir_value);
            switch (instruction.id)
            {
                IR.Instruction.ID.Load =>
                {
                    if (register_allocator.get_allocation(ir_value)) |load_operand|
                    {
                        log.debug("Value is already in a register\n", .{});
                        return load_operand;
                    }
                    else
                    {
                        // @Info: this is because the instruction allows an stack operand
                        log.debug("Getting stack operand. The value is not in a register\n", .{});
                        const alloca = @ptrCast(*IR.Instruction, instruction.operands.items[0]);
                        const stack_offset = get_stack_offset(function, alloca);
                        log.debug("Stack offset for load: {}\n", .{stack_offset});
                        const load_size = alloca.value.alloca.type.size;
                        assert(load_size <= 8);
                        const load_operand = stack_operand(stack_offset, load_size);
                        return load_operand;
                    }
                },
                IR.Instruction.ID.Add =>
                {
                    if (register_allocator.get_allocation(ir_value)) |operand|
                    {
                        log.debug("Value is already in a register\n", .{});
                        return operand;
                    }
                    else
                    {
                        // @Info: this is because the instruction allows an stack operand
                        panic("wtf\n", .{});
                    }
                },
                else => panic("ni: {}\n", .{instruction.id}),
            }
        },
        IR.Value.ID.ConstantInt =>
        {
            log.debug("Getting constant int. The value is not in a register\n", .{});
            const constant_int = @ptrCast(*IR.ConstantInt, ir_value);
            const constant_operand = constant_int_operand(constant_int);
            return constant_operand;
        },
        else => panic("ni: {}\n", .{ir_value.id}),
    }
}


pub fn foo() void
{
                            // @TODO: meditate deleting loads and doing it in the instructions that needed the loads
                            const operand_count = instruction.operands.items.len;

                            // @Info: here we check if the load is used. If it is, we find out if the next instruction can load the stack value itself
                            var next_instruction: *IR.Instruction = undefined;
                            var next_instruction_index = instruction_index + 1;
                            var load_index: u64 = blk: 
                            {
                                const block_instr_count = basic_block.instructions.items.len;

                                while (next_instruction_index < block_instr_count) : (next_instruction_index += 1)
                                {
                                    next_instruction = basic_block.instructions.items[next_instruction_index];
                                    assert(next_instruction.operands.items.len > 0);
                                    var i: u64 = 0;

                                    while (i < operand_count) : (i += 1)
                                    {
                                        const operand = @ptrCast(*IR.Instruction, next_instruction.operands.items[i]);
                                        if (operand == instruction)
                                        {
                                            break :blk i;
                                        }
                                    }
                                }

                                panic("Load is not used\n", .{});
                            };

                            log.debug("current block: {} current instruction: {}\n", .{block_index, instruction_index});
                            log.debug("Load index: {}. Next instruction: {} [{}]\n", .{load_index, next_instruction.id, next_instruction_index});

                            const mc_mnemonic = direct_ir_mc_translator(next_instruction.id);

                            const encodings = Encoding.instructions[@enumToInt(mc_mnemonic)];
                            const encoding_count = encodings.len;
                            log.debug("Encoding count: {}\n", .{encoding_count});
                            assert(encoding_count != 0);
                            for (encodings) |encoding|
                            {
                                const combination_count = encoding.operand_combination_count;
                                var combination_index: u64 = 0;
                                if (encoding.operand_combination_count == 0 and operand_count == 0)
                                {
                                    panic("reached here\n", .{});
                                }

                                while (combination_index < encoding.operand_combination_count) : (combination_index += 1)
                                {
                                    const combination = encoding.operand_combinations[combination_index];
                                    const next_operand_count = combination.count;

                                    if (load_index >= next_operand_count)
                                    {
                                        log.debug("Index ({}) is greater or equal than operand count: {}\n", .{load_index, next_operand_count});
                                        continue;
                                    }

                                    const operand = combination.operands[load_index];

                                    if (operand.id == Encoding.Operand.ID.register_or_memory or operand.id == Encoding.Operand.ID.memory)
                                    {
                                        log.debug("Load is not necessary. Skipping...\n", .{});
                                        continue :instruction_loop;
                                    }
                                }
                            }

                            log.debug("Load is necessary\n", .{});

                            log.debug("Operand count: {}\n", .{operand_count});

                            const alloca = @ptrCast(*IR.Instruction, instruction.operands.items[0]);
                            const stack_offset = get_stack_offset(function, alloca);
                            log.debug("Stack offset for load: {}\n", .{stack_offset});
                            const load_size = alloca.value.alloca.type.size;
                            assert(load_size <= 8);
                            const load_operand = stack_operand(stack_offset, load_size);

                            const loaded_register = register_allocator.allocate(@ptrCast(*IR.Value, instruction), false);
                            const operands = [2]Operand {loaded_register, load_operand};
                            var load_mov = create_instruction(Mnemonic.mov, operands[0..]);
                            mc_function.append(load_mov);
}

pub fn direct_ir_mc_translator(ir: IR.Instruction.ID) Mnemonic
{
    const mnemonic = switch (ir)
    {
        IR.Instruction.ID.ICmp => Mnemonic.cmp,
        IR.Instruction.ID.Ret => Mnemonic.ret,
        IR.Instruction.ID.Add => Mnemonic.add,
        else => panic("ni: {}\n", .{ir}),
    };
    return mnemonic;
}

pub fn do_binary_operation(mc_function: *Function, function: *IR.Function, register_allocator: *RegisterAllocator, instruction: *IR.Instruction, mnemonic: Mnemonic, first_operand_must_be_register: bool, result_stored_in_register: bool) void
{
    assert(instruction.operands.items.len == 2);

    const first_value = instruction.operands.items[0];
    const second_value = instruction.operands.items[1];
    var first_operand = get_mc_value_from_ir_value(function, register_allocator, first_value);

    if (first_operand_must_be_register and first_operand.value != Operand.ID.register)
    {
        const stack_op = first_operand;
        if (result_stored_in_register)
        {
            // @Info: This assumes the operation uses the register to store the result, this is, the register is an operand and a result. This can cause weird behavior on binary instructions that don't store the result in the register
            // @Info @Update: this should be fixed by this branching
            first_operand = register_allocator.allocate(@ptrCast(*IR.Value, instruction), false);
        }
        else
        {
            first_operand = register_allocator.allocate(@ptrCast(*IR.Value, first_value), false);
        }
        const operands = [2]Operand { first_operand, stack_op };
        const mov_operation = create_instruction(Mnemonic.mov, operands[0..]);
        mc_function.append(mov_operation);
    }
    else if (first_operand.value == Operand.ID.register and result_stored_in_register)
    {
        register_allocator.registers[@enumToInt(first_operand.value.register)].value = @ptrCast(*IR.Value, instruction);
    }

    const second_operand = get_mc_value_from_ir_value(function, register_allocator, second_value);
    const operands = [2]Operand { first_operand, second_operand };
    const new_instruction = create_instruction(mnemonic, operands[0..]);
    mc_function.append(new_instruction);
}

pub fn encode(allocator: *Allocator, module: *IR.Module) void
{
    log.debug("\n==============\nx86-64 CODEGEN\n==============\n\n", .{});
    var executable = Executable
    {
        .functions = FunctionBuffer.init(allocator),
        .code_base_RVA = 0,
        .code_buffer = undefined,
    };

    for (module.functions.list.items) |function_bucket|
    {
        var bucket_function_index: u64 = 0;

        while (bucket_function_index < function_bucket.len) : (bucket_function_index += 1)
        {
            const function = &function_bucket.items[bucket_function_index];
            var mc_function_value = Function
            {
                .instructions = InstructionBuffer.init(allocator),
                .labels = LabelBuffer.initCapacity(allocator, function.basic_blocks.items.len) catch |err| {
                    panic("Error allocating memory for labels\n", .{});
                },
            };

            mc_function_value.labels.resize(mc_function_value.labels.capacity) catch |err| {
                panic("Error resizing labels\n", .{});
            };

            for (mc_function_value.labels.items) |*label|
            {
                label.* = Label 
                {
                    .instruction_index = 0xcccccccccccccccc,
                    .target = 0xcccccccccccccccc,
                    .locations = ArrayList(Label.Location).init(allocator),
                };
            }

            executable.functions.append(mc_function_value) catch |err| {
                panic("Error allocating a new MC function\n", .{});
            };
        }
    }

    var function_index: u64 = 0;
    for (module.functions.list.items) |function_bucket|
    {
        var bucket_function_index: u64 = 0;

        while (bucket_function_index < function_bucket.len) : (bucket_function_index += 1)
        {
            const function = &function_bucket.items[bucket_function_index];
            var mc_function = &executable.functions.items[function_index];

            var register_allocator = RegisterAllocator.create();

            var stack_size = blk:
            {
                var stack_size: u64 = 0;
                var index: u64 = 0;
                while (true)
                {
                    const instruction = function.basic_blocks.items[0].instructions.items[index];
                    if (instruction.id != IR.Instruction.ID.Alloca)
                    {
                        break;
                    }
                    stack_size += instruction.value.alloca.type.size;
                    index += 1;
                }

                break :blk stack_size;
            };

            log.debug("Stack size for this function: {}\n", .{stack_size});

            for (function.basic_blocks.items) |basic_block, block_index|
            {
                log.debug("BasicBlock\n", .{});
                const first_instruction_index = mc_function.instructions.items.len;
                mc_function.labels.items[block_index].instruction_index = first_instruction_index;

                for (basic_block.instructions.items) |instruction, instruction_index|
                {
                    log.debug("Instruction: {}\n", .{instruction.id});

                    switch (instruction.id)
                    {
                        IR.Instruction.ID.Alloca =>
                        {
                            const alloca_type = instruction.value.alloca.type;
                            assert(alloca_type.size <= 8);

                            log.debug("Alloca size: {}\n", .{alloca_type.size});
                        },
                        IR.Instruction.ID.Store =>
                        {
                            const operand_count = instruction.operands.items.len;

                            log.debug("Operand count: {}\n", .{operand_count});

                            const alloca_i = @ptrCast(*IR.Instruction, instruction.operands.items[1]);
                            const stack_offset = get_stack_offset(function, alloca_i);

                            const store_size = alloca_i.value.alloca.type.size;
                            log.debug("Store size: {}. Stack offset: {}\n", .{store_size, stack_offset});

                            const store_operand = stack_operand(stack_offset, store_size);
                            const ir_value_operand = instruction.operands.items[0];
                            const value_operand = get_mc_value_from_ir_value(function, &register_allocator, ir_value_operand);
                            const operands = [2]Operand {store_operand, value_operand};
                            const store_mov = create_instruction(Mnemonic.mov, operands[0..]);
                            mc_function.append(store_mov);

                            if (value_operand.value == Operand.ID.register)
                            {
                                register_allocator.free(value_operand.value.register);
                            }
                        },
                        IR.Instruction.ID.Load =>
                        {
                            log.debug("We do loads on demand\n", .{});
                        },
                        IR.Instruction.ID.Ret =>
                        {
                            if (instruction.operands.items.len > 0)
                            {
                                assert(instruction.operands.items.len == 1);
                                // Here we place the return value
                                const ir_operand = instruction.operands.items[0];
                                const value_operand = get_mc_value_from_ir_value(function, &register_allocator, ir_operand);
                                const operand = blk:
                                {
                                    if (value_operand.value != Operand.ID.register or value_operand.value.register != Encoding.Register.A)
                                    {
                                        ensure_return_value(mc_function, function, &register_allocator, value_operand);
                                    }
                                    else
                                    {
                                        panic("ni\n", .{});
                                    }

                                    break :blk value_operand;
                                };
                            }

                            const mc_i = create_instruction(Mnemonic.ret, null);
                            mc_function.append(mc_i);
                        },
                        IR.Instruction.ID.Br =>
                        {
                            const is_conditional = instruction.operands.items.len == 3;
                            if (is_conditional)
                            {
                                const cmp_instr_value = instruction.operands.items[0];
                                assert(cmp_instr_value.id == IR.Value.ID.Instruction);
                                const cmp_instruction = @ptrCast(*IR.Instruction, cmp_instr_value);
                                assert(cmp_instruction.id == IR.Instruction.ID.ICmp);
                                const compare_type = cmp_instruction.value.compare_type;
                                const jmp_mnemonic = switch (compare_type)
                                {
                                    IR.CompareType.ICMP_SLT => Mnemonic.jge,
                                    IR.CompareType.ICMP_EQ => Mnemonic.jne,
                                    else => panic("ni: {}\n", .{compare_type}),
                                };
                                const ir_else_target = instruction.operands.items[2];
                                const jump_target = Operand.Relative.create(ir_else_target, module, function, function_index, &executable);
                                const operands = [1]Operand { jump_target };
                                const conditional_else_jump = create_instruction(jmp_mnemonic, operands[0..]);
                                mc_function.append(conditional_else_jump);
                            }
                            else
                            {
                                assert(instruction.operands.items.len == 1);
                                const operands = [1]Operand { Operand.Relative.create(instruction.operands.items[0], module, function, function_index, &executable) };

                                const jmp = create_instruction(Mnemonic.jmp, operands[0..]);
                                mc_function.append(jmp);
                            }
                        },
                        IR.Instruction.ID.ICmp =>
                        {
                            do_binary_operation(mc_function, function, &register_allocator, instruction, Mnemonic.cmp, false, false);
                        },
                        IR.Instruction.ID.Add =>
                        {
                            do_binary_operation(mc_function, function, &register_allocator, instruction, Mnemonic.add, true, true);
                        },
                        else => panic("ni: {}\n", .{instruction.id}),
                    }
                }
            }

            function_index += 1;
        }
    }

    var aprox_instruction_count: u64 = 0;
    const max_bytes_per_instruction: u8 = 15;

    for (executable.functions.items) |function|
    {
        aprox_instruction_count += 5 * function.instructions.items.len;
    }

    const aprox_code_size = aprox_instruction_count * max_bytes_per_instruction;
    log.debug("Aproximate code size: {}\n", .{aprox_code_size});

    var buffer_allocator = std.mem.Allocator
    {
        .allocFn = ExecutionBuffer.allocFn,
        .resizeFn = ExecutionBuffer.resizeFn,
    };

    executable.code_buffer = ArrayList(u8).initCapacity(&buffer_allocator, aprox_code_size) catch |err| {
        panic("Error allocating memory for code section buffer\n", .{});
    };

    executable.code_base_RVA = @ptrToInt(executable.code_buffer.items.ptr);

    switch (calling_convention)
    {
        CallingConvention.SystemV =>
        {
            for (executable.functions.items) |function|
            {
                {
                    var label = &function.labels.items[0];
                    label.target = @ptrToInt(executable.code_buffer.items.ptr) + executable.code_buffer.items.len;

                    for (label.locations.items) |location|
                    {
                        log.debug("Location\n", .{});
                        panic("Location not patched.\n", .{});
                    }

                    log.debug("patch label: 0x{x}. Locations: {}\n", .{label.target, label.locations.items.len});
                }

                if (encode_frame_pointer)
                {
                    log.debug("\nEncoding prologue\n", .{});
                    const rbp = Operand 
                    {
                        .value = Operand.Value {
                            .register = Encoding.Register.BP,
                        },
                        .size = @enumToInt(Operand.Size.bits64),
                    };

                    const rsp = Operand 
                    {
                        .value = Operand.Value {
                            .register = Encoding.Register.SP,
                        },
                        .size = @enumToInt(Operand.Size.bits64),
                    };

                    const rbp_arr = [1]Operand { rbp }; 
                    const push_rbp = create_instruction(Mnemonic.push, rbp_arr[0..]);
                    const operands = [2]Operand {rbp, rsp};
                    const mov_rbp_rsp = create_instruction(Mnemonic.mov, operands[0..]);
                    encode_instruction(allocator, &executable, push_rbp);
                    encode_instruction(allocator, &executable, mov_rbp_rsp);
                }

                var instruction_index: u64 = 0;
                var label_index: u64 = 1;

                const instruction_count = function.instructions.items.len;
                const label_count = function.labels.items.len;

                log.debug("\nLabel count: {}. Instruction count: {}\n", .{label_count, instruction_count});

                while (label_index < label_count) : (label_index += 1)
                {
                    var label = &function.labels.items[label_index];
                    const label_instruction_index = label.instruction_index;

                    while (instruction_index < label_instruction_index) : (instruction_index += 1)
                    {
                        const instruction = function.instructions.items[instruction_index];
                        assert(instruction.id != Mnemonic.ret);
                        log.debug("Encoding instruction {}\n", .{instruction_index});
                        encode_instruction(allocator, &executable, instruction);
                    }

                    label.target = @ptrToInt(executable.code_buffer.items.ptr) + executable.code_buffer.items.len;

                    for (label.locations.items) |location|
                    {
                        log.debug("Location\n", .{});
                        // @TODO: this can cause problems
                        const target = @intCast(i64, label.target); 
                        const address_after_instruction = @intCast(i64, location.address_after_instruction);
                        const difference = target - address_after_instruction;
                        assert(difference >= std.math.minInt(i32) and difference <= std.math.maxInt(i32));
                        // ???
                        assert(difference >= 0);
                        const relative_to_be_written = @intCast(i32, difference);
                        log.debug("Address of relative to be written: 0x{x}\n", .{location.relative_to_be_written_address});
                        var patch_target = @intToPtr(*align(1) i32, location.relative_to_be_written_address);
                        patch_target.* = relative_to_be_written;
                        log.debug("\n\n\nWrote relative: {}\n\n\n", .{relative_to_be_written});
                    }

                    log.debug("patch label: 0x{x}. Locations: {}\n", .{label.target, label.locations.items.len});
                }

                while (instruction_index < instruction_count - 1) : (instruction_index += 1)
                {
                    const instruction = function.instructions.items[instruction_index];
                    assert(instruction.id != Mnemonic.ret);
                    encode_instruction(allocator, &executable, instruction);
                }

                if (encode_frame_pointer)
                {
                    log.debug("Encoding epilogue\n", .{});
                    const rbp = Operand 
                    {
                        .value = Operand.Value {
                            .register = Encoding.Register.BP,
                        },
                        .size = @enumToInt(Operand.Size.bits64),
                    };
                    const rbp_arr = [1]Operand { rbp };
                    const pop_rbp = create_instruction(Mnemonic.pop, rbp_arr[0..]);
                    encode_instruction(allocator, &executable, pop_rbp);
                }

                assert(instruction_index == instruction_count - 1);
                const instruction = function.instructions.items[instruction_index];
                assert(instruction.id == Mnemonic.ret);
                encode_instruction(allocator, &executable, instruction);
            }
        },
        else => panic("ni: {}\n", .{calling_convention}),
    }
}
