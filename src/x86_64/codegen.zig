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
const reset_register_allocator_each_basic_block = true;

const log = std.log.scoped(.x86_64_codegen);
const log_enc = std.log.scoped(.x86_64_codegen_enc);

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
                IR.Value.ID.GlobalFunction =>
                {
                    const ir_function = @ptrCast(*IR.Function, value);
                    for (executable.functions.items) |*function|
                    {
                        if (function.ir_ref == ir_function)
                        {
                            label = &function.labels.items[0];
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

        fn get_minimum(n: u64) Operand
        {
            var immediate: Operand = Operand
            {
                .value = Operand.Value {
                    .immediate = undefined,
                },
                .size = undefined,
            };

            if (n <= std.math.maxInt(u8))
            {
                immediate.value.immediate.imm8 = @intCast(u8, n);
                immediate.size = 1;
            }
            else if (n <= std.math.maxInt(u16))
            {
                immediate.value.immediate.imm16 = @intCast(u16, n);
                immediate.size = 2;
            }
            else if (n <= std.math.maxInt(u32))
            {
                immediate.value.immediate.imm32 = @intCast(u32, n);
                immediate.size = 4;
            }
            else if (n <= std.math.maxInt(u64))
            {
                immediate.value.immediate.imm64 = n;
                immediate.size = 8;
            }
            else
            {
                panic("ni\n", .{});
            }

            return immediate;
        }
    };

    fn register(reg: Encoding.Register, size: u32) Operand
    {
        return Operand
        {
            .value = Operand.Value {
                .register = reg,
            },
            .size = size,
        };
    }

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
                        Encoding.Register.A => break :blk "rax",
                        else => panic("ni: {}\n", .{reg}),
                    }
                };

                if (displacement != 0)
                {
                    const displacement_abs = std.math.absCast(displacement);

                    try std.fmt.format(writer, "{s} PTR [{s} {c} {}]", .{word_str, reg_str, sign, displacement_abs});
                }
                else
                {
                    try std.fmt.format(writer, "{s} PTR [{s}]", .{word_str, reg_str});
                }
            },
            Operand.ID.relative =>
            {
                const label = self.value.relative.label;
                try std.fmt.format(writer, "0x{x}", .{label.target});
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

    fn create(instruction_id: Mnemonic, operands: ?[]const Operand) Instruction
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
            const operand_count = instruction.operand_count;
            assert(operand_slice.len <= instruction.operands.len);

            for (operand_slice) |operand|
            {
                instruction.operands[instruction.operand_count] = operand;
                instruction.operand_count += 1;
            }
        }

        return instruction;
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

const Function = struct
{
    instructions: InstructionBuffer,
    register_allocator: RegisterAllocator,
    stack_allocator: StackAllocator,
    labels: LabelBuffer,
    ir_ref: *IR.Function,
    rsp: u64,

    fn append(self: *Function, instruction: Instruction) void
    {
        log.debug("Appending instruction: {}\n", .{instruction});
        self.instructions.append(instruction) catch |err| {
            panic("Error allocating memory for instruction\n", .{});
        };
    }

    fn spill_register(self: *Function, register: Encoding.Register) Operand
    {
        const allocated_register = self.register_allocator.free(register);
        const stack_operand = self.stack_allocator.allocate(allocated_register.size);

        return stack_operand;
    }

    fn spill_registers_before_call(self: *Function, registers: []RegisterAllocator.AllocatedRegister, stack_operands: []Operand) u32
    {
        log.debug("Saving registers before call...\n", .{});
        var saved_register_count: u32 = 0;

        for (self.register_allocator.registers) |reg, i|
        {
            if (reg.value != null and RegisterAllocator.must_save(i))
            {
                registers[i] = reg;
                saved_register_count += 1;
            }
        }

        log.debug("Registers to be saved: {}\n", .{saved_register_count});

        if (saved_register_count > 0)
        {
            std.mem.set(Operand, stack_operands[0..], Operand
                {
                    .value = Operand.Value.none,
                    .size = 0,
                });

            for (registers) |reg, i|
            {
                if (reg.value != null)
                {
                    const size = reg.size;
                    const register = @intToEnum(Encoding.Register, @intCast(u8, i));
                    const register_operand = Operand
                    {
                        .value = Operand.Value {
                            .register = register,
                        },
                        .size = size,
                    };

                    stack_operands[i] = self.spill_register(register);
                    const operands = [2]Operand { stack_operands[i], register_operand };
                    const mov = Instruction.create(Mnemonic.mov, operands[0..]);
                    self.append(mov);
                }
            }
        }

        return saved_register_count;
    }

    fn restore_registers_after_call(self: *Function, saved_registers: []RegisterAllocator.AllocatedRegister, stack_operands: []Operand) void
    {
        log.debug("Restoring registers...\n", .{});

        for (saved_registers) |reg, i|
        {
            if (reg.value != null)
            // @Info: this register has been saved
            {
                log.debug("Restoring register: {}\n", .{@intToEnum(Encoding.Register, @intCast(u8, i))});
                const register = @intToEnum(Encoding.Register, @intCast(u8, i));
                const reg_operand = Operand
                {
                    .value = Operand.Value{
                        .register = register,
                    },
                    .size = reg.size,
                };

                const operands = [2]Operand { reg_operand, stack_operands[i] };
                const mov = Instruction.create(Mnemonic.mov, operands[0..]);
                self.append(mov);

                self.register_allocator.registers[i].value = reg.value.?;
                self.register_allocator.registers[i].size = reg.size;
            }
        }
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
    log_enc.debug("\nEncoding instruction: {}...\n", .{instruction.id});
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
                log_enc.debug("Operand count is different. Skipping...\n", .{});
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
                log_enc.debug("Matched instruction\n", .{});
                const instruction_offset = executable.code_buffer.items.len; 
                //const instruction_offset_address = @ptrToInt(executable.code_buffer.items.ptr) + instruction_offset;
                //log.debug("Instruction offset address: 0x{x}\n", .{instruction_offset_address});

                // @TODO: label
                //

                var rex_byte: u8 = @enumToInt(combination.rex);
                log_enc.debug("Initial REX byte: 0x{x}\n", .{rex_byte});
                var memory_encoding = false;
                var r_m_encoding = false;

                operand_index = 0;
                while (operand_index < operand_count) : (operand_index += 1)
                {
                    const operand = instruction.operands[operand_index];
                    const operand_encoding = combination.operands[operand_index];
                    log_enc.debug("Operand encoding: {}\n", .{operand_encoding.id});

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
                    log_enc.debug("It needs MOD RM. Doing operations...\n", .{});

                    operand_index = 0;

                    while (operand_index < operand_count) : (operand_index += 1)
                    {
                        const operand = instruction.operands[operand_index];
                        log_enc.debug("Operand {}. ID: {s}\n", .{operand_index, @tagName(operand.value)});

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
                    log_enc.debug("Writing rex byte: 0x{x}\n", .{rex_byte});

                    executable.code_buffer.append(rex_byte) catch unreachable;
                }
                else if ((instruction.operands[0].value == Operand.ID.register and instruction.operands[0].size == 2) or (instruction.operands[1].value == Operand.ID.register and instruction.operands[1].size == 2) or  encoding.options.explicit_byte_size == 2)
                {
                    executable.code_buffer.append(Encoding.Operand.size_override) catch unreachable;
                }

                // @TODO:
                const op_code_byte = op_code[0];
                log_enc.debug("Writing op code byte {}: 0x{x}\n", .{0, op_code_byte});
                executable.code_buffer.append(op_code_byte) catch unreachable;

                var op_code_index: u64 = 1;
                while (true) : (op_code_index += 1)
                {
                    if (op_code[op_code_index] == 0)
                    {
                        break;
                    }

                    executable.code_buffer.append(op_code[op_code_index]) catch unreachable;
                }

                if (need_mod_rm)
                {
                    log_enc.debug("Writing Mod RM: 0x{x}\n", .{mod_r_m});

                    executable.code_buffer.append(mod_r_m) catch unreachable;
                }

                if (need_sib)
                {
                    log_enc.debug("Writing SIB byte: 0x{x}\n", .{sib_byte});

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
                                        log_enc.debug("Writing displacement: 4 bytes\n", .{});
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
                log_enc.debug("Operand count: {}\n", .{operand_count});

                // Immediate, relatives
                while (operand_index < operand_count) : (operand_index += 1)
                {
                    const operand = instruction.operands[operand_index];

                    switch (operand.value)
                    {
                        Operand.ID.immediate =>
                        {
                            const operand_size = operand.size;
                            log_enc.debug("Writing immediate operand of size: {}\n", .{operand_size});
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
                                log_enc.debug("Label is resolved: 0x{x}\n", .{label.target});
                                const patch_target = @intToPtr(*align(1) i32, @ptrToInt(executable.code_buffer.items.ptr) + executable.code_buffer.items.len);
                                const instruction_after_address = @ptrToInt(patch_target) + jump_size;
                                const target = @intCast(i64, label.target); 
                                const address_after_instruction = @intCast(i64, instruction_after_address);
                                const sub_result = target - address_after_instruction;
                                assert(sub_result >= std.math.minInt(i32) and sub_result <= std.math.maxInt(i32));
                                const difference = @intCast(i32, sub_result);
                                log_enc.debug("Difference: {}\n", .{difference});
                                log_enc.debug("To be patch content: {}\n", .{patch_target.*});
                                executable.code_buffer.appendSlice(std.mem.asBytes(&difference)) catch |err| {
                                    panic("Error appending relative operand bytes\n", .{});
                                };
                            }
                            else
                            {
                                log_enc.debug("Label is not resolved, adding a patch location to the label\n", .{});
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

                log.debug("0x{x}: ================================================\n", .{@ptrToInt(executable.code_buffer.items.ptr) + instruction_offset});
                for (instruction_slice) |byte|
                {
                    log.debug("0x{x}  ", .{byte});
                }
                log.debug("\n{}\n", .{instruction});

                return;
            }
        }
    }

    log_enc.err("{s} ", .{@tagName(instruction.id)});
    var operand_index: u64 = 0;
    while (operand_index < instruction.operand_count) : (operand_index += 1)
    {
        const operand = instruction.operands[operand_index];
        log_enc.err("{s}.{}, ", .{@tagName(operand.value), operand.size});
    }
    panic("\nUnable to find a fitting instruction: {}", .{instruction.id});
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

const RegisterAllocator = struct
{
    registers: [16]AllocatedRegister,

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

    fn reset(self: *RegisterAllocator) void
    {
        std.mem.set(AllocatedRegister, self.registers[0..], AllocatedRegister { .value = null, .size = 0 });
    }

    fn free(self: *RegisterAllocator, register: Encoding.Register) AllocatedRegister
    {
        const register_index = @enumToInt(register);
        const freed_register = self.registers[register_index];
        self.registers[register_index].size = 0;
        self.registers[register_index].value = null;

        return freed_register;
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
                log.debug("There's a value in register {s}: {}\n", .{@tagName(@intToEnum(Encoding.Register, @intCast(u8, i))), ir_value.id});
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
        }

        log.debug("Value is not register allocated\n", .{});
        return null;
    }

    fn get_indirect(self: *RegisterAllocator, load: *IR.Instruction) Operand
    {
        log.debug("Getting indirect operand from the register allocator, being the pointer stored in a register\n", .{});

        if (self.get_allocation(@ptrCast(*IR.Value, load))) |ptr_loaded|
        {
            const indirect_size = get_indirect_size(load);
            log.debug("Indirect size: {}\n", .{indirect_size});

            assert(ptr_loaded.value == Operand.ID.register);
            const indirect = Operand
            {
                .value = Operand.Value
                {
                    .indirect = Operand.Indirect 
                    {
                        .displacement = 0,
                        .register = ptr_loaded.value.register,
                    },
                    },
                .size = @intCast(u32, indirect_size),
            };

            return indirect;
        }
        else
        {
            panic("Pointer is not loaded\n", .{});
        }
    }

    fn allocate(self: *RegisterAllocator, value: *IR.Value, return_register: bool) Operand
    {
        const size = value.type.size;
        assert(size > 0 and size <= 8);
        if (return_register)
        {
            var register_ptr = &self.registers[@enumToInt(AllocatedRegister.ID.A)];
            if (register_ptr.value != null and !return_register)
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

    fn copy_register(self: *RegisterAllocator, function: *Function, dst: Encoding.Register, src: Encoding.Register) Operand
    {
        const register_index = @enumToInt(src);
        log.debug("Copy register. Dst register: {}. Source register: {}\n", .{dst, src});
        assert(self.registers[register_index].value != null);
        const value = self.registers[register_index].value.?;
        const size = self.registers[register_index].size;
        const register_to_copy = Operand
        {
            .value = Operand.Value
            {
                .register = src,
            },
            .size = size,
        };

        const new_register_operand = self.allocate(value, false);
        _ = self.free(src);

        const operands = [2]Operand { new_register_operand, register_to_copy };
        const mov = Instruction.create(Mnemonic.mov, operands[0..]);
        function.append(mov);

        return new_register_operand;
    }

    fn must_save(r: u64) bool
    {
        const register = @intToEnum(Encoding.Register, @intCast(u8, r));
        switch (register)
        {
            Encoding.Register.A => return true,
            else => panic("ni: {}\n", .{register}),
        }
    }
};

const StackAllocator = struct
{
    allocations: ArrayList(Allocation),
    stack_offset: u64,

    const Allocation = struct
    {
        size: u64,
        alignment: u64,
        offset: u64,
    };

    fn align_n(n: u64, alignment: u64) u64
    {
        const mask = alignment - 1;
        assert(alignment & mask == 0);
        return (n + mask) & ~mask;
    }

    fn allocate(self: *StackAllocator, size: u64) Operand
    {
        self.stack_offset = StackAllocator.align_n(self.stack_offset + size, size);

        self.allocations.append(Allocation
            {
                .size = size,
                .alignment = size,
                .offset = self.stack_offset,
            }) catch |err| {
            panic("Error allocating in the stack\n", .{});
        };

        return Operand
        {
            .value = Operand.Value
            {
                .indirect = Operand.Indirect {
                    .displacement = @intCast(i32, self.stack_offset),
                    .register = get_stack_register(),
                },
            },
            .size = @intCast(u32, size),
        };
    }

    fn get_operand_from_alloca(self: *StackAllocator, ir_function: *IR.Function, alloca: *IR.Instruction) Operand
    {
        assert(alloca.id == IR.Instruction.ID.Alloca);
        var alloca_count: u64 = 0;

        const alloca_index = blk:
        {
            for (ir_function.basic_blocks.items[0].instructions.items) |instruction, i|
            {
                alloca_count += 1;

                if (instruction.id != IR.Instruction.ID.Alloca)
                {
                    panic("Internal error: alloca not found. Index: {}\n", .{i});
                }
                if (instruction == alloca)
                {
                    log.debug("Alloca found\n", .{});
                    break :blk i;
                }
            }

            panic("Alloca not found in the function\n", .{});
        };

        log.debug("Alloca count: {}\n", .{alloca_count});
        log.debug("Alloca index: {}\n", .{alloca_index});

        const allocation = self.allocations.items[alloca_index];

        return Operand
        {
            .value = Operand.Value {
                .indirect = Operand.Indirect
                {
                    .displacement = -@intCast(i32, allocation.offset),
                    .register = get_stack_register(),
                },
            },
            .size = @intCast(u32, allocation.size),
        };
    }
};

pub fn get_mc_value_from_ir_value(function: *IR.Function, mc_function: *Function, ir_value: *IR.Value) Operand
{
    log.debug("Getting value of {s}\n", .{@tagName(ir_value.id)});
    switch (ir_value.id)
    {
        IR.Value.ID.Instruction =>
        {
            const instruction = @ptrCast(*IR.Instruction, ir_value);
            log.debug("Getting value of {s}\n", .{@tagName(instruction.id)});
            switch (instruction.id)
            {
                IR.Instruction.ID.Load =>
                {
                    if (mc_function.register_allocator.get_allocation(ir_value)) |operand|
                    {
                        log.debug("Value is already in a register\n", .{});
                        return operand;
                    }
                    else
                    {
                        log.debug("@Info: here we are just returning the stack operand because the value doesn't need to be in a register\n", .{});

                        const first_operand = instruction.operands.items[0];
                        assert(first_operand.id == IR.Value.ID.Instruction);
                        const first_op_instruction = @ptrCast(*IR.Instruction, first_operand);
                        const operand = switch (first_op_instruction.id)
                        {
                            IR.Instruction.ID.Alloca => mc_function.stack_allocator.get_operand_from_alloca(function, instruction),
                            IR.Instruction.ID.Load => mc_function.register_allocator.get_indirect(first_op_instruction),
                            else => panic("ni:{}\n", .{first_op_instruction.id}),
                        };
                        return operand;
                    }
                },
                IR.Instruction.ID.Add, IR.Instruction.ID.Sub, IR.Instruction.ID.Mul, IR.Instruction.ID.Call =>
                {
                    if (mc_function.register_allocator.get_allocation(ir_value)) |operand|
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
                IR.Instruction.ID.Alloca =>
                {
                    const alloca_size = instruction.base.type.size;
                    log.debug("Alloca size: {}\n", .{alloca_size});

                    if (mc_function.register_allocator.get_allocation(ir_value)) |operand|
                    {
                        log.debug("Value is already in a register\n", .{});
                        return operand;
                    }
                    else
                    {
                        // @Info: always emit lea
                        const stack_operand = mc_function.stack_allocator.get_operand_from_alloca(function, instruction);
                        const r = mc_function.register_allocator.allocate(ir_value, false);
                        const operands = [2]Operand { r, stack_operand };
                        const lea = Instruction.create(Mnemonic.lea, operands[0..]);
                        mc_function.append(lea);
                        return r;
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
        IR.Value.ID.Argument =>
        {
            const argument = @ptrCast(*IR.Function.Argument, ir_value);
            panic("Argument: {}\n", .{argument});
        },
        else => panic("ni: {}\n", .{ir_value.id}),
    }
}

pub fn direct_ir_mc_translator(ir: IR.Instruction.ID) Mnemonic
{
    const mnemonic = switch (ir)
    {
        IR.Instruction.ID.ICmp => Mnemonic.cmp,
        IR.Instruction.ID.Ret => Mnemonic.ret,
        IR.Instruction.ID.Add => Mnemonic.add,
        IR.Instruction.ID.Mul => Mnemonic.imul,
        else => panic("ni: {}\n", .{ir}),
    };
    return mnemonic;
}

pub fn do_binary_operation(mc_function: *Function, function: *IR.Function, instruction: *IR.Instruction, mnemonic: Mnemonic, first_operand_must_be_register: bool, result_stored_in_register: bool) void
{
    assert(instruction.operands.items.len == 2);

    const first_value = instruction.operands.items[0];
    const second_value = instruction.operands.items[1];
    log.debug("Fetching first operand\n", .{});
    var first_operand = get_mc_value_from_ir_value(function, mc_function, first_value);

    if (first_operand_must_be_register and first_operand.value != Operand.ID.register)
    {
        log.debug("First operand must be register but it's not register\n", .{});
        const stack_op = first_operand;
        if (result_stored_in_register)
        {
            // @Info: This assumes the operation uses the register to store the result, this is, the register is an operand and a result. This can cause weird behavior on binary instructions that don't store the result in the register
            // @Info @Update: this should be fixed by this branching
            first_operand = mc_function.register_allocator.allocate(@ptrCast(*IR.Value, instruction), false);
        }
        else
        {
            first_operand = mc_function.register_allocator.allocate(@ptrCast(*IR.Value, first_value), false);
        }
        const operands = [2]Operand { first_operand, stack_op };
        const mov_operation = Instruction.create(Mnemonic.mov, operands[0..]);
        mc_function.append(mov_operation);
    }
    else if (first_operand.value == Operand.ID.register and result_stored_in_register)
    {
        log.debug("First operand is in a register and result must be stored in a register\n", .{});
        mc_function.register_allocator.registers[@enumToInt(first_operand.value.register)].value = @ptrCast(*IR.Value, instruction);
    }

    const second_operand = get_mc_value_from_ir_value(function, mc_function, second_value);
    log.debug("After second operand fetching\n", .{});
    if (first_operand.value == Operand.ID.indirect and second_operand.value == Operand.ID.indirect)
    {
        log.debug("Both values are in the stack, we should store first operand into a register\n", .{});
        const stack_op = first_operand;
        first_operand = mc_function.register_allocator.allocate(@ptrCast(*IR.Value, first_value), false);
        const operands = [2]Operand { first_operand, stack_op };
        const mov_operation = Instruction.create(Mnemonic.mov, operands[0..]);
        mc_function.append(mov_operation);
    }

    const operands = [2]Operand { first_operand, second_operand };
    const new_instruction = Instruction.create(mnemonic, operands[0..]);
    mc_function.append(new_instruction);
}

//fn make_stack_operand(function: *IR.Function, alloca: *IR.Instruction) Operand
//{
    //const stack_offset = get_stack_offset(function, alloca);

    //const store_size = alloca.value.alloca.type.size;
    //log.debug("Store size: {}. Stack offset: {}\n", .{store_size, stack_offset});

    //const result = stack_operand(stack_offset, store_size);
    //return result;
//}

pub fn get_alloca_size(alloca: *IR.Instruction) u64
{
    assert(alloca.id == IR.Instruction.ID.Alloca);
    return alloca.value.alloca.type.size;
}

pub fn get_indirect_size(load: *IR.Instruction) u64
{
    assert(load.id == IR.Instruction.ID.Load);
    const load_type = load.base.type;
    assert(load_type.id == IR.Type.ID.pointer);
    const pointer_type = @ptrCast(*IR.PointerType, load_type);
    const deref_size = pointer_type.type.size;
    return deref_size;
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
                .ir_ref = function,
                .rsp = 0,
                .register_allocator = RegisterAllocator.create(),
                .stack_allocator = StackAllocator {
                    .allocations = ArrayList(StackAllocator.Allocation).init(allocator),
                    .stack_offset = 0,
                },
            };

            for (function.basic_blocks.items[0].instructions.items) |instruction|
            {
                if (instruction.id != IR.Instruction.ID.Alloca)
                {
                    break;
                }

                const alloca_size = get_alloca_size(instruction);
                _ = mc_function_value.stack_allocator.allocate(alloca_size);
            }

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

            for (function.basic_blocks.items) |basic_block, block_index|
            {
                log.debug("BasicBlock\n", .{});

                const first_instruction_index = mc_function.instructions.items.len;
                mc_function.labels.items[block_index].instruction_index = first_instruction_index;
                if (reset_register_allocator_each_basic_block)
                {
                    mc_function.register_allocator.reset();
                }

                for (basic_block.instructions.items) |instruction, instruction_index|
                {
                    log.debug("Instruction: {}\n", .{instruction.id});

                    switch (instruction.id)
                    {
                        IR.Instruction.ID.Alloca =>
                        {
                            const alloca_size = get_alloca_size(instruction);
                            assert(alloca_size <= 8);

                            log.debug("Alloca size: {}\n", .{alloca_size});
                        },
                        IR.Instruction.ID.Store =>
                        {
                            const operand_count = instruction.operands.items.len;

                            log.debug("Operand count: {}\n", .{operand_count});
                            const ir_value_operand = instruction.operands.items[0];


                            const value_operand = get_mc_value_from_ir_value(function, mc_function, ir_value_operand);
                            if (value_operand.value == Operand.ID.register)
                            {
                                _ = mc_function.register_allocator.free(value_operand.value.register);
                            }

                            const ir_ptr_operand = instruction.operands.items[1];
                            assert(ir_ptr_operand.id == IR.Value.ID.Instruction);
                            const ptr_instruction = @ptrCast(*IR.Instruction, ir_ptr_operand);

                            const store_operand = switch (ptr_instruction.id)
                            {
                                IR.Instruction.ID.Alloca => blk:
                                {
                                    const alloca_i = ptr_instruction;
                                    const alloca_size = get_alloca_size(alloca_i);
                                    const value_size = ir_value_operand.type.size;
                                    if (alloca_size != value_size)
                                    {
                                        panic("Alloca size: {}. Value to be stored size: {}\n", .{alloca_size, value_size});
                                    }

                                    break :blk mc_function.stack_allocator.get_operand_from_alloca(function, alloca_i);
                                },
                                IR.Instruction.ID.Load => mc_function.register_allocator.get_indirect(ptr_instruction),
                                else => panic("ni: {}\n", .{ptr_instruction.id}),
                            };

                            const operands = [2]Operand { store_operand, value_operand };
                            const store_mov = Instruction.create(Mnemonic.mov, operands[0..]);
                            mc_function.append(store_mov);
                        },
                        IR.Instruction.ID.Load =>
                        {
                            if (mc_function.register_allocator.get_allocation(@ptrCast(*IR.Value, instruction))) |load_instruction|
                            {
                                panic("Value is already in a register\n", .{});
                            }

                            const use_count = instruction.base.uses.items.len;
                            log.debug("Load use count: {}\n", .{use_count});
                            assert(use_count == 1);

                            const use = instruction.base.uses.items[0];
                            var return_register = false;
                            switch (use.id)
                            {
                                IR.Value.ID.Instruction =>
                                {
                                    const use_i = @ptrCast(*IR.Instruction, use);
                                    
                                    const operand_index = blk:
                                    {
                                        for (use_i.operands.items) |operand, i|
                                        {
                                            if (operand == @ptrCast(*IR.Value, instruction))
                                            {
                                                break :blk i;
                                            }

                                        }
                                        panic("not found\n", .{});
                                    };

                                    log.debug("Load use: {}. Index: {}\n", .{use_i.id, operand_index});

                                    switch (use_i.id)
                                    {
                                        IR.Instruction.ID.Ret =>
                                        {
                                            return_register = true;
                                        },
                                        IR.Instruction.ID.ICmp =>
                                        {
                                            log.debug("@Info: here we don't need to load the stack operand for {s}\n", .{@tagName(direct_ir_mc_translator(use_i.id))});
                                            continue;
                                        },
                                        IR.Instruction.ID.Add, IR.Instruction.ID.Sub, IR.Instruction.ID.Mul =>
                                        {
                                            // @Info: We only need to allocate the first operand of the addition into a register
                                            if (operand_index != 0)
                                            {
                                                log.debug("Use in {s}. Not register allocated. Operand index: {}\n", .{@tagName(direct_ir_mc_translator(use_i.id)), operand_index});
                                                continue;
                                            }
                                        },
                                        IR.Instruction.ID.Load =>
                                        {
                                            log.debug("Loading a load. Do nothing special\n", .{});
                                        },
                                        IR.Instruction.ID.Store =>
                                        {
                                            log.debug("Loading something that is going to be stored. The store is probably the return value\n", .{});
                                        },
                                        else => panic("ni: {}\n", .{use_i.id}),
                                    }
                                },
                                else => panic("ni: {}\n", .{use.id}),
                            }

                            // @Info: this is because the instruction allows an stack operand
                            const load_ir_operand = @ptrCast(*IR.Instruction, instruction.operands.items[0]);
                            const load_operand = switch (load_ir_operand.id)
                            {
                                IR.Instruction.ID.Alloca => mc_function.stack_allocator.get_operand_from_alloca(function, load_ir_operand),
                                IR.Instruction.ID.Load => mc_function.register_allocator.get_indirect(load_ir_operand),
                                else => panic("ni: {}\n", .{load_ir_operand.id}),
                            };

                            const register_operand = mc_function.register_allocator.allocate(@ptrCast(*IR.Value, instruction), true);
                            const operands = [2]Operand { register_operand, load_operand };
                            const mov_i = Instruction.create(Mnemonic.mov, operands[0..]);
                            mc_function.append(mov_i);
                        },
                        IR.Instruction.ID.Ret =>
                        {
                            if (instruction.operands.items.len > 0)
                            {
                                assert(instruction.operands.items.len == 1);
                                // Here we place the return value
                                const ir_operand = instruction.operands.items[0];
                                //const value_operand = get_mc_value_from_ir_value(function, &register_allocator, ir_operand);
                                switch (ir_operand.id)
                                {
                                    IR.Value.ID.ConstantInt =>
                                    {
                                        const constant_int = @ptrCast(*IR.ConstantInt, ir_operand);
                                        const operand = constant_int_operand(constant_int);
                                        const register_a = Operand 
                                        {
                                            .value = Operand.Value {
                                                .register = Encoding.Register.A,
                                            },
                                            .size = operand.size,
                                        };

                                        const operands = [2]Operand { register_a, operand };
                                        const mov = Instruction.create(Mnemonic.mov, operands[0..]);
                                        mc_function.append(mov);
                                    },
                                    IR.Value.ID.Instruction =>
                                    {
                                        const ret_instruction = @ptrCast(*IR.Instruction, ir_operand);
                                        switch (ret_instruction.id)
                                        {
                                            IR.Instruction.ID.Load, IR.Instruction.ID.Add => {},
                                            else => panic("ni: {}\n", .{ret_instruction.id}),
                                        }
                                    },
                                    else => panic("ni: {}\n", .{ir_operand.id}),
                                }
                            }

                            const ret_i = Instruction.create(Mnemonic.ret, null);
                            mc_function.append(ret_i);
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
                                    IR.CompareType.ICMP_SGT => Mnemonic.jle,
                                    else => panic("ni: {}\n", .{compare_type}),
                                };
                                const ir_if_target = instruction.operands.items[1];
                                assert(ir_if_target.id == IR.Value.ID.BasicBlock);
                                const ir_if_block = @ptrCast(*IR.BasicBlock, ir_if_target);
                                if (ir_if_block != function.basic_blocks.items[block_index + 1])
                                {
                                    panic("not implemented. If block is not continous to the compare instruction\n", .{});
                                }
                                const ir_else_target = instruction.operands.items[2];
                                const jump_target = Operand.Relative.create(ir_else_target, module, function, function_index, &executable);
                                const operands = [1]Operand { jump_target };
                                const conditional_else_jump = Instruction.create(jmp_mnemonic, operands[0..]);
                                mc_function.append(conditional_else_jump);
                            }
                            else
                            {
                                assert(instruction.operands.items.len == 1);
                                const operand = instruction.operands.items[0];
                                assert(operand.id == IR.Value.ID.BasicBlock);
                                const target_basic_block = @ptrCast(*IR.BasicBlock, operand);
                                // @Info: if the jump is not to the next instruction, produce a jmp instruction
                                if (target_basic_block != function.basic_blocks.items[block_index + 1])
                                {
                                    const operands = [1]Operand { Operand.Relative.create(operand, module, function, function_index, &executable) };

                                    const jmp = Instruction.create(Mnemonic.jmp, operands[0..]);
                                    mc_function.append(jmp);
                                }
                            }
                        },
                        IR.Instruction.ID.ICmp =>
                        {
                            do_binary_operation(mc_function, function, instruction, Mnemonic.cmp, false, false);
                        },
                        IR.Instruction.ID.Add =>
                        {
                            log.debug("Doing add\n", .{});
                            do_binary_operation(mc_function, function, instruction, Mnemonic.add, true, true);
                        },
                        IR.Instruction.ID.Sub =>
                        {
                            do_binary_operation(mc_function, function, instruction, Mnemonic.sub, true, true);
                        },
                        IR.Instruction.ID.Mul =>
                        {
                            assert(instruction.operands.items.len == 2);

                            const first_value = instruction.operands.items[0];
                            const second_value = instruction.operands.items[1];
                            if (second_value.id != IR.Value.ID.ConstantInt)
                            {
                                do_binary_operation(mc_function, function, instruction, Mnemonic.sub, true, true);
                            }
                            else
                            {
                                panic("ni\n", .{});
                            }
                        },
                        IR.Instruction.ID.Call =>
                        {
                            // @TODO: Encoding of relative operand is maybe buggy
                            // do arg stuff
                            const callee = instruction.operands.items[0];

                            var allocated_registers: [16]RegisterAllocator.AllocatedRegister = std.mem.zeroes([16]RegisterAllocator.AllocatedRegister);
                            var saved_register_stack_operands: [16]Operand = undefined;
                            const saved_register_count = mc_function.spill_registers_before_call(allocated_registers[0..], saved_register_stack_operands[0..]);

                            const arg_count = instruction.operands.items.len - 1;
                            if (arg_count != 0)
                            {
                                panic("Arguments are not implemented yet\n", .{});
                            }

                            if (mc_function.rsp == 0)
                            {
                                mc_function.rsp += 16;
                            }

                            const operands = [1]Operand { Operand.Relative.create(callee, module, function, function_index, &executable) };
                            const call_i = Instruction.create(Mnemonic.call, operands[0..]);
                            mc_function.append(call_i);

                            const use_count = instruction.base.uses.items.len;
                            assert(use_count == 1);
                            const call_use = instruction.base.uses.items[0];

                            switch (call_use.id)
                            {
                                IR.Value.ID.Instruction =>
                                {
                                    const use_i = @ptrCast(*IR.Instruction, call_use);
                                    const operand_index = blk:
                                    {
                                        for (use_i.operands.items) |operand, i|
                                        {
                                            if (operand == @ptrCast(*IR.Value, instruction))
                                            {
                                                break :blk i;
                                            }
                                        }

                                        panic("instruction not used\n", .{});
                                    };

                                    switch (use_i.id)
                                    {
                                        IR.Instruction.ID.Add =>
                                        {
                                            if (operand_index != 0)
                                            {
                                                //panic("there could be some problem over here\n", .{});
                                            }
                                        },
                                        else => panic("ni: {}\n", .{use_i.id}),
                                    }
                                },
                                else => panic("ni: {}\n", .{call_use.id}),
                            }

                            var return_register_operand: Operand = undefined;
                            const return_type = instruction.base.type;
                            const ret_size = return_type.size;
                            assert(ret_size <= 8);
                            if (return_type.id != IR.Type.ID.void)
                            {
                                return_register_operand = mc_function.register_allocator.allocate(@ptrCast(*IR.Value, instruction), true);
                            }

                            if (saved_register_count > 0)
                            {
                                // @TODO: save return value
                                const new_register = blk:
                                {
                                    for (allocated_registers) |reg, i|
                                    {
                                        if (reg.value == null)
                                        {
                                            break :blk @intToEnum(Encoding.Register, @intCast(u8, i));
                                        }
                                    }

                                    panic("registers are full\n", .{});
                                };

                                const new_register_operand = mc_function.register_allocator.copy_register(mc_function, new_register, Encoding.Register.A);
                                log.debug("New register operand: {}\n", .{new_register_operand.value.register});
                                mc_function.restore_registers_after_call(allocated_registers[0..], saved_register_stack_operands[0..]);
                            }
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
        aprox_instruction_count += 6 + function.instructions.items.len;
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
                    log.info("Printing function:\n\n", .{});
                    for (function.instructions.items) |instruction, i|
                    {
                        log.info("{}: {}\n", .{i, instruction});
                    }
                    log.info("\n\n", .{});
                }

                {
                    var label = &function.labels.items[0];
                    label.target = @ptrToInt(executable.code_buffer.items.ptr) + executable.code_buffer.items.len;

                    for (label.locations.items) |location|
                    {
                        log_enc.debug("Location\n", .{});
                        panic("Location not patched.\n", .{});
                    }

                    log_enc.debug("patch label: 0x{x}. Locations: {}\n", .{label.target, label.locations.items.len});
                }

                if (encode_frame_pointer)
                {
                    log_enc.debug("\nEncoding prologue\n", .{});
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
                    const push_rbp = Instruction.create(Mnemonic.push, rbp_arr[0..]);
                    const operands = [2]Operand { rbp, rsp };
                    const mov_rbp_rsp = Instruction.create(Mnemonic.mov, operands[0..]);
                    encode_instruction(allocator, &executable, push_rbp);
                    encode_instruction(allocator, &executable, mov_rbp_rsp);
                }

                if (function.rsp > 0)
                {
                    const operands = [2]Operand
                    {
                        Operand 
                        {
                            .value = Operand.Value {
                                .register = Encoding.Register.SP,
                            },
                            .size = @enumToInt(Operand.Size.bits64),
                        },
                        Operand.Immediate.get_minimum(function.rsp),
                    };

                    const sub_rsp = Instruction.create(Mnemonic.sub, operands[0..]);
                    encode_instruction(allocator, &executable, sub_rsp);
                }

                var instruction_index: u64 = 0;
                var label_index: u64 = 1;

                const instruction_count = function.instructions.items.len;
                const label_count = function.labels.items.len;

                log_enc.debug("\nLabel count: {}. Instruction count: {}\n", .{label_count, instruction_count});

                while (label_index < label_count) : (label_index += 1)
                {
                    var label = &function.labels.items[label_index];
                    const label_instruction_index = label.instruction_index;

                    while (instruction_index < label_instruction_index) : (instruction_index += 1)
                    {
                        const instruction = function.instructions.items[instruction_index];
                        assert(instruction.id != Mnemonic.ret);
                        log_enc.debug("Encoding instruction {}\n", .{instruction_index});
                        encode_instruction(allocator, &executable, instruction);
                    }

                    label.target = @ptrToInt(executable.code_buffer.items.ptr) + executable.code_buffer.items.len;

                    for (label.locations.items) |location|
                    {
                        log_enc.debug("Location\n", .{});
                        // @TODO: this can cause problems
                        const target = @intCast(i64, label.target); 
                        const address_after_instruction = @intCast(i64, location.address_after_instruction);
                        const difference = target - address_after_instruction;
                        assert(difference >= std.math.minInt(i32) and difference <= std.math.maxInt(i32));
                        // ???
                        assert(difference >= 0);
                        const relative_to_be_written = @intCast(i32, difference);
                        log_enc.debug("Address of relative to be written: 0x{x}\n", .{location.relative_to_be_written_address});
                        var patch_target = @intToPtr(*align(1) i32, location.relative_to_be_written_address);
                        patch_target.* = relative_to_be_written;
                        log_enc.debug("\n\n\nWrote relative: {}\n\n\n", .{relative_to_be_written});
                    }

                    log_enc.debug("patch label: 0x{x}. Locations: {}\n", .{label.target, label.locations.items.len});
                }

                while (instruction_index < instruction_count - 1) : (instruction_index += 1)
                {
                    const instruction = function.instructions.items[instruction_index];
                    assert(instruction.id != Mnemonic.ret);
                    encode_instruction(allocator, &executable, instruction);
                }

                if (function.rsp > 0)
                {
                    const operands = [2]Operand
                    {
                        Operand 
                        {
                            .value = Operand.Value {
                                .register = Encoding.Register.SP,
                            },
                            .size = @enumToInt(Operand.Size.bits64),
                        },
                        Operand.Immediate.get_minimum(function.rsp),
                    };

                    const add_rsp = Instruction.create(Mnemonic.add, operands[0..]);
                    encode_instruction(allocator, &executable, add_rsp);
                }

                if (encode_frame_pointer)
                {
                    log_enc.debug("Encoding epilogue\n", .{});
                    const rbp = Operand 
                    {
                        .value = Operand.Value {
                            .register = Encoding.Register.BP,
                        },
                        .size = @enumToInt(Operand.Size.bits64),
                    };
                    const rbp_arr = [1]Operand { rbp };
                    const pop_rbp = Instruction.create(Mnemonic.pop, rbp_arr[0..]);
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
