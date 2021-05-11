const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const assert = std.debug.assert;
const panic = std.debug.panic;

const Internal = @import("../compiler.zig");
const Compiler = Internal.Compiler;
const Log = Compiler.LogLevel;

const IR = @import("../bytecode.zig");

const Encoding = @import("encoding.zig");
const Mnemonic = Encoding.Instruction.ID;

const encode_frame_pointer = true;

const c = @cImport({
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
        relative,
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

    const Immediate = extern union
    {
        imm8: u8,
        imm16: u16,
        imm32: u32,
        imm64: u64,
    };

    const Indirect = struct
    {
        displacement: i32,
        register: Encoding.Register,
    };
};

const Label = struct
{
    size: Operand.Size,
    target: usize,
    locations: ArrayList(Location),

    const Location = struct
    {
        to: usize, // @Info: target
        from: usize, // @Info: offset
    };
};

const Instruction = struct
{
    id: Mnemonic,
    operands: [4]Operand,
    operand_count: u64,
    label: Label,

    fn add_operand(self: *Instruction, operand: Operand) void
    {
        const operand_count = self.operand_count;
        assert(operand_count < self.operands.len);
        self.operands[operand_count] = operand;
        self.operand_count += 1;
    }
};

fn create_instruction(instruction_id: Mnemonic) Instruction
{
    var instruction = Instruction
    {
        .id = instruction_id,
        .operands = undefined,
        .label = undefined,
        .operand_count = 0,
    };

    const zero_operand = Operand
    {
        .value = Operand.ID.none,
        .size =  0,
    };

    std.mem.set(Operand, instruction.operands[0..], zero_operand);

    return instruction;
}

const InstructionBuffer = ArrayList(Instruction);

const Function = struct
{
    instructions: InstructionBuffer,
    start_address: u64,
};
const FunctionBuffer = ArrayList(Function);

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

fn encode_instruction(compiler: *Compiler, allocator: *Allocator, executable: *Executable, instruction: Instruction) void
{
    compiler.log(Log.debug, "Encoding instruction: {}...\n", .{instruction.id});
    const encodings = Encoding.instructions[@enumToInt(instruction.id)];
    if (encodings.len == 0)
    {
        panic("Instruction not implemented yet: {}\n", .{instruction.id});
    }

    compiler.log(Log.debug, "Encoding count: {}\n", .{encodings.len});
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

            compiler.log(Log.debug, "Operand count. Required: {}. Current: {}\n", .{operand_count, combination.count});
            if (operand_count != combination.count)
            {
                compiler.log(Log.debug, "Operand count is different. Skipping...\n", .{});
                continue;
            }

            var operand_index: u64 = 0;

            while (operand_index < operand_count) : (operand_index += 1)
            {
                const operand = instruction.operands[operand_index];
                const operand_encoding = combination.operands[operand_index];
                compiler.log(Log.debug, "Operand encoding ID: {}\n", .{operand_encoding.id});
                compiler.log(Log.debug, "Operand size: {}. Encoding size: {}\n", .{operand.size, operand_encoding.size});

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
                    else => panic("not implemented: {}\n", .{operand.value}),
                }

                matched = false;
                break;
            }
            
            if (matched)
            {
                // do everything here
                compiler.log(Log.debug, "Found instruction!!!!\n", .{});

                const instruction_offset = @ptrToInt(executable.code_buffer.items.ptr) + executable.code_buffer.items.len; 
                compiler.log(Log.debug, "Instruction start offset: 0x{x}\n", .{instruction_offset});

                // @TODO: label
                //

                var rex_byte: u8 = @enumToInt(combination.rex);
                var memory_encoding = false;
                var r_m_encoding = false;

                operand_index = 0;
                while (operand_index < operand_count) : (operand_index += 1)
                {
                    const operand = instruction.operands[operand_index];
                    const operand_encoding = combination.operands[operand_index];

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
                var encoding_stack_operand = false;

                if (need_mod_rm)
                {
                    compiler.log(Log.debug, "It needs MOD RM. Doing operations...\n", .{});
                    operand_index = 0;
                    while (operand_index < operand_count) : (operand_index += 1)
                    {
                        const operand = instruction.operands[operand_index];
                        compiler.log(Log.debug, "Operand {}. ID: {s}\n", .{operand_index, @tagName(operand.value)});

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
                                mod = @enumToInt(Encoding.Mod.displacement32);
                                r_m = indirect_register;
                                encoding_stack_operand = indirect_register == @enumToInt(get_stack_register());
                                need_sib = encoding_stack_operand;

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

                    mod_r_m = (mod << 6) | ((register_or_digit & 0b111) << 3) | (r_m & 0b111);
                }

                if (rex_byte != 0)
                {
                    compiler.log(Log.debug, "Writing rex byte: 0x{x}\n", .{rex_byte});
                    executable.code_buffer.append(rex_byte) catch unreachable;
                }
                else if ((instruction.operands[0].value == Operand.ID.register and instruction.operands[0].size == 2) or (instruction.operands[1].value == Operand.ID.register and instruction.operands[1].size == 2) or  encoding.options.explicit_byte_size == 2)
                {
                    executable.code_buffer.append(Encoding.Operand.size_override) catch unreachable;
                }

                for (op_code) |op_code_byte, i|
                {
                    if (op_code_byte != 0)
                    {
                        compiler.log(Log.debug, "Writing op code byte {}: 0x{x}\n", .{i, op_code_byte});
                        executable.code_buffer.append(op_code_byte) catch unreachable;
                    }
                }

                if (need_mod_rm)
                {
                    compiler.log(Log.debug, "Writing Mod RM: 0x{x}\n", .{mod_r_m});
                    executable.code_buffer.append(mod_r_m) catch unreachable;
                }

                if (need_sib)
                {
                    compiler.log(Log.debug, "Writing SIB byte: 0x{x}\n", .{sib_byte});
                    executable.code_buffer.append(sib_byte) catch unreachable;
                }

                if (need_mod_rm and mod != @enumToInt(Encoding.Mod.register))
                {
                    panic("not implemented\n", .{});
                }

                operand_index = 0;
                compiler.log(Log.debug, "Operand count: {}\n", .{operand_count});

                while (operand_index < operand_count) : (operand_index += 1)
                {
                    const operand = instruction.operands[operand_index];

                    switch (operand.value)
                    {
                        Operand.ID.immediate =>
                        {
                            panic("ni\n", .{});
                        },
                        Operand.ID.relative =>
                        {
                            panic("ni\n", .{});
                        },
                        Operand.ID.register => {},
                        else => panic("ni: {}\n", .{operand.value}),
                    }
                }

                compiler.log(Log.debug, "The code buffer has so far:\n", .{});

                for (executable.code_buffer.items) |byte|
                {
                    compiler.log(Log.debug, "0x{x}|", .{byte});
                }

                compiler.log(Log.debug, "\n", .{});
                return;
            }
        }
    }


    panic("Unable to find a fitting instruction: {}", .{instruction.id});
}

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

pub fn encode(compiler: *Compiler, allocator: *Allocator, module: *IR.Module) void
{
    compiler.current_module = Compiler.Module.machine_code;

    var executable = Executable
    {
        .functions = FunctionBuffer.init(allocator),
        .code_base_RVA = 0,
        .code_buffer = undefined,
    };

    for (module.functions.list.items) |function_bucket|
    {
        var function_index: u64 = 0;
        while (function_index < function_bucket.len) : (function_index += 1)
        {
            const function = &function_bucket.items[function_index];

            const mc_function_value = Function
            {
                .instructions = InstructionBuffer.init(allocator),
                .start_address = 0,
            };

            executable.functions.append(mc_function_value) catch |err| {
                panic("Error allocating a new MC function\n", .{});
            };

            var mc_function = &executable.functions.items[executable.functions.items.len - 1];

            for (function.basic_blocks.items) |basic_block|
            {
                compiler.log(Log.debug, "BasicBlock\n", .{});

                for (basic_block.instructions.items) |instruction|
                {
                    compiler.log(Log.debug, "Instruction\n", .{});

                    switch (instruction.id)
                    {
                        IR.Instruction.ID.Ret =>
                        {
                            if (instruction.operands.items.len > 0)
                            {
                                assert(instruction.operands.items.len == 1);
                                // Here we place the return value
                                const operand = instruction.operands.items[0];
                                switch (operand.id)
                                {
                                    IR.Value.ID.ConstantInt =>
                                    {
                                        const constant_int = @ptrCast(*IR.ConstantInt, operand);
                                        switch (constant_int.bit_count)
                                        {
                                            32 =>
                                            {
                                                if (constant_int.is_signed)
                                                {
                                                    const value: i32 = - @intCast(i32, constant_int.int_value);
                                                }
                                                else
                                                {
                                                    const value: u32 = @intCast(u32, constant_int.int_value);
                                                }
                                            },
                                            else => panic("ni: {}\n", .{constant_int.bit_count}),
                                        }
                                    },
                                    else => panic("ni: {}\n", .{operand.id}),
                                }
                                panic("returning a value is not yet implemented\n", .{});
                            }
                            else
                            {
                                const mc_i = create_instruction(Mnemonic.ret);
                                mc_function.instructions.append(mc_i) catch |err| {
                                    panic("Error allocating a new MC instruction\n", .{});
                                };
                            }
                        },
                        else => panic("ni: {}\n", .{instruction.id}),
                    }
                }
            }
        }
    }

    var aprox_instruction_count: u64 = 0;
    const max_bytes_per_instruction: u8 = 15;

    for (executable.functions.items) |function|
    {
        compiler.log(Log.debug, "function\n", .{});
        aprox_instruction_count += 5 * function.instructions.items.len;
    }

    const aprox_code_size = aprox_instruction_count * max_bytes_per_instruction;
    compiler.log(Log.debug, "Aproximate code size: {}\n", .{aprox_code_size});

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
                if (encode_frame_pointer)
                {
                    var push_rbp = create_instruction(Mnemonic.push);
                    push_rbp.add_operand(Operand 
                    {
                        .value = Operand.Value {
                            .register = Encoding.Register.BP,
                        },
                        .size = @enumToInt(Operand.Size.bits64),
                    });

                    var mov_rbp_rsp = create_instruction(Mnemonic.mov);
                    mov_rbp_rsp.add_operand(Operand 
                    {
                        .value = Operand.Value {
                            .register = Encoding.Register.BP,
                        },
                        .size = @enumToInt(Operand.Size.bits64),
                    });
                    mov_rbp_rsp.add_operand(Operand 
                    {
                        .value = Operand.Value {
                            .register = Encoding.Register.SP,
                        },
                        .size = @enumToInt(Operand.Size.bits64),
                    });

                    encode_instruction(compiler, allocator, &executable, push_rbp);
                    encode_instruction(compiler, allocator, &executable, mov_rbp_rsp);
                }

                var i: u64 = 0;
                while (i < function.instructions.items.len - 1) : (i += 1)
                {
                    const instruction = function.instructions.items[i];
                    assert(instruction.id != Mnemonic.ret);
                    encode_instruction(compiler, allocator, &executable, instruction);
                }

                if (encode_frame_pointer)
                {
                    var pop_rbp = create_instruction(Mnemonic.pop);
                    pop_rbp.add_operand(Operand 
                    {
                        .value = Operand.Value {
                            .register = Encoding.Register.BP,
                        },
                        .size = @enumToInt(Operand.Size.bits64),
                    });
                    encode_instruction(compiler, allocator, &executable, pop_rbp);
                }

                const instruction = function.instructions.items[i];
                assert(instruction.id == Mnemonic.ret);
                encode_instruction(compiler, allocator, &executable, instruction);

                const fn_type = fn() void;
                const fn_ptr = @ptrCast(fn_type, executable.code_buffer.items.ptr);
                fn_ptr();
            }
        },
        else => panic("ni: {}\n", .{calling_convention}),
    }
}
