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
        indirect,
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
    label: Label,
};

fn create_instruction(instruction_id: Mnemonic) Instruction
{
    var instruction = Instruction
    {
        .id = instruction_id,
        .operands = undefined,
        .label = undefined,
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

fn encode_instruction(compiler: *Compiler, allocator: *Allocator, executable: *Executable, instruction: Instruction) void
{
    const instruction_offset = @ptrToInt(executable.code_buffer.items.ptr) + executable.code_buffer.items.len; 
    compiler.log(Log.debug, "Instruction start offset: 0x{x}\n", .{instruction_offset});

    panic("Encoding not implemented\n", .{});
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
                                // Here we place the return value
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
                    push_rbp.operands[0] = Operand 
                    {
                        .value = Operand.Value {
                            .register = Encoding.Register.BP,
                        },
                        .size = @enumToInt(Operand.Size.bits64),
                    };

                    var mov_rbp_rsp = create_instruction(Mnemonic.mov);
                    push_rbp.operands[0] = Operand 
                    {
                        .value = Operand.Value {
                            .register = Encoding.Register.BP,
                        },
                        .size = @enumToInt(Operand.Size.bits64),
                    };
                    push_rbp.operands[1] = Operand 
                    {
                        .value = Operand.Value {
                            .register = Encoding.Register.SP,
                        },
                        .size = @enumToInt(Operand.Size.bits64),
                    };

                    encode_instruction(compiler, allocator, &executable, push_rbp);
                    encode_instruction(compiler, allocator, &executable, mov_rbp_rsp);
                }

                for (function.instructions.items) |instruction, i|
                {
                    if (instruction.id == Mnemonic.ret)
                    {
                        assert(i == function.instructions.items.len - 1);
                    }
                    encode_instruction(compiler, allocator, &executable, instruction);
                }

                //encode_instruction(compiler, allocator, &executable, 
                //encode_instruction(compiler, allocator, &executable, 
            }
        },
        else => panic("ni: {}\n", .{calling_convention}),
    }
}
