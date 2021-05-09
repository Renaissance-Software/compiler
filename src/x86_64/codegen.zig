const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const panic = std.debug.panic;

const Internal = @import("../compiler.zig");
const Compiler = Internal.Compiler;
const Log = Compiler.LogLevel;

const IR = @import("../bytecode.zig");

const Encoding = @import("encoding.zig");

const encode_frame_pointer = true;

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
        register,
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
    id: Encoding.Instruction.ID,
    operands: [4]Operand,
    label: Label,
};

fn create_instruction(instruction_id: Encoding.Instruction.ID, operands: []const *IR.Value) Instruction
{
    var instruction = Instruction
    {
        .id = instruction_id,
        .operands = undefined,
        .label = undefined,
    };
    if (operands.len > 0)
    {
        panic("ni:\n", .{});
    }
    else
    {
        const zero_operand = Operand
        {
            .value = Operand.ID.none,
            .size =  0,
        };
        std.mem.set(Operand, instruction.operands[0..], zero_operand);
    }

    return instruction;
}

const InstructionBuffer = ArrayList(Instruction);

pub fn encode(compiler: *Compiler, allocator: *Allocator, module: *IR.Module) void
{
    compiler.current_module = Compiler.Module.machine_code;

    for (module.functions.list.items) |function_bucket|
    {
        var function_index: u64 = 0;
        while (function_index < function_bucket.len) : (function_index += 1)
        {
            const function = &function_bucket.items[function_index];

            var instruction_buffer = InstructionBuffer.init(allocator);

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
                                const mc_i = create_instruction(Encoding.Instruction.ID.ret, instruction.operands.items);

                            }
                        },
                        else => panic("ni: {}\n", .{instruction.id}),
                    }
                }
            }

            // encode bytes
        }
    }
}
