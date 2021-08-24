const std = @import("std");
const print = std.debug.print;
const panic = std.debug.panic;
const Allocator = std.mem.Allocator;
const IR = @import("../../bytecode.zig");

const Register = enum(u6)
{
};

const Instruction = packed struct
{
    prefix: u3,
    opcode0: u4,
    rest: u25,

    const Opcode0 = enum
    {
        reserved,
        unallocated0001,
        sve,
        unallocated0011,
        data_processing_immediate,
        branches_exception_system,
        load_store,
        data_processing_register,
        data_processing_fp_simd,
    };

    fn determine(n: u32) void
    {
        print("Instruction: {b}\n", .{n});
        const i = @intToPtr(*Instruction, @ptrToInt(&n));
        print("Instruction opcode 0: {b}\n", .{i.opcode0});
    }
};

pub fn encode(_: *Allocator, module: *IR.Module) void
{
    if (true)
    {
        Instruction.determine(0xd65f03c0);
        panic("Reached here\n",.{});
    }

    for (module.functions.list.items) |*function_bucket|
    {
        for (function_bucket.items[0..function_bucket.len]) |*function|
        {
            for (function.basic_blocks.items) |basic_block|
            {
                for (basic_block.instructions.items) |instruction|
                {
                    switch (instruction.id)
                    {
                        IR.Instruction.ID.Ret =>
                        {
                            panic("Instruction not implemented: {s}\n", .{@tagName(instruction.id)});
                        },
                        else => panic("Instruction not implemented: {s}\n", .{@tagName(instruction.id)}),
                    }
                }
            }
        }
    }
}
