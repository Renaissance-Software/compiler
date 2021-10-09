const std = @import("std");

const x86 = @import("codegenv2.zig");
const Compiler = @import("../../compiler.zig");

pub fn log(comptime format: []const u8, arguments: anytype) void
{
    Compiler.log(.x86_64, format, arguments);
}

fn compare_resolved_instruction(expected: []const u8, instruction: x86.Instruction) !void
{
    log("Expected bytes:\n", .{});
    for (expected) |byte|
    {
        log("0x{x:0>2}\n", .{byte});
    }
    try std.testing.expectEqualSlices(u8, expected, instruction.resolution.resolved.bytes[0..instruction.resolution.resolved.size]);
}

test "add register immediate"
{
    {
        const value_12 = 0x0c;
        const add_eax_12 = [_]u8 { 0x83, 0xc0, value_12 };
        const my_add_eax_12 = x86.add_register_immediate(.A, 4, .{ .value = value_12, .signed = false });
        try compare_resolved_instruction(add_eax_12[0..], my_add_eax_12);
    }

    {
        const immediate = 0x4cf;
        const add_eax_immediate = [_]u8 { 0x05, 0xcf, 0x04, 0x00, 0x00 };
        const my_add_eax_immediate = x86.add_register_immediate(.A, 4, .{ .value = immediate, .signed = false });
        try compare_resolved_instruction(add_eax_immediate[0..], my_add_eax_immediate);
    }
}

test "cmp indirect immediate"
{
    {
        const immediate = 0x4cf;
        const cmp_indirect_immediate = [_]u8 { 0x81, 0x3c, 0x24, 0xcf, 0x04, 0x00, 0x00 };
        const my_cmp_indirect_immediate = x86.cmp_indirect_immediate(.SP, 0, 4, .{ .value = immediate, .signed = false });
        try compare_resolved_instruction(cmp_indirect_immediate[0..], my_cmp_indirect_immediate);
    }
}

test "sub register immediate"
{
    {
        const immediate = 0x64;
        const sub_register_immediate = [_]u8 { 0x83, 0xe8, 0x64 };
        const my_sub_register_immediate = x86.sub_register_immediate(.A, 4, .{ .value = immediate, .signed = false });
        try compare_resolved_instruction(sub_register_immediate[0..], my_sub_register_immediate);
    }
}
