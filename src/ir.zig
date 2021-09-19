const std = @import("std");
const print = std.debug.print;
const assert = std.debug.assert;
const panic = std.debug.panic;

const AST = @import("parser.zig").AST;
const Semantics = @import("semantics.zig");

pub fn generate(result: Semantics.Result) void
{
    for (result.external_functions) |function|
    {
        print("External function: {}\n", .{function});
    }
    for (result.internal_functions) |function|
    {
        print("Function: {}\n", .{function});
    }
}
