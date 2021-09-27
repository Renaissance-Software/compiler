const std = @import("std");
const Allocator = std.mem.Allocator;
const panic = std.debug.panic;
const print = std.debug.print;
const logger = std.log.scoped(.compiler);

const Lexer = @import("lexer.zig");
const Parser = @import("parser.zig");
const Semantics = @import("semantics.zig");
const IR = @import("bytecode.zig");
const NewIR = @import("ir.zig");
const Codegen = @import("codegen.zig");

pub fn make_executable(page_allocator: *Allocator, source_filename: []const u8, target: std.Target) void
{
    var arena = std.heap.ArenaAllocator.init(page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;

    const ast = Parser.AST.parse(allocator, source_filename, target);
    const semantics_result = Semantics.analyze(allocator, ast);

    const ir_program = NewIR.generate(allocator, semantics_result);
    Codegen.encode(allocator, &ir_program, target);
}

pub fn should_log(comptime scope: @TypeOf(.EnumLiteral)) bool
{
    return comptime switch (scope)
    {
        .x86_64 => true,
        else => false,
    };
}

pub fn log(comptime scope: @TypeOf(.EnumLiteral), comptime format: []const u8, arguments: anytype) void
{
    if (comptime should_log(scope))
    {
        const held = std.debug.getStderrMutex().acquire();
        defer held.release();
        const stderr = std.io.getStdErr().writer();
        nosuspend stderr.print(format, arguments) catch return;
    }
}
