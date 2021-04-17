const std = @import("std");
const print = std.debug.print;
const assert = std.debug.assert;
const panic = std.debug.panic;
const Allocator = std.mem.Allocator;

const Parser = @import("parser.zig");
const Lexer = @import("lexer.zig");
const IR = @import("bytecode.zig");
const Internal = @import("compiler.zig");
const Compiler = Internal.Compiler;
const Type = Internal.Type;

fn compiler_workflow(page_allocator: *Allocator, src_file: []const u8) bool
{
    var arena = std.heap.ArenaAllocator.init(page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;

    var compiler = Compiler{
        .errors_reported = false,
    };
    var types : Internal.TypeBuffer = Type.init(allocator);

    const lexer_result = Lexer.lexical_analyze(allocator, src_file, &types);
    // @Info: lexer_result.line_count is ignored

    const parser_result = Parser.parse(allocator, &compiler, lexer_result, &types);

    IR.encode(allocator, parser_result.function_declarations, &types);

    return true;
}

pub fn main() anyerror!void
{
    const src_file = "main :: () -> s32 { return 0; }";
    var page_allocator = std.heap.page_allocator;

    if (!compiler_workflow(page_allocator, src_file))
    {
        print("Compiler workflow failed\n", .{});
    }
}
