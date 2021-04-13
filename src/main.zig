const std = @import("std");
const print = std.debug.print;
const assert = std.debug.assert;
const panic = std.debug.panic;
const Parser = @import("parser.zig");
const Lexer = @import("lexer.zig");
const IR = @import("bytecode.zig");
const Internal = @import("compiler.zig");
const Compiler = Internal.Compiler;
const Type = Internal.Type;
const Allocator = std.mem.Allocator;


pub fn main() anyerror!void
{
    const src_file = "main :: () -> s32 { return 0; }";
    var compiler = Compiler {
        .errors_reported = false,
    };
    var allocator = std.heap.page_allocator;
    var types = Type.init(allocator);

    const lexer_result = Lexer.lexical_analyze(allocator, src_file, &types);
    // @Info: lexer_result.line_count is ignored

    print("Parser\n", .{});

    Parser.parse(allocator, &compiler, lexer_result, &types);


    print("IR\n", .{});

    IR.encode(allocator);
}
