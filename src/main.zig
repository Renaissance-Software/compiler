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

fn compiler_workflow(allocator: *Allocator, file_content: []const u8) bool
{

    var compiler = Compiler{
        .errors_reported = false,
    };
    var types : Internal.TypeBuffer = Type.init(allocator);

    const lexer_result = Lexer.lexical_analyze(allocator, file_content, &types);
    // @Info: lexer_result.line_count is ignored

    const parser_result = Parser.parse(allocator, &compiler, lexer_result, &types);

    IR.encode(allocator, parser_result.function_declarations, &types);

    return true;
}


const test_files = [_][]const u8{"hi", "ho"};

pub fn main() anyerror!void
{
    const tests = false;
    var page_allocator = std.heap.page_allocator;
    const cwd = std.fs.cwd();
    print("CWD: {}\n", .{cwd});

    if (tests)
    {
        for (test_files) |test_file|
        {
            var arena = std.heap.ArenaAllocator.init(page_allocator);
            defer arena.deinit();
            const allocator = &arena.allocator;
            const file_content = try cwd.readFileAlloc(allocator, test_file, 0xffffffff);
            defer allocator.free(file_content);
            if (!compiler_workflow(page_allocator, file_content))
            {
                print("Compiler workflow failed\n", .{});
            }
        }
    }
    else
    {
        const src_file = "main :: () -> s32 { a : s32 = 0; return a; }";
        var arena = std.heap.ArenaAllocator.init(page_allocator);
        defer arena.deinit();
        const allocator = &arena.allocator;
        if (!compiler_workflow(allocator, src_file))
        {
            print("Compiler workflow failed\n", .{});
        }
    }
}
