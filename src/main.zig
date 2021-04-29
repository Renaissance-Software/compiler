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

fn compiler_work_on_file_content(allocator: *Allocator, file_content: []const u8) bool
{
    var compiler = Compiler{
        .errors_reported = false,
    };
    var types : Internal.TypeBuffer = Type.init(allocator);

    const lexer_result = Lexer.lexical_analyze(allocator, &compiler, file_content, &types);
    // @Info: lexer_result.line_count is ignored

    const parser_result = Parser.parse(allocator, &compiler, lexer_result);

    //IR.encode(allocator, &compiler, parser_result.function_declarations, &types);

    return true;
}

fn compiler_file_workflow(page_allocator: *Allocator, cwd: std.fs.Dir, filename: []const u8, i: u64) !void
{
    var arena = std.heap.ArenaAllocator.init(page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;
    const file_content = try cwd.readFileAlloc(allocator, filename, 0xffffffff);
    print("\nTEST #{} ({s}):\n==========\n{s}\n", .{i, filename, file_content});
    defer allocator.free(file_content);

    if (!compiler_work_on_file_content(allocator, file_content))
    {
        print("Compiler workflow failed\n", .{});
    }
}

const test_dir = "tests/";
const test_files = [_][]const u8
{
    test_dir ++ "empty_void.rns",
    test_dir ++ "void_return.rns",
    test_dir ++ "lit_return.rns",
    test_dir ++ "var_return.rns",
    test_dir ++ "loop_if.rns",
    test_dir ++ "loop_and_nested_if.rns",
    test_dir ++ "more_complicated_loop_and_nested_if_and_else.rns",
    test_dir ++ "even_more_complicated_loop_and_nested_if_and_else.rns",
    test_dir ++ "compiler_crasher.rns",
    test_dir ++ "function_calls.rns",
    test_dir ++ "pointers.rns",
    test_dir ++ "function_args.rns",
    test_dir ++ "pointer_args.rns",
    test_dir ++ "pointer_and_branching.rns",
    test_dir ++ "array_basic.rns",
    test_dir ++ "array_assign.rns",
    test_dir ++ "struct_basic.rns",
};

pub fn main() anyerror!void
{
    const all_tests = true;
    var page_allocator = std.heap.page_allocator;
    const cwd = std.fs.cwd();

    if (all_tests)
    {
        for (test_files) |test_file, i|
        {
            try compiler_file_workflow(page_allocator, cwd, test_file, i);
        }
    }
    else
    {
        const index = test_files.len - 1;
        try compiler_file_workflow(page_allocator, cwd, test_files[index], index);
    }
}
