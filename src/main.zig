const std = @import("std");
const print = std.debug.print;
const assert = std.debug.assert;
const panic = std.debug.panic;
const Allocator = std.mem.Allocator;

const Lexer = @import("lexer.zig");
const Parser = @import("parser.zig");
const Semantics = @import("semantics.zig");
const IR = @import("bytecode.zig");
const Internal = @import("compiler.zig");
const Compiler = Internal.Compiler;
const Type = Internal.Type;

fn compiler_work_on_file_content(allocator: *Allocator, compiler: *Compiler, file_content: []const u8) bool
{
    var types : Internal.TypeBuffer = Type.init(allocator);

    const lexer_result = Lexer.lexical_analyze(allocator, compiler, file_content);
    // @Info: lexer_result.line_count is ignored

    var parser_result = Parser.parse(allocator, compiler, lexer_result);

    var semantics_result = Semantics.analyze(compiler, allocator, &parser_result);

    IR.encode(allocator, compiler, &semantics_result);

    return true;
}

fn compiler_file_workflow(page_allocator: *Allocator, cwd: std.fs.Dir, filename: []const u8, i: u64) !void
{
    var arena = std.heap.ArenaAllocator.init(page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;
    const file_content = try cwd.readFileAlloc(allocator, filename, 0xffffffff);

    const log_general = true;
    const log_lexer = false;
    const log_parser = false;
    const log_semantics = true;
    const log_bytecode = true;

    var compiler = Compiler
    {
        .log_level = Compiler.LogLevel.debug,
        .module_log = Compiler.get_log_module(log_general, log_lexer, log_parser, log_semantics, log_bytecode),
        .current_module = Compiler.Module.general,
    };

        compiler.log(Compiler.LogLevel.info, "\nTEST #{} ({s}):\n==========\n{s}\n", .{i, filename, file_content});
    defer allocator.free(file_content);

    if (!compiler_work_on_file_content(allocator, &compiler, file_content))
    {
        compiler.report_error("Compiler workflow failed\n", .{});
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
        //const index = 11;
        const index = test_files.len - 1;
        try compiler_file_workflow(page_allocator, cwd, test_files[index], index);
    }
}
