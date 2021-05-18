const std = @import("std");
const print = std.debug.print;
const assert = std.debug.assert;
const panic = std.debug.panic;
const Allocator = std.mem.Allocator;

const Lexer = @import("lexer.zig");
const Parser = @import("parser.zig");
const Semantics = @import("semantics.zig");
const IR = @import("bytecode.zig");
const Types = @import("ast_types.zig");
const Type = Types.Type;
const TypeBuffer = Types.TypeBuffer;
const logger = std.log.scoped(.general);

const CG = @import("x86_64/codegen.zig");

fn compiler_work_on_file_content(allocator: *Allocator, file_content: []const u8) bool
{
    var types: TypeBuffer = Types.init(allocator);

    const lexer_result = Lexer.lexical_analyze(allocator, file_content);
    // @Info: lexer_result.line_count is ignored

    var parser_result = Parser.parse(allocator, lexer_result);

    var semantics_result = Semantics.analyze(allocator, &parser_result);

    var module = IR.encode(allocator, &semantics_result);

    CG.encode(allocator, &module);

    return true;
}

fn compiler_file_workflow(page_allocator: *Allocator, cwd: std.fs.Dir, filename: []const u8, i: u64) !void
{
    var arena = std.heap.ArenaAllocator.init(page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;
    const file_content = try cwd.readFileAlloc(allocator, filename, 0xffffffff);

    logger.info("\nTEST #{} ({s}):\n==========\n{s}\n", .{i, filename, file_content});
    defer allocator.free(file_content);

    if (!compiler_work_on_file_content(allocator, file_content))
    {
        logger.err("Compiler workflow failed\n", .{});
    }
}

fn compile_load_all_tests(page_allocator: *Allocator, cwd: std.fs.Dir) !void
{
    var arena = std.heap.ArenaAllocator.init(page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;

    var test_file_contents: [test_files.len][]const u8 = undefined;

    for (test_files) |test_filename, i|
    {
        test_file_contents[i] = try cwd.readFileAlloc(allocator, test_filename, 0xffffffff);
    }

    const iterations : u64 = 10000;
    var i: u64 = 0;
    while (i < iterations) : (i += 1)
    {
        for (test_file_contents) |test_file_content, file_index|
        {
            logger.debug("\nTEST #{} ({s}):\n==========\n{s}\n", .{i, filename, file_content});
            if (!compiler_work_on_file_content(allocator, &compiler, test_file_content))
            {
                compiler.report_error("Compiler workflow failed\n", .{});
            }
        }
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


pub fn log(comptime level: std.log.Level, comptime scope: @TypeOf(.EnumLiteral), comptime format: []const u8, args: anytype) void
{
    switch (scope)
    {
        .general =>
        {
            if (!log_general)
            {
                return;
            }
        },
        .lexer =>
        {
            if (!log_lexer)
            {
                return;
            }
        },
        .parser =>
        {
            if (!log_parser)
            {
                return;
            }
        },
        .semantics =>
        {
            if (!log_semantics)
            {
                return;
            }
        },
        .bytecode =>
        {
            if (!log_bytecode)
            {
                return;
            }
        },
        .x86_64_codegen =>
        {
            if (!log_x86_64)
            {
                return;
            }
        },
        .x86_64_codegen_enc =>
        {
            if (!log_x86_64_encoding)
            {
                return;
            }
        },
        else => panic("ni: {}\n", .{scope}),
    }
    //switch (scope)
    //{
        //.default => print("Logging default: ", .{}),
        //else => panic("not implemented: {}\n", .{scope}),
    //}

    // Print the message to stderr, silently ignoring any errors
    const held = std.debug.getStderrMutex().acquire();
    defer held.release();
    const stderr = std.io.getStdErr().writer();
    nosuspend stderr.print(format, args) catch return;
}

pub const log_level: std.log.Level = .debug;
pub const log_general = true;
pub const log_lexer = false;
pub const log_parser = false;
pub const log_semantics = false;
pub const log_bytecode = false;
pub const log_x86_64 = true;
pub const log_x86_64_encoding = false;

pub fn main() anyerror!void
{
    const all_tests = false;
    const benchmark = false;
    var page_allocator = std.heap.page_allocator;
    const cwd = std.fs.cwd();

    if (!benchmark)
    {
        if (all_tests)
        {
            for (test_files) |test_file, i|
            {
                try compiler_file_workflow(page_allocator, cwd, test_file, i);
            }
        }
        else
        {
            const index = 10;
            //const index = test_files.len - 1;
            try compiler_file_workflow(page_allocator, cwd, test_files[index], index);
        }
    }
    else
    {
        try compile_load_all_tests(page_allocator, cwd);
    }
}
