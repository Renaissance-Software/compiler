const std = @import("std");
const print = std.debug.print;
const assert = std.debug.assert;
const panic = std.debug.panic;
const Allocator = std.mem.Allocator;

const Compiler = @import("compiler.zig");
const Lexer = @import("lexer.zig");
const Parser = @import("parser.zig");
const Semantics = @import("semantics.zig");
const IR = @import("bytecode.zig");
const Codegen = @import("codegen.zig");
const Types = @import("ast_types.zig");
const Type = Types.Type;
const TypeBuffer = Types.TypeBuffer;
const logger = std.log.scoped(.general);

pub fn log(comptime _: std.log.Level, comptime scope: @TypeOf(.EnumLiteral), comptime format: []const u8, args: anytype) void
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
        .x86_64_codegen_enc =>
        {
            if (!log_x86_64_encoding)
            {
                return;
            }
        },
        .compiler =>
        {
            if (!log_compiler)
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

const test_files_windows = [_][]const u8
{
    test_dir ++ "windows_hello_world.rns",
};

const test_files_linux = [_][]const u8
{
    test_dir ++ "linux_hello_world.rns",
};

pub const log_level: std.log.Level = .debug;
pub const log_general = true;
pub const log_lexer = false;
pub const log_parser = false;
pub const log_semantics = false;
pub const log_bytecode = true;
pub const log_x86_64 = true;
pub const log_x86_64_encoding = false;
pub const log_compiler = true;

pub fn main() anyerror!void
{
    const all_tests = false;
    var page_allocator = std.heap.page_allocator;
    const cwd = std.fs.cwd();
    const target = std.builtin.target;

    // mac os workaround
    if (false)
    {
        const filename = "macho_exe";
        var arena = std.heap.ArenaAllocator.init(page_allocator);
        defer arena.deinit();
        const allocator = &arena.allocator;
        const file_content = cwd.readFileAlloc(allocator, filename, 0xffffffff) catch {
            panic("Error reading file: {s}\n", .{filename});
        };

        defer allocator.free(file_content);
        var fake_code = [_]u8{ 0x00, 0x00, 0x80, 0x52, 0xc0, 0x03, 0x5f, 0xd6 };
        macho.experiment(allocator, file_content, fake_code[0..]);
        return;
    }

    if (all_tests)
    {
        for (test_files) |test_file, i|
        {
            print("[Test #{}] {s}\n", .{i, test_file});
            Compiler.make_executable(page_allocator, test_file, target);
        }
    }
    else
    {
        const index = 0;
        //const index = test_files.len - 1;
        Compiler.make_executable(page_allocator, test_files[index], target);
    }
}
