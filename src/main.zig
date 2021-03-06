const std = @import("std");
const assert = std.debug.assert;
const panic = std.debug.panic;
const Allocator = std.mem.Allocator;

const Compiler = @import("compiler.zig");
const log_general = Compiler.log;

fn log(comptime format: []const u8, arguments: anytype) void
{
    log_general(.main, format, arguments);
}

fn make_exe_name(allocator: *Allocator, name: []const u8, target: std.Target) []const u8
{
    const os = target.os.tag;
    const exe_termination = if (os == .windows) "exe" else "out";
    const file_union = [_][]const u8 { name[0..name.len - 3], exe_termination };

    const exe_filename = std.mem.join(allocator, "", file_union[0..]) catch unreachable;
    return exe_filename;
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


pub fn main() anyerror!void
{
    const all_tests = false;
    var page_allocator = std.heap.page_allocator;
    const target = std.builtin.target;

    if (all_tests)
    {
        inline for (test_files) |test_file, i|
        {
            log("[Test #{}] {s}\n", .{i, test_file});
            Compiler.make_executable(page_allocator, test_file, target);
        }
    }
    else
    {
        const index = 16;
        //const index = test_files.len - 1;
        const test_file = test_files[index];
        Compiler.make_executable(page_allocator, test_file, make_exe_name(page_allocator, test_file, target), target);
    }
}
