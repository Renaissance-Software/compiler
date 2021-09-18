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

    NewIR.generate(semantics_result);


    //Codegen.encode(allocator, &module, target);
}

//pub fn load_all_tests(page_allocator: *Allocator, cwd: std.fs.Dir) !void
//{
    //var arena = std.heap.ArenaAllocator.init(page_allocator);
    //defer arena.deinit();
    //const allocator = &arena.allocator;

    //var test_file_contents: [test_files.len][]const u8 = undefined;

    //for (test_files) |test_filename, i|
    //{
        //test_file_contents[i] = try cwd.readFileAlloc(allocator, test_filename, 0xffffffff);
    //}

    //const iterations : u64 = 10000;
    //var i: u64 = 0;
    //while (i < iterations) : (i += 1)
    //{
        //for (test_file_contents) |test_file_content|
        //{
            //logger.debug("\nTEST #{} ({s}):\n==========\n{s}\n", .{i, filename, file_content});
            //if (!make_executable(allocator, &compiler, test_file_content))
            //{
                //compiler.report_error("Compiler workflow failed\n", .{});
            //}
        //}
    //}
//}
