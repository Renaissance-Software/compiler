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

pub const log_level: std.log.Level = .debug;
pub const log_general = true;
pub const log_lexer = false;
pub const log_parser = false;
pub const log_semantics = false;
pub const log_bytecode = true;
pub const log_x86_64 = true;
pub const log_x86_64_encoding = false;

pub fn main() anyerror!void
{
    const all_tests = true;
    const benchmark = false;
    var page_allocator = std.heap.page_allocator;
    const cwd = std.fs.cwd();

    //if (!benchmark)
    //{
        //if (all_tests)
        //{
            //for (test_files) |test_file, i|
            //{
                //try compiler_file_workflow(page_allocator, cwd, test_file, i);
            //}
        //}
        //else
        //{
            ////const index = 16;
            //const index = test_files.len - 1;
            //try compiler_file_workflow(page_allocator, cwd, test_files[index], index);
        //}
    //}
    //else
    //{
        //try compile_load_all_tests(page_allocator, cwd);
    //}
    //

    const minimal_elf64 = [_]u8
    {
        // e_ident
        0x7f, // magic number
        0x45, 0x4c, 0x46, // "ELF"
        0x02, // Bit count
        0x01, // Endianness
        0x01, // ELF header version
        0x00, // ABI
        0x00, // ABI version
        // padding
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

        // object type
        0x02, 0x00,
        // machine
        0x3e, 0x00,
        // ELF version
        0x01, 0x00, 0x00, 0x00,
        // Program entry position
        0x78, 0x80, 0x04, 0x08, 0x00, 0x00, 0x00, 0x00,

        // Program header table position
        0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        // Section header table position
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        // Flags
        0x00, 0x00, 0x00, 0x00,
        // Header size
        0x40, 0x00,
        // Size of an entry in the program header table
        0x38, 0x00,
        // Number of entries in the program header table
        0x01, 0x00,
        // Size of an entry in the section header table
        0x00, 0x00,
        // Number of entries in the section header table
        0x00, 0x00,
        // Index in section header table with the section names
        0x00, 0x00,

        // Program header
        //
        // Segment type
        0x01, 0x00, 0x00, 0x00,
        // Flags
        0x05, 0x00, 0x00, 0x00,
        // Offset of the segment in file image
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        // Virtual address of the segment in memory
        0x00, 0x80, 0x04, 0x08, 0x00, 0x00, 0x00, 0x00,
        // Physical address of the segment in memory
        0x00, 0x80, 0x04, 0x08, 0x00, 0x00, 0x00, 0x00,
        // File size
        0x82, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        // Memory size
        0x82, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        // Alignment
        0x00, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,


        // Code
        0x66, 0xbf, 0x2e, 0x00, 0x31, 0xc0, 0xb0, 0x3c, 0x0f, 0x05,
    };

    const Elf64 = struct
    {
        const Header = extern struct
        {
            // e_ident
            magic: u8 = 0x7f,
            elf_id: [3]u8 = "ELF".*,
            bit_count: u8 = @enumToInt(Bits.b64),
            endianness: u8 = @enumToInt(Endianness.little),
            header_version: u8 = 1,
            os_abi: u8 = @enumToInt(ABI.SystemV),
            abi_version: u8 = 0,
            padding: [7]u8 = [_]u8 { 0 } ** 7,
            object_type: u16 = @enumToInt(ObjectFileType.executable), // e_type
            machine : u16 = @enumToInt(Machine.AMD64),
            version: u32 = 1,
            entry: u64,
            program_header_offset: u64 = @enumToInt(ProgramHeaderOffset.b64),
            section_header_offset: u64,
            flags: u32 = 0,
            header_size: u16 = @enumToInt(HeaderSize.b64),
            program_header_size: u16 = @enumToInt(ProgramHeaderSize.b64),
            program_header_entry_count: u16 = 1,
            section_header_size: u16 = 0,
            section_header_entry_count: u16,
            name_section_header_index: u16,

            const Bits = enum(u8)
            {
                b32 = 1,
                b64 = 2,
            };

            const Endianness = enum(u8)
            {
                little = 1,
                big = 2,
            };

            const ABI = enum(u8)
            {
                SystemV = 0,
            };

            const ObjectFileType = enum(u16)
            {
                none = 0,
                relocatable = 1,
                executable = 2,
                dynamic = 3,
                core = 4,
                lo_os = 0xfe00,
                hi_os = 0xfeff,
                lo_proc = 0xff00,
                hi_proc = 0xffff,
            };

            const Machine = enum(u16)
            {
                AMD64 = 0x3e,
            };

            const ProgramHeaderOffset = enum(u64)
            {
                b32 = 52,
                b64 = 64,
            };

            const HeaderSize = enum(u16)
            {
                b32 = 52,
                b64 = 64,
            };

            const ProgramHeaderSize = enum(u16)
            {
                b32 = 32,
                b64 = 56,
            };

            const SectionHeaderSize = enum(u16)
            {
                b32 = 40,
                b64 = 64,
            };
        };

        const ProgramHeader = extern struct
        {
            type: ProgramHeaderType = ProgramHeaderType.load,
            flags: u32 = @enumToInt(Flags.readable) | @enumToInt(Flags.executable),
            offset: u64,
            virtual_address: u64,
            physical_address: u64,
            size_in_file: u64,
            size_in_memory: u64,
            alignment: u64 = 0x100,

            const ProgramHeaderType = enum(u32)
            {
                @"null" = 0,
                load = 1,
                dynamic = 2,
                interpreter = 3,
                note = 4,
                shlib = 5, // reserved
                program_header = 6,
                tls = 7,
                lo_os = 0x60000000,
                hi_os = 0x6fffffff,
                lo_proc = 0x70000000,
                hi_proc = 0x7fffffff,
            };

            const Flags = enum(u8)
            {
                executable = 1,
                writable = 2,
                readable = 4,
            };
        };
    };
    

    const program_header_base_address: u64 = 0x08048000;
    const entry = program_header_base_address + @enumToInt(Elf64.Header.ProgramHeaderOffset.b64) + @sizeOf(Elf64.ProgramHeader);
    const section_header_offset: u64 = 0;
    const section_header_entry_count: u16 = 0;
    const name_section_header_index: u16 = 0;

    const code = [_]u8 { 0x66, 0xbf, 0x2e, 0x00, 0x31, 0xc0, 0xb0, 0x3c, 0x0f, 0x05 };

    assert(@sizeOf(Elf64.Header) == 0x40);
    assert(@sizeOf(Elf64.ProgramHeader) == 0x38);
    assert(0x40 + 0x38 + code.len == 130);

    const file_size = @sizeOf(Elf64.Header) + @sizeOf(Elf64.ProgramHeader) + code.len;
    logger.debug("File size: {}\n", .{file_size});

    const elf_header = Elf64.Header
    {
        .entry = entry,
        .section_header_offset = section_header_offset,
        .section_header_entry_count = section_header_entry_count,
        .name_section_header_index = name_section_header_index,
    };

    const program_header = Elf64.ProgramHeader
    {
        .offset = 0,
        .virtual_address = program_header_base_address,
        .physical_address = program_header_base_address,
        .size_in_file = file_size,
        .size_in_memory = file_size,
        .alignment = 0x100,
    };
    const file_buffer = blk: {
        var file_buffer = std.ArrayList(u8).initCapacity(std.heap.page_allocator, file_size) catch |err| {
            panic("Couldn't allocate memory for file buffer\n", .{});
        };

        file_buffer.appendSlice(std.mem.asBytes(&elf_header)) catch unreachable;
        file_buffer.appendSlice(std.mem.asBytes(&program_header)) catch unreachable;
        file_buffer.appendSlice(code[0..]) catch unreachable;

        break :blk file_buffer;
    };

    logger.debug("Size of ELF64 header: {}\n", .{@sizeOf(Elf64.Header)});
    assert(file_size == file_buffer.items.len);

    const file = try std.fs.cwd().createFile("minimal.elf64", .{ .mode = 0o777});
    defer file.close();

    try file.writeAll(file_buffer.items[0..]);
}
