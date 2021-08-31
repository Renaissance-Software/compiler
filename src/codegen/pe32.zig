const std = @import("std");
const Allocator = std.mem.Allocator;
const panic = std.debug.panic;
const assert = std.debug.assert;
const print = std.debug.print;

const Codegen = @import("../codegen.zig");
const DataBuffer = Codegen.DataBuffer;
const Import = Codegen.Import;
const TextSection = Codegen.TextSection;

const x86_64 = @import("x86_64/codegen.zig");

pub const file_alignment = 0x200;
pub const section_alignment = 0x1000;
const min_windows_version_vista = 6;

const NameType = [:0]const u8;

// @TODO: work around. We need to abstract this away from platforms

const DirectoryIndex = enum
{
    @"export",
    import,
    resource,
    exception,
    security,
    relocation,
    debug,
    architecture,
    global_pointer,
    TLS,
    load_config,
    bound,
    IAT,
    delay_import,
    CLR,
};

pub fn get_RVA(header: *ImageSectionHeader, section_buffer: []u8) u32
{
    return header.virtual_address + @intCast(u32, section_buffer.len);
}

const RDataSection = struct
{
    const Self = @This();

    buffer: []const u8,
    IAT: struct
    {
        RVA: u32,
        size: u32,
    },
    import_directory: struct
    {
        RVA: u32,
        size: u32,
    },

    fn encode(allocator: *Allocator, data_buffer: []const u8, import_libraries: []Import.Library, header: *ImageSectionHeader) Self
    {
        var expected_encoded_size: u64 = 0;

        const data_base_RVA = header.virtual_address;
        _ = data_base_RVA;

        for (import_libraries) |import_library|
        {
            expected_encoded_size += std.mem.alignForward(import_library.name.len + 1, 2);

            for (import_library.symbols) |import_symbol|
            {
                expected_encoded_size += @sizeOf(u16);
                expected_encoded_size += std.mem.alignForward(import_symbol.name.len + 1, 2);

                expected_encoded_size += @sizeOf(u64);

                expected_encoded_size += @sizeOf(u64);

                expected_encoded_size += @sizeOf(ImageImportDescriptor);
            }

            expected_encoded_size += @sizeOf(u64);
            expected_encoded_size += @sizeOf(u64);
            expected_encoded_size += @sizeOf(ImageImportDescriptor);
        }

        const global_data_size = std.mem.alignForward(data_buffer.len, 16);
        expected_encoded_size += global_data_size;

        var rdata_buffer = DataBuffer.initCapacity(allocator, expected_encoded_size) catch
        {
            panic("Error initializing the rdata section buffer\n", .{});
        };

        rdata_buffer.appendSlice(data_buffer) catch unreachable;

        for (import_libraries) |*import_library|
        {
            for (import_library.symbols) |*import_symbol|
            {
                import_symbol.name_RVA = get_RVA(header, rdata_buffer.items);
                rdata_buffer.appendSlice(std.mem.asBytes(&@intCast(i16, 0))) catch unreachable;
                const name_len = import_symbol.name.len;
                const name_size = name_len + 1;
                const aligned_name_size = std.mem.alignForward(name_size, 2);
                const remainding_size = aligned_name_size - name_len;

                rdata_buffer.appendSlice(import_symbol.name) catch panic("Error appending symbol to the rdata buffer\n", .{});
                rdata_buffer.appendNTimes(0, remainding_size) catch panic("Error appending zero padding\n", .{});
            }
        }

        const IAT_RVA = get_RVA(header, rdata_buffer.items);
        _ = IAT_RVA;

        for (import_libraries) |*import_library|
        {
            import_library.RVA = get_RVA(header, rdata_buffer.items);

            for (import_library.symbols) |*import_symbol|
            {
                const offset_in_data: u64 = get_RVA(header, rdata_buffer.items) - header.virtual_address;
                import_symbol.offset_in_data = @intCast(u32, offset_in_data);
                rdata_buffer.appendSlice(std.mem.asBytes(&offset_in_data)) catch panic("error appending offset in data\n", .{});
            }

            const zero: u64 = 0;
            rdata_buffer.appendSlice(std.mem.asBytes(&zero)) catch panic("error appending 8-byte zero padding\n", .{});
        }

        const IAT_size = @intCast(u32, rdata_buffer.items.len);

        // Image thunks
        for (import_libraries) |*import_library|
        {
            import_library.image_thunk_RVA = get_RVA(header, rdata_buffer.items);

            for (import_library.symbols) |*import_symbol|
            {
                const symbol_name_RVA: u64 = import_symbol.name_RVA;
                rdata_buffer.appendSlice(std.mem.asBytes(&symbol_name_RVA)) catch panic("error appending symbol name RVA\n", .{});
            }

            const zero: u64 = 0;
            rdata_buffer.appendSlice(std.mem.asBytes(&zero)) catch panic("error appending 8-byte zero padding\n", .{});
        }

        // Library names
        for (import_libraries) |*import_library|
        {
            import_library.name_RVA = get_RVA(header, rdata_buffer.items);
            const name_len = import_library.name.len;
            const name_size = name_len + 1;
            const aligned_name_size = std.mem.alignForward(name_size, 2);
            const remainding_size = aligned_name_size - name_len;

            rdata_buffer.appendSlice(import_library.name) catch panic("Error appending symbol to the rdata buffer\n", .{});
            rdata_buffer.appendNTimes(0, remainding_size) catch panic("Error appending zero padding\n", .{});
        }

        // Import directory
        const import_directory_RVA = get_RVA(header, rdata_buffer.items);

        for (import_libraries) |*import_library|
        {
            const image_import_descriptor = std.mem.zeroInit(ImageImportDescriptor, .
            {
                .characteristics_or_original_first_thunk = import_library.image_thunk_RVA,
                .name = import_library.name_RVA,
                .first_thunk = import_library.RVA,
            });

            rdata_buffer.appendSlice(std.mem.asBytes(&image_import_descriptor)) catch panic("error appending image_import_descriptor\n", .{});
        }

        const import_directory_size = get_RVA(header, rdata_buffer.items) - import_directory_RVA;
        _ = import_directory_size;

        const null_image_import_descrptor = std.mem.zeroes(ImageImportDescriptor);

        rdata_buffer.appendSlice(std.mem.asBytes(&null_image_import_descrptor)) catch panic("Error appending null image import descriptor\n", .{});

        const rdata_buffer_len = rdata_buffer.items.len;
        if (expected_encoded_size > 0)
        {
            assert(rdata_buffer_len <= expected_encoded_size);
            assert(rdata_buffer_len <= std.math.maxInt(i32));
        }

        header.misc.virtual_size = @intCast(u32, rdata_buffer_len);
        header.size_of_raw_data = @intCast(u32, std.mem.alignForward(rdata_buffer_len, file_alignment));

        return
        .{
            .buffer = rdata_buffer.items,
            .IAT = .{ .RVA = IAT_RVA, .size = IAT_size, },
            .import_directory = .{ .RVA = import_directory_RVA, .size = import_directory_size },
        };
    }
};

const image_sizeof_short_name = 8;

pub const ImageSectionHeader = extern struct
{
    name: [image_sizeof_short_name]u8,
    misc: extern union
    {
        physical_address: u32,
        virtual_size: u32,
    },
    virtual_address: u32,
    size_of_raw_data: u32,
    pointer_to_raw_data: u32,
    pointer_to_relocations: u32,
    pointer_to_line_numbers: u32,
    relocation_count: u16,
    line_number_count: u16,
    characteristics: u32,

    const Characteristics = enum(u32)
    {
        contains_code = 0x20,
        contains_initialized_data = 0x40,
        contains_uninitialized_data = 0x80,
        contains_link_information = 0x200,
        memory_discardable = 0x02000000,
        memory_not_cached = 0x04000000,
        memory_not_paged = 0x08000000,
        memory_shared = 0x10000000,
        memory_execute = 0x20000000,
        memory_read = 0x40000000,
        memory_write = 0x80000000,
    };
};

const ImageDOSHeader = extern struct
{
    magic_number: u16,
    bytes_last_page_of_file: u16,
    pages_in_file: u16,
    relocations: u16,
    size_of_header_in_paragraphs: u16,
    minimum_extra_paragraphs: u16,
    maximum_extra_paragraphs: u16,
    initial_ss_value: u16,
    initial_sp_value: u16,
    cheksum: u16,
    initial_ip_value: u16,
    initial_cs_value: u16,
    file_address_of_relocation_table: u16,
    overlay_number: u16,
    reserved_words: [4]u16,
    oem_id: u16,
    oem_info: u16,
    reserved_words2: [10]u16,
    file_address_of_new_exe_header: u32,
};

const ImageFileHeader = extern struct
{
    machine: u16,
    section_count: u16,
    time_date_stamp: u32,
    pointer_to_symbol_table: u32,
    symbol_count: u32,
    size_of_optional_header: u16,
    characteristics: u16,

    const Characteristics = enum(u16)
    {
        relocations_stripped = 0x0001,
        executable_image = 0x0002,
        stripped_line_count = 0x0004,
        stripped_local_symbols = 0x0008,
        aggressive_ws_trim = 0x0010,
        large_address_aware = 0x0020,
        bytes_reversed_lo = 0x0080,
        machine_32bit = 0x0100,
        stripped_debug = 0x0200,
        removable_run_from_swap = 0x0400,
        net_run_from_swap = 0x0800,
        system = 0x1000,
        dll = 0x2000,
        up_systems_only = 0x4000,
        bytes_reversed_hi = 0x8000,
    };
};

const ImageDataDirectory = extern struct
{
    virtual_address: u32,
    size: u32,
};

const ImageOptionalHeader = extern struct
{
    magic: u16,
    major_linker_version: u8,
    minor_linker_version: u8,
    size_of_code: u32,
    size_of_initialized_data: u32,
    size_of_uninitialized_data: u32,
    address_of_entry_point: u32,
    base_of_code: u32,
    image_base: u64,
    section_alignment: u32,
    file_alignment: u32,
    major_os_version: u16,
    minor_os_version: u16,
    major_image_version: u16,
    minor_image_version: u16,
    major_subsystem_version: u16,
    minor_subsystem_version: u16,
    win32_version_value: u32,
    size_of_image: u32,
    size_of_headers: u32,
    checksum: u32,
    subsystem: u16,
    dll_characteristics: u16,
    size_of_stack_reserve: u64,
    size_of_stack_commit: u64,
    size_of_heap_reserve: u64,
    size_of_heap_commit: u64,
    loader_flags: u32,
    number_of_RVA_and_sizes: u32,
    data_directory: [image_number_of_directory_entries]ImageDataDirectory,

    const magic = 0x20b;
};

const ImageImportDescriptor = extern struct
{
    characteristics_or_original_first_thunk: u32, // Win32 uses an union here but it is not needed
    time_date_stamp: u32,
    forwarder_chain: u32,
    name: u32,
    first_thunk: u32,
};

const ImageFileMachine = enum(u16)
{
    unknown           = 0,
    target_host       = 0x0001,  // Useful for indicating we want to interact with the host and not a WoW guest.
    i386              = 0x014c,  // Intel 386.
    r3000             = 0x0162,  // MIPS little-endian, 0x160 big-endian
    r4000             = 0x0166,  // MIPS little-endian
    r10000            = 0x0168,  // MIPS little-endian
    wcemipsv2         = 0x0169,  // MIPS little-endian WCE v2
    alpha             = 0x0184,  // Alpha_AXP
    sh3               = 0x01a2,  // SH3 little-endian
    sh3dsp            = 0x01a3,
    sh3e              = 0x01a4,  // SH3E little-endian
    sh4               = 0x01a6,  // SH4 little-endian
    sh5               = 0x01a8,  // SH5
    arm               = 0x01c0,  // ARM Little-Endian
    thumb             = 0x01c2,  // ARM Thumb/Thumb-2 Little-Endian
    armnt             = 0x01c4,  // ARM Thumb-2 Little-Endian
    am33              = 0x01d3,
    powerpc           = 0x01F0,  // IBM PowerPC Little-Endian
    powerpcfp         = 0x01f1,
    ia64              = 0x0200,  // Intel 64
    mips16            = 0x0266,  // MIPS
    alpha64           = 0x0284,  // ALPHA64
    mipsfpu           = 0x0366,  // MIPS
    mipsfpu16         = 0x0466,  // MIPS
    tricore           = 0x0520,  // Infineon
    cef               = 0x0CEF,
    ebc               = 0x0EBC,  // EFI Byte Code
    amd64             = 0x8664,  // AMD64 (K8)
    m32r              = 0x9041,  // M32R little-endian
    arm64             = 0xAA64,  // ARM64 Little-Endian
    cee               = 0xC0EE,

    const axp64 = ImageFileMachine.alpha64;
};

const ImageDLLCharacteristics = enum(u16)
{
    high_entropy_va = 0x0020,
    dynamic_base = 0x0040,
    force_integrity = 0x0080,
    nx_compat = 0x0100,
    no_isolation = 0x0200,
    no_seh = 0x0400,
    no_bind = 0x0800,
    app_container = 0x1000,
    wdm_driver = 0x2000,
    guard_cf = 0x4000,
    terminal_server_aware = 0x8000,
};

const text_section_name =  [image_sizeof_short_name]u8 {'.', 't', 'e', 'x', 't', 0, 0, 0};
const rdata_section_name = [image_sizeof_short_name]u8 {'.', 'r', 'd', 'a', 't', 'a', 0, 0};
const pdata_section_name = [image_sizeof_short_name]u8 {'.', 'p', 'd', 'a', 't', 'a', 0, 0};

const image_DOS_signature = 0x5A4D;
const image_NT_signature: u32 = 0x00004550;

const image_number_of_directory_entries = 0x10;

fn append_string(file_buffer: *DataBuffer, string: []const u8) void
{
    const len = string.len + 1;
    file_buffer.appendSlice(string) catch unreachable;
    file_buffer.append(0) catch unreachable;
    // if is not aligned to 2
    if (len & 1 != 0)
    {
        file_buffer.append(0) catch unreachable;
    }
}

pub fn write(allocator: *Allocator, executable: anytype, exe_name: []const u8, data_buffer: []const u8, import_libraries: []Import.Library, target: std.Target) void
{
    //var section_headers = [_]ImageSectionHeader
    //{
        //std.mem.zeroInit(ImageSectionHeader, .{
            //.name = rdata_section_name,
            //.characteristics = @enumToInt(ImageSectionHeader.Characteristics.contains_initialized_data) | @enumToInt(ImageSectionHeader.Characteristics.memory_read),
        //}),
        //std.mem.zeroInit(ImageSectionHeader, .{
            //.name = text_section_name,
            //.characteristics = @enumToInt(ImageSectionHeader.Characteristics.contains_code) | @enumToInt(ImageSectionHeader.Characteristics.memory_read) | @enumToInt(ImageSectionHeader.Characteristics.memory_execute),
        //}),
        //std.mem.zeroes(ImageSectionHeader),
    //};

    //comptime assert(section_headers.len == 3);

    //const file_size_of_headers = @intCast(u32, std.mem.alignForward(@sizeOf(ImageDOSHeader) + @sizeOf(i32) + @sizeOf(ImageFileHeader) + @sizeOf(ImageOptionalHeader) + @sizeOf(@TypeOf(section_headers)), file_alignment));
    //const virtual_size_of_headers = std.mem.alignForward(file_size_of_headers, section_alignment);

    //var rdata_section_header = &section_headers[0];
    //rdata_section_header.pointer_to_raw_data = @intCast(u32, file_size_of_headers);
    //rdata_section_header.virtual_address = @intCast(u32, virtual_size_of_headers);

    //const rdata_section = RDataSection.encode(allocator, data_buffer, import_libraries, rdata_section_header);
    //_ = rdata_section;

    //var text_section_header = &section_headers[1];
    //text_section_header.pointer_to_raw_data = rdata_section_header.pointer_to_raw_data + rdata_section_header.size_of_raw_data;
    //text_section_header.virtual_address = rdata_section_header.virtual_address + @intCast(u32, std.mem.alignForward(rdata_section_header.size_of_raw_data, section_alignment));

    //const text_section: TextSection = switch (target.cpu.arch)
    //{
        //.x86_64 => blk:
        //{
            //const exe = @ptrCast(*x86_64.Executable, executable);
            //break :blk exe.encode_text_section_pe32(allocator, text_section_header);
        //},
        //.aarch64 => unreachable,
        //else => unreachable,
    //};
    //_ = text_section;

    //const virtual_size_of_image = @intCast(u32, text_section_header.virtual_address + std.mem.alignForward(text_section_header.size_of_raw_data, section_alignment));
    //_ = virtual_size_of_image;
    //const max_file_buffer_size = file_size_of_headers + rdata_section_header.size_of_raw_data + text_section_header.size_of_raw_data;

    // @TODO: change this
    _ = import_libraries;
    _ = data_buffer;
    _ = executable;
    const max_file_buffer_size = 0x4000;

    var file_buffer = DataBuffer.initCapacity(allocator, max_file_buffer_size) catch panic("Error creating file buffer\n", .{});
    file_buffer.items.len = max_file_buffer_size;
    std.mem.set(u8, file_buffer.items[0..max_file_buffer_size], 0);
    file_buffer.items.len = 0;
    const minimal_exe = std.fs.cwd().readFileAlloc(allocator, "tests/minimal.exe", std.math.maxInt(u32)) catch unreachable;
    const minimal_exe_base_addr = @ptrToInt(minimal_exe.ptr);
    const minimal_image_dos_header = @intToPtr(*ImageDOSHeader, minimal_exe_base_addr);

    const image_dos_header = std.mem.zeroInit(ImageDOSHeader, .
    {
        .magic_number = image_DOS_signature,
        .file_address_of_new_exe_header = 0x40,
    });

    comptime assert(@sizeOf(ImageDOSHeader) == 0x40);
    
    print("{}\n", .{image_dos_header});
    print("{}\n", .{minimal_image_dos_header});

    file_buffer.appendSlice(std.mem.asBytes(&image_dos_header)) catch panic("Error appending image DOS header\n", .{});

    //const first_offset = 64;
    //const second_offset = 208;
    //const diff = second_offset - first_offset;
    //const first_slice = @intToPtr([*]u8, minimal_exe_base_addr + first_offset)[0..diff];
    //const second_slice = @intToPtr([*]u8, minimal_exe_base_addr + second_offset)[0..200];

    //const debug_chunk = [_]u8
    //{
        //0x0e, 0x1f, 0xba, 0x0e, 0x00, 0xb4, 0x09, 0xcd, 0x21, 0xb8, 0x01, 0x4c, 0xcd, 0x21, 0x54, 0x68, 0x69, 0x73, 0x20, 0x70, 0x72, 0x6f, 0x67, 0x72, 0x61, 0x6d, 0x20, 0x63, 0x61, 0x6e, 0x6e, 0x6f, 0x74, 0x20, 0x62, 0x65, 0x20, 0x72, 0x75, 0x6e, 0x20, 0x69, 0x6e, 0x20, 0x44, 0x4f, 0x53, 0x20, 0x6d, 0x6f, 0x64, 0x65, 0x2e, 0x0d, 0x0d, 0x0a, 0x24, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xd1, 0x02, 0x04, 0xc7, 0x95, 0x63, 0x6a, 0x94, 0x95, 0x63, 0x6a, 0x94, 0x95, 0x63, 0x6a, 0x94, 0x81, 0x08, 0x6b, 0x95, 0x96, 0x63, 0x6a, 0x94, 0x95, 0x63, 0x6b, 0x94, 0x94, 0x63, 0x6a, 0x94, 0x20, 0x16, 0x6e, 0x95, 0x94, 0x63, 0x6a, 0x94, 0x20, 0x16, 0x68, 0x95, 0x94, 0x63, 0x6a, 0x94, 0x52, 0x69, 0x63, 0x68, 0x95, 0x63, 0x6a, 0x94, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    //};

    //print("First slice:\n", .{});
    //for (first_slice) |b|
    //{
        //print("0x{x:0>2}, ", .{b});
    //}
    //print("\nSecond slice\n", .{});
    //for (second_slice) |b|
    //{
        //print("0x{x:0>2}, ", .{b});
    //}

    //print("Type: {}\n", .{@TypeOf(second_slice)});
    //

    //file_buffer.appendSlice(debug_chunk[0..]) catch panic("Error appending image debug chunk\n", .{});
    //print("Debug chunk size: {}\n. Offset: {}\n", .{@sizeOf(@TypeOf(debug_chunk)), file_buffer.items.len});
    file_buffer.appendSlice(std.mem.asBytes(&image_NT_signature))catch panic("Error appending image NT signature\n", .{});

    const image_file_header = std.mem.zeroInit(ImageFileHeader, .
        {
            .machine = @enumToInt(switch(target.cpu.arch)
                {
                    .x86_64 => ImageFileMachine.amd64,
                    .aarch64 => ImageFileMachine.arm64,
                    else => panic("ni: {}\n", .{target.cpu.arch}),
                }),
            .section_count = 2,
            // @TODO: correct timestamp
            .time_date_stamp = 0x612d5c23,
            .size_of_optional_header = @sizeOf(ImageOptionalHeader),
            .characteristics = @enumToInt(ImageFileHeader.Characteristics.executable_image) | @enumToInt(ImageFileHeader.Characteristics.large_address_aware),
        });

    file_buffer.appendSlice(std.mem.asBytes(&image_file_header)) catch panic("Error appending image file header\n", .{});

    print("\n{}\n", .{image_file_header});
    const correct_image_file_header = @intToPtr(*ImageFileHeader, minimal_exe_base_addr + 208);
    print("\n{}\n", .{correct_image_file_header});

    const optional_header_index = file_buffer.items.len;

    comptime assert(@sizeOf(ImageOptionalHeader) == 0xf0);
    file_buffer.appendSlice(std.mem.asBytes(&std.mem.zeroInit(ImageOptionalHeader, .
            {
                .magic = ImageOptionalHeader.magic,
                .major_linker_version = 0x0e,
                .minor_linker_version = 0x1d,
                .size_of_code = 0x200,
                .size_of_initialized_data = 0x400,
                .address_of_entry_point = 0x1000,
                .base_of_code = 0x1000,
                .image_base = 0x0000000140000000,
                .section_alignment = section_alignment,
                .file_alignment = file_alignment,
                .major_os_version = min_windows_version_vista,
                .major_subsystem_version = min_windows_version_vista,
                .size_of_image = 0x3000,
                .size_of_headers = 0,
                .checksum = 0,
                .subsystem = 3,
                .dll_characteristics = @enumToInt(ImageDLLCharacteristics.high_entropy_va) | @enumToInt(ImageDLLCharacteristics.nx_compat) | @enumToInt(ImageDLLCharacteristics.dynamic_base) | @enumToInt(ImageDLLCharacteristics.terminal_server_aware),
                .size_of_stack_reserve = 0x100000,
                .size_of_stack_commit = 0x1000,
                .size_of_heap_reserve = 0x100000,
                .size_of_heap_commit = 0x1000,
                .number_of_RVA_and_sizes = image_number_of_directory_entries,
            }))) catch panic("Error appending image optional header\n", .{});

    var image_optional_header = @intToPtr(*align(1) ImageOptionalHeader, @ptrToInt(file_buffer.items.ptr) + optional_header_index);
    image_optional_header.data_directory[@enumToInt(DirectoryIndex.import)].virtual_address = 0x2130;
    image_optional_header.data_directory[@enumToInt(DirectoryIndex.import)].size = 0x28;
    //image_optional_header.data_directory[@enumToInt(DirectoryIndex.exception)].virtual_address = 0x3000;
    //image_optional_header.data_directory[@enumToInt(DirectoryIndex.exception)].size = 0x0c;
    //image_optional_header.data_directory[@enumToInt(DirectoryIndex.debug)].virtual_address = 0x2010;
    //image_optional_header.data_directory[@enumToInt(DirectoryIndex.debug)].size = 0x1c;
    image_optional_header.data_directory[@enumToInt(DirectoryIndex.IAT)].virtual_address = 0x2000;
    image_optional_header.data_directory[@enumToInt(DirectoryIndex.IAT)].size = 0x10;


    //const correct_image_optional_header = @intToPtr(*ImageOptionalHeader, minimal_exe_base_addr + 208 + @sizeOf(ImageFileHeader));

    print("\n{}\n", .{image_optional_header});
    //print("\n{}\n", .{correct_image_optional_header});

    const text_section_header_size_of_raw_data = image_optional_header.size_of_code;
    const text_section_index = file_buffer.items.len;

    file_buffer.appendSlice(std.mem.asBytes(&std.mem.zeroInit(ImageSectionHeader, .
                {
                    .name = text_section_name,
                    .misc = .{ .virtual_size = 0x0d },
                    .virtual_address = image_optional_header.base_of_code,
                    .size_of_raw_data = text_section_header_size_of_raw_data,
                    .pointer_to_raw_data = 0,
                    .characteristics = @enumToInt(ImageSectionHeader.Characteristics.contains_code) | @enumToInt(ImageSectionHeader.Characteristics.memory_read) | @enumToInt(ImageSectionHeader.Characteristics.memory_execute),
                }))) catch unreachable;

    const rdata_section_size_of_raw_data = 0x200;
    const rdata_section_index = file_buffer.items.len;

    file_buffer.appendSlice(std.mem.asBytes(&std.mem.zeroInit(ImageSectionHeader, .
                {
                    .name = rdata_section_name,
                    .misc = .{ .virtual_size = 0x184 },
                    .virtual_address = 0x2000,
                    .size_of_raw_data = rdata_section_size_of_raw_data,
                    .pointer_to_raw_data = 0,
                    .characteristics = @enumToInt(ImageSectionHeader.Characteristics.contains_initialized_data) | @enumToInt(ImageSectionHeader.Characteristics.memory_read),
                }))) catch unreachable;

    file_buffer.appendSlice(std.mem.asBytes(&std.mem.zeroes(ImageSectionHeader))) catch unreachable;

    const size_of_headers = @intCast(u32, std.mem.alignForward(file_buffer.items.len, image_optional_header.file_alignment));
    file_buffer.items.len = size_of_headers;
    image_optional_header.size_of_headers = size_of_headers;

    const text_section_header = @intToPtr(*ImageSectionHeader, @ptrToInt(file_buffer.items.ptr) + text_section_index);
    const rdata_section_header = @intToPtr(*ImageSectionHeader, @ptrToInt(file_buffer.items.ptr) + rdata_section_index);

    const sections = [_]*ImageSectionHeader
    {
        text_section_header,
        rdata_section_header,
    };

    var section_offset = size_of_headers;
    for (sections) |section|
    {
        section.pointer_to_raw_data = section_offset;
        section_offset += section.size_of_raw_data;
    }

    //const pdata_section_size_of_raw_data = 0x200;
    //const pdata_section_header_pointer_to_raw_data = section_offset;
    //file_buffer.appendSlice(std.mem.asBytes(&std.mem.zeroInit(ImageSectionHeader, .
                //{
                    //.name = pdata_section_name,
                    //.misc = .{ .virtual_size = 0x0c },
                    //.virtual_address = 0x3000,
                    //.size_of_raw_data = pdata_section_size_of_raw_data,
                    //.pointer_to_raw_data = pdata_section_header_pointer_to_raw_data,
                    //.characteristics = @enumToInt(ImageSectionHeader.Characteristics.contains_initialized_data) | @enumToInt(ImageSectionHeader.Characteristics.memory_read),
                //}))) catch unreachable;

    //section_offset += pdata_section_size_of_raw_data;

    // null section

    //for (section_headers) |*section_header|
    //{
        //file_buffer.appendSlice(std.mem.asBytes(section_header)) catch panic("Error appending section header\n", .{});
    //}

    // .text section
    file_buffer.items.len = text_section_header.pointer_to_raw_data;
    print("Code being appended at {}\n", .{file_buffer.items.len});

    const code = [_]u8
    {
        // sub rsp
        0x48, 0x83, 0xEC, 0x28, 0x33, 0xc9, 0xff, 0x15, 0xf4,
        // call
        0x0f, 0x00, 0x00, 0xcc
    };

    file_buffer.appendSlice(code[0..]) catch panic("Error appending text section\n", .{});
    print("Code ends at {}\n", .{file_buffer.items.len});

    // .rdata section
    file_buffer.items.len = rdata_section_header.pointer_to_raw_data;
    print("Appending IAT RVA at {}\n", .{file_buffer.items.len});
    const iat_rva: u32 = 0x2168;
    file_buffer.appendSlice(std.mem.asBytes(&iat_rva)) catch unreachable;

    file_buffer.items.len = rdata_section_header.pointer_to_raw_data + 0x10;
    print("Debug bytes being appended at {}\n", .{file_buffer.items.len});
    const debug_bytes = [_]u8
    {
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x18, 0x00, 0x00, 0x00, 0x01, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x44, 0x20, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x0d, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x0d, 0x00, 0x00, 0x00, 0x2e, 0x74, 0x65, 0x78, 0x74, 0x24, 0x6d, 0x6e, 0x00, 0x00, 0x00, 0x00, 0x00, 0x20, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x2e, 0x69, 0x64, 0x61, 0x74, 0x61, 0x24, 0x35, 0x00, 0x00, 0x00, 0x00, 0x10, 0x20, 0x00, 0x00, 0x1c, 0x00, 0x00, 0x00, 0x2e, 0x72, 0x64, 0x61, 0x74, 0x61, 0x00, 0x00, 0x2c, 0x20, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00, 0x2e, 0x72, 0x64, 0x61, 0x74, 0x61, 0x24, 0x76, 0x6f, 0x6c, 0x74, 0x6d, 0x64, 0x00, 0x00, 0x00, 0x4c, 0x20, 0x00, 0x00, 0xdc, 0x00, 0x00, 0x00, 0x2e, 0x72, 0x64, 0x61, 0x74, 0x61, 0x24, 0x7a, 0x7a, 0x7a, 0x64, 0x62, 0x67, 0x00, 0x00, 0x00, 0x28, 0x21, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x2e, 0x78, 0x64, 0x61, 0x74, 0x61, 0x00, 0x00, 0x30, 0x21, 0x00, 0x00, 0x14, 0x00, 0x00, 0x00, 0x2e, 0x69, 0x64, 0x61, 0x74, 0x61, 0x24, 0x32, 0x00, 0x00, 0x00, 0x00, 0x44, 0x21, 0x00, 0x00, 0x14, 0x00, 0x00, 0x00, 0x2e, 0x69, 0x64, 0x61, 0x74, 0x61, 0x24, 0x33, 0x00, 0x00, 0x00, 0x00, 0x58, 0x21, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x2e, 0x69, 0x64, 0x61, 0x74, 0x61, 0x24, 0x34, 0x00, 0x00, 0x00, 0x00, 0x68, 0x21, 0x00, 0x00, 0x1c, 0x00, 0x00, 0x00, 0x2e, 0x69, 0x64, 0x61, 0x74, 0x61, 0x24, 0x36, 0x00, 0x00, 0x00, 0x00, 0x00, 0x30, 0x00, 0x00, 0x0c, 0x00, 0x00, 0x00, 0x2e, 0x70, 0x64, 0x61, 0x74, 0x61, 0x00, 0x00, 0x01, 0x04, 0x01, 0x00, 0x04, 0x42, 0x00, 0x00, 0x58, 0x21, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x76, 0x21, 0x00, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x68, 0x21, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x67, 0x01, 0x45, 0x78, 0x69, 0x74, 0x50, 0x72, 0x6f, 0x63, 0x65, 0x73, 0x73, 0x00, 0x4b, 0x45, 0x52, 0x4e, 0x45, 0x4c, 0x33, 0x32, 0x2e, 0x64, 0x6c, 0x6c,
    };

    file_buffer.appendSlice(debug_bytes[0..]) catch panic("Error appending text section\n", .{});
    print("Debug bytes end at [{}]\n", .{file_buffer.items.len});

    // .idata section
    file_buffer.items.len = rdata_section_header.pointer_to_raw_data + 0xf8;

    print("Image import descriptor appended at {}\n", .{file_buffer.items.len});
    file_buffer.appendSlice(std.mem.asBytes(&std.mem.zeroInit(ImageImportDescriptor, .{
        .characteristics_or_original_first_thunk = 0x6164692e,
        .time_date_stamp = 0x34246174,
        .name = 0x2168,
        .first_thunk = 0x1c,
    }))) catch unreachable;

    file_buffer.items.len = rdata_section_header.pointer_to_raw_data + 0x120;
    print("Random number 1 being generated at [{}]\n", .{file_buffer.items.len});
    file_buffer.appendSlice(std.mem.asBytes(&@intCast(u64, 0x61746164702e))) catch unreachable;

    file_buffer.items.len = rdata_section_header.pointer_to_raw_data + 0x130;
    print("Random number 2 being generated at [{}]\n", .{file_buffer.items.len});
    file_buffer.appendSlice(std.mem.asBytes(&@intCast(u16, 0x2158))) catch unreachable;

    //const function_name = "ExitProcess";
    //append_string(&file_buffer, function_name);

    //file_buffer.items.len = rdata_section_header.pointer_to_raw_data + 0x13e;

    //const library_name = "KERNEL32.DLL";
    //append_string(&file_buffer, library_name);

    //file_buffer.items.len = pdata_section_header_pointer_to_raw_data;
    //print("Pdata section at [{}]\n", .{file_buffer.items.len});
    //const pdata_section = [_]u8
    //{  
        //0x00,
        //0x10,
        //0x00,
        //0x00,
        //0x0d,
        //0x10,
        //0x00,
        //0x00,
        //0x28,
        //0x21,
    //};

    //file_buffer.appendSlice(pdata_section[0..]) catch unreachable;
    //print("Pdata section ends at [{}]\n", .{file_buffer.items.len});
    
    file_buffer.items.len = rdata_section_header.pointer_to_raw_data + rdata_section_size_of_raw_data;

    var error_count: u32 = 0;
    for (file_buffer.items) |b, i|
    {
        //if (error_count == 45) break;

        if (b != minimal_exe[i])
        {
            print("[{}] Diff. Minimal: {x:0>2}\tMine: {x:0>2}\n", .{i, minimal_exe[i], b});
            error_count += 1;
        }
    }

    print("Mine length: {}. Minimal length: {}\n", .{file_buffer.items.len, minimal_exe.len});


    //file_buffer.items.len = rdata_section_header.pointer_to_raw_data;
    //print("Rdata being encoded at {}\n", .{file_buffer.items.len});
    //file_buffer.appendSlice(rdata_section.buffer) catch panic("Error appending rdata section\n", .{});

    //// useless statement
    //file_buffer.items.len = rdata_section_header.pointer_to_raw_data + rdata_section_header.size_of_raw_data;



    //print("Minimal executable\n", .{});

    //const mine_len = file_buffer.items.len;
    //var index: u64 = 0;
    //while (index < mine_len) : (index += 30)
    //{
    //    const top = index + 30;
    //    print("\n{:0>2}:\t", .{index});
    //    for (minimal_exe[index..top]) |b|
    //    {
    //        print("{x:0>2}|", .{b});
    //    }

    //    print("\n{:0>2}:\t", .{index});
    //    for (file_buffer.items[index..top]) |b|
    //    {
    //        print("{x:0>2}|", .{b});
    //    }
    //    print("\n", .{});
    //}
    //
    //for (file_buffer.items) |b, i|
    //{
    //    if (b != minimal_exe[i])
    //    {
    //        print("Diff at [{}] [MINE] {x:0>2}; [MINIMAL] {x:0>2}\n", .{i, b, minimal_exe[i]});
    //    }
    //}

    //print("Mine: {}. Minimal: {}\n", .{file_buffer.items.len, minimal_exe.len});

    print("Size of DOS header: {}\n", .{@sizeOf(@TypeOf(image_dos_header))});
    Codegen.write_executable(exe_name, file_buffer.items);
}
