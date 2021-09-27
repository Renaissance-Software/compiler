const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const panic = std.debug.panic;
const assert = std.debug.assert;
const print = std.debug.print;
const alignForward = std.mem.alignForward;

const Codegen = @import("../codegen.zig");
const CodeBuffer = Codegen.CodeBuffer;
const DataBuffer = Codegen.DataBuffer;
const Import = Codegen.Import;

const BucketArray = @import("../bucket_array.zig").BucketArray;

const x86_64 = @import("x86_64/codegen.zig");
const x86_64v2 = @import("x86_64/codegenv2.zig");

pub const file_alignment = 0x200;
pub const section_alignment = 0x1000;
const min_windows_version_vista = 6;

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

pub fn get_RVA(header: ImageSectionHeader, section_buffer_len: usize) u32
{
    return header.virtual_address + @intCast(u32, section_buffer_len);
}

//const RDataSection = struct
//{
    //const Self = @This();

    //buffer: []const u8,
    //IAT: struct
    //{
        //RVA: u32,
        //size: u32,
    //},
    //import_directory: struct
    //{
        //RVA: u32,
        //size: u32,
    //},

    //fn encode(allocator: *Allocator, data_buffer: []const u8, import_libraries: []Import.Library, header: *ImageSectionHeader) Self
    //{
        //var expected_encoded_size: u64 = 0;

        //const data_base_RVA = header.virtual_address;
        //_ = data_base_RVA;

        //for (import_libraries) |import_library|
        //{
            //expected_encoded_size += alignForward(import_library.name.len + 1, 2);

            //for (import_library.symbols) |import_symbol|
            //{
                //expected_encoded_size += @sizeOf(u16);
                //expected_encoded_size += alignForward(import_symbol.name.len + 1, 2);

                //expected_encoded_size += @sizeOf(u64);

                //expected_encoded_size += @sizeOf(u64);

                //expected_encoded_size += @sizeOf(ImageImportDescriptor);
            //}

            //expected_encoded_size += @sizeOf(u64);
            //expected_encoded_size += @sizeOf(u64);
            //expected_encoded_size += @sizeOf(ImageImportDescriptor);
        //}

        //const global_data_size = alignForward(data_buffer.len, 16);
        //expected_encoded_size += global_data_size;

        //var rdata_buffer = DataBuffer.initCapacity(allocator, expected_encoded_size) catch
        //{
            //panic("Error initializing the rdata section buffer\n", .{});
        //};

        //rdata_buffer.appendSlice(data_buffer) catch unreachable;

        //for (import_libraries) |*import_library|
        //{
            //for (import_library.symbols) |*import_symbol|
            //{
                //import_symbol.name_RVA = get_RVA(header, rdata_buffer.items);
                //rdata_buffer.appendSlice(std.mem.asBytes(&@intCast(i16, 0))) catch unreachable;
                //const name_len = import_symbol.name.len;
                //const name_size = name_len + 1;
                //const aligned_name_size = alignForward(name_size, 2);
                //const remainding_size = aligned_name_size - name_len;

                //rdata_buffer.appendSlice(import_symbol.name) catch panic("Error appending symbol to the rdata buffer\n", .{});
                //rdata_buffer.appendNTimes(0, remainding_size) catch panic("Error appending zero padding\n", .{});
            //}
        //}

        //const IAT_RVA = get_RVA(header, rdata_buffer.items);
        //_ = IAT_RVA;

        //for (import_libraries) |*import_library|
        //{
            //import_library.RVA = get_RVA(header, rdata_buffer.items);

            //for (import_library.symbols) |*import_symbol|
            //{
                //const offset_in_data: u64 = get_RVA(header, rdata_buffer.items) - header.virtual_address;
                //import_symbol.offset_in_data = @intCast(u32, offset_in_data);
                //rdata_buffer.appendSlice(std.mem.asBytes(&offset_in_data)) catch panic("error appending offset in data\n", .{});
            //}

            //const zero: u64 = 0;
            //rdata_buffer.appendSlice(std.mem.asBytes(&zero)) catch panic("error appending 8-byte zero padding\n", .{});
        //}

        //const IAT_size = @intCast(u32, rdata_buffer.items.len);

        //// Image thunks
        //for (import_libraries) |*import_library|
        //{
            //import_library.image_thunk_RVA = get_RVA(header, rdata_buffer.items);

            //for (import_library.symbols) |*import_symbol|
            //{
                //const symbol_name_RVA: u64 = import_symbol.name_RVA;
                //rdata_buffer.appendSlice(std.mem.asBytes(&symbol_name_RVA)) catch panic("error appending symbol name RVA\n", .{});
            //}

            //const zero: u64 = 0;
            //rdata_buffer.appendSlice(std.mem.asBytes(&zero)) catch panic("error appending 8-byte zero padding\n", .{});
        //}

        //// Library names
        //for (import_libraries) |*import_library|
        //{
            //import_library.name_RVA = get_RVA(header, rdata_buffer.items);
            //const name_len = import_library.name.len;
            //const name_size = name_len + 1;
            //const aligned_name_size = alignForward(name_size, 2);
            //const remainding_size = aligned_name_size - name_len;

            //rdata_buffer.appendSlice(import_library.name) catch panic("Error appending symbol to the rdata buffer\n", .{});
            //rdata_buffer.appendNTimes(0, remainding_size) catch panic("Error appending zero padding\n", .{});
        //}

        //// Import directory
        //const import_directory_RVA = get_RVA(header, rdata_buffer.items);

        //for (import_libraries) |*import_library|
        //{
            //const image_import_descriptor = std.mem.zeroInit(ImageImportDescriptor, .
            //{
                //.characteristics_or_original_first_thunk = import_library.image_thunk_RVA,
                //.name = import_library.name_RVA,
                //.first_thunk = import_library.RVA,
            //});

            //rdata_buffer.appendSlice(std.mem.asBytes(&image_import_descriptor)) catch panic("error appending image_import_descriptor\n", .{});
        //}

        //const import_directory_size = get_RVA(header, rdata_buffer.items) - import_directory_RVA;
        //_ = import_directory_size;

        //const null_image_import_descrptor = std.mem.zeroes(ImageImportDescriptor);

        //rdata_buffer.appendSlice(std.mem.asBytes(&null_image_import_descrptor)) catch panic("Error appending null image import descriptor\n", .{});

        //const rdata_buffer_len = rdata_buffer.items.len;
        //if (expected_encoded_size > 0)
        //{
            //assert(rdata_buffer_len <= expected_encoded_size);
            //assert(rdata_buffer_len <= std.math.maxInt(i32));
        //}

        //header.misc.virtual_size = @intCast(u32, rdata_buffer_len);
        //header.size_of_raw_data = @intCast(u32, alignForward(rdata_buffer_len, file_alignment));

        //return
        //.{
            //.buffer = rdata_buffer.items,
            //.IAT = .{ .RVA = IAT_RVA, .size = IAT_size, },
            //.import_directory = .{ .RVA = import_directory_RVA, .size = import_directory_size },
        //};
    //}
//};

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
    data_directory: [image_number_of_directory_entries]Section.Directory,

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
const data_section_name = [image_sizeof_short_name]u8 {'.', 'd', 'a', 't', 'a', 0, 0, 0};

const image_DOS_signature = 0x5A4D;
const image_NT_signature: u32 = 0x00004550;

const image_number_of_directory_entries = 0x10;

pub const Offset = struct
{
    file: u32,
    virtual: u32,

    const Self = @This();

    fn new(size: u32) Self
    {
        return
        .{
            .file = @intCast(u32, alignForward(size, file_alignment)),
            .virtual = @intCast(u32, alignForward(size, section_alignment)),
        };
    }

    pub fn after_size(self: *Self, size: u32) void
    {
        self.* =
        .{
            .file = self.file + @intCast(u32, alignForward(size, file_alignment)),
            .virtual = self.virtual + @intCast(u32, alignForward(size, section_alignment)),
        };
    }
};

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

fn file_rva(section_header: *ImageSectionHeader, file_len: usize) u32
{
    return @intCast(u32, file_len) - section_header.pointer_to_raw_data + section_header.virtual_address;
}

fn get_pointer(comptime T: type, file_buffer: []u8, index: usize) *align(1) T
{
    return @intToPtr(*align(1) T, @ptrToInt(file_buffer.ptr) + index);
}

const TextSection = struct
{
    fn encode_old(allocator: *Allocator, header: *ImageSectionHeader, offset: *Offset) Section
    {
        var text = Section
        {
            .header = header.*,
            .buffer = CodeBuffer.init(allocator),
            .base_RVA = header.virtual_address,
            .name = ".text",
            .permissions = @enumToInt(Section.Permission.execute) | @enumToInt(Section.Permission.read),
        };
        text.header.pointer_to_raw_data = offset.file;
        text.header.virtual_address = offset.virtual;

        const code = [_]u8
        {
            0x48, 0x83, 0xEC, 0x28, 0xC7, 0xC1, 0x2A, 0x00, 0x00, 0x00, 0x48, 0xFF, 0x15, 0xFD, 0x0F, 0x00, 0x00, 0x48, 0x83, 0xC4, 0x28, 0xC3, 0x48, 0x83, 0xEC, 0x00, 0xE9, 0xE1, 0xFF, 0xFF, 0xFF, 0x48, 0x83, 0xC4, 0x00, 0xC3
        };

        text.buffer.appendSlice(code[0..]) catch unreachable;

        const code_size = @intCast(u32, text.buffer.items.len);
        text.header.misc.virtual_size = code_size;
        text.header.size_of_raw_data = @intCast(u32, alignForward(code_size, file_alignment));

        offset.after_size(text.header.size_of_raw_data);

        return text;
    }

    fn encode(executable: anytype, allocator: *Allocator, header: *ImageSectionHeader, patches: *ArrayList(Patch), arch: std.Target.Cpu.Arch, offset: *Offset) Section
    {
        _ = header;
        _ = executable;
        _ = allocator;

        switch (arch)
        {
            .x86_64 =>
            {
                var x86_64_program = @ptrCast(*x86_64v2.Program, executable);
                return x86_64_program.encode_text_section_pe(allocator, header, patches, offset);
            },
            else => panic("CPU arch: {}\n", .{arch}),
        }

    }
};

const ImportLibrary = struct
{
    name_RVA: u32,
    RVA: u32,
    image_thunk_RVA: u32,
    symbol_RVAs: []u32,
    symbol_offsets: []u32,
};

const RDataSection = struct
{
    section: Section,
    IAT: Section.Directory,
    import_directory: Section.Directory,
    libraries: []ImportLibrary,

    fn encode(allocator: *Allocator, header: *ImageSectionHeader, import_libraries: []Import.Library, offset: *Offset) RDataSection
    {
        header.pointer_to_raw_data = offset.file;
        header.virtual_address = offset.virtual;

        var rdata = Section
        {
            .header = header.*,
            .buffer = DataBuffer.init(allocator),
        };

        var pe32_libraries = ArrayList(ImportLibrary).initCapacity(allocator, import_libraries.len) catch unreachable;

        for (import_libraries) |*library|
        {
            const symbol_count = library.symbols.items.len;
            var symbol_RVAs = ArrayList(u32).initCapacity(allocator, symbol_count) catch unreachable;

            for (library.symbols.items) |*symbol|
            {
                std.debug.print("RData offset: {}\n", .{rdata.buffer.items.len});
                const symbol_rva = get_RVA(rdata.header, rdata.buffer.items.len);
                symbol_RVAs.appendAssumeCapacity(symbol_rva);
                rdata.buffer.appendSlice(std.mem.asBytes(&@intCast(u16, 0))) catch unreachable;
                append_string(&rdata.buffer, symbol.name);
            }

            pe32_libraries.append(ImportLibrary
                {
                    .name_RVA = 0,
                    .RVA = 0,
                    .image_thunk_RVA = 0,
                    .symbol_RVAs = symbol_RVAs.items,
                    .symbol_offsets = blk:
                    {
                        var symbol_offsets = ArrayList(u32).initCapacity(allocator, symbol_count) catch unreachable;
                        symbol_offsets.resize(symbol_count) catch unreachable;
                        break :blk symbol_offsets.items;
                    },
                }) catch unreachable;
        }

        // IAT list
        const IAT_RVA = get_RVA(rdata.header, rdata.buffer.items.len);

        for (pe32_libraries.items) |*pe32_lib|
        {
            pe32_lib.RVA = get_RVA(rdata.header, rdata.buffer.items.len);

            for (pe32_lib.symbol_RVAs) |symbol_rva, symbol_i|
            {
                std.debug.print("RData offset: {}\n", .{rdata.buffer.items.len});
                const symbol_offset = get_RVA(rdata.header, rdata.buffer.items.len) - rdata.header.virtual_address;
                pe32_lib.symbol_offsets[symbol_i] = symbol_offset;

                rdata.buffer.appendSlice(std.mem.asBytes(&@intCast(u64, symbol_rva))) catch unreachable;
            }

            rdata.buffer.appendSlice(std.mem.asBytes(&@intCast(u64, 0))) catch unreachable;
        }

        const IAT_size = @intCast(u32, rdata.buffer.items.len);

        // Image thunks
        for (pe32_libraries.items) |*pe32_lib|
        {
            pe32_lib.image_thunk_RVA = get_RVA(rdata.header, rdata.buffer.items.len);

            for (pe32_lib.symbol_RVAs) |symbol_RVA|
            {
                rdata.buffer.appendSlice(std.mem.asBytes(&@intCast(u64, symbol_RVA))) catch unreachable;
            }

            rdata.buffer.appendSlice(std.mem.asBytes(&@intCast(u64, 0))) catch unreachable;
        }

        // Library names
        for (import_libraries) |*library, library_i|
        {
            var pe32_lib = &pe32_libraries.items[library_i];
            pe32_lib.name_RVA = get_RVA(rdata.header, rdata.buffer.items.len);

            append_string(&rdata.buffer, library.name);
        }

        // Import directory
        const import_directory_RVA = get_RVA(rdata.header, rdata.buffer.items.len);

        for (pe32_libraries.items) |pe32_lib|
        {
            rdata.buffer.appendSlice(std.mem.asBytes(&std.mem.zeroInit(ImageImportDescriptor, .
                        {
                            .characteristics_or_original_first_thunk = pe32_lib.image_thunk_RVA,
                            .name = pe32_lib.name_RVA,
                            .first_thunk = pe32_lib.RVA,
                        }))) catch unreachable;
        }

        const import_directory_size = get_RVA(rdata.header, rdata.buffer.items.len) - import_directory_RVA;

        if (import_directory_size > 0)
        {
            rdata.buffer.appendSlice(std.mem.asBytes(&std.mem.zeroes(ImageImportDescriptor))) catch unreachable;
        }

        // @TODO: @INFO: Exception directory: dont code for now

        rdata.header.misc.virtual_size = @intCast(u32, rdata.buffer.items.len);
        rdata.header.size_of_raw_data = @intCast(u32, alignForward(rdata.buffer.items.len, file_alignment));
        offset.after_size(rdata.header.size_of_raw_data);

        return .
        {
            .section = rdata,
            .IAT = .{ .RVA = IAT_RVA, .size = IAT_size, },
            .import_directory = .{ .RVA = import_directory_RVA, .size = import_directory_size, },
            .libraries = pe32_libraries.items,
        };
    }
};

const DataSection = struct
{
    fn encode(allocator: *Allocator, header: *ImageSectionHeader, offset: *Offset) Section
    {
        var data = Section
        {
            .header = header.*,
            .buffer = DataBuffer.init(allocator),
        };
        data.header.pointer_to_raw_data = offset.file;
        data.header.virtual_address = offset.virtual;

        // dont hardcode this
        data.buffer.append(0) catch unreachable;
        
        data.header.misc.virtual_size = @intCast(u32, data.buffer.items.len);
        data.header.size_of_raw_data = @intCast(u32, alignForward(data.buffer.items.len, file_alignment));
        offset.after_size(data.header.size_of_raw_data);

        return data;
    }
};

pub const Section = struct
{
    header: ImageSectionHeader,
    buffer: DataBuffer,

    const count = std.enums.values(Index).len;

    const Index = enum(u8)
    {
        @".text",
        @".rdata",
        @".data",
    };

    const NameType = [image_sizeof_short_name]u8;
    const names = blk:
    {
        var section_names: [Section.count]NameType = undefined;
        
        section_names[@enumToInt(Section.Index.@".text")] = NameType { '.', 't', 'e', 'x', 't',   0, 0, 0 };
        section_names[@enumToInt(Section.Index.@".rdata")] = NameType { '.', 'r', 'd', 'a', 't', 'a', 0, 0 };
        section_names[@enumToInt(Section.Index.@".data")] = NameType { '.', 'd', 'a', 't', 'a', 0, 0, 0 };
        break :blk section_names;
    };

    const characteristics = blk:
    {
        var section_characteristics: [Section.count]u32 = [_]u32{0} ** Section.count;

        section_characteristics[@enumToInt(Section.Index.@".text")] = @enumToInt(ImageSectionHeader.Characteristics.contains_code) | @enumToInt(ImageSectionHeader.Characteristics.memory_read) | @enumToInt(ImageSectionHeader.Characteristics.memory_execute);
        section_characteristics[@enumToInt(Section.Index.@".rdata")] = @enumToInt(ImageSectionHeader.Characteristics.contains_initialized_data) | @enumToInt(ImageSectionHeader.Characteristics.memory_read);
        section_characteristics[@enumToInt(Section.Index.@".data")] = @enumToInt(ImageSectionHeader.Characteristics.contains_initialized_data) | @enumToInt(ImageSectionHeader.Characteristics.memory_read) | @enumToInt(ImageSectionHeader.Characteristics.memory_write);

        break :blk section_characteristics;
    };

    pub const Permission = enum(u8)
    {
        read = 1,
        write = 2,
        execute = 4,
    };

    pub const Directory = extern struct
    {
        RVA: u32,
        size: u32,
    };
};

pub const Patch = struct
{
    section_buffer_index_to: u32,
    section_buffer_index_from: u32,
    section_to_write_to: Section.Index,
    section_to_read_from: Section.Index,
};

pub fn write(allocator: *Allocator, executable: anytype, exe_name: []const u8, data_buffer: []const u8, import_libraries: []Import.Library, target: std.Target) void
{
    _ = data_buffer;
    var patches = ArrayList(Patch).init(allocator);
    
    var section_headers: [Section.count + 1]ImageSectionHeader = undefined;
    for (section_headers[0..Section.count]) |*sh, sh_i|
    {
        sh.* = std.mem.zeroInit(ImageSectionHeader, .
            {
                .name = Section.names[sh_i],
                .characteristics = Section.characteristics[sh_i],
            });
    }

    section_headers[Section.count] = std.mem.zeroes(ImageSectionHeader);
    
    const file_size_of_headers = @intCast(u32, alignForward(@sizeOf(ImageDOSHeader) + @sizeOf(@TypeOf(image_NT_signature)) + @sizeOf(ImageFileHeader) + @sizeOf(ImageOptionalHeader) + @sizeOf(@TypeOf(section_headers)), file_alignment));

    var offset = Offset.new(file_size_of_headers);

    //const text = TextSection.encode_old(allocator, text_section_header, &offset);
    const text = TextSection.encode(executable, allocator, &section_headers[@enumToInt(Section.Index.@".text")], &patches, target.cpu.arch, &offset);
    const rdata = RDataSection.encode(allocator, &section_headers[@enumToInt(Section.Index.@".rdata")], import_libraries, &offset);
    const data = DataSection.encode(allocator, &section_headers[@enumToInt(Section.Index.@".data")], &offset);

    var sections = [Section.count]Section
    {
        text,
        rdata.section,
        data,
    };

    // program patch labels
    for (patches.items) |patch|
    {
        // assuming all patches are 4 bytes
        const section_to_patch = &sections[@enumToInt(patch.section_to_write_to)];
        const jump_from = patch.section_buffer_index_to + 4;
        const jump_from_RVA = section_to_patch.header.virtual_address + jump_from;
        var patch_address = @ptrCast(*align(1) i32, &section_to_patch.buffer.items[patch.section_buffer_index_to]);
        const jump_to_RVA = switch (patch.section_to_read_from)
        {
            .@".rdata" => blk:
            {
                const library_index = @truncate(u16, (patch.section_buffer_index_from & 0xffff0000) >> 16);
                const function_index = @truncate(u16, patch.section_buffer_index_from);
                std.debug.print("Library index: {}. Function index: {}\n", .{library_index, function_index});
                const symbol_offset = rdata.libraries[library_index].symbol_offsets[function_index];
                const symbol_RVA = rdata.section.header.virtual_address + symbol_offset;
                break :blk symbol_RVA;
            },
            else => panic("Not implemented: {}\n", .{patch.section_to_read_from}),
        };
        const relative = @intCast(i32, @intCast(i64, jump_to_RVA) - @intCast(i64, jump_from_RVA));
        patch_address.* = relative;
    }

    const virtual_size_of_image = offset.virtual;

    const max_executable_buffer_size = offset.file;

    var exe_buffer = DataBuffer.initCapacity(allocator, max_executable_buffer_size) catch unreachable;
    exe_buffer.appendSlice(std.mem.asBytes(&std.mem.zeroInit(ImageDOSHeader,.
                {
                    .magic_number = image_DOS_signature,
                    .file_address_of_new_exe_header = @sizeOf(ImageDOSHeader),
                }))) catch unreachable;

    exe_buffer.appendSlice(std.mem.asBytes(&image_NT_signature)) catch unreachable;

    exe_buffer.appendSlice(std.mem.asBytes(&std.mem.zeroInit(ImageFileHeader, .
                {
                    .machine = @enumToInt(switch(target.cpu.arch)
                        {
                            .x86_64 => ImageFileMachine.amd64,
                            .aarch64 => ImageFileMachine.arm64,
                            else => panic("ni: {}\n", .{target.cpu.arch}),
                            }),
                    .section_count = section_headers.len - 1,
                    // @TODO: correct timestamp
                    .time_date_stamp = @intCast(u32, @intCast(i32, std.time.timestamp())),
                    .size_of_optional_header = @sizeOf(ImageOptionalHeader),
                    .characteristics = @enumToInt(ImageFileHeader.Characteristics.executable_image) | @enumToInt(ImageFileHeader.Characteristics.large_address_aware),
                }))) catch unreachable;

    // @TODO: dummy: substitute with real programming
    const entry_point_byte_index = 0;
    comptime assert(@sizeOf(ImageOptionalHeader) == 0xf0);
    const image_optional_header_index = exe_buffer.items.len;
    exe_buffer.appendSlice(std.mem.asBytes(&std.mem.zeroInit(ImageOptionalHeader, .
            {
                .magic = ImageOptionalHeader.magic,
                .size_of_code = sections[@enumToInt(Section.Index.@".text")].header.size_of_raw_data,
                .size_of_initialized_data = sections[@enumToInt(Section.Index.@".rdata")].header.size_of_raw_data + sections[@enumToInt(Section.Index.@".data")].header.size_of_raw_data,
                // entry point address is the first byte of the text section
                .address_of_entry_point = sections[@enumToInt(Section.Index.@".text")].header.virtual_address + entry_point_byte_index,
                .base_of_code = sections[@enumToInt(Section.Index.@".text")].header.virtual_address,
                .image_base = 0x0000000140000000,
                .section_alignment = section_alignment,
                .file_alignment = file_alignment,
                .major_os_version = min_windows_version_vista,
                .major_subsystem_version = min_windows_version_vista,
                .size_of_image = virtual_size_of_image,
                .size_of_headers = file_size_of_headers,
                .checksum = 0,
                // @TODO: support also GUI applications. Turn this into an enum
                .subsystem = 3,
                .dll_characteristics = @enumToInt(ImageDLLCharacteristics.high_entropy_va) | @enumToInt(ImageDLLCharacteristics.nx_compat) | @enumToInt(ImageDLLCharacteristics.dynamic_base) | @enumToInt(ImageDLLCharacteristics.terminal_server_aware),
                .size_of_stack_reserve = 0x100000,
                .size_of_stack_commit = 0x1000,
                .size_of_heap_reserve = 0x100000,
                .size_of_heap_commit = 0x1000,
                .number_of_RVA_and_sizes = image_number_of_directory_entries,
            }))) catch panic("Error appending image optional header\n", .{});

    var image_optional_header = @intToPtr(* align(1) ImageOptionalHeader, @ptrToInt(exe_buffer.items.ptr) + image_optional_header_index);

    if (rdata.IAT.size > 0)
    {
        const iat_data_directory = &image_optional_header.data_directory[@enumToInt(DirectoryIndex.IAT)];
        iat_data_directory.* = rdata.IAT;
    }

    if (rdata.import_directory.size > 0)
    {
        const import_data_directory = &image_optional_header.data_directory[@enumToInt(DirectoryIndex.import)];
        import_data_directory.* = rdata.import_directory;
    }

    // ignore exception directory
    //
    //

    // copy sections
    for (sections) |*section|
    {
        exe_buffer.appendSlice(std.mem.asBytes(&section.header)) catch unreachable;
    }

    for (sections) |*section|
    {
        exe_buffer.items.len = section.header.pointer_to_raw_data;
        exe_buffer.appendSlice(section.buffer.items) catch unreachable;
    }

    exe_buffer.items.len = offset.file;
    assert(exe_buffer.items.len % file_alignment == 0);

    const working_exe = std.fs.cwd().readFileAlloc(allocator, "working.exe", 0xffffffff) catch unreachable;

    for (working_exe) |working_b, working_i|
    {
        const exe_b = exe_buffer.items[working_i];
        if (exe_b != working_b)
        {
            std.debug.print("[{x}]: W: 0x{x}. E: 0x{x}\n", .{working_i, working_b, exe_b});
        }
    }

    Codegen.write_executable(exe_name, exe_buffer.items);
}
