const std = @import("std");
const Allocator = std.mem.Allocator;
const panic = std.debug.panic;
const assert = std.debug.assert;
const print = std.debug.print;
const alignForward = std.mem.alignForward;

const Codegen = @import("../codegen.zig");
const CodeBuffer = Codegen.CodeBuffer;
const DataBuffer = Codegen.DataBuffer;
const Import = Codegen.Import;
const Section = Codegen.Section;

const BucketArray = @import("../bucket_array.zig").BucketArray;

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

pub fn get_RVA(header: *ImageSectionHeader, section_buffer_len: usize) u32
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

const Offset = struct
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

    fn after_size(self: *Self, size: u32) void
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
    fn encode(allocator: *Allocator, header: *ImageSectionHeader) Section
    {
        var text = Section
        {
            .buffer = CodeBuffer.init(allocator),
            .base_RVA = header.virtual_address,
            .name = ".text",
            .permissions = @enumToInt(Section.Permission.execute) | @enumToInt(Section.Permission.read),
        };
        const code = [_]u8
        {
            0x48, 0x83, 0xEC, 0x28, 0xC7, 0xC1, 0x2A, 0x00, 0x00, 0x00, 0x48, 0xFF, 0x15, 0xFD, 0x0F, 0x00, 0x00, 0x48, 0x83, 0xC4, 0x28, 0xC3, 0x48, 0x83, 0xEC, 0x00, 0xE9, 0xE1, 0xFF, 0xFF, 0xFF, 0x48, 0x83, 0xC4, 0x00, 0xC3
        };

        text.buffer.appendSlice(code[0..]) catch unreachable;

        header.misc.virtual_size = @intCast(u32, text.buffer.items.len);
        header.size_of_raw_data = @intCast(u32, alignForward(text.buffer.items.len, file_alignment));

        return text;
    }
};

const RDataSection = struct
{
    section: Section,
    IAT: Section.Directory,
    import_directory: Section.Directory,

    fn encode(allocator: *Allocator, header: *ImageSectionHeader) RDataSection
    {
        var rdata = Section
        {
            .buffer = DataBuffer.init(allocator),
            .base_RVA = header.virtual_address,
            .name = ".rdata",
            .permissions = @enumToInt(Section.Permission.read),
        };

        // loop these
        const symbol = "ExitProcess";
        // loop these
        const library = "KERNEL32.DLL";

        const symbol_rva = get_RVA(header, rdata.buffer.items.len);
        rdata.buffer.appendSlice(std.mem.asBytes(&@intCast(u16, 0))) catch unreachable;
        append_string(&rdata.buffer, symbol);

        const IAT_RVA = get_RVA(header, rdata.buffer.items.len);
        // Loop these
        const library_rva = get_RVA(header, rdata.buffer.items.len);

        // loop these
        const offset = get_RVA(header, rdata.buffer.items.len) - header.virtual_address;
        _ = offset;
        rdata.buffer.appendSlice(std.mem.asBytes(&@intCast(u64, symbol_rva))) catch unreachable;

        rdata.buffer.appendSlice(std.mem.asBytes(&@intCast(u64, 0))) catch unreachable;

        const IAT_size = @intCast(u32, rdata.buffer.items.len);

        // loop these
        const lib_image_thunk_rva = get_RVA(header, rdata.buffer.items.len);
        // loop these
        rdata.buffer.appendSlice(std.mem.asBytes(&@intCast(u64, symbol_rva))) catch unreachable;

        rdata.buffer.appendSlice(std.mem.asBytes(&@intCast(u64, 0))) catch unreachable;

        // library names
        // loop these
        //
        const library_name_RVA = get_RVA(header, rdata.buffer.items.len);
        append_string(&rdata.buffer, library);

        const import_directory_RVA = get_RVA(header, rdata.buffer.items.len);

        // loop these
        rdata.buffer.appendSlice(std.mem.asBytes(&std.mem.zeroInit(ImageImportDescriptor, .
                    {
                        .characteristics_or_original_first_thunk = lib_image_thunk_rva,
                        .name = library_name_RVA,
                        .first_thunk = library_rva,
                    }))) catch unreachable;

        const import_directory_size = get_RVA(header, rdata.buffer.items.len) - import_directory_RVA;

        if (import_directory_size > 0)
        {
            rdata.buffer.appendSlice(std.mem.asBytes(&std.mem.zeroes(ImageImportDescriptor))) catch unreachable;
        }

        // Exception
        // dont code for now
        //

        header.misc.virtual_size = @intCast(u32, rdata.buffer.items.len);
        header.size_of_raw_data = @intCast(u32, alignForward(rdata.buffer.items.len, file_alignment));
        return .
        {
            .section = rdata,
            .IAT = .{ .RVA = IAT_RVA, .size = IAT_size, },
            .import_directory = .{ .RVA = import_directory_RVA, .size = import_directory_size, },
        };
    }
};

const DataSection = struct
{
    fn encode(allocator: *Allocator, header: *ImageSectionHeader) Section
    {
        var data = Section
        {
            .buffer = DataBuffer.init(allocator),
            .base_RVA = header.virtual_address,
            .name = ".data",
            .permissions = @enumToInt(Section.Permission.read) | @enumToInt(Section.Permission.write),
        };

        // dont hardcode this
        data.buffer.append(0) catch unreachable;
        
        header.misc.virtual_size = @intCast(u32, data.buffer.items.len);
        header.size_of_raw_data = @intCast(u32, alignForward(data.buffer.items.len, file_alignment));

        return data;
    }
};

pub fn write(allocator: *Allocator, executable: anytype, exe_name: []const u8, data_buffer: []const u8, import_libraries: []Import.Library, target: std.Target) void
{
    _ = import_libraries;
    _ = data_buffer;
    _ = executable;
    
    var section_headers = [_]ImageSectionHeader
    {
        std.mem.zeroInit(ImageSectionHeader, .{
            .name = text_section_name,
            .characteristics = @enumToInt(ImageSectionHeader.Characteristics.contains_code) | @enumToInt(ImageSectionHeader.Characteristics.memory_read) | @enumToInt(ImageSectionHeader.Characteristics.memory_execute),
        }),
        std.mem.zeroInit(ImageSectionHeader, .{
            .name = rdata_section_name,
            .characteristics = @enumToInt(ImageSectionHeader.Characteristics.contains_initialized_data) | @enumToInt(ImageSectionHeader.Characteristics.memory_read),
        }),
        std.mem.zeroInit(ImageSectionHeader, .{
            .name = data_section_name,
            .characteristics = @enumToInt(ImageSectionHeader.Characteristics.contains_initialized_data) | @enumToInt(ImageSectionHeader.Characteristics.memory_read) | @enumToInt(ImageSectionHeader.Characteristics.memory_write),
        }),
        std.mem.zeroes(ImageSectionHeader),
    };
    
    const file_size_of_headers = @intCast(u32, alignForward(@sizeOf(ImageDOSHeader) + @sizeOf(@TypeOf(image_NT_signature)) + @sizeOf(ImageFileHeader) + @sizeOf(ImageOptionalHeader) + @sizeOf(@TypeOf(section_headers)), file_alignment));

    var offset = Offset.new(file_size_of_headers);

    // .text
    var text_section_header = &section_headers[0];
    text_section_header.pointer_to_raw_data = offset.file;
    text_section_header.virtual_address = offset.virtual;

    const text = TextSection.encode(allocator, text_section_header);
    offset.after_size(text_section_header.size_of_raw_data);

    // .rdata
    var rdata_section_header = &section_headers[1];
    rdata_section_header.pointer_to_raw_data = offset.file;
    rdata_section_header.virtual_address = offset.virtual;

    const rdata = RDataSection.encode(allocator, rdata_section_header);
    offset.after_size(rdata_section_header.size_of_raw_data);

    var data_section_header = &section_headers[2];
    data_section_header.pointer_to_raw_data = offset.file;
    data_section_header.virtual_address = offset.virtual;

    const data = DataSection.encode(allocator, data_section_header);
    offset.after_size(data_section_header.size_of_raw_data);

    // program patch labels

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
                .size_of_code = text_section_header.size_of_raw_data,
                .size_of_initialized_data = rdata_section_header.size_of_raw_data + data_section_header.size_of_raw_data,
                // entry point address is the first byte of the text section
                .address_of_entry_point = text_section_header.virtual_address + entry_point_byte_index,
                .base_of_code = text_section_header.virtual_address,
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
    exe_buffer.appendSlice(std.mem.asBytes(&section_headers)) catch unreachable;

    // .text segment
    {
        exe_buffer.items.len = text_section_header.pointer_to_raw_data;
        exe_buffer.appendSlice(text.buffer.items) catch unreachable;
    }

    // .rdata segment
    {
        exe_buffer.items.len = rdata_section_header.pointer_to_raw_data;
        exe_buffer.appendSlice(rdata.section.buffer.items) catch unreachable;
    }
    
    // .data segment
    {
        exe_buffer.items.len = data_section_header.pointer_to_raw_data;
        exe_buffer.appendSlice(data.buffer.items) catch unreachable;
    }

    exe_buffer.items.len = offset.file;
    assert(exe_buffer.items.len % file_alignment == 0);

    Codegen.write_executable(exe_name, exe_buffer.items);
}
