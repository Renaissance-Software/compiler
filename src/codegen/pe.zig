const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const panic = std.debug.panic;
const assert = std.debug.assert;
const alignForward = std.mem.alignForward;

const Compiler = @import("../compiler.zig");
const Parser = @import("../parser.zig");
const Semantics = @import("../semantics.zig");
const Codegen = @import("../codegen.zig");
const CodeBuffer = Codegen.CodeBuffer;
const DataBuffer = Codegen.DataBuffer;
const Import = Codegen.Import;

const x86_64 = @import("x86_64/codegenv2.zig");

pub const file_alignment = 0x200;
pub const section_alignment = 0x1000;
const min_windows_version_vista = 6;

fn log(comptime format: []const u8, arguments: anytype) void
{
    Compiler.log(.pe, format, arguments);
}

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

const image_DOS_signature = 0x5A4D;
const image_NT_signature: u32 = 0x00004550;

const image_number_of_directory_entries = 0x10;

pub const Offset = struct
{
    file: u32,
    virtual: u32,

    const Self = @This();

    pub fn after_size(self: *Self, size: u32) void
    {
        self.* =
        .{
            .file = self.file + @intCast(u32, alignForward(size, file_alignment)),
            .virtual = self.virtual + @intCast(u32, alignForward(size, section_alignment)),
        };
    }
};

const ImportLibrary = struct
{
    name_RVA: u32,
    RVA: u32,
    image_thunk_RVA: u32,
    symbol_RVAs: []u32,
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

    pub const Directory = extern struct
    {
        RVA: u32,
        size: u32,
    };

    pub fn get_RVA(self: *Section) u32
    {
        return self.header.virtual_address + @intCast(u32, self.buffer.items.len);
    }

    fn append_string(self: *Section, string: []const u8) void
    {
        const len = string.len + 1;
        self.buffer.appendSlice(string) catch unreachable;
        self.buffer.append(0) catch unreachable;
        // if is not aligned to 2
        if (len & 1 != 0)
        {
            self.buffer.append(0) catch unreachable;
        }
    }

    const RData = struct
    {
        const EncodingOutput = struct
        {
            libraries_offsets: ArrayList(LibraryOffsets),
            iat: Section.Directory,
            import: Section.Directory
        };
    };

    fn encode_rdata_section(allocator: *Allocator, external: Semantics.External, rdata_out: *RData.EncodingOutput, offset: *Offset) Section
    {
        var rdata = Section
        {
            .header = std.mem.zeroInit(ImageSectionHeader, .
            {
                .pointer_to_raw_data = offset.file,
                .virtual_address = offset.virtual,
                .name = Section.names[@enumToInt(Section.Index.@".rdata")],
                .characteristics = Section.characteristics[@enumToInt(Section.Index.@".rdata")],
            }),
            .buffer = DataBuffer.init(allocator),
        };

        var pe32_libraries = ArrayList(ImportLibrary).initCapacity(allocator, external.libraries.len) catch unreachable;

        for (external.libraries) |library|
        {
            var symbol_RVAs = ArrayList(u32).initCapacity(allocator, library.symbols.len) catch unreachable;
            for (library.symbols) |symbol_i|
            {
                symbol_RVAs.appendAssumeCapacity(rdata.get_RVA());
                rdata.buffer.appendSlice(std.mem.asBytes(&@intCast(u16, 0))) catch unreachable;
                const symbol_name = external.symbol_names[symbol_i];
                rdata.append_string(symbol_name);
            }
            
            pe32_libraries.append(ImportLibrary
                {
                    .name_RVA = 0,
                    .RVA = 0,
                    .image_thunk_RVA = 0,
                    .symbol_RVAs = symbol_RVAs.items,
                }) catch unreachable;
        }

        // IAT list
        const IAT_RVA = rdata.get_RVA();

        rdata_out.libraries_offsets = ArrayList(LibraryOffsets).initCapacity(allocator, pe32_libraries.items.len) catch unreachable;

        for (pe32_libraries.items) |*pe32_lib|
        {
            pe32_lib.RVA = rdata.get_RVA();

            var symbol_offsets = ArrayList(u32).initCapacity(allocator, pe32_lib.symbol_RVAs.len) catch unreachable;

            for (pe32_lib.symbol_RVAs) |symbol_rva|
            {
                const symbol_offset = @intCast(u32, rdata.buffer.items.len);
                symbol_offsets.appendAssumeCapacity(symbol_offset);

                rdata.buffer.appendSlice(std.mem.asBytes(&@intCast(u64, symbol_rva))) catch unreachable;
            }

            rdata.buffer.appendSlice(std.mem.asBytes(&@intCast(u64, 0))) catch unreachable;

            rdata_out.libraries_offsets.appendAssumeCapacity(.
                {
                    .symbol_offsets = symbol_offsets.items,
                });
        }

        const IAT_size = @intCast(u32, rdata.buffer.items.len);

        // Image thunks
        for (pe32_libraries.items) |*pe32_lib|
        {
            pe32_lib.image_thunk_RVA = rdata.get_RVA();

            for (pe32_lib.symbol_RVAs) |symbol_RVA|
            {
                rdata.buffer.appendSlice(std.mem.asBytes(&@intCast(u64, symbol_RVA))) catch unreachable;
            }

            rdata.buffer.appendSlice(std.mem.asBytes(&@intCast(u64, 0))) catch unreachable;
        }

        // Library names
        for (external.library_names) |library_name, library_i|
        {
            var pe32_lib = &pe32_libraries.items[library_i];
            pe32_lib.name_RVA = rdata.get_RVA();

            rdata.append_string(library_name);
        }

        // Import directory
        const import_directory_RVA = rdata.get_RVA();

        for (pe32_libraries.items) |pe32_lib|
        {
            rdata.buffer.appendSlice(std.mem.asBytes(&std.mem.zeroInit(ImageImportDescriptor, .
                        {
                            .characteristics_or_original_first_thunk = pe32_lib.image_thunk_RVA,
                            .name = pe32_lib.name_RVA,
                            .first_thunk = pe32_lib.RVA,
                        }))) catch unreachable;
        }

        const import_directory_size = rdata.get_RVA() - import_directory_RVA;

        if (import_directory_size > 0)
        {
            rdata.buffer.appendSlice(std.mem.asBytes(&std.mem.zeroes(ImageImportDescriptor))) catch unreachable;
        }

        // @TODO: @INFO: Exception directory: dont code for now

        rdata.header.misc.virtual_size = @intCast(u32, rdata.buffer.items.len);
        rdata.header.size_of_raw_data = @intCast(u32, alignForward(rdata.buffer.items.len, file_alignment));
        offset.after_size(rdata.header.size_of_raw_data);

        rdata_out.iat = .{ .RVA = IAT_RVA, .size = IAT_size };
        rdata_out.import = .{ .RVA = import_directory_RVA, .size = import_directory_size };

        return rdata;
    }

    pub const Text = struct
    {
        pub const EncodingOutput = struct
        {
            patches: ArrayList(Patch),
        };
    };

    fn encode_text_section(executable: anytype, allocator: *Allocator, text_out: *Text.EncodingOutput, arch: std.Target.Cpu.Arch, offset: *Offset) Section
    {
        var text = Section
        {
            .header = std.mem.zeroInit(ImageSectionHeader, .
            {
                .pointer_to_raw_data = offset.file,
                .virtual_address = offset.virtual,
                .name = Section.names[@enumToInt(Section.Index.@".text")],
                .characteristics = Section.characteristics[@enumToInt(Section.Index.@".text")],
            }),
            .buffer = DataBuffer.init(allocator),
        };

        switch (arch)
        {
            .x86_64 =>
            {
                var x86_64_program = @ptrCast(*x86_64.Program, executable);
                x86_64_program.encode_text_section_pe(allocator, &text, text_out, offset);
            },
            else => panic("CPU arch: {}\n", .{arch}),
        }

        return text;
    }

    fn encode_data_section(allocator: *Allocator, data_buffer: []const u8, offset: *Offset) Section
    {
        const data_buffer_length = if (data_buffer.len == 0) 1 else data_buffer.len;
        var data = Section
        {
            .header = std.mem.zeroInit(ImageSectionHeader, .
            {
                .pointer_to_raw_data = offset.file,
                .virtual_address = offset.virtual,
                .name = Section.names[@enumToInt(Section.Index.@".data")],
                .characteristics = Section.characteristics[@enumToInt(Section.Index.@".data")],
            }),
            .buffer = DataBuffer.initCapacity(allocator, data_buffer_length) catch unreachable,
        };

        if (data_buffer.len == 0)
        {
            data.buffer.appendAssumeCapacity(0);
        }
        else
        {
            data.buffer.appendSliceAssumeCapacity(data_buffer);
        }

        data.header.misc.virtual_size = @intCast(u32, data.buffer.items.len);
        data.header.size_of_raw_data = @intCast(u32, alignForward(data.buffer.items.len, file_alignment));
        offset.after_size(data.header.size_of_raw_data);

        return data;
    }
};

pub const Patch = struct
{
    section_buffer_index_to: u32,
    section_buffer_index_from: u32,
    section_to_write_to: Section.Index,
    section_to_read_from: Section.Index,
};

const LibraryOffsets = struct
{
    symbol_offsets: []u32,
};

pub fn write(allocator: *Allocator, executable: anytype, data_buffer: []const u8, executable_filename: []const u8, external: Semantics.External, target: std.Target) void
{
    const null_section_header_count = 1;
    var section_headers_size = @sizeOf(ImageSectionHeader) * (Section.count + null_section_header_count);
    
    const file_size_of_headers = @intCast(u32, alignForward(@sizeOf(ImageDOSHeader) + @sizeOf(@TypeOf(image_NT_signature)) + @sizeOf(ImageFileHeader) + @sizeOf(ImageOptionalHeader) + section_headers_size, file_alignment));

    var offset = Offset
    {
        .file = @intCast(u32, alignForward(file_size_of_headers, file_alignment)),
        .virtual = @intCast(u32, alignForward(file_size_of_headers, section_alignment)),
    };

    var text_out: Section.Text.EncodingOutput = undefined;
    var rdata_out: Section.RData.EncodingOutput = undefined;

    var sections = [Section.count]Section
    {
        Section.encode_text_section(executable, allocator, &text_out, target.cpu.arch, &offset),
        Section.encode_rdata_section(allocator, external, &rdata_out, &offset),
        Section.encode_data_section(allocator, data_buffer, &offset),
    };

    log("Patch count: {}\n", .{text_out.patches.items.len});
    // program patch labels
    for (text_out.patches.items) |patch|
    {
        // assuming all patches are 4 bytes
        const section_to_patch = &sections[@enumToInt(patch.section_to_write_to)];
        const jump_from = patch.section_buffer_index_to + 4;
        log("Text index: {}\n", .{jump_from}); 
        const jump_from_RVA = section_to_patch.header.virtual_address + jump_from;
        var patch_address = @ptrCast(*align(1) i32, &section_to_patch.buffer.items[patch.section_buffer_index_to]);
        const jump_to_RVA = switch (patch.section_to_read_from)
        {
            .@".rdata" => blk:
            {
                const index = Parser.Function.External.Index.from_u32(patch.section_buffer_index_from);
                const symbol_offset = rdata_out.libraries_offsets.items[index.library].symbol_offsets[index.function];
                const symbol_RVA = sections[@enumToInt(Section.Index.@".rdata")].header.virtual_address + symbol_offset;
                break :blk symbol_RVA;
            },
            .@".data" => blk:
            {
                const symbol_offset = patch.section_buffer_index_from;
                const symbol_RVA = sections[@enumToInt(Section.Index.@".data")].header.virtual_address + symbol_offset;
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
                    .section_count = Section.count,
                    // @TODO: correct timestamp
                    .time_date_stamp = @intCast(u32, @intCast(i32, std.time.timestamp())),
                    .size_of_optional_header = @sizeOf(ImageOptionalHeader),
                    .characteristics = @enumToInt(ImageFileHeader.Characteristics.executable_image) | @enumToInt(ImageFileHeader.Characteristics.large_address_aware),
                }))) catch unreachable;

    comptime assert(@sizeOf(ImageOptionalHeader) == 0xf0);

    const image_optional_header_index = exe_buffer.items.len;
    exe_buffer.appendSlice(std.mem.asBytes(&std.mem.zeroInit(ImageOptionalHeader, .
            {
                .magic = ImageOptionalHeader.magic,
                .size_of_code = sections[@enumToInt(Section.Index.@".text")].header.size_of_raw_data,
                .size_of_initialized_data = sections[@enumToInt(Section.Index.@".rdata")].header.size_of_raw_data + sections[@enumToInt(Section.Index.@".data")].header.size_of_raw_data,
                // entry point address is the first byte of the text section
                .address_of_entry_point = sections[@enumToInt(Section.Index.@".text")].header.virtual_address,
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

    if (rdata_out.iat.size > 0)
    {
        var iat_data_directory = &image_optional_header.data_directory[@enumToInt(DirectoryIndex.IAT)];
        iat_data_directory.* = rdata_out.iat;
    }

    if (rdata_out.import.size > 0)
    {
        var import_data_directory = &image_optional_header.data_directory[@enumToInt(DirectoryIndex.import)];
        import_data_directory.* = rdata_out.import;
    }

    // @TODO: we are ignoring the exception directory

    // copy section headers
    for (sections) |*section|
    {
        exe_buffer.appendSlice(std.mem.asBytes(&section.header)) catch unreachable;
    }

    // @TODO: can this null section header cause problems?
    exe_buffer.appendSlice(std.mem.asBytes(&std.mem.zeroes(ImageSectionHeader))) catch unreachable;

    // copy sections
    for (sections) |*section|
    {
        exe_buffer.items.len = section.header.pointer_to_raw_data;
        exe_buffer.appendSlice(section.buffer.items) catch unreachable;
    }

    exe_buffer.items.len = offset.file;
    assert(exe_buffer.items.len % file_alignment == 0);

    Codegen.write_executable(executable_filename, exe_buffer.items);
}
