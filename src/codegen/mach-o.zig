    const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const print = std.debug.print;
const panic = std.debug.panic;
const assert = std.debug.assert;

const MachO = @This();

const VirtualMemoryProtection = struct
{
    const Type = u32;

    const none = 0x0;
    const read = 0x1;
    const write = 0x2;
    const execute = 0x4;
};

const Header = extern struct
{
    magic: u32 = 0xfeedfacf,
    cpu_type: CPU.Type,
    cpu_subtype: u32 = 0,
    filetype: Filetype,
    load_command_count: u32,
    load_command_size: u32,
    flags: Flags.Type,
    reserved: u32 = 0,

    const CPU = struct
    {
        const Type = enum(u32)
        {
            vax = 1,
            romp = 2,
            ns32032 = 4,
            ns32332 = 5,
            mc680x0 = 6,
            i386 = 7,
            x86_64 = 7 | arch_abi64,
            mips = 8,
            ns32532 = 9,
            hppa = 11,
            arm = 12,
            mc88000 = 13,
            sparc = 14,
            i860 = 15,
            i860_little = 16,
            rs6000 = 17,
            mc98000 = 18,
            powerpc64 = powerpc | arch_abi64,
            veo = 255,
            arm64 = 12 | arch_abi64,
            arm64_32 = 12 | arch_abi64_32,
            any = 0xffffffff,
        };

        const powerpc = 18;
        const arch_abi64 = 0x1000000;
        const arch_abi64_32 = 0x2000000;

        const SubType = extern struct
        {
            const mask = 0xff000000;
        };
    };

    const Filetype = enum(u32)
    {
        object = 1,
        executable = 2,
        fvmlib = 3,
        core = 4,
        preload = 5,
        dylib = 6,
        dylinker = 7,
        bundle = 8,
        dylib_stub = 9,
        dsym = 10,
        kext_bundle = 11,
        fileset = 12,
    };

    const Flags = struct
    {
        const Type = u32;

        const noundefs = 0x1;
        const incr_link = 0x2;
        const dyldlink = 0x4;
        const bind_at_load = 0x8;
        const prebound = 0x10;
        const split_segs = 0x20;
        const lazy_init = 0x40;
        const two_level = 0x80;
        const force_flat = 0x100;
        const no_multi_defs = 0x200;
        const no_fix_prebinding = 0x400;
        const prebindable = 0x800;
        const all_mods_bound = 0x1000;
        const subsections_via_symbols = 0x2000;
        const canonical = 0x4000;
        const weak_defines = 0x8000;
        const binds_to_weak = 0x10000;
        const allow_stack_execution = 0x20000;
        const root_safe = 0x40000;
        const setuid_safe = 0x80000;
        const no_reexported_dylibs = 0x100000;
        const pie = 0x200000;
        const dead_strippable_dylib = 0x400000;
        const has_tlv_descriptors = 0x800000;
        const no_heap_execution = 0x1000000;
        const app_extension_safe = 0x2000000;
        const nlist_outofsync_with_dyldinfo = 0x4000000;
        const sym_support = 0x8000000;
        const dylib_in_cache = 0x80000000;
    };
};

const LoadCommand = struct
{
    const ID = enum(u32)
    {
        segment = 0x1,
        sym_tab = 0x2,
        sym_seg = 0x3,
        thread = 0x4,
        unix_thread = 0x5,
        load_fvm_lib = 0x6,
        id_fvm_lib = 0x7,
        ident = 0x8,
        fvm_file = 0x9,
        prepage = 0xa,
        dy_sym_tab = 0xb,
        load_dylib = 0xc,
        id_dylib = 0xd,
        load_dylinker = 0xe,
        id_dylinker = 0xf,
        prebound_dylib = 0x10,
        routines = 0x11,
        sub_framework = 0x12,
        sub_umbrella = 0x13,
        sub_client = 0x14,
        sub_library = 0x15,
        two_level_hints = 0x16,
        prebind_checksum = 0x17,

        // dynamic library stuff
        load_weak_dylib = 0x18 | req_dyld,
        segment_64 = 0x19,
        routines_64 = 0x1a,
        uuid = 0x1b,
        rpath = 0x1c | req_dyld,
        code_signature = 0x1d,
        segment_split_info = 0x1e,
        reexport_dylib = 0x1f | req_dyld,
        lazy_load_dylib = 0x20,
        encryption_info = 0x21,
        dyld_info = 0x22,
        dyld_info_only = 0x22 | req_dyld,
        load_upward_dylib = 0x23,
        version_min_macosx = 0x24,
        version_min_iphoneos = 0x25,
        function_starts = 0x26,
        dyld_environment = 0x27,
        main = 0x28 | req_dyld,
        data_in_code = 0x29,
        source_version = 0x2a,
        dylib_code_sign_drs = 0x2b,
        encryption_info_64 = 0x2c,
        linker_option = 0x2d,
        linker_optimization_hint = 0x2e,
        version_min_tvos = 0x2f,
        version_min_watchos = 0x30,
        note = 0x31,
        build_version = 0x32,
        dyld_exports_trie = 0x33 | req_dyld,
        dyld_chained_fixups = 0x34 | req_dyld,
        fileset_entry = 0x35 | req_dyld,

        const req_dyld = 0x80000000;
    };

    const LCHeader = extern struct
    {
        id: ID,
        size: u32,
    };

    const String = extern union
    {
        offset: u32,
        ptr: [*]u8,
    };

    const Segment = extern struct
    {
        header: LCHeader,
        name: [16]u8,
        virtual_memory_address: u64,
        virtual_memory_size: u64,
        file_offset: u64,
        file_size: u64,
        max_protection: VirtualMemoryProtection.Type,
        initial_protection: VirtualMemoryProtection.Type,
        section_count: u32,
        flags: Flags.Type,

        const Section = extern struct
        {
            name: [16]u8,
            segment_name: [16]u8,
            address: u64,
            size: u64,
            file_offset: u32,
            power_of_2_alignment: u32,
            relocation_offset: u32,
            relocation_count: u32,
            flags: Flags.Type,
            reserved1: u32 = 0,
            reserved2: u32 = 0,
            reserved3: u32 = 0,

            const Flags = struct
            {
                const Type = u32;

                const section_type = 0x000000ff;
                const section_attributes = 0xffffff00;

                const regular = 0x0;
                const zero_fill = 0x1;
                const c_string_literals = 0x2;
                const literals_4_bytes = 0x3;
                const literals_8_bytes = 0x4;
                const literal_pointers = 0x5;
                const non_lazy_symbol_pointers = 0x6;
                const lazy_symbol_pointers = 0x7;
                const symbol_stubs = 0x8;
                const mod_init_func_pointers = 0x9;
                const mod_term_func_pointers = 0xa;
                const coalesced = 0xb;
                const gb_zerofill = 0xc;
                const interposing = 0xd;
                const byte16_literals = 0xe;
                const dtrace_dof = 0xf;
                const lazy_dylib_symbol_pointers = 0x10;
                const thread_local_regular = 0x11;
                const thread_local_zerofill = 0x12;
                const thread_local_variables = 0x13;
                const thread_local_variable_pointers = 0x14;
                const thread_local_init_function_pointers = 0x15;
                const init_func_offsets = 0x16;
                const section_attributes_usr = 0xff000000;
                const attr_pure_instructions = 0x80000000;
                const s_attr_no_toc  = 0x40000000;
                const s_attr_strip_static_syms = 0x2000000;
                const s_attr_no_dead_strip = 0x10000000;
                const s_attr_live_support = 0x08000000;
                const s_attr_self_modifying_code = 0x04000000;
                const s_attr_debug = 0x02000000;
                const section_attributes_sys = 0x00ffff00;
                const s_attr_some_instructions = 0x00000400;
                const s_attr_ext_reloc = 0x00000200;
                const s_attr_loc_reloc = 0x00000100;
            };
        };

        const Flags = struct
        {
            const Type = u32;

            const high_vm = 0x1;
            const fvmlib = 0x2;
            const no_reloc = 0x4;
            const protected_version = 0x8;
            const read_only = 0x10;
        };
    };


    const FVMLib = extern struct
    {
        header: LCHeader,
        name: String,
        minor_version: u32,
        header_address: u32,
    };

    const DYLibCommand = extern struct
    {
        header: LCHeader,
        name: String,
        timestamp: u32,
        current_version: u32,
        compatibility_version: u32,
    };

    const SubFramework = extern struct
    {
        header: LCHeader,
        umbrella: String,
    };

    const SubClient = extern struct
    {
        header: LCHeader,
        client: String,
    };

    const SubUmbrella = extern struct
    {
        header: LCHeader,
        sub_umbrella: String,
    };

    const SubLibrary = extern struct
    {
        header: LCHeader,
        sub_library: String,
    };

    const PreboundDYLib = extern struct
    {
        header: LCHeader,
        name: String,
        module_count: u32,
        linked_modules: String,
    };

    const DYLinker = extern struct
    {
        header: LCHeader,
        name: String,
    };

    const Thread = extern struct
    {
        header: LCHeader, // LC_THREAD OR LC_UNIXTHREAD
    };

    const Routines = extern struct
    {
        header: LCHeader,
        initialization_address: u64,
        initialization_module: u64,
        reserved: [6]u64,
    };

    const SymTab = extern struct
    {
        header: LCHeader,
        symbol_offset: u32,
        symbol_count: u32,
        string_table_offset: u32,
        string_table_size: u32,
    };

    const DYSymTab = extern struct
    {
        const Index = extern struct
        {
            index: u32,
            count: u32,
        };

        const Table = extern struct
        {
            offset: u32,
            entry_count: u32,
        };

        header: LCHeader,

        local_symbols: Index,
        extern_symbols: Index,
        undefined_symbols: Index,
        content_table: Table,
        module_table: Table,
        referenced_symbol_table: Table,
        indirect_symbol_table: Table,
        external_relocation_table: Table,
        local_relocation_table: Table,
    };

    const TwoLevelHints = extern struct
    {
        header: LCHeader,
        offset: u32,
        hint_count: u32,
    };

    const PrebindChecksum = extern struct
    {
        header: LCHeader,
        checksum: u32,
    };

    const UUID = extern struct
    {
        header: LCHeader,
        uuid: [16]u8,
    };

    const RPath = extern struct
    {
        header: LCHeader,
        path: String,
    };

    const LinkedItData = extern struct
    {
        header: LCHeader,
        data_offset: u32,
        data_size: u32,
    };

    const EncryptionInfo = extern struct
    {
        header: LCHeader,
        encrypted_range_offset: u32,
        encrypted_range_size: u32,
        encryption_id: u32,
        pad: u32,
    };

    const VersionMin = extern struct
    {
        header: LCHeader,
        version: u32,
        sdk: u32,
    };

    const BuildVersion = extern struct
    {
        header: LCHeader,
        platform: u32,
        min_os: u32,
        sdk: u32,
        tool_entry_count: u32,
    };

    const DYLDInfo = extern struct
    {
        const Ref = extern struct
        {
            offset: u32,
            size: u32,
        };

        header: LCHeader,
        rebase: Ref,
        binding: Ref,
        weak_binding: Ref,
        lazy_binding: Ref,
        export_: Ref,
    };

    const LinkerOption = extern struct
    {
        header: LCHeader,
        count: u32,
    };

    const SymSeg = extern struct
    {
        header: LCHeader,
        offset: u32,
        size: u32,
    };

    const Ident = extern struct
    {
        header: LCHeader,
    };

    const FVMFile = extern struct
    {
        header: LCHeader,
        name: String,
        header_address: u32,
    };

    const EntryPoint = extern struct
    {
        header: LCHeader,
        offset: u64,
        stack_size: u64,
    };

    const SourceVersion = extern struct
    {
        header: LCHeader,
        version: u64,
    };

    const Note = extern struct
    {
        header: LCHeader,
        data_owner: [16]u8,
        offset: u64,
        size: u64,
    };
};

const DYLib = struct
{
    const TableOfContents = extern struct
    {
        symbol_index: u32,
        module_index: u32,
    };

    const Module = extern struct
    {
        const Ref = extern struct
        {
            index: u32,
            count: u32,
        };

        name: u32,
        extern_symbols: Ref,
        reference_symbol_table: Ref,
        local_symbols: Ref,
        extern_relocations: Ref,
        init_and_term_sections: Ref,
        objc_module_info_size: u32,
        objc_module_info_address: u64,
    };

    const Reference = packed struct
    {
        index_to_symbol_table: u24,
        flags: u8,
    };
};

const TwoLevelHint = packed struct
{
    index_to_subimages: u8,
    index_to_table_of_contents: u24,
};

const BuildToolVersion = extern struct
{
    tool: u32,
    version: u32,
};

const DataInCodeEntry = extern struct
{
    offset: u32,
    length: u16,
    kind: u16,
};

const TLVDescriptor = extern struct
{
    const Self = @This();
    thunk: fn(self: *Self) u64, // return a void*
    key: u64,
    offset: u64,
};

const base_memory_address = 0x100000000;

const page_zero_segment = blk:
{
    var pz_seg = std.mem.zeroes(LoadCommand.Segment);
    pz_seg.header.id = .segment_64;
    pz_seg.header.size = @sizeOf(LoadCommand.Segment);
    std.mem.copy(u8, pz_seg.name[0..], "__PAGEZERO");
    pz_seg.virtual_memory_size = base_memory_address;

    break :blk pz_seg;
};

fn align_number(n: u64, alignment: u64) u64
{
    const mask = alignment - 1;
    assert(alignment & mask == 0);
    return (n + mask) & ~mask;
}

fn add_to_exe(comptime T: type, exe_bytes: *ArrayList(u8), structure: *const T) callconv(.Inline) void
{
    print("Offset before adding: {}\n\n", .{exe_bytes.items.len});
    exe_bytes.appendSlice(std.mem.asBytes(structure)) catch unreachable;
    print("Structure added: {}\n\n", .{structure.*});
}

const magic_numbers_after_dylinker = [2]u64 { 0x6c79642f62696c2f, 0x64 };
const magic_number_after_build_version: u64 = 0x28a090000000003;
const magic_number_after_load_dylib = [3]u64 { 0x7473795362696c2f, 0x6c79642e422e6d65, 0x6269 };

pub fn experiment(allocator: *Allocator, file_content: []const u8, text_section: []u8) void
{
    const file_size = file_content.len;
    print("File size: {}\n", .{file_size});
    const macho_header = @intToPtr(*MachO.Header, @ptrToInt(file_content.ptr));
    print("{}\n\n", .{macho_header});

    var load_command_offset = @ptrToInt(macho_header) + @sizeOf(MachO.Header);
    const load_command_top = load_command_offset + macho_header.load_command_size;
    print("Load command top: {}\n", .{load_command_top - @ptrToInt(macho_header)});
    var next_load_command_offset: u64 = 0;

    while (load_command_offset < load_command_top) : (load_command_offset += next_load_command_offset)
    {
        print("\nOffset: {}\n\n", .{load_command_offset - @ptrToInt(macho_header)});
        const load_command_header = @intToPtr(*LoadCommand.LCHeader, load_command_offset);
        next_load_command_offset = load_command_header.size;

        switch (load_command_header.id)
        {
            LoadCommand.ID.segment_64 =>
            {
                const segment = @intToPtr(*LoadCommand.Segment, load_command_offset);
                print("Segment: {}\n", .{segment});
                print("Segment name: {s}\n\n", .{segment.name});
                if (segment.header.size > @sizeOf(LoadCommand.Segment))
                {
                    const sections_size = segment.header.size - @sizeOf(LoadCommand.Segment);
                    const section_count = segment.section_count;
                    assert(sections_size % @sizeOf(LoadCommand.Segment.Section) == 0 and sections_size / section_count == @sizeOf(LoadCommand.Segment.Section));
                    const sections = @intToPtr([*]LoadCommand.Segment.Section, load_command_offset + @sizeOf(LoadCommand.Segment))[0..section_count];

                    for (sections) |section, i|
                    {
                        print("Section name: {s}\n", .{section.name});
                        print("Section {}: {}\n", .{i, section});
                    }
                    print("\n", .{});
                }
            },
            LoadCommand.ID.dyld_info_only =>
            {
                const dyld_info = @intToPtr(*LoadCommand.DYLDInfo, load_command_offset);
                print("DYLDInfo only. Command size: {}. Size of struct: {}\n", .{dyld_info.header.size, @sizeOf(LoadCommand.DYLDInfo)});
                assert(@sizeOf(LoadCommand.DYLDInfo) == dyld_info.header.size);
                print("DYLD Info: {}\n", .{dyld_info});
            },
            LoadCommand.ID.sym_tab =>
            {
                const sym_tab = @intToPtr(*LoadCommand.SymTab, load_command_offset);
                print("Symbol table: {}\n", .{sym_tab});
            },
            LoadCommand.ID.dy_sym_tab =>
            {
                const dysym_tab = @intToPtr(*LoadCommand.DYSymTab, load_command_offset);
                print("Dynamic symbol table: {}\n", .{dysym_tab});
            },
            LoadCommand.ID.load_dylinker =>
            {
                const load_dylinker = @intToPtr(*LoadCommand.DYLinker, load_command_offset);
                print("Load DYLinker: {}\n", .{load_dylinker});
                const name_bytes = @ptrCast(*u64, &load_dylinker.name).*;
                print("Name bytes: {}\n", .{name_bytes});

                const bytes_left_to_explore = load_dylinker.header.size - @sizeOf(LoadCommand.DYLinker);
                const bytes = @intToPtr([*]u64, load_command_offset + @sizeOf(LoadCommand.DYLinker))[0..bytes_left_to_explore / @sizeOf(u64)];
                for (bytes) |byte|
                {
                    print("Byte: 0x{x}\n", .{byte});
                }
            },
            LoadCommand.ID.uuid =>
            {
                const uuid = @intToPtr(*LoadCommand.UUID, load_command_offset);
                print("UUID: {}\n", .{uuid});
            },
            LoadCommand.ID.build_version =>
            {
                const build_version = @intToPtr(*LoadCommand.BuildVersion, load_command_offset);
                print("Build version: {}\n", .{build_version});
                const bytes_left_to_explore = build_version.header.size - @sizeOf(LoadCommand.BuildVersion);
                const bytes = @intToPtr([*]u64, load_command_offset + @sizeOf(LoadCommand.BuildVersion))[0..bytes_left_to_explore / @sizeOf(u64)];
                for (bytes) |byte|
                {
                    print("Byte: 0x{x}\n", .{byte});
                }
            },
            LoadCommand.ID.source_version =>
            {
                const source_version = @intToPtr(*LoadCommand.SourceVersion, load_command_offset);
                print("Source version: {}\n", .{source_version});
            },
            LoadCommand.ID.main =>
            {
                const main = @intToPtr(*LoadCommand.EntryPoint, load_command_offset);
                print("Main: {}\n", .{main});
            },
            LoadCommand.ID.load_dylib =>
            {
                const dylib = @intToPtr(*LoadCommand.DYLibCommand, load_command_offset);
                const name = @ptrCast([*]u32, &dylib.name)[0..2];
                print("Load DYLib: {}\n", .{dylib});
                print("Name 0: {}. Name 1: {}\n", .{name[0], name[1]});
                const full_name = @ptrCast(*u64, &dylib.name).*;
                print("Full name: 0x{x}\n", .{full_name});
                const bytes_left_to_explore = dylib.header.size - @sizeOf(LoadCommand.DYLibCommand);
                const bytes = @intToPtr([*]u64, load_command_offset + @sizeOf(LoadCommand.DYLibCommand))[0..bytes_left_to_explore / @sizeOf(u64)];
                for (bytes) |byte|
                {
                    print("Byte: 0x{x}\n", .{byte});
                }
            },
            LoadCommand.ID.function_starts =>
            {
                const function_starts = @intToPtr(*LoadCommand.LinkedItData, load_command_offset);
                print("Function starts: {}\n", .{function_starts});
            },
            LoadCommand.ID.data_in_code =>
            {
                const data_in_code = @intToPtr(*LoadCommand.LinkedItData, load_command_offset);
                print("Data in code: {}\n", .{data_in_code});
            },
            LoadCommand.ID.code_signature =>
            {
                const code_signature = @intToPtr(*LoadCommand.LinkedItData, load_command_offset);
                print("Code signature: {}\n", .{code_signature});
            },
            else => panic("Not implemented: {}\n", .{load_command_header.id}),
        }
    }


    var code = ArrayList(u8).initCapacity(allocator, file_content.len) catch
    {
        panic("Error creating arraylist\n", .{});
    };

    add_to_exe(MachO.Header, &code, 
    &MachO.Header
    {
        .cpu_type = .arm64,
        .filetype = .executable,
        .load_command_count = 0,
        .load_command_size = 0,
        .flags = Header.Flags.pie | Header.Flags.two_level | Header.Flags.dyldlink | Header.Flags.noundefs,
    });

    // @TODO: here we need to write the load command count and size
    var header = @intToPtr(*MachO.Header, @ptrToInt(code.items.ptr));

    add_to_exe(LoadCommand.Segment, &code, &page_zero_segment);

    var file_offset: u64 = 0;
    var virtual_memory_address: u64 = base_memory_address;

    const text_segment_section_count = 2;
    const code_length = text_section.len;
    const aproximate_header_size = 760 + @sizeOf(MachO.Header);
    const unwind_info_section_size = 72;
    const minimal_text_segment_size = 0x4000;
    const code_offset = minimal_text_segment_size - unwind_info_section_size - code_length;

    const code_fits_in_first_section = code_length < code_offset - aproximate_header_size;
    assert(code_fits_in_first_section);

    const text_segment_offset = code.items.len;
    add_to_exe(LoadCommand.Segment, &code,
    &LoadCommand.Segment
    {
        .header = LoadCommand.LCHeader
        {
            .id = .segment_64,
            .size = @sizeOf(LoadCommand.Segment) + (text_segment_section_count * @sizeOf(LoadCommand.Segment.Section)),
        },
        .name = [_]u8 {95, 95, 84, 69, 88, 84, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
        .virtual_memory_address = virtual_memory_address,
        .virtual_memory_size = minimal_text_segment_size,
        .file_offset = 0,
        .file_size = minimal_text_segment_size,
        .max_protection = VirtualMemoryProtection.read | VirtualMemoryProtection.execute,
        .initial_protection = VirtualMemoryProtection.read | VirtualMemoryProtection.execute,
        .section_count = text_segment_section_count,
        .flags = 0,
    });

    virtual_memory_address += code_offset;

    // text section
    add_to_exe(LoadCommand.Segment.Section, &code,
    &LoadCommand.Segment.Section
    {
        .name = [_]u8 {95, 95, 116, 101, 120, 116, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
        .segment_name = [_]u8 {95, 95, 84, 69, 88, 84, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
        .address = virtual_memory_address,
        .size = code_length,
        .file_offset = @truncate(u32, code_offset),
        .power_of_2_alignment = 2,
        .relocation_offset = 0,
        .relocation_count = 0,
        // @TODO: figure out the number
        .flags = 2147484672,
    });
    print("Code offset: {}\n", .{code_offset});

    virtual_memory_address += code_length;

    const unwind_info_section_offset = code_offset + code_length;
    // unwind_info section
    add_to_exe(LoadCommand.Segment.Section, &code,
    &LoadCommand.Segment.Section
    {
        .name = [_]u8 {95, 95, 117, 110, 119, 105, 110, 100, 95, 105, 110, 102, 111, 0, 0, 0 },
        .segment_name = [_]u8 {95, 95, 84, 69, 88, 84, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
        .address = virtual_memory_address,
        .size = unwind_info_section_size,
        .file_offset = @truncate(u32, unwind_info_section_offset),
        .power_of_2_alignment = 2,
        .relocation_offset = 0,
        .relocation_count = 0,
        .flags = 0,
    });

    const text_segment_size = code.items.len - text_segment_offset;
    const expected_text_segment_size = @sizeOf(LoadCommand.Segment) + (@sizeOf(LoadCommand.Segment.Section) * text_segment_section_count);
    assert(text_segment_size == expected_text_segment_size);

    virtual_memory_address += unwind_info_section_size;

    // linkedit segment
    add_to_exe(LoadCommand.Segment, &code,
    &LoadCommand.Segment
    {
        .header = LoadCommand.LCHeader
        {
            .id = .segment_64,
            .size = @sizeOf(LoadCommand.Segment),
        },
        .name = [_]u8 {95, 95, 76, 73, 78, 75, 69, 68, 73, 84, 0, 0, 0, 0, 0, 0 },
        .virtual_memory_address = virtual_memory_address,
        .virtual_memory_size = minimal_text_segment_size,
        .file_offset = minimal_text_segment_size,
        .file_size = 433,
        .max_protection = VirtualMemoryProtection.read,
        .initial_protection = VirtualMemoryProtection.read,
        .section_count = 0,
        .flags = 0,
    });

    add_to_exe(LoadCommand.DYLDInfo, &code,
    &LoadCommand.DYLDInfo
    {
        .header = LoadCommand.LCHeader
        {
            .id = LoadCommand.ID.dyld_info_only,
            .size = @sizeOf(LoadCommand.DYLDInfo),
        },
        .rebase = std.mem.zeroes(LoadCommand.DYLDInfo.Ref),
        .binding = std.mem.zeroes(LoadCommand.DYLDInfo.Ref),
        .weak_binding = std.mem.zeroes(LoadCommand.DYLDInfo.Ref),
        .lazy_binding = std.mem.zeroes(LoadCommand.DYLDInfo.Ref),
        .export_ = LoadCommand.DYLDInfo.Ref
        {
            .offset = 0x4000,
            .size = @sizeOf(LoadCommand.DYLDInfo),
        },
    });

    add_to_exe(LoadCommand.SymTab, &code,
    &LoadCommand.SymTab
    {
        .header = LoadCommand.LCHeader
        {
            .id = LoadCommand.ID.sym_tab,
            .size = @sizeOf(LoadCommand.SymTab),
        },
        // figure out these hardcoded values
        .symbol_offset = 16440,
        .symbol_count = 3,
        .string_table_offset = 16488,
        .string_table_size = 48,
    });

    add_to_exe(LoadCommand.DYSymTab, &code,
    &LoadCommand.DYSymTab
    {
        .header = LoadCommand.LCHeader
        {
            .id = LoadCommand.ID.dy_sym_tab,
            .size = @sizeOf(LoadCommand.DYSymTab),
        },
        .local_symbols = std.mem.zeroes(LoadCommand.DYSymTab.Index),
        .extern_symbols = LoadCommand.DYSymTab.Index { .index = 0, .count = 2 },
        .undefined_symbols = LoadCommand.DYSymTab.Index { .index = 2, .count = 1 },
        .content_table = std.mem.zeroes(LoadCommand.DYSymTab.Table),
        .module_table = std.mem.zeroes(LoadCommand.DYSymTab.Table),
        .referenced_symbol_table = std.mem.zeroes(LoadCommand.DYSymTab.Table),
        .indirect_symbol_table = std.mem.zeroes(LoadCommand.DYSymTab.Table),
        .external_relocation_table = std.mem.zeroes(LoadCommand.DYSymTab.Table),
        .local_relocation_table = std.mem.zeroes(LoadCommand.DYSymTab.Table),
    });

    add_to_exe(LoadCommand.DYLinker, &code,
    &LoadCommand.DYLinker
    {
        .header = LoadCommand.LCHeader
        {
            .id = LoadCommand.ID.load_dylinker,
            .size = @sizeOf(LoadCommand.DYLinker) + 16,
        },
        // @TODO: change
        .name = LoadCommand.String { .ptr = @intToPtr([*]u8, 8247064187369422860), },
    });

    for (magic_numbers_after_dylinker) |n|
    {
        code.appendSlice(std.mem.asBytes(&n)) catch unreachable;
    }

    add_to_exe(LoadCommand.UUID, &code,
    &LoadCommand.UUID
    {
        .header = LoadCommand.LCHeader
        {
            .id = LoadCommand.ID.uuid,
            .size = @sizeOf(LoadCommand.UUID),
        },
        .uuid = [_]u8 { 123, 85, 245, 146, 128, 67, 50, 226, 158, 194, 15, 32, 68, 223, 34, 72 }
    });

    add_to_exe(LoadCommand.BuildVersion, &code,
    &LoadCommand.BuildVersion
    {
        .header = LoadCommand.LCHeader
        {
            .id = LoadCommand.ID.build_version,
            .size = @sizeOf(LoadCommand.BuildVersion) + 8,
        },
        .platform = 1,
        .min_os = 720896,
        .sdk = 721664,
        .tool_entry_count = 1,
    });

    code.appendSlice(std.mem.asBytes(&magic_number_after_build_version)) catch unreachable;

    add_to_exe(LoadCommand.SourceVersion, &code,
    &LoadCommand.SourceVersion
    {
        .header = LoadCommand.LCHeader
        {
            .id = LoadCommand.ID.source_version,
            .size = @sizeOf(LoadCommand.SourceVersion),
        },
        .version = 0,
    });

    add_to_exe(LoadCommand.EntryPoint, &code,
    &LoadCommand.EntryPoint
    {
        .header = LoadCommand.LCHeader
        {
            .id = LoadCommand.ID.main,
            .size = @sizeOf(LoadCommand.EntryPoint),
        },
        .offset = code_offset,
        .stack_size = 0,
    });

    add_to_exe(LoadCommand.DYLibCommand, &code,
    &LoadCommand.DYLibCommand
    {
        .header = LoadCommand.LCHeader
        {
            .id = LoadCommand.ID.load_dylib,
            .size = 56,
        },
        .name = LoadCommand.String { .ptr = @intToPtr([*]u8, 0x200000018) },
        .timestamp = 84698117,
        .current_version = 65536,
        .compatibility_version = 1920169263,
    });

    for (magic_number_after_load_dylib) |n|
    {
        code.appendSlice(std.mem.asBytes(&n)) catch unreachable;
    }

    add_to_exe(LoadCommand.LinkedItData, &code,
    &LoadCommand.LinkedItData
    {
        .header = LoadCommand.LCHeader
        {
            .id = LoadCommand.ID.function_starts,
            .size = @sizeOf(LoadCommand.LinkedItData),
        },
        .data_offset = 16432,
        .data_size = 8,
    });

    add_to_exe(LoadCommand.LinkedItData, &code,
    &LoadCommand.LinkedItData
    {
        .header = LoadCommand.LCHeader
        {
            .id = LoadCommand.ID.data_in_code,
            .size = @sizeOf(LoadCommand.LinkedItData),
        },
        .data_offset = 16440,
        .data_size = 0,
    });

    add_to_exe(LoadCommand.LinkedItData, &code,
    &LoadCommand.LinkedItData
    {
        .header = LoadCommand.LCHeader
        {
            .id = LoadCommand.ID.code_signature,
            .size = @sizeOf(LoadCommand.LinkedItData),
        },
        .data_offset = 16544,
        .data_size = 273,
    });

    print("Executable length so far: {}\n", .{code.items.len});
}

comptime
{
    std.testing.refAllDecls(@This());
}
