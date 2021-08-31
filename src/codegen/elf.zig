const ELF = @This();

const std = @import("std");
const Codegen = @import("../codegen.zig");
const CodeBuffer = Codegen.CodeBuffer;

const FileHeader = extern struct
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
    program_header_offset: u64 = 0x40,
    section_header_offset: u64,
    flags: u32 = 0,
    header_size: u16 = 0x40,
    program_header_size: u16 = @sizeOf(ProgramHeader),
    program_header_entry_count: u16 = 1,
    section_header_size: u16 = @sizeOf(SectionHeader),
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
};

const ProgramHeader = extern struct
{
    type: u32 = @enumToInt(ProgramHeaderType.load),
    flags: u32 = @enumToInt(Flags.readable) | @enumToInt(Flags.executable),
    offset: u64,
    virtual_address: u64,
    physical_address: u64,
    size_in_file: u64,
    size_in_memory: u64,
    alignment: u64 = 0,

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

const SectionHeader = extern struct
{
    name_offset: u32,
    type: u32,
    flags: u64,
    address: u64,
    offset: u64,
    size: u64,
    // section index
    link: u32,
    info: u32,
    alignment: u64,
    entry_size: u64,

    // type
    const ID = enum(u32)
    {
        @"null" = 0,
        program_data = 1,
        symbol_table = 2,
        string_table = 3,
        relocation_entries_addends = 4,
        symbol_hash_table = 5,
        dynamic_linking_info = 6,
        notes = 7,
        program_space_no_data = 8,
        relocation_entries = 9,
        reserved = 10,
        dynamic_linker_symbol_table = 11,
        array_of_constructors = 14,
        array_of_destructors = 15,
        array_of_pre_constructors = 16,
        section_group = 17,
        extended_section_indices = 18,
        number_of_defined_types = 19,
        start_os_specific = 0x60000000,
    };

    const Flag = enum(u64)
    {
        writable = 0x01,
        alloc = 0x02,
        executable = 0x04,
        mergeable = 0x10,
        contains_null_terminated_strings = 0x20,
        info_link = 0x40,
        link_order = 0x80,
        os_non_conforming = 0x100,
        section_group = 0x200,
        tls = 0x400,
        mask_os = 0x0ff00000,
        mask_processor = 0xf0000000,
        ordered = 0x4000000,
        exclude = 0x8000000,
    };
};

pub fn write(allocator: *Allocator, function_buffer: *FunctionBuffer, target: std.Target) void
{
    var aprox_instruction_count: u64 = 0;
    const max_bytes_per_instruction: u8 = 15;

    for (self.functions.items) |function|
    {
        aprox_instruction_count += 6 + function.instructions.items.len;
    }

    const aprox_code_size = aprox_instruction_count * max_bytes_per_instruction;

    log.debug("Aproximate code size: {}\n", .{aprox_code_size});

    const file_header_size = @sizeOf(Elf64.FileHeader);
    const program_header_size = @sizeOf(Elf64.ProgramHeader);
    const section_header_size = @sizeOf(Elf64.SectionHeader);

    assert(file_header_size == 0x40);
    assert(program_header_size == 0x38);
    assert(section_header_size == 0x40);

    // @Info: this has to do with the way I order sections and it's not official in any way
    const sections = comptime blk:
    {
        assert(Section.count == 5);

        const section_names_str = std.enums.directEnumArray(Section, []const u8, 0, .
            {
                .@"null" = "\x00",
                .data = ".data\x00",
                .text = ".text\x00",
                .shstrtab = ".shstrtab\x00",
                .bss = ".bss\x00",
            });

        var section_names: []const u8 = "";
        var section_names_byte_count: u32 = 0;
        var name_offsets: [Section.count]u32 = undefined;

        inline for (section_names_str) |section_name, i|
        {
            section_names = section_names ++ section_name;
            name_offsets[i] = section_names_byte_count;
            section_names_byte_count += @intCast(u32, section_name.len);
        }

        assert(section_names.len == section_names_byte_count);

        break :blk Sections
        {
            .name_offsets = name_offsets,
            .names = section_names,
        };
    };

    // @TODO: turn this into a loop
    const base_address: u64 = 0x08048000;
    const header_offset = file_header_size + program_header_size;
    const data_length = data_buffer.items.len;
    const shstrtab_length = sections.names.len;
    // @TODO: buggy?
    const bss_length = 0;
    const section_headers_size = Section.count * @sizeOf(Elf64.SectionHeader);

    const file_header_value = Elf64.FileHeader
    {
        .entry = undefined, // entry_offset,
        .section_header_offset = undefined, // sections_offset,
        .section_header_entry_count = Section.count,
        .name_section_header_index = @enumToInt(Section.shstrtab),
    };

    const program_header_value = Elf64.ProgramHeader
    {
        .offset = 0,
        .virtual_address = base_address,
        .physical_address = base_address,
        .size_in_file = undefined, // file size
        .size_in_memory = undefined, // file size
    };

    // @Info: we estimate this we don't want any reallocation which makes us have a bad pointer
    const aproximate_file_size = header_offset + data_length + aprox_code_size + shstrtab_length + bss_length + section_headers_size;

    var file_buffer = ArrayList(u8).initCapacity(allocator, aproximate_file_size) catch {
        panic("Error allocating memory for code section buffer\n", .{});
    };

    var code_buffer = CodeBuffer.initCapacity(allocator, aprox_code_size) catch {
        panic("Error allocating the code buffer\n", .{});
    };

    const file_header_offset = file_buffer.items.len;
    file_buffer.appendSlice(std.mem.asBytes(&file_header_value)) catch unreachable;
    const program_header_offset = file_buffer.items.len;
    file_buffer.appendSlice(std.mem.asBytes(&program_header_value)) catch unreachable;

    // @TODO: alignment
    //
    // @Info: data buffer is already initialized; don't need to do anything
    self.data_base_RVA = file_buffer.items.len;
    file_buffer.appendSlice(data_buffer.items) catch unreachable;

    // @TODO: alignment
    self.code_base_RVA = file_buffer.items.len;

    // Encode entry point
    {
        const ir_main_function = self.main_function.ir_ref;

        const function_type = @ptrCast(*IR.FunctionType, ir_main_function.type);
        const return_type = function_type.ret_type;
        const return_void = return_type.id == IR.Type.ID.void;

        const callee_operand = Operand.Relative.function_operand(@ptrCast(*IR.Value, ir_main_function), self);
        const call_main_operands = [1]Operand { callee_operand };

        const call_main = Instruction.create(Mnemonic.call, call_main_operands[0..]);
        encode_instruction(self, &code_buffer, call_main);

        if (return_void)
        {
            // call main
            // mov di, 0
            // xor eax, eax
            // mov al, 60
            // syscall
            //

            const di = Operand
            {
                .value = Operand.Value {
                    .register = Encoding.Register.DI,
                },
                .size = 2,
            };

            const zero_imm16 = Operand
            {
                .value = Operand.Value {
                    .immediate = Operand.Immediate {
                        .imm16 = 0,
                    },
                    },
                .size = 2,
            };

            const mov_di_operands = [2]Operand { di, zero_imm16 };
            const mov_di = Instruction.create(Mnemonic.mov, mov_di_operands[0..]);
            encode_instruction(self, &code_buffer, mov_di);

            const eax = Operand
            {
                .value = Operand.Value {
                    .register = Encoding.Register.A,
                },
                .size = 4,
            };

            const xor_eax_operands = [2]Operand { eax, eax };
            const xor_eax = Instruction.create(Mnemonic.xor, xor_eax_operands[0..]);
            encode_instruction(self, &code_buffer, xor_eax);

            const al = Operand
            {
                .value = Operand.Value {
                    .register = Encoding.Register.A,
                },
                .size = 1,
            };

            const sixty_imm8 = Operand
            {
                .value = Operand.Value {
                    .immediate = Operand.Immediate {
                        .imm8 = 60,
                    },
                    },
                .size = 1,
            };

            const mov_al_60_operands = [2]Operand { al, sixty_imm8 };
            const mov_al_60 = Instruction.create(Mnemonic.mov, mov_al_60_operands[0..]);
            encode_instruction(self, &code_buffer, mov_al_60);

            const syscall = Instruction.create(Mnemonic.syscall, null);
            encode_instruction(self, &code_buffer, syscall);
        }
        else
        {
            // i32 return
            // call main
            // mov edi, eax
            // xor eax, eax
            // mov al, 60
            // syscall

            const edi = Operand
            {
                .value = Operand.Value {
                    .register = Encoding.Register.DI,
                },
                .size = 4,
            };

            const eax = Operand
            {
                .value = Operand.Value {
                    .register = Encoding.Register.A,
                },
                .size = 4,
            };

            const mov_edi_eax_operands = [2]Operand { edi, eax };
            const mov_edi_eax = Instruction.create(Mnemonic.mov, mov_edi_eax_operands[0..]);
            encode_instruction(self, &code_buffer, mov_edi_eax);

            const xor_eax_operands = [2]Operand { eax, eax };
            const xor_eax = Instruction.create(Mnemonic.xor, xor_eax_operands[0..]);
            encode_instruction(self, &code_buffer, xor_eax);

            const al = Operand
            {
                .value = Operand.Value {
                    .register = Encoding.Register.A,
                },
                .size = 1,
            };

            const sixty_imm8 = Operand
            {
                .value = Operand.Value {
                    .immediate = Operand.Immediate {
                        .imm8 = 60,
                    },
                    },
                .size = 1,
            };

            const mov_al_60_operands = [2]Operand { al, sixty_imm8 };
            const mov_al_60 = Instruction.create(Mnemonic.mov, mov_al_60_operands[0..]);
            encode_instruction(self, &code_buffer, mov_al_60);

            const syscall = Instruction.create(Mnemonic.syscall, null);
            encode_instruction(self, &code_buffer, syscall);
        }
    }

    switch (abi)
    {
        .gnu =>
        {
            for (self.functions.items) |function|
            {
                {
                    log.info("Printing function:\n\n", .{});
                    for (function.instructions.items) |instruction, instruction_index|
                    {
                        log.info("{}: {}\n", .{instruction_index, instruction});
                    }
                    log.info("\n\n", .{});
                }

                {
                    var label = &function.labels.items[0];
                    label.patch(&code_buffer);
                }

                if (encode_frame_pointer)
                {
                    log_enc.debug("\nEncoding prologue\n", .{});
                    const rbp = Operand 
                    {
                        .value = Operand.Value {
                            .register = Encoding.Register.BP,
                        },
                        .size = @enumToInt(Operand.Size.bits64),
                    };

                    const rsp = Operand 
                    {
                        .value = Operand.Value {
                            .register = Encoding.Register.SP,
                        },
                        .size = @enumToInt(Operand.Size.bits64),
                    };

                    const rbp_arr = [1]Operand { rbp }; 
                    const push_rbp = Instruction.create(Mnemonic.push, rbp_arr[0..]);
                    const operands = [2]Operand { rbp, rsp };
                    const mov_rbp_rsp = Instruction.create(Mnemonic.mov, operands[0..]);
                    encode_instruction(self, &code_buffer, push_rbp);
                    encode_instruction(self, &code_buffer, mov_rbp_rsp);
                }

                if (function.rsp > 0)
                {
                    const operands = [2]Operand
                    {
                        Operand 
                        {
                            .value = Operand.Value {
                                .register = Encoding.Register.SP,
                            },
                            .size = @enumToInt(Operand.Size.bits64),
                        },
                        Operand.Immediate.get_minimum(function.rsp),
                    };

                    const sub_rsp = Instruction.create(Mnemonic.sub, operands[0..]);
                    encode_instruction(self, &code_buffer, sub_rsp);
                }

                var instruction_index: u64 = 0;
                var label_index: u64 = 1;

                const instruction_count = function.instructions.items.len;
                const label_count = function.labels.items.len;

                log_enc.debug("\nLabel count: {}. Instruction count: {}\n", .{label_count, instruction_count});

                while (label_index < label_count) : (label_index += 1)
                {
                    var label = &function.labels.items[label_index];
                    const label_instruction_index = label.instruction_index;

                    while (instruction_index < label_instruction_index) : (instruction_index += 1)
                    {
                        const instruction = function.instructions.items[instruction_index];
                        assert(instruction.id != Mnemonic.ret);
                        log_enc.debug("Encoding instruction {}\n", .{instruction_index});
                        encode_instruction(self, &code_buffer, instruction);
                    }

                    label.target = @ptrToInt(code_buffer.items.ptr) + code_buffer.items.len;

                    for (label.locations.items) |location|
                    {
                        log_enc.debug("Location\n", .{});
                        // @TODO: this can cause problems
                        const target = @intCast(i64, label.target); 
                        const address_after_instruction = @intCast(i64, location.address_after_instruction);
                        const difference = target - address_after_instruction;
                        assert(difference >= std.math.minInt(i32) and difference <= std.math.maxInt(i32));
                        // ???
                        assert(difference >= 0);
                        const relative_to_be_written = @intCast(i32, difference);
                        log_enc.debug("Address of relative to be written: 0x{x}\n", .{location.relative_to_be_written_address});
                        var patch_target = @intToPtr(*align(1) i32, location.relative_to_be_written_address);
                        patch_target.* = relative_to_be_written;
                        log_enc.debug("\n\n\nWrote relative: {}\n\n\n", .{relative_to_be_written});
                    }

                    log_enc.debug("patch label: 0x{x}. Locations: {}\n", .{label.target, label.locations.items.len});
                }

                while (instruction_index < instruction_count - 1) : (instruction_index += 1)
                {
                    const instruction = function.instructions.items[instruction_index];
                    assert(instruction.id != Mnemonic.ret);
                    encode_instruction(self, &code_buffer, instruction);
                }

                if (function.rsp > 0)
                {
                    const operands = [2]Operand
                    {
                        Operand 
                        {
                            .value = Operand.Value {
                                .register = Encoding.Register.SP,
                            },
                            .size = @enumToInt(Operand.Size.bits64),
                        },
                        Operand.Immediate.get_minimum(function.rsp),
                    };

                    const add_rsp = Instruction.create(Mnemonic.add, operands[0..]);
                    encode_instruction(self, &code_buffer, add_rsp);
                }

                if (encode_frame_pointer)
                {
                    log_enc.debug("Encoding epilogue\n", .{});
                    const rbp = Operand 
                    {
                        .value = Operand.Value {
                            .register = Encoding.Register.BP,
                        },
                        .size = @enumToInt(Operand.Size.bits64),
                    };
                    const rbp_arr = [1]Operand { rbp };
                    const pop_rbp = Instruction.create(Mnemonic.pop, rbp_arr[0..]);
                    encode_instruction(self, &code_buffer, pop_rbp);
                }

                assert(instruction_index == instruction_count - 1);
                const instruction = function.instructions.items[instruction_index];
                assert(instruction.id == Mnemonic.ret);
                encode_instruction(self, &code_buffer, instruction);
            }
        },
        .msvc => panic("MSVC ABI is not implemented\n", .{}),
        else => panic("ni: {}\n", .{abi}),
    }

    const code_size = code_buffer.items.len;
    log.debug("\n\nAproximate code size: {}. Real code size: {}\n", .{aprox_code_size, code_size});

    file_buffer.appendSlice(code_buffer.items) catch unreachable;
    // @TODO: alignment
    const string_table_offset = file_buffer.items.len;
    file_buffer.appendSlice(sections.names) catch unreachable;

    // @TODO: 
    const bss_offset = file_buffer.items.len;

    const section_headers_offset = file_buffer.items.len;

    // can't be computed yet
    const section_headers = blk:
    {
        const data_section_flags = dsf_blk:
        {
            const is_data = data_buffer.items.len != 0;
            if (is_data)
            {
                break :dsf_blk @enumToInt(Elf64.SectionHeader.Flag.alloc) | @enumToInt(Elf64.SectionHeader.Flag.writable);
            }
            else
            {
                break :dsf_blk 0;
            }
        };

        var section_headers = [Section.count]Elf64.SectionHeader
        {
            // null
            std.mem.zeroInit(Elf64.SectionHeader, .{
                .name_offset = sections.name_offsets[0],
                .type = @enumToInt(Elf64.SectionHeader.ID.program_space_no_data),
            }),
            // data
            std.mem.zeroInit(Elf64.SectionHeader, .{
                .name_offset = sections.name_offsets[@enumToInt(Section.data)],
                .type = @enumToInt(Elf64.SectionHeader.ID.program_data),
                .address = base_address + self.data_base_RVA,
                .offset = self.data_base_RVA,
                .size = data_length,
                .flags = data_section_flags,
            }),
            // text
            std.mem.zeroInit(Elf64.SectionHeader, .{
                .name_offset = sections.name_offsets[@enumToInt(Section.text)],
                .type = @enumToInt(Elf64.SectionHeader.ID.program_data),
                .address = base_address + self.code_base_RVA,
                .offset = self.code_base_RVA,
                .size = code_buffer.items.len,
                .flags = @enumToInt(Elf64.SectionHeader.Flag.alloc) | @enumToInt(Elf64.SectionHeader.Flag.executable),
            }),
            // shstrtab
            std.mem.zeroInit(Elf64.SectionHeader, .{
                .name_offset = sections.name_offsets[3],
                .type = @enumToInt(Elf64.SectionHeader.ID.string_table),
                .address = base_address + string_table_offset,
                .offset = string_table_offset,
                .size = shstrtab_length,
            }),
            // bss
            std.mem.zeroInit(Elf64.SectionHeader, .{
                .name_offset = sections.name_offsets[4],
                .type = @enumToInt(Elf64.SectionHeader.ID.program_space_no_data),
                .address = base_address + bss_offset,
                .offset = bss_offset,
                .size = bss_length,
            }),
        };

        break :blk section_headers;
    };

    for (section_headers) |*section_header, section_header_index|
    {
        log.debug("Section header {}: {}\n", .{section_header_index, section_header.*});
        file_buffer.appendSlice(std.mem.asBytes(section_header)) catch unreachable;
    }

    const file_size = file_buffer.items.len;
    assert(file_size <= aproximate_file_size);

    log.debug("File header offset: {}\n", .{file_header_offset});
    log.debug("Program header offset: {}\n", .{program_header_offset});
    log.debug("Data offset: {}\n", .{self.data_base_RVA});
    log.debug("Code offset: {}\n", .{self.code_base_RVA});
    log.debug("String table offset: {}\n", .{string_table_offset});
    log.debug("BSS offset: {}\n", .{bss_offset});
    log.debug("Section header offset: {}\n", .{section_headers_offset});

    log.debug("\nBefore patching:\n", .{});
    for (file_buffer.items[0x0..program_header_offset]) |byte, byte_index|
    {
        if (byte_index % 0x10 == 0)
        {
            log.debug("\n", .{});
        }
        log.debug("{x:0>2} ", .{byte});
    }
    log.debug("\n", .{});

    // @Info: Patch file  and program headers
    var file_header = @ptrCast(*align(1) Elf64.FileHeader, &file_buffer.items[file_header_offset]);
    log.debug("File header before patching: {}\n\n", .{file_header.*});
    file_header.entry = base_address + self.code_base_RVA;
    file_header.section_header_offset = section_headers_offset;
    var program_header = @ptrCast(*align(1) Elf64.ProgramHeader, &file_buffer.items[program_header_offset]);
    log.debug("Program header before patching: {}\n\n", .{program_header.*});
    program_header.size_in_file = file_size;
    program_header.size_in_memory = file_size;

    log.debug("File header after patching: {}\n\n", .{file_header.*});
    log.debug("File header after patching: {}\n\n", .{program_header.*});
    log.debug("\nAfter patching:\n", .{});
    for (file_buffer.items[0x0..program_header_offset]) |byte, byte_index|
    {
        if (byte_index % 0x10 == 0)
        {
            log.debug("\n", .{});
        }
        log.debug("{x:0>2} ", .{byte});
    }

    log.debug("\n", .{});
    log.debug("Writing file {s}; size: {} bytes\n", .{filename, file_size});
    Codegen.write_executable(filename, file_buffer.items);
}
