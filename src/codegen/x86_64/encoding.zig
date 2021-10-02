const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;

pub const Rex = enum(u8)
{
    None = 0,
    Rex = 0x40,
    B = 0x41,
    X = 0x42,
    R = 0x44,
    W = 0x48,
};

pub const SIB = enum(u8)
{
    scale1 = 0b00,
    scale2 = 0b01,
    scale3 = 0b10,
    scale4 = 0b11,
};

pub const Mod = enum(u8)
{
    no_displacement = 0b00,
    displacement8 = 0b01,
    displacement32 = 0b10,
    register = 0b11,
};

pub const Register = enum(u8)
{
    A = 0,
    C = 1,
    D = 2,
    B = 3,
    SP = 4,
    BP = 5,
    SI = 6,
    DI = 7,

    r8 = 8,
    r9 = 9,
    r10 = 10,
    r11 = 11,
    r12 = 12,
    r13 = 13,
    r14 = 14,
    r15 = 15,

    pub const AH: u8 = 4;
    pub const CH: u8 = 5;
    pub const DH: u8 = 6;
    pub const BH: u8 = 7;

    pub const NFlag: u8 = 0b1000;
};

pub const Operand = struct
{
    id: ID,
    size: u8,

    pub const ID = enum
    {
        none,
        register,
        register_A,
        register_or_memory,
        relative,
        memory,
        immediate,
    };

    pub const Combination = struct
    {
        rex: Rex,
        operands: [4]Operand,
        count: u64,

        fn add_operand(self: *Combination, operand: Operand) void
        {
            const len = self.count;
            assert(len < self.operands.len);
            self.operands[len] = operand;
            self.count += 1;
        }
    };

    pub const size_override: u8 = 0x66;
};

pub const Instruction = struct
{
    op_code: [4]u8,
    options: Options,
    operand_combinations: [4]Operand.Combination,
    operand_combination_count: u64,

    fn NoOperandCombination(self: *Instruction) void
    {
        self.operand_combination_count += 1;
    }

    fn OneOperandCombination(self: *Instruction, rex: Rex, operand: Operand.ID, size: u8) void
    {
        const index = self.operand_combination_count;

        self.operand_combinations[index].rex = rex;

        self.operand_combinations[index].add_operand(Operand { .id = operand, .size = size });
        self.operand_combination_count += 1;
    }

    fn RegisterA_Immediate(self: *Instruction, rex: Rex, size1: u8, size2: u8) void
    {
        const index = self.operand_combination_count;

        self.operand_combinations[index].rex = rex;

        self.operand_combinations[index].add_operand(Operand { .id = Operand.ID.register_A, .size = size1 });
        self.operand_combinations[index].add_operand(Operand { .id = Operand.ID.immediate, .size = size2 });
        self.operand_combination_count += 1;
    }

    fn RegisterMemory_Register(self: *Instruction, rex: Rex, size1: u8, size2: u8) void
    {
        const index = self.operand_combination_count;

        self.operand_combinations[index].rex = rex;

        self.operand_combinations[index].add_operand(Operand { .id = Operand.ID.register_or_memory, .size = size1 });
        self.operand_combinations[index].add_operand(Operand { .id = Operand.ID.register, .size = size2 });
        self.operand_combination_count += 1;
    }

    fn Register_RegisterMemory(self: *Instruction, rex: Rex, size1: u8, size2: u8) void
    {
        const index = self.operand_combination_count;

        self.operand_combinations[index].rex = rex;

        self.operand_combinations[index].add_operand(Operand { .id = Operand.ID.register, .size = size1 });
        self.operand_combinations[index].add_operand(Operand { .id = Operand.ID.register_or_memory, .size = size2 });
        self.operand_combination_count += 1;
    }

    fn Register_Memory(self: *Instruction, rex: Rex, size1: u8, size2: u8) void
    {
        const index = self.operand_combination_count;

        self.operand_combinations[index].rex = rex;

        self.operand_combinations[index].add_operand(Operand { .id = Operand.ID.register, .size = size1 });
        self.operand_combinations[index].add_operand(Operand { .id = Operand.ID.memory, .size = size2 });
        self.operand_combination_count += 1;
    }

    fn Register_RegisterMemory_Immediate(self: *Instruction, rex: Rex, size1: u8, size2: u8, size3: u8) void
    {
        const index = self.operand_combination_count;

        self.operand_combinations[index].rex = rex;

        self.operand_combinations[index].add_operand(Operand { .id = Operand.ID.register, .size = size1 });
        self.operand_combinations[index].add_operand(Operand { .id = Operand.ID.register_or_memory, .size = size2 });
        self.operand_combinations[index].add_operand(Operand { .id = Operand.ID.immediate, .size = size3 });
        self.operand_combination_count += 1;
    }

    fn Register_Immediate(self: *Instruction, rex: Rex, size1: u8, size2: u8) void
    {
        const index = self.operand_combination_count;

        self.operand_combinations[index].rex = rex;

        self.operand_combinations[index].add_operand(Operand { .id = Operand.ID.register, .size = size1 });
        self.operand_combinations[index].add_operand(Operand { .id = Operand.ID.immediate, .size = size2 });
        self.operand_combination_count += 1;
    }

    fn RegisterMemory_Immediate(self: *Instruction, rex: Rex, size1: u8, size2: u8) void
    {
        const index = self.operand_combination_count;

        self.operand_combinations[index].rex = rex;

        self.operand_combinations[index].add_operand(Operand { .id = Operand.ID.register_or_memory, .size = size1 });
        self.operand_combinations[index].add_operand(Operand { .id = Operand.ID.immediate, .size = size2 });
        self.operand_combination_count += 1;
    }

    pub const Options = struct
    {
        option: Option,
        digit: u8,
        explicit_byte_size: u8,

        pub const Option = enum
        {
            None,
            Digit,
            Reg,
            OpCodePlusReg,
            ExplicitByteSize,
        };
    };

    pub const ID = enum
    {
        adc,
        adcx,
        add,
        adox,
        @"and",
        andn,
        bextr,
        blsi,
        blsmsk,
        blsr,
        bndcl,
        bndcu,
        bndcn,
        bndldx,
        bndmk,
        bndmov,
        bndstx,
        bsf,
        bsr,
        bswap,
        bt,
        btc,
        btr,
        bts,
        bzhi,
        call,
        cbw,
        cwde,
        cdqe,
        clac,
        clc,
        cld,
        cldemote,
        clflush,
        clflushopt,
        cli,
        clrssbsy,
        clts,
        clwb,
        cmc,
        cmov,
        cmp,
        cmpxchg,
        cmpxchg8,
        cmpxchg16,
        cpuid,
        crc32,
        cwd,
        cdq,
        cqo,
        dec,
        div,
        endbr32,
        endbr64,
        enter,
        hlt,
        idiv,
        imul,
        in,
        inc,
        incssp,
        ins,
        int3,
        int,
        invd,
        invlpg,
        invpcid,
        iret,
        ja,
        jae,
        jb,
        jbe,
        jc,
        jecxz,
        jrcxz,
        je,
        jg,
        jge,
        jl,
        jle,
        jna,
        jnae,
        jnb,
        jnbe,
        jnc,
        jne,
        jng,
        jnge,
        jnl,
        jnle,
        jno,
        jnp,
        jns,
        jnz,
        jo,
        jp,
        jpe,
        jpo,
        js,
        jz,
        jmp,
        lar,
        lds,
        lss,
        les,
        lfs,
        lgs,
        lea,
        leave,
        lfence,
        lgdt,
        lidt,
        lldt,
        lmsw,
        lock,
        lods,
        lodsb,
        lodsw,
        lodsd,
        lodsq,
        loop,
        loope,
        loopne,
        lsl,
        ltr,
        lzcnt,
        mfence,
        mov,
        movcr,
        movdbg,
        movbe,
        movdq,
        movdiri,
        movdir64,
        movq,
        movs,
        movsx,
        movzx,
        mul,
        mulx,
        mwait,
        neg,
        nop,
        not,
        @"or",
        out,
        outs,
        pause,
        pdep,
        pext,
        pop,
        popcnt,
        popf,
        por,
        prefetch,
        prefetchw,
        ptwrite,
        push,
        pushf,
        rotate,
        rdfsbase,
        rdgsbase,
        rdmsr,
        rdpid,
        rdpmc,
        rdrand,
        rdseed,
        rdssp,
        rdtsc,
        rdtscp,
        rep,
        ret,
        rsm,
        rstorssp,
        sahf,
        sal,
        sar,
        shl,
        shr,
        sarx,
        shlx,
        shrx,
        saveprevssp,
        sbb,
        scas,
        seta,
        setae,
        setb,
        setbe,
        setc,
        sete,
        setg,
        setge,
        setl,
        setle,
        setna,
        setnae,
        setnb,
        setnbe,
        setnc,
        setne,
        setng,
        setnge,
        setnl,
        setnle,
        setno,
        setnp,
        setns,
        setnz,
        seto,
        setp,
        setpe,
        setpo,
        sets,
        setssbsy,
        sfence,
        sgdt,
        shld,
        shrd,
        sidt,
        sldt,
        smsw,
        stac,
        stc,
        std,
        sti,
        stos,
        str,
        sub,
        swapgs,
        syscall,
        sysenter,
        sysexit,
        sysret,
        @"test",
        tpause,
        tzcnt,
        ud,
        umonitor,
        umwait,
        wait,
        wbinvd,
        wbnoinvd,
        wrfsbase,
        wrgsbase,
        wrmsr,
        wrss,
        wruss,
        xacquire,
        xrelease,
        xabort,
        xadd,
        xbegin,
        xchg,
        xend,
        xgetbv,
        xlat,
        xor,
        xrstor,
        xrstors,
        xsave,
        xsavec,
        xsaveopt,
        xsaves,
        xsetbv,
        xtest,
    };
};

fn zero_instruction_encoding(comptime encoding_count: u64) [encoding_count]Instruction
{
    var result: [encoding_count]Instruction = std.mem.zeroes([encoding_count]Instruction);
    return result;
}

const add_encoding = blk:
{
    const encoding_count = 9;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x04;
    result[i].RegisterA_Immediate(Rex.None, 1, 1);

    i += 1;

    result[i].op_code[0] = 0x05;
    result[i].RegisterA_Immediate(Rex.None, 2, 2);
    result[i].RegisterA_Immediate(Rex.None, 4, 4);
    result[i].RegisterA_Immediate(Rex.W, 8, 4);

    i += 1;

    result[i].op_code[0] = 0x80;
    result[i].options.option = Instruction.Options.Option.Digit;
    result[i].options.digit = 0;
    result[i].RegisterMemory_Immediate(Rex.None, 1, 1);
    result[i].RegisterMemory_Immediate(Rex.Rex, 1, 1);

    i += 1;

    result[i].op_code[0] = 0x81;
    result[i].options.digit = 0;
    result[i].options.option = Instruction.Options.Option.Digit;
    result[i].RegisterMemory_Immediate(Rex.None, 2, 2);
    result[i].RegisterMemory_Immediate(Rex.None, 4, 4);
    result[i].RegisterMemory_Immediate(Rex.W, 8, 4);

    i += 1;

    result[i].op_code[0] = 0x83;
    result[i].options.digit = 0;
    result[i].options.option = Instruction.Options.Option.Digit;
    result[i].RegisterMemory_Immediate(Rex.None, 2, 1);
    result[i].RegisterMemory_Immediate(Rex.None, 4, 1);
    result[i].RegisterMemory_Immediate(Rex.W, 8, 1);

    i += 1;

    result[i].op_code[0] = 0x00;
    result[i].options.option = Instruction.Options.Option.Reg;
    result[i].RegisterMemory_Register(Rex.None, 1, 1);
    result[i].RegisterMemory_Register(Rex.Rex, 1, 1);

    i += 1;

    result[i].op_code[0] = 0x01;
    result[i].options.option = Instruction.Options.Option.Reg;
    result[i].RegisterMemory_Register(Rex.None, 2, 2);
    result[i].RegisterMemory_Register(Rex.None, 4, 4);
    result[i].RegisterMemory_Register(Rex.W, 8, 8);

    i += 1;

    result[i].op_code[0] = 0x02;
    result[i].options.option = Instruction.Options.Option.Reg;
    result[i].Register_RegisterMemory(Rex.None, 1, 1);
    result[i].Register_RegisterMemory(Rex.Rex, 1, 1);

    i += 1;

    result[i].op_code[0] = 0x03;
    result[i].options.option = Instruction.Options.Option.Reg;
    result[i].Register_RegisterMemory(Rex.None, 2, 2);
    result[i].Register_RegisterMemory(Rex.None, 4, 4);
    result[i].Register_RegisterMemory(Rex.W, 8, 8);

    break :blk result;
};

const call_encoding = blk:
{
    const encoding_count = 2;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;
    result[i].op_code[0] = 0xe8;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 2);
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    i += 1;

    result[i].op_code[0] = 0xff;
    result[i].options.option = Instruction.Options.Option.Digit;
    result[i].options.digit = 2;
    result[i].OneOperandCombination(Rex.None, Operand.ID.register_or_memory, 8);

    break :blk result;
};

const cmp_encoding = blk:
{
    const encoding_count = 9;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x3c;
    result[i].RegisterA_Immediate(Rex.None, 1, 1);

    i += 1;

    result[i].op_code[0] = 0x3d;
    result[i].RegisterA_Immediate(Rex.None, 2, 2);
    result[i].RegisterA_Immediate(Rex.None, 4, 4);
    result[i].RegisterA_Immediate(Rex.W, 8, 8);

    i += 1;

    result[i].op_code[0] = 0x80;
    result[i].options.option = Instruction.Options.Option.Digit;
    result[i].options.digit = 7;
    result[i].RegisterMemory_Immediate(Rex.None, 1, 1);
    result[i].RegisterMemory_Immediate(Rex.Rex, 1, 1);

    i += 1;

    result[i].op_code[0] = 0x81;
    result[i].options.digit = 7;
    result[i].options.option = Instruction.Options.Option.Digit;
    result[i].RegisterMemory_Immediate(Rex.None, 2, 2);
    result[i].RegisterMemory_Immediate(Rex.None, 4, 4);
    result[i].RegisterMemory_Immediate(Rex.W, 8, 4);

    i += 1;

    result[i].op_code[0] = 0x83;
    result[i].options.digit = 7;
    result[i].options.option = Instruction.Options.Option.Digit;
    result[i].RegisterMemory_Immediate(Rex.None, 2, 1);
    result[i].RegisterMemory_Immediate(Rex.None, 4, 1);
    result[i].RegisterMemory_Immediate(Rex.W, 8, 1);

    i += 1;

    result[i].op_code[0] = 0x38;
    result[i].options.option = Instruction.Options.Option.Reg;
    result[i].RegisterMemory_Register(Rex.None, 1, 1);
    result[i].RegisterMemory_Register(Rex.Rex, 1, 1);

    i += 1;

    result[i].op_code[0] = 0x39;
    result[i].options.option = Instruction.Options.Option.Reg;
    result[i].RegisterMemory_Register(Rex.None, 2, 2);
    result[i].RegisterMemory_Register(Rex.None, 4, 4);
    result[i].RegisterMemory_Register(Rex.W, 8, 8);

    i += 1;

    result[i].op_code[0] = 0x3a;
    result[i].options.option = Instruction.Options.Option.Reg;
    result[i].Register_RegisterMemory(Rex.None, 1, 1);
    result[i].Register_RegisterMemory(Rex.Rex, 1, 1);

    i += 1;

    result[i].op_code[0] = 0x3b;
    result[i].options.option = Instruction.Options.Option.Reg;
    result[i].Register_RegisterMemory(Rex.None, 2, 2);
    result[i].Register_RegisterMemory(Rex.None, 4, 4);
    result[i].Register_RegisterMemory(Rex.W, 8, 8);

    break :blk result;
};

const imul_encoding = blk:
{
    const encoding_count = 5;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0xf6;
    result[i].options.option = Instruction.Options.Option.Digit;
    result[i].options.digit = 5;
    result[i].OneOperandCombination(Rex.None, Operand.ID.register_or_memory, 1);

    i += 1;

    result[i].op_code[0] = 0xf7;
    result[i].options.option = Instruction.Options.Option.Digit;
    result[i].options.digit = 5;
    result[i].OneOperandCombination(Rex.None, Operand.ID.register_or_memory, 2);
    result[i].OneOperandCombination(Rex.None, Operand.ID.register_or_memory, 4);
    result[i].OneOperandCombination(Rex.W, Operand.ID.register_or_memory, 8);

    i += 1;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0xaf;
    result[i].options.option = Instruction.Options.Option.Reg;
    result[i].Register_RegisterMemory(Rex.None, 2, 2);
    result[i].Register_RegisterMemory(Rex.None, 4, 4);
    result[i].Register_RegisterMemory(Rex.W, 8, 8);

    i += 1;

    result[i].op_code[0] = 0x6b;
    result[i].options.option = Instruction.Options.Option.Reg;
    result[i].Register_RegisterMemory_Immediate(Rex.None, 2, 2, 1);
    result[i].Register_RegisterMemory_Immediate(Rex.None, 4, 4, 1);
    result[i].Register_RegisterMemory_Immediate(Rex.W, 8, 8, 1);

    i += 1;

    result[i].op_code[0] = 0x69;
    result[i].options.option = Instruction.Options.Option.Reg;
    result[i].Register_RegisterMemory_Immediate(Rex.None, 2, 2, 2);
    result[i].Register_RegisterMemory_Immediate(Rex.None, 4, 4, 4);
    result[i].Register_RegisterMemory_Immediate(Rex.W, 8, 8, 4);

    break :blk result;
};

const ja_encoding = blk:
{
    const encoding_count = 2;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x77;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    i += 1;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0x87;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    break :blk result;
};

const jae_encoding = blk:
{
    const encoding_count = 2;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x73;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    i += 1;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0x83;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    break :blk result;
};

const jb_encoding = blk:
{
    const encoding_count = 2;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x72;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    i += 1;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0x82;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    break :blk result;
};

const jbe_encoding = blk:
{
    const encoding_count = 2;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x76;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    i += 1;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0x86;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    break :blk result;
};

const jc_encoding = blk:
{
    const encoding_count = 2;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x72;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    i += 1;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0x82;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    break :blk result;
};

const jecxz_encoding = blk:
{
    const encoding_count = 1;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0xe3;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    break :blk result;
};

const jrcxz_encoding = blk:
{
    const encoding_count = 1;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0xe3;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    break :blk result;
};

const je_encoding = blk:
{
    const encoding_count = 2;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x74;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    i += 1;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0x84;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    break :blk result;
};

const jg_encoding = blk:
{
    const encoding_count = 2;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x7f;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    i += 1;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0x8f;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    break :blk result;
};

const jge_encoding = blk:
{
    const encoding_count = 2;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x7d;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    i += 1;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0x8d;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    break :blk result;
};

const jl_encoding = blk:
{
    const encoding_count = 2;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x7c;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    i += 1;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0x8c;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    break :blk result;
};

const jle_encoding = blk:
{
    const encoding_count = 2;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x7e;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    i += 1;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0x8e;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    break :blk result;
};

const jna_encoding = blk:
{
    const encoding_count = 2;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x76;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    i += 1;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0x86;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    break :blk result;
};

const jnae_encoding = blk:
{
    const encoding_count = 2;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x72;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    i += 1;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0x82;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    break :blk result;
};

const jnb_encoding = blk:
{
    const encoding_count = 2;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x73;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    i += 1;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0x83;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    break :blk result;
};

const jnbe_encoding = blk:
{
    const encoding_count = 2;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x77;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    i += 1;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0x87;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    break :blk result;
};

const jnc_encoding = blk:
{
    const encoding_count = 2;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x73;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    i += 1;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0x83;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    break :blk result;
};

const jne_encoding = blk:
{
    const encoding_count = 2;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x77;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    i += 1;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0x85;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    break :blk result;
};

const jng_encoding = blk:
{
    const encoding_count = 2;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x7e;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    i += 1;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0x8e;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    break :blk result;
};

const jnge_encoding = blk:
{
    const encoding_count = 2;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x7c;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    i += 1;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0x8c;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    break :blk result;
};

const jnl_encoding = blk:
{
    const encoding_count = 2;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x7d;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    i += 1;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0x8d;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    break :blk result;
};

const jnle_encoding = blk:
{
    const encoding_count = 2;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x7f;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    i += 1;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0x8f;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    break :blk result;
};

const jno_encoding = blk:
{
    const encoding_count = 2;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x71;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    i += 1;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0x81;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    break :blk result;
};

const jnp_encoding = blk:
{
    const encoding_count = 2;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x7b;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    i += 1;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0x8b;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    break :blk result;
};

const jns_encoding = blk:
{
    const encoding_count = 2;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x79;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    i += 1;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0x89;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    break :blk result;
};

const jnz_encoding = blk:
{
    const encoding_count = 2;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x75;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    i += 1;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0x85;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    break :blk result;
};

const jo_encoding = blk:
{
    const encoding_count = 2;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x70;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    i += 1;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0x80;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    break :blk result;
};

const jp_encoding = blk:
{
    const encoding_count = 2;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x7a;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    i += 1;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0x8a;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    break :blk result;
};

const jpe_encoding = blk:
{
    const encoding_count = 2;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x7a;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    i += 1;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0x8a;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    break :blk result;
};

const jpo_encoding = blk:
{
    const encoding_count = 2;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x7b;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    i += 1;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0x8b;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    break :blk result;
};

const js_encoding = blk:
{
    const encoding_count = 2;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x78;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    i += 1;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0x88;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    break :blk result;
};

const jz_encoding = blk:
{
    const encoding_count = 2;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x74;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    i += 1;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0x84;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    break :blk result;
};

const jmp_encoding = blk:
{
    const encoding_count = 3;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0xeb;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 1);

    i += 1;

    result[i].op_code[0] = 0xe9;
    result[i].OneOperandCombination(Rex.None, Operand.ID.relative, 4);

    i += 1;

    result[i].op_code[0] = 0xff;
    result[i].options.option = Instruction.Options.Option.Digit;
    result[i].options.digit = 4;
    result[i].OneOperandCombination(Rex.None, Operand.ID.register_or_memory, 8);

    break :blk result;
};

const lea_encoding = blk:
{
    const encoding_count = 3;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x8d;
    result[i].options.option = Instruction.Options.Option.Reg;
    result[i].Register_Memory(Rex.None, 2, 8);
    result[i].Register_Memory(Rex.None, 4, 8);
    result[i].Register_Memory(Rex.W, 8, 8);

    break :blk result;
};

const mov_encoding = blk:
{
    const encoding_count = 8;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;
    result[i].op_code[0] = 0x88;
    result[i].options.option = Instruction.Options.Option.Reg;
    result[i].RegisterMemory_Register(Rex.None, 1, 1);
    result[i].RegisterMemory_Register(Rex.Rex, 1, 1);

    i += 1;

    result[i].op_code[0] = 0x89;
    result[i].options.option = Instruction.Options.Option.Reg;
    result[i].RegisterMemory_Register(Rex.None, 2, 2);
    result[i].RegisterMemory_Register(Rex.None, 4, 4);
    result[i].RegisterMemory_Register(Rex.W, 8, 8);

    i += 1;

    result[i].op_code[0] = 0x8a;
    result[i].options.option = Instruction.Options.Option.Reg;
    result[i].Register_RegisterMemory(Rex.None, 1, 1);
    result[i].Register_RegisterMemory(Rex.Rex, 1, 1);

    i += 1;

    result[i].op_code[0] = 0x8b;
    result[i].options.option = Instruction.Options.Option.Reg;
    result[i].Register_RegisterMemory(Rex.None, 2, 2);
    result[i].Register_RegisterMemory(Rex.None, 4, 4);
    result[i].Register_RegisterMemory(Rex.W, 8, 8);

    i += 1;

    //  @TODO: NOT CODED SEGMENT AND OFFSET INSTRUCTIONS

    result[i].op_code[0] = 0xb0;
    result[i].options.option = Instruction.Options.Option.OpCodePlusReg;
    result[i].Register_Immediate(Rex.None, 1, 1);
    result[i].Register_Immediate(Rex.Rex, 1, 1);

    i += 1;

    result[i].op_code[0] = 0xb8;
    result[i].options.option = Instruction.Options.Option.OpCodePlusReg;
    result[i].Register_Immediate(Rex.None, 2, 2);
    result[i].Register_Immediate(Rex.None, 4, 4);
    result[i].Register_Immediate(Rex.W, 8, 8);
    
    i += 1;

    result[i].op_code[0] = 0xc6;
    result[i].options.option = Instruction.Options.Option.Digit;
    result[i].options.digit = 0;
    result[i].RegisterMemory_Immediate(Rex.None, 1, 1);
    result[i].RegisterMemory_Immediate(Rex.Rex, 1, 1);

    i += 1;

    result[i].op_code[0] = 0xc7;
    result[i].options.digit = 0;
    result[i].options.option = Instruction.Options.Option.Digit;
    result[i].RegisterMemory_Immediate(Rex.None, 2, 2);
    result[i].RegisterMemory_Immediate(Rex.None, 4, 4);
    result[i].RegisterMemory_Immediate(Rex.W, 8, 8);
    
    i += 1;

    break :blk result;
};

const pop_encoding = blk:
{
    const encoding_count = 3;
    var result = zero_instruction_encoding(encoding_count);

    var i: u64 = 0;

    result[i].op_code[0] = 0x58;
    result[i].options.option = Instruction.Options.Option.OpCodePlusReg;
    result[i].OneOperandCombination(Rex.None, Operand.ID.register, 2);
    result[i].OneOperandCombination(Rex.None, Operand.ID.register, 8);

    i += 1;

    result[i].op_code[0] = 0x8f;
    result[i].options.option = Instruction.Options.Option.Digit;
    result[i].options.digit = 6;
    result[i].OneOperandCombination(Rex.None, Operand.ID.register_or_memory, 2);
    result[i].OneOperandCombination(Rex.None, Operand.ID.register_or_memory, 8);

    i += 1;

    result[i].op_code[0] = 0x6a;
    result[i].OneOperandCombination(Rex.None, Operand.ID.immediate, 1);

    // @TODO: pop fs, pop gs

    break :blk result;
};

const push_encoding = blk:
{
    const encoding_count = 4;
    var result = zero_instruction_encoding(encoding_count);

    var i: u64 = 0;

    result[i].op_code[0] = 0x50;
    result[i].options.option = Instruction.Options.Option.OpCodePlusReg;
    result[i].OneOperandCombination(Rex.None, Operand.ID.register, 2);
    result[i].OneOperandCombination(Rex.None, Operand.ID.register, 8);

    i += 1;

    result[i].op_code[0] = 0xff;
    result[i].options.option = Instruction.Options.Option.Digit;
    result[i].options.digit = 6;
    result[i].OneOperandCombination(Rex.None, Operand.ID.register_or_memory, 2);
    result[i].OneOperandCombination(Rex.None, Operand.ID.register_or_memory, 8);

    i += 1;

    result[i].op_code[0] = 0x6a;
    result[i].OneOperandCombination(Rex.None, Operand.ID.immediate, 1);

    i += 1;

    result[i].op_code[0] = 0x68;
    result[i].OneOperandCombination(Rex.None, Operand.ID.immediate, 2);
    result[i].OneOperandCombination(Rex.None, Operand.ID.immediate, 4);

    break :blk result;
};

const ret_encoding = blk:
{
    const encoding_count = 4;
    var result = zero_instruction_encoding(encoding_count);

    var i: u64 = 0;

    result[i].op_code[0] = 0xc3;
    result[i].NoOperandCombination();

    i += 1;

    result[i].op_code[0] = 0xcb;
    result[i].NoOperandCombination();

    i += 1;

    result[i].op_code[0] = 0xc2;
    result[i].OneOperandCombination(Rex.None, Operand.ID.immediate, 2);

    i += 1;

    result[i].op_code[0] = 0xca;
    result[i].OneOperandCombination(Rex.None, Operand.ID.immediate, 2);

    break :blk result;
};

const sub_encoding = blk:
{
    const encoding_count = 9;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x2c;
    result[i].RegisterA_Immediate(Rex.None, 1, 1);

    i += 1;

    result[i].op_code[0] = 0x2d;
    result[i].RegisterA_Immediate(Rex.None, 2, 2);
    result[i].RegisterA_Immediate(Rex.None, 4, 4);
    result[i].RegisterA_Immediate(Rex.W, 8, 4);

    i += 1;

    result[i].op_code[0] = 0x80;
    result[i].options.option = Instruction.Options.Option.Digit;
    result[i].options.digit = 5;
    result[i].RegisterMemory_Immediate(Rex.None, 1, 1);
    result[i].RegisterMemory_Immediate(Rex.Rex, 1, 1);

    i += 1;

    result[i].op_code[0] = 0x81;
    result[i].options.digit = 5;
    result[i].options.option = Instruction.Options.Option.Digit;
    result[i].RegisterMemory_Immediate(Rex.None, 2, 2);
    result[i].RegisterMemory_Immediate(Rex.None, 4, 4);
    result[i].RegisterMemory_Immediate(Rex.W, 8, 4);

    i += 1;

    result[i].op_code[0] = 0x83;
    result[i].options.digit = 5;
    result[i].options.option = Instruction.Options.Option.Digit;
    result[i].RegisterMemory_Immediate(Rex.None, 2, 1);
    result[i].RegisterMemory_Immediate(Rex.None, 4, 1);
    result[i].RegisterMemory_Immediate(Rex.W, 8, 1);

    i += 1;

    result[i].op_code[0] = 0x28;
    result[i].options.option = Instruction.Options.Option.Reg;
    result[i].RegisterMemory_Register(Rex.None, 1, 1);
    result[i].RegisterMemory_Register(Rex.Rex, 1, 1);

    i += 1;

    result[i].op_code[0] = 0x29;
    result[i].options.option = Instruction.Options.Option.Reg;
    result[i].RegisterMemory_Register(Rex.None, 2, 2);
    result[i].RegisterMemory_Register(Rex.None, 4, 4);
    result[i].RegisterMemory_Register(Rex.W, 8, 8);

    i += 1;

    result[i].op_code[0] = 0x2a;
    result[i].options.option = Instruction.Options.Option.Reg;
    result[i].Register_RegisterMemory(Rex.None, 1, 1);
    result[i].Register_RegisterMemory(Rex.Rex, 1, 1);

    i += 1;

    result[i].op_code[0] = 0x2b;
    result[i].options.option = Instruction.Options.Option.Reg;
    result[i].Register_RegisterMemory(Rex.None, 2, 2);
    result[i].Register_RegisterMemory(Rex.None, 4, 4);
    result[i].Register_RegisterMemory(Rex.W, 8, 8);

    break :blk result;
};

const syscall_encoding = blk:
{
    const encoding_count = 9;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x0f;
    result[i].op_code[1] = 0x05;
    result[i].NoOperandCombination();

    break :blk result;
};

const xor_encoding = blk:
{
    const encoding_count = 9;
    var result = zero_instruction_encoding(encoding_count);

    var i = 0;

    result[i].op_code[0] = 0x34;
    result[i].RegisterA_Immediate(Rex.None, 1, 1);

    i += 1;

    result[i].op_code[0] = 0x35;
    result[i].RegisterA_Immediate(Rex.None, 2, 2);
    result[i].RegisterA_Immediate(Rex.None, 4, 4);
    result[i].RegisterA_Immediate(Rex.W, 8, 4);

    i += 1;

    result[i].op_code[0] = 0x80;
    result[i].options.option = Instruction.Options.Option.Digit;
    result[i].options.digit = 6;
    result[i].RegisterMemory_Immediate(Rex.None, 1, 1);
    result[i].RegisterMemory_Immediate(Rex.Rex, 1, 1);

    i += 1;

    result[i].op_code[0] = 0x81;
    result[i].options.digit = 6;
    result[i].options.option = Instruction.Options.Option.Digit;
    result[i].RegisterMemory_Immediate(Rex.None, 2, 2);
    result[i].RegisterMemory_Immediate(Rex.None, 4, 4);
    result[i].RegisterMemory_Immediate(Rex.W, 8, 4);

    i += 1;

    result[i].op_code[0] = 0x83;
    result[i].options.digit = 6;
    result[i].options.option = Instruction.Options.Option.Digit;
    result[i].RegisterMemory_Immediate(Rex.None, 2, 1);
    result[i].RegisterMemory_Immediate(Rex.None, 4, 1);
    result[i].RegisterMemory_Immediate(Rex.W, 8, 1);

    i += 1;

    result[i].op_code[0] = 0x30;
    result[i].options.option = Instruction.Options.Option.Reg;
    result[i].RegisterMemory_Register(Rex.None, 1, 1);
    result[i].RegisterMemory_Register(Rex.Rex, 1, 1);

    i += 1;

    result[i].op_code[0] = 0x31;
    result[i].options.option = Instruction.Options.Option.Reg;
    result[i].RegisterMemory_Register(Rex.None, 2, 2);
    result[i].RegisterMemory_Register(Rex.None, 4, 4);
    result[i].RegisterMemory_Register(Rex.W, 8, 8);

    i += 1;

    result[i].op_code[0] = 0x32;
    result[i].options.option = Instruction.Options.Option.Reg;
    result[i].Register_RegisterMemory(Rex.None, 1, 1);
    result[i].Register_RegisterMemory(Rex.Rex, 1, 1);

    i += 1;

    result[i].op_code[0] = 0x33;
    result[i].options.option = Instruction.Options.Option.Reg;
    result[i].Register_RegisterMemory(Rex.None, 2, 2);
    result[i].Register_RegisterMemory(Rex.None, 4, 4);
    result[i].Register_RegisterMemory(Rex.W, 8, 8);

    break :blk result;
};

pub const instructions = blk:
{
    @setEvalBranchQuota(10_000);
    const result = std.enums.directEnumArray(Instruction.ID, []const Instruction, 0, .
    {
        .adc = undefined,
        .adcx = undefined,
        .add = add_encoding[0..],
        .adox = undefined,
        .@"and" = undefined,
        .andn = undefined,
        .bextr = undefined,
        .blsi = undefined,
        .blsmsk = undefined,
        .blsr = undefined,
        .bndcl = undefined,
        .bndcu = undefined,
        .bndcn = undefined,
        .bndldx = undefined,
        .bndmk = undefined,
        .bndmov = undefined,
        .bndstx = undefined,
        .bsf = undefined,
        .bsr = undefined,
        .bswap = undefined,
        .bt = undefined,
        .btc = undefined,
        .btr = undefined,
        .bts = undefined,
        .bzhi = undefined,
        .call = call_encoding[0..],
        .cbw = undefined,
        .cwde = undefined,
        .cdqe = undefined,
        .clac = undefined,
        .clc = undefined,
        .cld = undefined,
        .cldemote = undefined,
        .clflush = undefined,
        .clflushopt = undefined,
        .cli = undefined,
        .clrssbsy = undefined,
        .clts = undefined,
        .clwb = undefined,
        .cmc = undefined,
        .cmov = undefined,
        .cmp = cmp_encoding[0..],
        .cmpxchg = undefined,
        .cmpxchg8 = undefined,
        .cmpxchg16 = undefined,
        .cpuid = undefined,
        .crc32 = undefined,
        .cwd = undefined,
        .cdq = undefined,
        .cqo = undefined,
        .dec = undefined,
        .div = undefined,
        .endbr32 = undefined,
        .endbr64 = undefined,
        .enter = undefined,
        .hlt = undefined,
        .idiv = undefined,
        .imul = imul_encoding[0..],
        .in = undefined,
        .inc = undefined,
        .incssp = undefined,
        .ins = undefined,
        .int3 = undefined,
        .int = undefined,
        .invd = undefined,
        .invlpg = undefined,
        .invpcid = undefined,
        .iret = undefined,
        .ja = ja_encoding[0..],
        .jae = jae_encoding[0..],
        .jb = jb_encoding[0..],
        .jbe = jbe_encoding[0..],
        .jc = jc_encoding[0..],
        .jecxz = jecxz_encoding[0..],
        .jrcxz = jrcxz_encoding[0..],
        .je = je_encoding[0..],
        .jg = jg_encoding[0..],
        .jge = jge_encoding[0..],
        .jl = jl_encoding[0..],
        .jle = jle_encoding[0..],
        .jna = jna_encoding[0..],
        .jnae = jnae_encoding[0..],
        .jnb = jnb_encoding[0..],
        .jnbe = jnbe_encoding[0..],
        .jnc = jnc_encoding[0..],
        .jne = jne_encoding[0..],
        .jng = jng_encoding[0..],
        .jnge = jnge_encoding[0..],
        .jnl = jnl_encoding[0..],
        .jnle = jnle_encoding[0..],
        .jno = jno_encoding[0..],
        .jnp = jnp_encoding[0..],
        .jns = jns_encoding[0..],
        .jnz = jnz_encoding[0..],
        .jo = jo_encoding[0..],
        .jp = jp_encoding[0..],
        .jpe = jpe_encoding[0..],
        .jpo = jpo_encoding[0..],
        .js = js_encoding[0..],
        .jz = jz_encoding[0..],
        .jmp = jmp_encoding[0..],
        .lar = undefined,
        .lds = undefined,
        .lss = undefined,
        .les = undefined,
        .lfs = undefined,
        .lgs = undefined,
        .lea = lea_encoding[0..],
        .leave = undefined,
        .lfence = undefined,
        .lgdt = undefined,
        .lidt = undefined,
        .lldt = undefined,
        .lmsw = undefined,
        .lock = undefined,
        .lods = undefined,
        .lodsb = undefined,
        .lodsw = undefined,
        .lodsd = undefined,
        .lodsq = undefined,
        .loop = undefined,
        .loope = undefined,
        .loopne = undefined,
        .lsl = undefined,
        .ltr = undefined,
        .lzcnt = undefined,
        .mfence = undefined,
        .mov = mov_encoding[0..],
        .movcr = undefined,
        .movdbg = undefined,
        .movbe = undefined,
        .movdq = undefined,
        .movdiri = undefined,
        .movdir64 = undefined,
        .movq = undefined,
        .movs = undefined,
        .movsx = undefined,
        .movzx = undefined,
        .mul = undefined,
        .mulx = undefined,
        .mwait = undefined,
        .neg = undefined,
        .nop = undefined,
        .not = undefined,
        .@"or" = undefined,
        .out = undefined,
        .outs = undefined,
        .pause = undefined,
        .pdep = undefined,
        .pext = undefined,
        .pop = pop_encoding[0..],
        .popcnt = undefined,
        .popf = undefined,
        .por = undefined,
        .prefetch = undefined,
        .prefetchw = undefined,
        .ptwrite = undefined,
        .push = push_encoding[0..],
        .pushf = undefined,
        .rotate = undefined,
        .rdfsbase = undefined,
        .rdgsbase = undefined,
        .rdmsr = undefined,
        .rdpid = undefined,
        .rdpmc = undefined,
        .rdrand = undefined,
        .rdseed = undefined,
        .rdssp = undefined,
        .rdtsc = undefined,
        .rdtscp = undefined,
        .rep = undefined,
        .ret = ret_encoding[0..],
        .rsm = undefined,
        .rstorssp = undefined,
        .sahf = undefined,
        .sal = undefined,
        .sar = undefined,
        .shl = undefined,
        .shr = undefined,
        .sarx = undefined,
        .shlx = undefined,
        .shrx = undefined,
        .saveprevssp = undefined,
        .sbb = undefined,
        .scas = undefined,
        .seta = undefined,
        .setae = undefined,
        .setb = undefined,
        .setbe = undefined,
        .setc = undefined,
        .sete = undefined,
        .setg = undefined,
        .setge = undefined,
        .setl = undefined,
        .setle = undefined,
        .setna = undefined,
        .setnae = undefined,
        .setnb = undefined,
        .setnbe = undefined,
        .setnc = undefined,
        .setne = undefined,
        .setng = undefined,
        .setnge = undefined,
        .setnl = undefined,
        .setnle = undefined,
        .setno = undefined,
        .setnp = undefined,
        .setns = undefined,
        .setnz = undefined,
        .seto = undefined,
        .setp = undefined,
        .setpe = undefined,
        .setpo = undefined,
        .sets = undefined,
        .setssbsy = undefined,
        .sfence = undefined,
        .sgdt = undefined,
        .shld = undefined,
        .shrd = undefined,
        .sidt = undefined,
        .sldt = undefined,
        .smsw = undefined,
        .stac = undefined,
        .stc = undefined,
        .std = undefined,
        .sti = undefined,
        .stos = undefined,
        .str = undefined,
        .sub = sub_encoding[0..],
        .swapgs = undefined,
        .syscall = syscall_encoding[0..],
        .sysenter = undefined,
        .sysexit = undefined,
        .sysret = undefined,
        .@"test" = undefined,
        .tpause = undefined,
        .tzcnt = undefined,
        .ud = undefined,
        .umonitor = undefined,
        .umwait = undefined,
        .wait = undefined,
        .wbinvd = undefined,
        .wbnoinvd = undefined,
        .wrfsbase = undefined,
        .wrgsbase = undefined,
        .wrmsr = undefined,
        .wrss = undefined,
        .wruss = undefined,
        .xacquire = undefined,
        .xrelease = undefined,
        .xabort = undefined,
        .xadd = undefined,
        .xbegin = undefined,
        .xchg = undefined,
        .xend = undefined,
        .xgetbv = undefined,
        .xlat = undefined,
        .xor = xor_encoding[0..],
        .xrstor = undefined,
        .xrstors = undefined,
        .xsave = undefined,
        .xsavec = undefined,
        .xsaveopt = undefined,
        .xsaves = undefined,
        .xsetbv = undefined,
        .xtest = undefined,
    });

    break :blk result;
};
