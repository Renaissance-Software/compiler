const std = @import("std");
const panic = std.debug.panic;

const Rex = enum(u8)
{
    None = 0,
    Rex = 0x40,
    B = 0x41,
    X = 0x42,
    R = 0x44,
    W = 0x48,
};

const SIB = enum(u8)
{
    scale1 = 0b00,
    scale2 = 0b01,
    scale3 = 0b10,
    scale4 = 0b11,
};

const Mod = enum(u8)
{
    no_displacement = 0b00,
    displacement8 = 0b01,
    displacement32 = 0b10,
    register = 0b11,
};

const Register = enum(u8)
{
    A = 0,
    C = 1,
    D = 2,
    B = 3,
    SP = 4,
    BP = 5,
    SI = 6,
    DI = 7,
    AH = SP,
    CH = BP,
    DH = SI,
    BH = DI,

    r8 = 8,
    r9 = 9,
    r10 = 10,
    r11 = 11,
    r12 = 12,
    r13 = 13,
    r14 = 14,
    r15 = 15,
};



pub const Operand = struct
{
    id: ID,
    size: u8,

    const ID = enum
    {
        none,
        register,
        register_A,
        register_or_memory,
        relative,
        memory,
        immediate,
    };

    const Combination = struct
    {
        rex: Rex,
        encodings: [4]Operand,
    };

    const size_override: u8 = 0x66;
};


pub const Mnemonic = extern struct
{
};

pub const Instruction = struct
{
    op_code: [4]u8,
    options: Options,
    operand_combinations: [4]Operand.Combination,

    const Options = struct
    {
        id: ID,
        digit: u8,
        explicit_byte_size: u8,

        const ID = enum
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

const ret_encoding : [_]Instruction = blk:
{
    var instructions: [4]Instruction = std.mem.zeroes([4]Instruction);

    instructions[0].op_code[0] = 0xc3;
    instructions[1].op_code[0] = 0xcb;
    instructions[2].op_code[0] = 0xc2;
    instructions[2].operand_combinations[0].encodings[0] = Operand { .id = Operand.ID.immediate, .size = 16 };
    instructions[3].op_code[0] = 0xca;
    instructions[3].operand_combinations[0].encodings[0] = Operand { .id = Operand.ID.immediate, .size = 16 };

    break :blk instructions;
};

pub const encodings = blk:
{
    @setEvalBranchQuota(10_000);
    const result = std.enums.directEnumArray(Instruction.ID, []const Instruction, 0, .
    {
        .adc = undefined,
        .adcx = undefined,
        .add = undefined,
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
        .call = undefined,
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
        .cmp = undefined,
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
        .imul = undefined,
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
        .ja = undefined,
        .jae = undefined,
        .jb = undefined,
        .jbe = undefined,
        .jc = undefined,
        .jecxz = undefined,
        .jrcxz = undefined,
        .je = undefined,
        .jg = undefined,
        .jge = undefined,
        .jl = undefined,
        .jle = undefined,
        .jna = undefined,
        .jnae = undefined,
        .jnb = undefined,
        .jnbe = undefined,
        .jnc = undefined,
        .jne = undefined,
        .jng = undefined,
        .jnge = undefined,
        .jnl = undefined,
        .jnle = undefined,
        .jno = undefined,
        .jnp = undefined,
        .jns = undefined,
        .jnz = undefined,
        .jo = undefined,
        .jp = undefined,
        .jpe = undefined,
        .jpo = undefined,
        .js = undefined,
        .jz = undefined,
        .jmp = undefined,
        .lar = undefined,
        .lds = undefined,
        .lss = undefined,
        .les = undefined,
        .lfs = undefined,
        .lgs = undefined,
        .lea = undefined,
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
        .mov = undefined,
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
        .pop = undefined,
        .popcnt = undefined,
        .popf = undefined,
        .por = undefined,
        .prefetch = undefined,
        .prefetchw = undefined,
        .ptwrite = undefined,
        .push = undefined,
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
        .sub = undefined,
        .swapgs = undefined,
        .syscall = undefined,
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
        .xor = undefined,
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
