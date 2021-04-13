const std = @import("std");
const Allocator = std.mem.Allocator;

const Type = struct {
    name: []const u8,
    id: ID,

    const ID = enum {
        @"void",
        @"label",
        @"integer",
        @"float",
        @"pointer",
        @"vector",
        @"struct",
        @"array",
        @"function",
    };
};

const FloatType = struct {
    base: Type,
    bits: u16,
};

const IntegerType = struct {
    base: Type,
    bits: u16,
};

const PointerType = struct {
    base: Type,
    type: *Type,
};

const StructType = struct {
    base: Type,
    field_types: []*Type,
};

const ArrayType = struct {
    base: Type,
    type: *Type,
    count: usize,
};

const FunctionType = struct {
    base: Type,
    arg_types: []*Type,
    ret_type: *Type,
};

const Value = struct {
    type: *Type,
    id: ID,

    const ID = enum {
        Undefined,
        Argument,
        BasicBlock,
        InlineASM,
        Metadata,
        BlockAddress,
        ConstantArray,
        ConstantStruct,
        ConstantVector,
        ConstantAggregateZero,
        ConstantDataArray,
        ConstantDataVector,
        ConstantFP,
        ConstantInt,
        ConstantPointerNull,
        ConstExprBinary, // not exposed
        ConstExprCompare,
        ConstExprExtractElement,
        ConstExprExtractValue,
        ConstExprGetElementPtr,
        ConstExprInsertElement,
        ConstExprInsertValue,
        ConstExprSelect,
        ConstExprShuffleVector,
        ConstExprUnary,
        GlobalIndirectFunction,
        GlobalIndirectAlias,
        GlobalFunction,
        GlobalVariable,
        MemoryAccess,
        Instruction,
        Intrinsic, // In this we defer from llvm, since LLVM doesn't have a Intrinsic category, but they are call instruction children
        Operator,
        OperatorAddrSpaceCast,
        OperatorBitCast,
        OperatorGEP,
        OperatorPointerToInt,
        OperatorZeroExtend,
        OperatorFPMathOperator,
        // @TODO: figure out Signed-wrapping
    };
};

const ConstantArray = struct {
    base: Value,
    array_type: *Type,
    array_values: []*Value,
};

const Intrinsic = struct {
    base: Value,
    id: ID,

    const ID = enum(u16) {
        addressofreturnaddress = 1, // llvm.addressofreturnaddress
        adjust_trampoline, // llvm.adjust.trampoline
        annotation, // llvm.annotation
        assume, // llvm.assume
        bitreverse, // llvm.bitreverse
        bswap, // llvm.bswap
        call_preallocated_arg, // llvm.call.preallocated.arg
        call_preallocated_setup, // llvm.call.preallocated.setup
        call_preallocated_teardown, // llvm.call.preallocated.teardown
        canonicalize, // llvm.canonicalize
        ceil, // llvm.ceil
        clear_cache, // llvm.clear_cache
        codeview_annotation, // llvm.codeview.annotation
        convert_from_fp16, // llvm.convert.from.fp16
        convert_to_fp16, // llvm.convert.to.fp16
        copysign, // llvm.copysign
        coro_alloc, // llvm.coro.alloc
        coro_alloca_alloc, // llvm.coro.alloca.alloc
        coro_alloca_free, // llvm.coro.alloca.free
        coro_alloca_get, // llvm.coro.alloca.get
        coro_begin, // llvm.coro.begin
        coro_destroy, // llvm.coro.destroy
        coro_done, // llvm.coro.done
        coro_end, // llvm.coro.end
        coro_frame, // llvm.coro.frame
        coro_free, // llvm.coro.free
        coro_id, // llvm.coro.id
        coro_id_retcon, // llvm.coro.id.retcon
        coro_id_retcon_once, // llvm.coro.id.retcon.once
        coro_noop, // llvm.coro.noop
        coro_param, // llvm.coro.param
        coro_prepare_retcon, // llvm.coro.prepare.retcon
        coro_promise, // llvm.coro.promise
        coro_resume, // llvm.coro.resume
        coro_save, // llvm.coro.save
        coro_size, // llvm.coro.size
        coro_subfn_addr, // llvm.coro.subfn.addr
        coro_suspend, // llvm.coro.suspend
        coro_suspend_retcon, // llvm.coro.suspend.retcon
        cos, // llvm.cos
        ctlz, // llvm.ctlz
        ctpop, // llvm.ctpop
        cttz, // llvm.cttz
        dbg_addr, // llvm.dbg.addr
        dbg_declare, // llvm.dbg.declare
        dbg_label, // llvm.dbg.label
        dbg_value, // llvm.dbg.value
        debugtrap, // llvm.debugtrap
        donothing, // llvm.donothing
        eh_dwarf_cfa, // llvm.eh.dwarf.cfa
        eh_exceptioncode, // llvm.eh.exceptioncode
        eh_exceptionpointer, // llvm.eh.exceptionpointer
        eh_recoverfp, // llvm.eh.recoverfp
        eh_return_i32, // llvm.eh.return.i32
        eh_return_i64, // llvm.eh.return.i64
        eh_sjlj_callsite, // llvm.eh.sjlj.callsite
        eh_sjlj_functioncontext, // llvm.eh.sjlj.functioncontext
        eh_sjlj_longjmp, // llvm.eh.sjlj.longjmp
        eh_sjlj_lsda, // llvm.eh.sjlj.lsda
        eh_sjlj_setjmp, // llvm.eh.sjlj.setjmp
        eh_sjlj_setup_dispatch, // llvm.eh.sjlj.setup.dispatch
        eh_typeid_for, // llvm.eh.typeid.for
        eh_unwind_init, // llvm.eh.unwind.init
        exp, // llvm.exp
        exp2, // llvm.exp2
        expect, // llvm.expect
        expect_with_probability, // llvm.expect.with.probability
        experimental_constrained_ceil, // llvm.experimental.constrained.ceil
        experimental_constrained_cos, // llvm.experimental.constrained.cos
        experimental_constrained_exp, // llvm.experimental.constrained.exp
        experimental_constrained_exp2, // llvm.experimental.constrained.exp2
        experimental_constrained_fadd, // llvm.experimental.constrained.fadd
        experimental_constrained_fcmp, // llvm.experimental.constrained.fcmp
        experimental_constrained_fcmps, // llvm.experimental.constrained.fcmps
        experimental_constrained_fdiv, // llvm.experimental.constrained.fdiv
        experimental_constrained_floor, // llvm.experimental.constrained.floor
        experimental_constrained_fma, // llvm.experimental.constrained.fma
        experimental_constrained_fmul, // llvm.experimental.constrained.fmul
        experimental_constrained_fmuladd, // llvm.experimental.constrained.fmuladd
        experimental_constrained_fpext, // llvm.experimental.constrained.fpext
        experimental_constrained_fptosi, // llvm.experimental.constrained.fptosi
        experimental_constrained_fptoui, // llvm.experimental.constrained.fptoui
        experimental_constrained_fptrunc, // llvm.experimental.constrained.fptrunc
        experimental_constrained_frem, // llvm.experimental.constrained.frem
        experimental_constrained_fsub, // llvm.experimental.constrained.fsub
        experimental_constrained_llrint, // llvm.experimental.constrained.llrint
        experimental_constrained_llround, // llvm.experimental.constrained.llround
        experimental_constrained_log, // llvm.experimental.constrained.log
        experimental_constrained_log10, // llvm.experimental.constrained.log10
        experimental_constrained_log2, // llvm.experimental.constrained.log2
        experimental_constrained_lrint, // llvm.experimental.constrained.lrint
        experimental_constrained_lround, // llvm.experimental.constrained.lround
        experimental_constrained_maximum, // llvm.experimental.constrained.maximum
        experimental_constrained_maxnum, // llvm.experimental.constrained.maxnum
        experimental_constrained_minimum, // llvm.experimental.constrained.minimum
        experimental_constrained_minnum, // llvm.experimental.constrained.minnum
        experimental_constrained_nearbyint, // llvm.experimental.constrained.nearbyint
        experimental_constrained_pow, // llvm.experimental.constrained.pow
        experimental_constrained_powi, // llvm.experimental.constrained.powi
        experimental_constrained_rint, // llvm.experimental.constrained.rint
        experimental_constrained_round, // llvm.experimental.constrained.round
        experimental_constrained_roundeven, // llvm.experimental.constrained.roundeven
        experimental_constrained_sin, // llvm.experimental.constrained.sin
        experimental_constrained_sitofp, // llvm.experimental.constrained.sitofp
        experimental_constrained_sqrt, // llvm.experimental.constrained.sqrt
        experimental_constrained_trunc, // llvm.experimental.constrained.trunc
        experimental_constrained_uitofp, // llvm.experimental.constrained.uitofp
        experimental_deoptimize, // llvm.experimental.deoptimize
        experimental_gc_relocate, // llvm.experimental.gc.relocate
        experimental_gc_result, // llvm.experimental.gc.result
        experimental_gc_statepoint, // llvm.experimental.gc.statepoint
        experimental_guard, // llvm.experimental.guard
        experimental_patchpoint_i64, // llvm.experimental.patchpoint.i64
        experimental_patchpoint_void, // llvm.experimental.patchpoint.void
        experimental_stackmap, // llvm.experimental.stackmap
        experimental_vector_reduce_add, // llvm.experimental.vector.reduce.add
        experimental_vector_reduce_and, // llvm.experimental.vector.reduce.and
        experimental_vector_reduce_fmax, // llvm.experimental.vector.reduce.fmax
        experimental_vector_reduce_fmin, // llvm.experimental.vector.reduce.fmin
        experimental_vector_reduce_mul, // llvm.experimental.vector.reduce.mul
        experimental_vector_reduce_or, // llvm.experimental.vector.reduce.or
        experimental_vector_reduce_smax, // llvm.experimental.vector.reduce.smax
        experimental_vector_reduce_smin, // llvm.experimental.vector.reduce.smin
        experimental_vector_reduce_umax, // llvm.experimental.vector.reduce.umax
        experimental_vector_reduce_umin, // llvm.experimental.vector.reduce.umin
        experimental_vector_reduce_v2_fadd, // llvm.experimental.vector.reduce.v2.fadd
        experimental_vector_reduce_v2_fmul, // llvm.experimental.vector.reduce.v2.fmul
        experimental_vector_reduce_xor, // llvm.experimental.vector.reduce.xor
        experimental_widenable_condition, // llvm.experimental.widenable.condition
        fabs, // llvm.fabs
        floor, // llvm.floor
        flt_rounds, // llvm.flt.rounds
        fma, // llvm.fma
        fmuladd, // llvm.fmuladd
        frameaddress, // llvm.frameaddress
        fshl, // llvm.fshl
        fshr, // llvm.fshr
        gcread, // llvm.gcread
        gcroot, // llvm.gcroot
        gcwrite, // llvm.gcwrite
        get_active_lane_mask, // llvm.get.active.lane.mask
        get_dynamic_area_offset, // llvm.get.dynamic.area.offset
        hwasan_check_memaccess, // llvm.hwasan.check.memaccess
        hwasan_check_memaccess_shortgranules, // llvm.hwasan.check.memaccess.shortgranules
        icall_branch_funnel, // llvm.icall.branch.funnel
        init_trampoline, // llvm.init.trampoline
        instrprof_increment, // llvm.instrprof.increment
        instrprof_increment_step, // llvm.instrprof.increment.step
        instrprof_value_profile, // llvm.instrprof.value.profile
        invariant_end, // llvm.invariant.end
        invariant_start, // llvm.invariant.start
        is_constant, // llvm.is.constant
        launder_invariant_group, // llvm.launder.invariant.group
        lifetime_end, // llvm.lifetime.end
        lifetime_start, // llvm.lifetime.start
        llrint, // llvm.llrint
        llround, // llvm.llround
        load_relative, // llvm.load.relative
        localaddress, // llvm.localaddress
        localescape, // llvm.localescape
        localrecover, // llvm.localrecover
        log, // llvm.log
        log10, // llvm.log10
        log2, // llvm.log2
        loop_decrement, // llvm.loop.decrement
        loop_decrement_reg, // llvm.loop.decrement.reg
        lrint, // llvm.lrint
        lround, // llvm.lround
        masked_compressstore, // llvm.masked.compressstore
        masked_expandload, // llvm.masked.expandload
        masked_gather, // llvm.masked.gather
        masked_load, // llvm.masked.load
        masked_scatter, // llvm.masked.scatter
        masked_store, // llvm.masked.store
        matrix_column_major_load, // llvm.matrix.column.major.load
        matrix_column_major_store, // llvm.matrix.column.major.store
        matrix_multiply, // llvm.matrix.multiply
        matrix_transpose, // llvm.matrix.transpose
        maximum, // llvm.maximum
        maxnum, // llvm.maxnum
        memcpy, // llvm.memcpy
        memcpy_element_unordered_atomic, // llvm.memcpy.element.unordered.atomic
        memcpy_inline, // llvm.memcpy.inline
        memmove, // llvm.memmove
        memmove_element_unordered_atomic, // llvm.memmove.element.unordered.atomic
        memset, // llvm.memset
        memset_element_unordered_atomic, // llvm.memset.element.unordered.atomic
        minimum, // llvm.minimum
        minnum, // llvm.minnum
        nearbyint, // llvm.nearbyint
        objc_arc_annotation_bottomup_bbend, // llvm.objc.arc.annotation.bottomup.bbend
        objc_arc_annotation_bottomup_bbstart, // llvm.objc.arc.annotation.bottomup.bbstart
        objc_arc_annotation_topdown_bbend, // llvm.objc.arc.annotation.topdown.bbend
        objc_arc_annotation_topdown_bbstart, // llvm.objc.arc.annotation.topdown.bbstart
        objc_autorelease, // llvm.objc.autorelease
        objc_autoreleasePoolPop, // llvm.objc.autoreleasePoolPop
        objc_autoreleasePoolPush, // llvm.objc.autoreleasePoolPush
        objc_autoreleaseReturnValue, // llvm.objc.autoreleaseReturnValue
        objc_clang_arc_use, // llvm.objc.clang.arc.use
        objc_copyWeak, // llvm.objc.copyWeak
        objc_destroyWeak, // llvm.objc.destroyWeak
        objc_initWeak, // llvm.objc.initWeak
        objc_loadWeak, // llvm.objc.loadWeak
        objc_loadWeakRetained, // llvm.objc.loadWeakRetained
        objc_moveWeak, // llvm.objc.moveWeak
        objc_release, // llvm.objc.release
        objc_retain, // llvm.objc.retain
        objc_retain_autorelease, // llvm.objc.retain.autorelease
        objc_retainAutorelease, // llvm.objc.retainAutorelease
        objc_retainAutoreleaseReturnValue, // llvm.objc.retainAutoreleaseReturnValue
        objc_retainAutoreleasedReturnValue, // llvm.objc.retainAutoreleasedReturnValue
        objc_retainBlock, // llvm.objc.retainBlock
        objc_retainedObject, // llvm.objc.retainedObject
        objc_storeStrong, // llvm.objc.storeStrong
        objc_storeWeak, // llvm.objc.storeWeak
        objc_sync_enter, // llvm.objc.sync.enter
        objc_sync_exit, // llvm.objc.sync.exit
        objc_unretainedObject, // llvm.objc.unretainedObject
        objc_unretainedPointer, // llvm.objc.unretainedPointer
        objc_unsafeClaimAutoreleasedReturnValue, // llvm.objc.unsafeClaimAutoreleasedReturnValue
        objectsize, // llvm.objectsize
        pcmarker, // llvm.pcmarker
        pow, // llvm.pow
        powi, // llvm.powi
        prefetch, // llvm.prefetch
        preserve_array_access_index, // llvm.preserve.array.access.index
        preserve_struct_access_index, // llvm.preserve.struct.access.index
        preserve_union_access_index, // llvm.preserve.union.access.index
        ptr_annotation, // llvm.ptr.annotation
        ptrmask, // llvm.ptrmask
        read_register, // llvm.read_register
        read_volatile_register, // llvm.read_volatile_register
        readcyclecounter, // llvm.readcyclecounter
        returnaddress, // llvm.returnaddress
        rint, // llvm.rint
        round, // llvm.round
        roundeven, // llvm.roundeven
        sadd_sat, // llvm.sadd.sat
        sadd_with_overflow, // llvm.sadd.with.overflow
        sdiv_fix, // llvm.sdiv.fix
        sdiv_fix_sat, // llvm.sdiv.fix.sat
        set_loop_iterations, // llvm.set.loop.iterations
        sideeffect, // llvm.sideeffect
        sin, // llvm.sin
        smul_fix, // llvm.smul.fix
        smul_fix_sat, // llvm.smul.fix.sat
        smul_with_overflow, // llvm.smul.with.overflow
        sponentry, // llvm.sponentry
        sqrt, // llvm.sqrt
        ssa_copy, // llvm.ssa.copy
        ssub_sat, // llvm.ssub.sat
        ssub_with_overflow, // llvm.ssub.with.overflow
        stackguard, // llvm.stackguard
        stackprotector, // llvm.stackprotector
        stackrestore, // llvm.stackrestore
        stacksave, // llvm.stacksave
        strip_invariant_group, // llvm.strip.invariant.group
        test_set_loop_iterations, // llvm.test.set.loop.iterations
        thread_pointer, // llvm.thread.pointer
        trap, // llvm.trap
        trunc, // llvm.trunc
        type_checked_load, // llvm.type.checked.load
        type_test, // llvm.type.test
        uadd_sat, // llvm.uadd.sat
        uadd_with_overflow, // llvm.uadd.with.overflow
        udiv_fix, // llvm.udiv.fix
        udiv_fix_sat, // llvm.udiv.fix.sat
        umul_fix, // llvm.umul.fix
        umul_fix_sat, // llvm.umul.fix.sat
        umul_with_overflow, // llvm.umul.with.overflow
        usub_sat, // llvm.usub.sat
        usub_with_overflow, // llvm.usub.with.overflow
        vacopy, // llvm.va_copy
        vaend, // llvm.va_end
        vastart, // llvm.va_start
        var_annotation, // llvm.var.annotation
        vp_add, // llvm.vp.add
        vp_and, // llvm.vp.and
        vp_ashr, // llvm.vp.ashr
        vp_lshr, // llvm.vp.lshr
        vp_mul, // llvm.vp.mul
        vp_or, // llvm.vp.or
        vp_sdiv, // llvm.vp.sdiv
        vp_shl, // llvm.vp.shl
        vp_srem, // llvm.vp.srem
        vp_sub, // llvm.vp.sub
        vp_udiv, // llvm.vp.udiv
        vp_urem, // llvm.vp.urem
        vp_xor, // llvm.vp.xor
        vscale, // llvm.vscale
        write_register, // llvm.write_register
        xray_customevent, // llvm.xray.customevent
        xray_typedevent, // llvm.xray.typedevent
        num_intrinsics = 8052
    };
};

const ConstantInt = struct{
    base: Value,
    int_value: u64,
    bit_count: u32,
    is_signed: bool,
};

const OperatorBitCast = struct{
    base: Value,
    cast_value: *Value,
};

pub const Module = struct {
    functions: std.ArrayList(Function),
};

const Function = struct {
    base: Value,
    name: []const u8,
    type: *Type,
    basic_blocks: std.ArrayList(*BasicBlock),
    arguments: []Argument,
    parent: *Module,

    const Argument = struct {
        base: Value,
        arg_index: usize,
    };
};

const Instruction = struct{
    base: Value,
    id: ID,
    parent: *BasicBlock,

    const ID = enum {
        // Terminator
        Ret = 1,
        Br = 2,
        Switch_ = 3,
        Indirectbr = 4,
        Invoke = 5,
        Resume = 6,
        Unreachable = 7,
        Cleanup_ret = 8,
        Catch_ret = 9,
        Catch_switch = 10,
        Call_br = 11,

        // Unary
        Fneg = 12,

        // Binary
        Add = 13,
        Fadd = 14,
        Sub = 15,
        Fsub = 16,
        Mul = 17,
        Fmul = 18,
        Udiv = 19,
        Sdiv = 20,
        Fdiv = 21,
        Urem = 22,
        Srem = 23,
        Frem = 24,

        // Logical
        Shl = 25,
        Lshr = 26,
        Ashr = 27,
        And = 28,
        Or = 29,
        Xor = 30,

        // Memory
        Alloca = 31,
        Load = 32,
        Store = 33,
        GetElementPtr = 34,
        Fence = 35,
        AtomicCmpXchg = 36,
        AtomicRMW = 37,

        // Cast
        Trunc = 38,
        ZExt = 39,
        SExt = 40,
        FPToUI = 41,
        FPToSI = 42,
        UIToFP = 43,
        SIToFP = 44,
        FPTrunc = 45,
        FPExt = 46,
        PtrToInt = 47,
        IntToPtr = 48,
        BitCast = 49,
        AddrSpaceCast = 50,

        // FuncLetPad
        CleanupPad = 51,
        CatchPad = 52,

        // Other
        ICmp = 53,
        FCmp = 54,
        Phi = 55,
        Call = 56,
        Select = 57,
        UserOp1 = 58,
        UserOp2 = 59,
        VAArg = 60,
        ExtractElement = 61,
        InsertElement = 62,
        ShuffleVector = 63,
        ExtractValue = 64,
        InsertValue = 65,
        LandingPad = 66,
    };
};

const BasicBlock = struct {
    base: Value,
    parent: *Function,
    instructions: std.ArrayList(*Instruction),
    use_count: u32,
};

const Builder = struct {
    context: *Context,
    current: *BasicBlock,
    function: *Function,
    basic_block_buffer: *std.ArrayList(BasicBlock),
    instruction_buffer: *std.ArrayList(Instruction),
    module: *Module,
    next_alloca_index: usize,
    return_alloca: *Instruction,
    exit_block: *BasicBlock,
    conditional_alloca: bool,
    emitted_return: bool,
    explicit_return: bool,
};

const Context = struct {
    void_type: Type,
    label_type: Type,
    i1_type: Type,
    i8_type: Type,
    i16_type: Type,
    i32_type: Type,
    i64_type: Type,
    f32_type: Type,
    f64_type: Type,

    function_types: std.ArrayList(FunctionType),
    array_types: std.ArrayList(ArrayType),
    pointer_types: std.ArrayList(PointerType),
    constant_arrays: std.ArrayList(ConstantArray),
    constant_ints: std.ArrayList(ConstantInt),
    intrinsics: std.ArrayList(Intrinsic),

    fn create() Context {
        var context : Context = undefined;
        context.void_type = Type { .name = "void", .id = Type.ID.@"void" };

        return context;
    }
};

pub fn encode(allocator: *Allocator) void {
    var module = Module {
        .functions = std.ArrayList(Function).init(allocator),
    };

    var context = Context.create();
}
