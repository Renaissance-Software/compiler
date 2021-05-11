const std = @import("std");
const assert = std.debug.assert;
const panic = std.debug.panic;
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const BucketArrayList = @import("bucket_array.zig").BucketArrayList;

const Internal = @import("compiler.zig");
const Compiler = Internal.Compiler;
const Log = Compiler.LogLevel;

const Parser = @import("parser.zig");
const Node = Parser.Node;
const BinaryOp = Parser.BinaryExpression.ID;
const UnaryOp = Parser.UnaryExpression.ID;

const Semantics = @import("semantics.zig");
const SemanticsResult = Semantics.SemanticsResult;

const Type = struct
{
    name: []const u8,
    id: ID,

    const ID = enum
    {
        void,
        label,
        integer,
        float,
        pointer,
        vector,
        @"struct",
        array,
        function,
    };

    pub fn format(self: *const Type, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void
    {
        switch (self.id)
        {
            Type.ID.pointer => 
            {
                const pointer_type = @ptrCast(*const PointerType, self);
                // @TODO: this can be buggy, use std.fmt.format?
                try pointer_type.type.format("{}", options, writer);
                try writer.writeAll("*");
            },
            Type.ID.integer =>
            {
                const integer_type = @ptrCast(*const IntegerType, self);
                const bits = integer_type.bits;
                try std.fmt.format(writer, "i{}", .{bits});
            },
            Type.ID.void =>
            {
                try writer.writeAll("void");
            },
            Type.ID.array =>
            {
                const array_type = @ptrCast(*const ArrayType, self);
                const length = array_type.count;
                const arr_type = array_type.type;
                try std.fmt.format(writer, "[{} x {}]", .{length, arr_type});
            },
            Type.ID.@"struct" =>
            {
                const struct_type = @ptrCast(*const StructType, self);
                try std.fmt.format(writer, "%struct.{s}", .{struct_type.name});
            },
            else =>
            {
                panic("Not implemented: {}\n", .{self.id});
            }
        }
    }
};

const FloatType = struct
{
    base: Type,
    bits: u16,
};

const IntegerType = struct
{
    base: Type,
    bits: u16,
};

const PointerType = struct
{
    base: Type,
    type: *Type,
};

const StructType = struct
{
    base: Type,
    field_types: []*Type,
    name: []const u8,
};

const ArrayType = struct
{
    base: Type,
    type: *Type,
    count: usize,
};

const FunctionType = struct
{
    base: Type,
    arg_types: []*Type,
    ret_type: *Type,
};

pub const Value = struct
{
    type: *Type,
    id: ID,
    parent: *Value,

    pub const ID = enum
    {
        Undefined,
        Module,
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

    pub fn format(self: *const Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void
    {
        switch (self.id)
        {
            Value.ID.ConstantInt =>
            {
                const constant_int = @ptrCast(*const ConstantInt, self);
                try std.fmt.format(writer, "{}", .{constant_int});
            },
            Value.ID.Instruction =>
            {
                const instruction = @ptrCast(*const Instruction, self);
                switch (instruction.id)
                {
                    else =>
                    {
                        panic("Instruction value printing not implemented: {}\n", .{instruction.id});
                    }
                }
            },
            Value.ID.GlobalFunction =>
            {
                const function = @ptrCast(*const Function, self);
                try std.fmt.format(writer, "{} @{s}", .{function.base.type, function.name});
            },
            Value.ID.Argument =>
            {
                const argument = @ptrCast(*const Function.Argument, self);
                try std.fmt.format(writer, "{}", .{argument.*});
            },
            Value.ID.Intrinsic =>
            {
                const intrinsic = @ptrCast(*const Intrinsic, self);
                try std.fmt.format(writer, "{}", .{intrinsic.*});
            },
            Value.ID.OperatorBitCast =>
            {
                const bitcast_operator = @ptrCast(*const OperatorBitCast, self);
                try std.fmt.format(writer, "{}", .{bitcast_operator.*});
            },
            Value.ID.ConstantArray =>
            {
                const constant_array = @ptrCast(*const ConstantArray, self);
                try std.fmt.format(writer, "{}", .{constant_array.*});
            },
            else =>
            {
                panic("Not implemented: {}\n", .{self.id});
            }
        }
    }
};

const CompareType = extern enum(u32)
{
    // Opcode            U L G E    Intuitive operation
    FCMP_FALSE = 0, //< 0 0 0 0    Always false (always folded)
    FCMP_OEQ = 1,   //< 0 0 0 1    True if ordered and equal
    FCMP_OGT = 2,   //< 0 0 1 0    True if ordered and greater than
    FCMP_OGE = 3,   //< 0 0 1 1    True if ordered and greater than or equal
    FCMP_OLT = 4,   //< 0 1 0 0    True if ordered and less than
    FCMP_OLE = 5,   //< 0 1 0 1    True if ordered and less than or equal
    FCMP_ONE = 6,   //< 0 1 1 0    True if ordered and operands are unequal
    FCMP_ORD = 7,   //< 0 1 1 1    True if ordered (no nans)
    FCMP_UNO = 8,   //< 1 0 0 0    True if unordered: isnan(X) | isnan(Y)
    FCMP_UEQ = 9,   //< 1 0 0 1    True if unordered or equal
    FCMP_UGT = 10,  //< 1 0 1 0    True if unordered or greater than
    FCMP_UGE = 11,  //< 1 0 1 1    True if unordered, greater than, or equal
    FCMP_ULT = 12,  //< 1 1 0 0    True if unordered or less than
    FCMP_ULE = 13,  //< 1 1 0 1    True if unordered, less than, or equal
    FCMP_UNE = 14,  //< 1 1 1 0    True if unordered or not equal
    FCMP_TRUE = 15, //< 1 1 1 1    Always true (always folded)
    FIRST_FCMP_PREDICATE = 15,
    LAST_FCMP_PREDICATE = 15,
    BAD_FCMP_PREDICATE = 15 + 1,
    ICMP_EQ = 32,  //< equal
    ICMP_NE = 33,  //< not equal
    ICMP_UGT = 34, //< unsigned greater than
    ICMP_UGE = 35, //< unsigned greater or equal
    ICMP_ULT = 36, //< unsigned less than
    ICMP_ULE = 37, //< unsigned less or equal
    ICMP_SGT = 38, //< signed greater than
    ICMP_SGE = 39, //< signed greater or equal
    ICMP_SLT = 40, //< signed less than
    ICMP_SLE = 41, //< signed less or equal
    FIRST_ICMP_PREDICATE = 32,
    LAST_ICMP_PREDICATE = 41,
    BAD_ICMP_PREDICATE = 41 + 1,

    const Self = @This();

    pub fn format(self: Self, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void
    {
        switch (self)
        {
            CompareType.ICMP_EQ =>
            {
                try writer.writeAll("eq");
            },
            CompareType.ICMP_SLE =>
            {
                try writer.writeAll("sle");
            },
            CompareType.ICMP_SGE =>
            {
                try writer.writeAll("sge");
            },
            CompareType.ICMP_SGT =>
            {
                try writer.writeAll("sgt");
            },
            else =>
            {
                panic("Not implemented: {}\n", .{self});
            }
        }
    }
};

const ConstantArray = struct
{
    base: Value,
    array_type: *Type,
    array_values: []*Value,

    pub fn format(self: ConstantArray, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void
    {
        const lvalue = self.base.parent;
        assert(lvalue.id == Value.ID.Instruction);
        const instruction = @ptrCast(*Instruction, lvalue);
        assert(instruction.id == Instruction.ID.Alloca);

        var it: *Value = self.base.parent;
        while (true)
        {
            if (it.id == Value.ID.GlobalFunction)
            {
                break;
            }
            it = it.parent;
        }

        const function = @ptrCast(*Function, it);

        try std.fmt.format(writer, "@__const.{s}.%{}", .{function.name, @ptrToInt(lvalue)});
    }
};

const Intrinsic = struct
{
    base: Value,
    id: ID,

    const ID = enum(u16)
    {
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

    pub fn format(self: *const Intrinsic, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void
    {
        try std.fmt.format(writer, "{} @intrinsic.{s}", .{self.base.type, @tagName(self.id)});
    }
};

pub const ConstantInt = struct
{
    base: Value,
    int_value: u64,
    bit_count: u32,
    is_signed: bool,

    pub fn format(self: *const ConstantInt, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void
    {
        // @TODO: is this supposed to display the type as well? Should this have bit count?
        if (self.is_signed)
        {
            try std.fmt.format(writer, "i{} -{}", .{self.bit_count, self.int_value});
        }
        else
        {
            try std.fmt.format(writer, "i{} {}", .{self.bit_count, self.int_value});
        }
    }
};

const OperatorBitCast = struct
{
    base: Value,
    cast_value: *Value,
    pub fn format(self: *const OperatorBitCast, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void
    {
        switch (self.cast_value.id)
        {
            Value.ID.ConstantArray =>
            {
                const constant_array = @ptrCast(*ConstantArray, self.cast_value);
                try std.fmt.format(writer, "{} bitcast ({}* {} to {})", .{self.base.type, self.cast_value.type, constant_array, self.base.type});
            },
            else =>
            {
                try std.fmt.format(writer, "operator bitcast value id not implemented: {}", .{self.cast_value.id});
                panic("operator bitcast value id not implemented: {}\n", .{self.cast_value.id});
            },
        }
    }
};

const function_bucket_count = 64;
const FunctionBuffer = BucketArrayList(Function, function_bucket_count);
pub const Module = struct
{
    base: Value,
    functions: FunctionBuffer,

    fn find_function(self: *Module, name: []const u8) *Function
    {
        assert(name.len > 0);
        for (self.functions.list.items) |function_bucket|
        {
            for (function_bucket.items) |*function|
            {
                if (std.mem.eql(u8, name, function.name))
                {
                    return function;
                }
            }
        }

        panic("Function not found\n", .{});
    }
};

const Function = struct
{
    base: Value,
    name: []const u8,
    type: *Type,
    basic_blocks: ArrayList(*BasicBlock),
    arguments: []Argument,

    const Argument = struct
    {
        base: Value,
        arg_index: usize,

        pub fn format(self: Argument, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void
        {
            try std.fmt.format(writer, "{} %{}", .{self.base.type, self.arg_index});
        }
    };

    fn create(allocator: *Allocator, module: *Module, type_expr: *Type, name: []const u8) void
    {
        const function_type = @ptrCast(*FunctionType, type_expr);
        const ret_type = function_type.ret_type;

        const function_value = Function {
            .base = Value {
                .type = ret_type,
                .id = Value.ID.GlobalFunction,
                .parent = @ptrCast(*Value, module),
            },
            .basic_blocks = ArrayList(*BasicBlock).init(allocator),
            .arguments = undefined, // @Info: this is defined later as the arguments are collected, and not in the function declaration
            .name = name,
            .type = type_expr,
        };
        _ = module.functions.append(function_value) catch |err| {
            panic("Cannot allocate memory for bytecode function\n", .{});
        };
    }
};

pub const Instruction = struct
{
    base: Value,
    id: ID,
    value: InstructionValue,
    operands: ArrayList(*Value),

    pub const InstructionValue = extern union {
        alloca: Alloca,
        compare_type: CompareType,
    };

    pub const Alloca = struct 
    {
        type: *Type,
    };

    pub const ID = enum {
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

pub const BasicBlock = struct
{
    base: Value,
    instructions: ArrayList(*Instruction),
    use_count: u32,
};

const BlockBuffer = BucketArrayList(BasicBlock, 64);
const InstructionBuffer = BucketArrayList(Instruction, 64);

const Builder = struct
{
    context: *Context,
    current: *BasicBlock,
    function: *Function,

    basic_block_buffer: *BlockBuffer,
    instruction_buffer: *InstructionBuffer,

    module: *Module,
    next_alloca_index: usize,
    return_alloca: ?*Instruction,
    exit_block: ?*BasicBlock,
    conditional_alloca: bool,
    emitted_return: bool,
    explicit_return: bool,

    fn create_block(self: *Builder, allocator: *Allocator) *BasicBlock
    {
        const basic_block_value = BasicBlock {
            .base = Value {
                .type = self.context.get_label_type(),
                .id = Value.ID.BasicBlock,
                .parent = @ptrCast(*Value, self.function),
            },
            .instructions = ArrayList(*Instruction).init(allocator),
            .use_count = 0,
        };

        const result = self.basic_block_buffer.append(basic_block_value) catch |err| {
            panic("Couldn't allocate memory for basic block\n", .{});
        };
        return result;
    }

    fn append_to_current_function(self: *Builder, basic_block: *BasicBlock) void
    {
        self.function.basic_blocks.append(basic_block) catch |err|{
            panic("Couldn't allocate memory for basic block reference in function\n", .{});
        };
        basic_block.base.parent = @ptrCast(*Value, self.function);
    }

    fn set_block(self: *Builder, block: *BasicBlock) void
    {
        const previous = self.current;
        assert(@ptrToInt(previous) != @ptrToInt(block));
        self.current = block;
    }

    fn create_alloca(self: *Builder, alloca_type: *Type, array_size: ?*Value) *Instruction
    {
        assert(array_size == null);
        var entry_block = self.function.basic_blocks.items[0];

        const instruction = Instruction {
            .base = Value {
                .type = self.context.get_pointer_type(alloca_type),
                .id = Value.ID.Instruction,
                .parent = @ptrCast(*Value, entry_block),
            },
            .id = Instruction.ID.Alloca,
            .operands = undefined,
            //.operands = ArrayList(*Value).init(allocator),
            .value = Instruction.InstructionValue
            {
                .alloca = Instruction.Alloca {
                    .type = alloca_type,
                },
            }
        };

        const result = self.instruction_buffer.append(instruction) catch |err| {
            panic("Failed to allocate memory for instruction\n", .{});
        };

        entry_block.instructions.insert(self.next_alloca_index, result) catch |err| {
            panic("Failed to insert alloca instruction reference inside the entry block\n", .{});
        };
        self.next_alloca_index += 1;

        return result;
    }

    fn create_store(self: *Builder, allocator: *Allocator, value: *Value, ptr: *Value) *Instruction
    {
        var i = Instruction
        {
            .base = Value {
                .type = self.context.get_void_type(),
                .id = Value.ID.Instruction,
                .parent = undefined,
            },
            .id = Instruction.ID.Store,
            .operands = ArrayList(*Value).initCapacity(allocator, 2) catch |err| {
                panic("Can't allocate memory for ret operands\n", .{});
            },
            .value = undefined,
        };

        i.operands.append(value) catch |err| {
            panic("Failed to allocate memory for store value operand\n", .{});
        };
        i.operands.append(ptr) catch |err| {
            panic("Failed to allocate memory for store pointer operand\n", .{});
        };

        return self.insert_at_the_end(i);
    }

    fn create_load(self: *Builder, allocator: *Allocator, load_type: *Type, value: *Value) *Instruction
    {
        var i = Instruction
        {
            .base = Value {
                .type = load_type,
                .id = Value.ID.Instruction,
                .parent = undefined,
            },
            .id = Instruction.ID.Load,
            .operands = ArrayList(*Value).initCapacity(allocator, 1) catch |err| {
                panic("Can't allocate memory for ret operands\n", .{});
            },
            .value = undefined,
        };

        i.operands.append(value) catch |err| {
            panic("Failed to allocate memory for store value operand\n", .{});
        };

        return self.insert_at_the_end(i);
    }

    fn create_inbounds_GEP(self: *Builder, allocator: *Allocator, gep_type: *Type, pointer: *Value, indices: []const *Value) *Instruction
    {
        var i = Instruction {
            .base = Value {
                .type = self.context.get_pointer_type(gep_type),
                .id = Value.ID.Instruction,
                .parent = undefined,
            },
            .id = Instruction.ID.GetElementPtr,
            .operands = ArrayList(*Value).initCapacity(allocator, indices.len) catch |err| {
                panic("Error allocating memory for GEP operands\n", .{});
            },
            .value = undefined,
        };
        i.operands.append(pointer) catch |err| {
            panic("Error allocating memory for GEP operands\n", .{});
        };
        i.operands.appendSlice(indices) catch |err| {
            panic("Error allocating memory for GEP operands\n", .{});
        };

        return self.insert_at_the_end(i);
    }

    fn create_ret(self: *Builder, allocator: *Allocator, maybe_value: ?*Value) *Instruction
    {
        const function_type = @ptrCast(*FunctionType, self.function.type);
        const function_ret_type = function_type.ret_type;

        if (!self.is_terminated())
        {
            var i = Instruction
            {
                .base = Value {
                    .type = undefined,
                    .id = Value.ID.Instruction,
                    .parent = undefined,
                },
                .id = Instruction.ID.Ret,
                .operands = ArrayList(*Value).init(allocator),
                .value = undefined,
            };

            if (maybe_value) |value|
            {
                assert(function_ret_type == value.type);
                i.base.type = value.type;
                // @Info: always use ensureCapacity as it doesn't add length to the array
                i.operands.ensureCapacity(1) catch |err| {
                    panic("Failed to allocate memory for ret operand\n", .{});
                };

                i.operands.append(value) catch |err| {
                    panic("Failed to append ret operand\n", .{});
                };
            }
            else
            {
                assert(function_ret_type.id == Type.ID.void);
                i.base.type = self.context.get_void_type();
            }

            return self.insert_at_the_end(i);
        }
        else
        {
            // panic("Trying to create a ret in a terminated basic block\n", .{});
            return undefined;
        }
    }

    fn create_ret_void(self: *Builder, allocator: *Allocator) *Instruction
    {
        return self.create_ret(allocator, null);
    }

    // @TODO: this assumes the instruction is going to be appended to the current basic block and it's going to jump to the dst_basic_block
    fn create_br(self: *Builder, allocator: *Allocator, dst_basic_block: *BasicBlock) *Instruction
    {
        if (!self.is_terminated())
        {
            var i = Instruction
            {
                .base = Value {
                    .type = dst_basic_block.base.type,
                    .id = Value.ID.Instruction,
                    .parent = undefined,
                },
                .id = Instruction.ID.Br,
                .operands = ArrayList(*Value).initCapacity(allocator, 1) catch |err| {
                    panic("Failed to allocate memory for br operand\n", .{});
                },
                .value = undefined,
            };

            i.operands.append(@ptrCast(*Value, dst_basic_block)) catch |err| {
                panic("Failed to allocate memory for br operand\n", .{});
            };

            dst_basic_block.use_count += 1;

            return self.insert_at_the_end(i);
        }
        else
        {
            //panic("Trying to terminate basic block with a br instruction but block is already terminated. Block instructions: {}\n", .{self.current.?.instructions.items.len});
            return undefined;
        }
    }

    fn create_conditional_br(self: *Builder, allocator: *Allocator, if_block: *BasicBlock, else_block: *BasicBlock, condition: *Value) *Instruction
    {
        if (!self.is_terminated())
        {
            var i = Instruction
            {
                .base = Value {
                    .type = if_block.base.type,
                    .id = Value.ID.Instruction,
                    .parent = undefined,
                },
                .id = Instruction.ID.Br,
                .operands = ArrayList(*Value).initCapacity(allocator, 3) catch |err| {
                    panic("Failed to allocate memory for br operand\n", .{});
                },
                .value = undefined,
            };

            i.operands.append(condition) catch |err| {
                panic("Failed to allocate memory for br operand\n", .{});
            };
            i.operands.append(@ptrCast(*Value, if_block)) catch |err| {
                panic("Failed to allocate memory for br operand\n", .{});
            };
            i.operands.append(@ptrCast(*Value, else_block)) catch |err| {
                panic("Failed to allocate memory for br operand\n", .{});
            };

            if_block.use_count += 1;
            else_block.use_count += 1;

            return self.insert_at_the_end(i);
        }
        else
        {
            panic("Trying to terminate basic block with a br instruction but block is already terminated\n", .{});
        }
    }

    fn create_call(self: *Builder, allocator: *Allocator, callee: *Value, maybe_arguments: ?[]*Value) *Instruction
    {
        if (maybe_arguments) |arguments|
        {
            var i = Instruction
            {
                .base = Value {
                    .type = callee.type,
                    .id = Value.ID.Instruction,
                    .parent = undefined,
                },
                .id = Instruction.ID.Call,
                .operands = ArrayList(*Value).initCapacity(allocator, arguments.len + 1) catch |err| {
                    panic("Can't allocate memory for ret operands\n", .{});
                },
                .value = undefined,
            };

            i.operands.append(callee) catch |err| {
                panic("Failed to allocate memory for function call callee operand\n", .{});
            };

            for (arguments) |arg|
            {
                i.operands.append(arg) catch |err| {
                    panic("Failed to allocate memory for function call arg operand\n", .{});
                };
            }

            return self.insert_at_the_end(i);
        }
        else
        {
            var i = Instruction
            {
                .base = Value {
                    .type = callee.type,
                    .id = Value.ID.Instruction,
                    .parent = undefined,
                },
                .id = Instruction.ID.Call,
                .operands = ArrayList(*Value).initCapacity(allocator, 1) catch |err| {
                    panic("Can't allocate memory for ret operands\n", .{});
                },
                .value = undefined,
            };

            i.operands.append(callee) catch |err| {
                panic("Failed to allocate memory for function call callee operand\n", .{});
            };

            return self.insert_at_the_end(i);
        }
    }

    fn create_icmp(self: *Builder, allocator: *Allocator, comparation: CompareType, left: *Value, right: *Value) *Instruction
    {
        var i = Instruction
        {
            .base = Value {
                .type = self.context.get_boolean_type(),
                .id = Value.ID.Instruction,
                .parent = undefined,
            },
            .id = Instruction.ID.ICmp,
            .operands = ArrayList(*Value).initCapacity(allocator, 2) catch |err| {
                panic("Can't allocate memory for ret operands\n", .{});
            },
            .value = Instruction.InstructionValue {
                .compare_type = comparation,
            },
        };

        i.operands.append(left) catch |err| {
            panic("Failed to allocate memory for icmp value operand\n", .{});
        };
        i.operands.append(right) catch |err| {
            panic("Failed to allocate memory for icmp value operand\n", .{});
        };

        return self.insert_at_the_end(i);
    }

    fn create_add(self: *Builder, allocator: *Allocator, left: *Value, right: *Value) *Instruction
    {
        var i = Instruction
        {
            .base = Value {
                .type = left.type,
                .id = Value.ID.Instruction,
                .parent = undefined,
            },
            .id = Instruction.ID.Add,
            .operands = ArrayList(*Value).initCapacity(allocator, 2) catch |err| {
                panic("Can't allocate memory for add operands\n", .{});
            },
            .value = undefined,
        };

        i.operands.append(left) catch |err| {
            panic("Failed to allocate memory for add value operand\n", .{});
        };
        i.operands.append(right) catch |err| {
            panic("Failed to allocate memory for add value operand\n", .{});
        };

        return self.insert_at_the_end(i);
    }

    fn create_sub(self: *Builder, allocator: *Allocator, left: *Value, right: *Value) *Instruction
    {
        var i = Instruction
        {
            .base = Value {
                .type = left.type,
                .id = Value.ID.Instruction,
                .parent = undefined,
            },
            .id = Instruction.ID.Sub,
            .operands = ArrayList(*Value).initCapacity(allocator, 2) catch |err| {
                panic("Can't allocate memory for sub operands\n", .{});
            },
            .value = undefined,
        };

        i.operands.append(left) catch |err| {
            panic("Failed to allocate memory for sub value operand\n", .{});
        };
        i.operands.append(right) catch |err| {
            panic("Failed to allocate memory for sub value operand\n", .{});
        };

        return self.insert_at_the_end(i);
    }

    fn create_mul(self: *Builder, allocator: *Allocator, left: *Value, right: *Value) *Instruction
    {
        var i = Instruction
        {
            .base = Value {
                .type = left.type,
                .id = Value.ID.Instruction,
                .parent = undefined,
            },
            .id = Instruction.ID.Mul,
            .operands = ArrayList(*Value).initCapacity(allocator, 2) catch |err| {
                panic("Can't allocate memory for mul operands\n", .{});
            },
            .value = undefined,
        };

        i.operands.append(left) catch |err| {
            panic("Failed to allocate memory for mul value operand\n", .{});
        };
        i.operands.append(right) catch |err| {
            panic("Failed to allocate memory for mul value operand\n", .{});
        };

        return self.insert_at_the_end(i);
    }

    fn create_bitcast(self: *Builder, allocator: *Allocator, value: *Value, cast_type: *Type) *Instruction
    {
        var i = Instruction
        {
            .base = Value {
                .type = cast_type,
                .id = Value.ID.Instruction,
                .parent = undefined,
            },
            .id = Instruction.ID.BitCast,
            .operands = ArrayList(*Value).initCapacity(allocator, 1) catch |err| {
                panic("Can't allocate memory for mul operands\n", .{});
            },
            .value = undefined,
        };
        i.operands.append(value) catch |err| {
            panic("Failed to allocate memory for mul value operand\n", .{});
        };

        return self.insert_at_the_end(i);
    }

    fn create_bitcast_operator(self: *Builder, allocator: *Allocator, value: *Value, cast_type: *Type) *OperatorBitCast
    {
        const bitcast_operator = OperatorBitCast
        {
            .base = Value {
                .type = cast_type,
                .id = Value.ID.OperatorBitCast,
                .parent = undefined,
            },
            .cast_value = value,
        };

        // @TODO: batch these
        const result = allocator.alloc(OperatorBitCast, 1) catch |err| {
            panic("Failed to allocate memory for bitcast_operator\n", .{});
        };
        result.ptr.* = bitcast_operator;
        return &result[0];
    }

    fn create_memcpy_intrinsic(self: *Builder, allocator: *Allocator, arguments: []*Value) *Instruction
    {
        var intrinsic : ?*Intrinsic = null;
        for (self.context.intrinsics.list.items) |intrinsic_bucket|
        {
            var i : u64 = 0;
            while (i < intrinsic_bucket.len) : (i += 1)
            {
                const elem = &intrinsic_bucket.items[i];
                if (elem.id == Intrinsic.ID.memcpy)
                {
                    intrinsic = elem;
                    break;
                }
            }
            if (intrinsic != null) break;
        }

        if (intrinsic == null)
        {
            const intrinsic_value = Intrinsic 
            {
                .base = Value {
                    .type = self.context.get_void_type(),
                    .id = Value.ID.Intrinsic,
                    // not used
                    .parent = undefined,
                },
                .id = Intrinsic.ID.memcpy,
            };

            const result = self.context.intrinsics.append(intrinsic_value) catch |err| {
                panic("Couldn't allocate intrinsic\n", .{});
            };
            intrinsic = result;
        }
        const intrinsic_ptr = @ptrCast(*Value, intrinsic.?);
        const intrinsic_call = self.create_call(allocator, intrinsic_ptr, arguments);
        return intrinsic_call;
    }

    fn is_terminated(self: *Builder) bool
    {
        if (self.current.instructions.items.len > 0)
        {
            const last_instruction = self.current.instructions.items[self.current.instructions.items.len - 1];

            switch (last_instruction.id)
            {
                Instruction.ID.Br, Instruction.ID.Ret => 
                {
                    return true;
                },
                else => 
                {
                    return false;
                }
            }
        }
        else
        {
            return false;
        }
    }

    fn insert_at_the_end(self: *Builder, instruction: Instruction) *Instruction
    {

        const result = self.instruction_buffer.append(instruction) catch |err| {
            panic("Failed to allocate memory for instruction\n", .{});
        };
        result.base.parent = @ptrCast(*Value, self.current);

        self.current.instructions.append(result) catch |err| {
            panic("Failed to allocate memory for instruction\n", .{});
        };

        return result;
    }
};

const FunctionTypeBuffer   = BucketArrayList(FunctionType, 64);
const ArrayTypeBuffer      = BucketArrayList(ArrayType, 64);
const PointerTypeBuffer    = BucketArrayList(PointerType, 64);
const StructTypeBuffer     = BucketArrayList(StructType, 64);
const ConstantArrayBuffer  = BucketArrayList(ConstantArray, 64);
const ConstantIntBuffer    = BucketArrayList(ConstantInt, 64);
const IntrinsicBuffer      = BucketArrayList(Intrinsic, 64);

const Context = struct
{
    void_type: Type,
    label_type: Type,
    i1_type:  IntegerType,
    i8_type:  IntegerType,
    i16_type: IntegerType,
    i32_type: IntegerType,
    i64_type: IntegerType,
    f32_type: FloatType,
    f64_type: FloatType,

    function_types: FunctionTypeBuffer,
    array_types: ArrayTypeBuffer,
    pointer_types: PointerTypeBuffer,
    structure_types: StructTypeBuffer,
    constant_arrays: ConstantArrayBuffer,
    constant_ints: ConstantIntBuffer,
    intrinsics: IntrinsicBuffer,

    fn create(allocator: *Allocator) Context
    {
        var context : Context = undefined;

        context.void_type = Type { .name = "void", .id = Type.ID.@"void" };
        context.label_type = Type { .name = "label", .id = Type.ID.@"label" };

        context.i1_type = IntegerType { .base =  Type { .name = "i1", .id = Type.ID.@"integer" }, .bits = 1, };

        context.i8_type = IntegerType { .base =  Type { .name = "i8", .id = Type.ID.@"integer" }, .bits = 8, };
        context.i16_type = IntegerType { .base =  Type { .name = "i16", .id = Type.ID.@"integer" }, .bits = 16, };
        context.i32_type = IntegerType { .base =  Type { .name = "i32", .id = Type.ID.@"integer" }, .bits = 32, };
        context.i64_type = IntegerType { .base =  Type { .name = "i64", .id = Type.ID.@"integer" }, .bits = 64, };

        context.f32_type = FloatType { .base =  Type { .name = "f32", .id = Type.ID.@"float" }, .bits = 32, };
        context.f64_type = FloatType { .base =  Type { .name = "f64", .id = Type.ID.@"float" }, .bits = 64, };

        context.function_types  = FunctionTypeBuffer.init(allocator) catch |err| {
            panic("Failed to allocate big type buffer\n", .{});
        }; 
        context.array_types     = ArrayTypeBuffer.init(allocator) catch |err| {
            panic("Failed to allocate big type buffer\n", .{});
        }; 
        context.pointer_types   = PointerTypeBuffer.init(allocator) catch |err| {
            panic("Failed to allocate big type buffer\n", .{});
        }; 
        context.structure_types   = StructTypeBuffer.init(allocator) catch |err| {
            panic("Failed to allocate big type buffer\n", .{});
        }; 
        context.constant_arrays = ConstantArrayBuffer.init(allocator) catch |err| {
            panic("Failed to allocate big type buffer\n", .{});
        }; 
        context.constant_ints   = ConstantIntBuffer.init(allocator) catch |err| {
            panic("Failed to allocate big type buffer\n", .{});
        }; 
        context.intrinsics      = IntrinsicBuffer.init(allocator) catch |err| {
            panic("Failed to allocate big type buffer\n", .{});
        }; 

        return context;
    }

    fn get_void_type(self: *Context) *Type
    {
        return &self.void_type;
    }

    fn get_label_type(self: *Context) *Type
    {
        return &self.label_type;
    }

    fn get_integer_type(self: *Context, bits: u16) *Type
    {
        switch (bits)
        {
            1 => return @ptrCast(*Type, &self.i1_type),
            8 => return @ptrCast(*Type, &self.i8_type),
            16 => return @ptrCast(*Type, &self.i16_type),
            32 => return @ptrCast(*Type, &self.i32_type),
            64 => return @ptrCast(*Type, &self.i64_type),
            else =>
            {
                panic("Integer type with {} bits not implemented\n", .{bits});
            }
        }
    }

    fn get_boolean_type(self: *Context) *Type
    {
        const result = self.get_integer_type(1);
        return result;
    }

    fn get_array_type(self: *Context, array_type: *Type, array_length: u64) *Type
    {
        for (self.array_types.list.items) |array_type_bucket|
        {
            var type_index : u64 = 0;

            while (type_index < array_type_bucket.len) : (type_index += 1)
            {
                const existent_array_type = &array_type_bucket.items[type_index];
                if (existent_array_type.type == array_type and existent_array_type.count == array_length)
                {
                    return @ptrCast(*Type, existent_array_type);
                }
            }
        }

        const array_type_value = ArrayType
        {
            .base = Type {
                .name = undefined,
                .id = Type.ID.@"array",
            },
            .type = array_type,
            .count = array_length,
        };

        const result = self.array_types.append(array_type_value) catch |err| {
            panic("Failed to allocate memory for array type\n", .{});
        };

        return @ptrCast(*Type, result);
    }
    
    fn get_pointer_type(self: *Context, p_type: *Type) *Type
    {
        for (self.pointer_types.list.items) |pointer_type_bucket|
        {
            var type_index : u64 = 0;

            while (type_index < pointer_type_bucket.len) : (type_index += 1)
            {
                const pointer_type = &pointer_type_bucket.items[type_index];
                if (pointer_type.type == p_type)
                {
                    return @ptrCast(*Type, pointer_type);
                }
            }
        }

        const pointer_type_value = PointerType
        {
            .base = Type {
                .name = undefined,
                .id = Type.ID.@"pointer",
            },
            .type = p_type,
        };

        const result = self.pointer_types.append(pointer_type_value) catch |err| {
            panic("Failed to allocate memory for pointer type\n", .{});
        };

        return @ptrCast(*Type, result);
    }

    fn get_struct_type(self: *Context, field_types: []*Type, name: []const u8) *Type
    {
        for (self.structure_types.list.items) |struct_type_bucket|
        {
            var type_index : u64 = 0;

            while (type_index < struct_type_bucket.len) : (type_index += 1)
            {
                const struct_type = &struct_type_bucket.items[type_index];
                if (std.mem.eql(u8, struct_type.name, name) and struct_type.field_types.len == field_types.len)
                {
                    var found = true;
                    for (struct_type.field_types) |field_type, i|
                    {
                        if (field_type != field_types[i])
                        {
                            found = false;
                            break;
                        }

                        continue;
                    }

                    if (found)
                    {
                        return @ptrCast(*Type, struct_type);
                    }
                }
            }
        }

        const new_struct = StructType {
            .base = Type {
                .name = undefined,
                .id = Type.ID.@"struct",
            },
            .field_types = field_types,
            .name = name,
        };

        const result = self.structure_types.append(new_struct) catch |err| {
            panic("Can't allocate new struct type\n", .{});
        };

        return @ptrCast(*Type, result);
    }

    fn get_constant_int(self: *Context, int_type: *Type, value: u64, is_signed: bool) *ConstantInt
    {
        const integer_type = @ptrCast(*IntegerType, int_type);
        const bits = integer_type.bits;
        assert(bits == 1 or bits == 8 or bits == 16 or bits == 32 or bits == 64);

        const new_int = ConstantInt
        {
            .base = Value {
                .type = int_type,
                .id = Value.ID.ConstantInt,
                .parent = undefined,
            }, 
            .int_value = value,
            .bit_count = bits,
            .is_signed = is_signed,
        };

        const result = self.constant_ints.append(new_int) catch |err| {
            panic("Failed to allocate memory for constant int\n", .{});
        };
        return result;
    }

    fn get_constant_array(self: *Context, array_values: []*Value, array_type: *Type) *ConstantArray
    {
        const new_array = ConstantArray 
        {
            .base = Value {
                .type = array_type,
                .id = Value.ID.ConstantArray,
                .parent = undefined,
            },
            .array_type = array_type,
            .array_values = array_values,
        };

        const result = self.constant_arrays.append(new_array) catch |err| {
            panic("Failed to allocate memory for constant array\n", .{});
        };

        return result;
    }
};

fn introspect_for_allocas(builder: *Builder, ast_block: *Node) bool
{
    for (ast_block.value.block_expr.statements.items) |ast_statement|
    {
        if (ast_statement.value == Node.ID.return_expr)
        {
            return true;
        }
        else if (ast_statement.value == Node.ID.branch_expr)
        {
            if (introspect_for_allocas(builder, ast_statement.value.branch_expr.if_block))
            {
                return true;
            }
            if (ast_statement.value.branch_expr.else_block) |ast_else_block|
            {
                if (introspect_for_allocas(builder, ast_else_block))
                {
                    return true;
                }
            }
        }
        else if (ast_statement.value == Node.ID.loop_expr)
        {
            if (introspect_for_allocas(builder, ast_statement.value.loop_expr.body))
            {
                return true;
            }
        }
    }

    return false;
}

fn count_instructions(builder: *Builder) u64
{
    var count: u64 = 0;
    const current_fn = builder.function;
    for (current_fn.basic_blocks.items) |basic_block|
    {
        count += basic_block.instructions.items.len;
    }

    return count;
}

pub const ValueSide = enum 
{
    RValue,
    LValue,
};

fn do_node(allocator: *Allocator, compiler: *Compiler, builder: *Builder, ast_types: *Internal.TypeBuffer, node: *Node, expected_type: ?*Type, expected_value: ?*Value) ?*Value
{
    var result: ?*Value = null;
    var instruction_count_before : u64 = 0; 
    const should_log = compiler.should_log(Log.debug);
    if (should_log)
    {
        instruction_count_before = count_instructions(builder);
    }

    switch (node.value)
    {
        Node.ID.block_expr =>
        {
            assert(node.value_type == Node.ValueType.RValue);
            for (node.value.block_expr.statements.items) |ast_statement|
            {
                if (!builder.emitted_return)
                {
                    _ = do_node(allocator, compiler, builder, ast_types, ast_statement, null, null);
                }
            }
        },
        Node.ID.return_expr =>
        {
            assert(node.value_type == Node.ValueType.RValue);
            // @TODO: tolerate this in the future?
            assert(!builder.emitted_return);

            if (node.value.return_expr.expression) |ast_return_expression|
            {
                builder.emitted_return = true;
                builder.explicit_return = true;

                const function_type = @ptrCast(*FunctionType, builder.function.type);
                const return_type = function_type.ret_type;

                if (do_node(allocator, compiler, builder, ast_types, ast_return_expression, return_type, null)) |ret_value|
                {
                    if (builder.conditional_alloca)
                    {
                        assert(builder.return_alloca != null);
                        assert(builder.exit_block != null);
                        _ = builder.create_store(allocator, ret_value, @ptrCast(*Value, builder.return_alloca));
                        _ = builder.create_br(allocator, builder.exit_block.?);
                    }
                    else
                    {
                        _ = builder.create_ret(allocator, ret_value);
                    }
                }
                else
                {
                    panic("Couldn't infer return expression\n", .{});
                }
            }
            else
            {
                if (!builder.explicit_return)
                {
                    _ = builder.create_ret_void(allocator);
                }
                else
                {
                    assert(builder.exit_block != null);
                    _ = builder.create_br(allocator, builder.exit_block.?);
                    builder.emitted_return = true;
                    builder.explicit_return = true;
                }
            }
        },
        Node.ID.int_lit =>
        {
            assert(node.value_type == Node.ValueType.RValue);
            result = @ptrCast(*Value, builder.context.get_constant_int(builder.context.get_integer_type(32), node.value.int_lit.value, node.value.int_lit.signed));
        },
        Node.ID.var_decl => 
        {
            const ast_type = node.type;
            const rns_type = get_type(allocator, builder.context, ast_type, ast_types);
            // @TODO: for arrays, consider creating an array alloca
            // @TODO: verify that LLVM indeed does this
            const var_alloca = builder.create_alloca(rns_type, null);
            node.value.var_decl.backend_ref = @ptrToInt(var_alloca);
        },
        Node.ID.resolved_identifier =>
        {
            const ast_declaration = node.value.resolved_identifier;
            assert(ast_declaration.value == Node.ID.var_decl);
            const alloca_ptr = @intToPtr(*Value, ast_declaration.value.var_decl.backend_ref);
            const ast_type = ast_declaration.type;
            const var_type = get_type(allocator, builder.context, ast_type, ast_types);

            if (node.value_type == Node.ValueType.RValue)
            {

                const var_load = builder.create_load(allocator, var_type, alloca_ptr);
                result = @ptrCast(*Value, var_load);
            }
            else
            {
                if (expected_value) |rvalue|
                {
                    switch (var_type.id)
                    {
                        Type.ID.integer,
                        Type.ID.pointer =>
                        {
                            const var_store = builder.create_store(allocator, rvalue, alloca_ptr);
                        },
                        Type.ID.array =>
                        {
                            // @TODO: maybe unify in a big if if the variable is an array (see above TODOs
                            assert(rvalue.id == Value.ID.ConstantArray);
                            rvalue.parent = @ptrCast(*Value, alloca_ptr);
                            const pointer_to_i8_type = builder.context.get_pointer_type(builder.context.get_integer_type(8));
                            const array_cast_to_i8 = builder.create_bitcast(allocator, @ptrCast(*Value, alloca_ptr), pointer_to_i8_type);
                            const memcpy_size = get_size(var_type);
                            const memcpy_size_value = builder.context.get_constant_int(builder.context.get_integer_type(64), memcpy_size, false);
                            // @Info: We need to bitcast **as operator** the constant array
                            assert(rvalue.id == Value.ID.ConstantArray);
                            const bitcast_constant_array = builder.create_bitcast_operator(allocator, rvalue, pointer_to_i8_type);
                            var memcpy_args = ArrayList(*Value).initCapacity(allocator, 3) catch |err| {
                                panic("Error allocating memory for memcpy args\n", .{});
                            }; 
                            memcpy_args.append(@ptrCast(*Value, array_cast_to_i8)) catch |err| {
                                panic("Error appending new memcpy arg\n", .{});
                            };
                            memcpy_args.append(@ptrCast(*Value, bitcast_constant_array)) catch |err| {
                                panic("Error appending new memcpy arg\n", .{});
                            };
                            memcpy_args.append(@ptrCast(*Value, memcpy_size_value)) catch |err| {
                                panic("Error appending new memcpy arg\n", .{});
                            };
                            _ = builder.create_memcpy_intrinsic(allocator, memcpy_args.items);
                        },
                        else => panic("ni: {}\n", .{var_type.id}),
                    }
                }
                else
                {
                    panic("There is no rvalue available for the assignment\n", .{});
                }
            }
        },
        Node.ID.loop_expr =>
        {
            assert(node.value_type == Node.ValueType.RValue);
            const ast_loop_prefix = node.value.loop_expr.prefix;
            const ast_loop_body = node.value.loop_expr.body;
            const ast_loop_postfix = node.value.loop_expr.postfix;

            var loop_prefix_block = builder.create_block(allocator);
            var loop_body_block = builder.create_block(allocator);
            var loop_postfix_block = builder.create_block(allocator);
            var loop_end_block = builder.create_block(allocator);

            const loop_continue_block = loop_postfix_block;
            node.value.loop_expr.continue_block_ref = @ptrToInt(loop_continue_block);
            node.value.loop_expr.exit_block_ref = @ptrToInt(loop_end_block);

            _ = builder.create_br(allocator, loop_prefix_block);
            builder.append_to_current_function(loop_prefix_block);
            builder.set_block(loop_prefix_block);

            assert(ast_loop_prefix.value.block_expr.statements.items.len == 1);
            const ast_condition = ast_loop_prefix.value.block_expr.statements.items[0];

            if (do_node(allocator, compiler, builder, ast_types, ast_condition, builder.context.get_boolean_type(), null)) |condition|
            {
                _ = builder.create_conditional_br(allocator, loop_body_block, loop_end_block, condition);
                builder.append_to_current_function(loop_body_block);
                builder.set_block(loop_body_block);
                _ = do_node(allocator, compiler, builder, ast_types, ast_loop_body, null, null);

                _ = builder.create_br(allocator, loop_postfix_block);
                builder.append_to_current_function(loop_postfix_block);
                builder.set_block(loop_postfix_block);
                _ = do_node(allocator, compiler, builder, ast_types, ast_loop_postfix, null, null);

                if (!builder.emitted_return)
                {
                    _ = builder.create_br(allocator, loop_prefix_block);
                    builder.append_to_current_function(loop_end_block);
                    builder.set_block(loop_end_block);
                }
            }
            else
            {
                panic("Couldn't find condition for loop expression\n", .{});
            }


        },
        Node.ID.binary_expr =>
        {
            const ast_left = node.value.binary_expr.left;
            const ast_right = node.value.binary_expr.right;
            const binary_op = node.value.binary_expr.id;

            if (binary_op == BinaryOp.Assignment)
            {
                assert(node.value_type == Node.ValueType.RValue);

                var alloca_value: *Value = undefined;
                const ast_left_type = ast_left.type;
                const left_type = get_type(allocator, builder.context, ast_left_type, ast_types);

                switch (left_type.id)
                {
                    Type.ID.integer, Type.ID.pointer, Type.ID.array =>
                    {
                        if (do_node(allocator, compiler, builder, ast_types, ast_right, left_type, null)) |right_value|
                        {
                            _ = do_node(allocator, compiler, builder, ast_types, ast_left, null, right_value);
                        }
                        else
                        { 
                            panic("Couldn't get right-side of binary expression\n", .{});
                        }
                    },
                    else => panic("ni: {}\n", .{left_type.id}),
                }
            }
            else
            {
                if (node.value_type == Node.ValueType.RValue)
                {
                    const ltype = get_type(allocator, builder.context, ast_left.type, ast_types);
                    const rtype = get_type(allocator, builder.context, ast_right.type, ast_types);
                    assert(ltype == rtype);

                    if (do_node(allocator, compiler, builder, ast_types, ast_left, ltype, null)) |left|
                    {
                        if (do_node(allocator, compiler, builder, ast_types, ast_right, ltype, expected_value)) |right|
                        {
                            var binary_op_instruction: *Instruction = undefined;

                            switch (binary_op)
                            {
                                BinaryOp.Compare_LessThan =>
                                {
                                    binary_op_instruction = builder.create_icmp(allocator, CompareType.ICMP_SLE, left, right);
                                },
                                BinaryOp.Compare_Equal =>
                                {
                                    binary_op_instruction = builder.create_icmp(allocator, CompareType.ICMP_EQ, left, right);
                                },
                                BinaryOp.Compare_GreaterThan =>
                                {
                                    binary_op_instruction = builder.create_icmp(allocator, CompareType.ICMP_SGT, left, right);
                                },
                                BinaryOp.Plus =>
                                {
                                    binary_op_instruction = builder.create_add(allocator, left, right);
                                },
                                BinaryOp.Minus =>
                                {
                                    binary_op_instruction = builder.create_sub(allocator, left, right);
                                },
                                BinaryOp.Multiplication =>
                                {
                                    binary_op_instruction = builder.create_mul(allocator, left, right);
                                },
                                else =>
                                {
                                    panic("Not implemented: {}\n", .{binary_op});
                                }
                            }

                            result = @ptrCast(*Value, binary_op_instruction);
                            compiler.log(Log.debug, "Left type: {} --- Right type: {} --- Result type: {}\n", .{ltype, rtype, result.?.type});
                            // assert(result.?.type == ltype and result.?.type == rtype);
                        }
                        else
                        {
                            panic("Couldn't get right side of the expression\n", .{});
                        }
                    }
                    else
                    {
                        panic("Couldn't get left side of the expression\n", .{});
                    }
                }
                else
                {
                    panic("ni:\n", .{});
                }
            }
        },
        Node.ID.branch_expr =>
        {
            assert(node.value_type == Node.ValueType.RValue);
            const saved_emitted_return = builder.emitted_return;
            const ast_condition = node.value.branch_expr.condition;
            const ast_if_block = node.value.branch_expr.if_block;

            var exit_block = builder.create_block(allocator);
            var if_block = builder.create_block(allocator);
            var else_block: *BasicBlock = undefined;

            var do_else_block = false;
            // @TODO: this can cause bugs
            var ast_else_block: *Node = undefined;
            if (node.value.branch_expr.else_block) |ast_else_block_result|
            {
                ast_else_block = ast_else_block_result;
                else_block = builder.create_block(allocator);
            }
            else
            {
                else_block = exit_block;
            }

            node.value.branch_expr.exit_block_ref = @ptrToInt(exit_block);


            var exit_block_in_use = true;
            if (do_node(allocator, compiler, builder, ast_types, ast_condition, builder.context.get_boolean_type(), null)) |condition|
            {
                if (if_block != else_block)
                {
                    _ = builder.create_conditional_br(allocator, if_block, else_block, condition);
                }
                else
                {
                    exit_block_in_use = exit_block.use_count > 0;
                    if (exit_block_in_use)
                    {
                        _ = builder.create_br(allocator, exit_block);
                    }
                }
            }
            else
            {
                panic("Couldn't get condition bytecode\n", .{});
            }

            builder.emitted_return = false;

            builder.append_to_current_function(if_block);
            builder.set_block(if_block);
            _ = do_node(allocator, compiler, builder, ast_types, ast_if_block, null, null);
            const if_block_returned = builder.emitted_return;

            _ = builder.create_br(allocator, exit_block);

            builder.emitted_return = false;

            if (else_block != exit_block)
            {
                builder.append_to_current_function(else_block);
                builder.set_block(else_block);
                _ = do_node(allocator, compiler, builder, ast_types, ast_else_block, null, null);

                _ = builder.create_br(allocator, exit_block);
            }

            const else_block_returned = builder.emitted_return;

            builder.emitted_return = if_block_returned and else_block_returned;

            if (exit_block_in_use and !builder.emitted_return)
            {
                builder.append_to_current_function(exit_block);
                builder.set_block(exit_block);
            }
        },
        Node.ID.break_expr =>
        {
            assert(node.value_type == Node.ValueType.RValue);
            const ast_jump_target = node.value.break_expr.target;
            assert(ast_jump_target.value == Node.ID.loop_expr);
            const jump_target = @intToPtr(*BasicBlock, ast_jump_target.value.loop_expr.exit_block_ref);
            // @TODO: this may be buggy
            _ = builder.create_br(allocator, jump_target);
        },
        Node.ID.invoke_expr =>
        {
            assert(node.value_type == Node.ValueType.RValue);
            const ast_invoke_expr = node.value.invoke_expr.expression;
            assert(ast_invoke_expr.value == Node.ID.function_decl);
            const function_name = ast_invoke_expr.value.function_decl.name;
            const function = builder.module.find_function(function_name);
            const arg_count = node.value.invoke_expr.arguments.items.len;

            if (arg_count == 0)
            {
                const call = builder.create_call(allocator, @ptrCast(*Value, function), null);
                result = @ptrCast(*Value, call);
            }
            else
            {
                // @TODO: think this in a better way.
                // One option is to make a std.arraylist which then can be appended
                // directly to the instruction, substituting their owned operands
                // arraylist and including as first operand the callee function
                var argument_buffer : [256]*Value = undefined;
                assert(arg_count <= 15);
                const node_arg_buffer = node.value.invoke_expr.arguments;
                const function_type = @ptrCast(*FunctionType, function.type);

                for (node_arg_buffer.items) |ast_arg, i|
                {
                    const arg_type = get_type(allocator, builder.context, ast_arg.type, ast_types);
                    assert(arg_type == function_type.arg_types[i]);
                    if (do_node(allocator, compiler, builder, ast_types, ast_arg, arg_type, null)) |arg|
                    {
                        arg.type = arg_type;
                        argument_buffer[i] = arg;
                    }
                    else
                    {
                        panic("Couldn't generate bytecode for function argument\n", .{});
                    }
                }

                const call = builder.create_call(allocator, @ptrCast(*Value, function), argument_buffer[0..arg_count]);
                result = @ptrCast(*Value, call);
            }
        },
        Node.ID.unary_expr =>
        {
            const ast_unary_op_type = node.value.unary_expr.id;
            const ast_unary_op_expr = node.value.unary_expr.node_ref;
            assert(ast_unary_op_expr.value == Node.ID.resolved_identifier);
            const ast_var_decl = ast_unary_op_expr.value.resolved_identifier;
            assert(ast_var_decl.value == Node.ID.var_decl);
            const ast_type = ast_var_decl.type;
            const var_alloca = @intToPtr(*Value, ast_var_decl.value.var_decl.backend_ref);

            switch (ast_unary_op_type)
            {
                Parser.UnaryExpression.ID.AddressOf =>
                {
                    return var_alloca;
                },
                Parser.UnaryExpression.ID.Dereference =>
                {
                    assert(ast_type.value == Internal.Type.ID.pointer);
                    const pointer_type = get_type(allocator, builder.context, ast_type, ast_types);
                    const pointer_load = builder.create_load(allocator, pointer_type, var_alloca);

                    if (node.value_type == Node.ValueType.RValue)
                    {
                        const pointer_type_unwrapped = @ptrCast(*PointerType, pointer_type);
                        const deref_type = pointer_type_unwrapped.type;
                        const deref_expr = builder.create_load(allocator, deref_type, @ptrCast(*Value, pointer_load));
                        result = @ptrCast(*Value, deref_expr);
                    }
                    else
                    {
                        result = @ptrCast(*Value, pointer_load);
                    }
                },
            }
        },
        Node.ID.array_lit =>
        {
            assert(node.value_type == Node.ValueType.RValue);
            const count = node.value.array_lit.elements.items.len;
            assert(count > 0);
            assert(expected_type != null);
            const type_expr = expected_type.?;
            assert(type_expr.id == Type.ID.array);
            const array_type = @ptrCast(*ArrayType, type_expr);

            var array_values = ArrayList(*Value).initCapacity(allocator, count) catch |err| {
                panic("Error allocating memory for array literal\n", .{});
            };

            for (node.value.array_lit.elements.items) |ast_array_lit_elem|
            {
                if (do_node(allocator, compiler, builder, ast_types, ast_array_lit_elem, array_type.type, null)) |array_lit|
                {
                    array_values.append(array_lit) catch |err| {
                        panic("Error appending array literal element\n", .{});
                    };
                }
                else
                {
                    panic("Couldn't generate bytecode for array element\n", .{});
                }
            }

            const constant_array = builder.context.get_constant_array(array_values.items, type_expr);
            result = @ptrCast(*Value, constant_array);
        },
        Node.ID.array_subscript_expr =>
        {
            const ast_array_subscript_expr = node.value.array_subscript_expr.expression;
            const ast_array_index_expr = node.value.array_subscript_expr.index;

            const index_value = do_node(allocator, compiler, builder, ast_types, ast_array_index_expr, null, null);
            if (index_value) |index_const|
            {
                assert(ast_array_subscript_expr.value == Node.ID.resolved_identifier);
                const ast_var_decl = ast_array_subscript_expr.value.resolved_identifier;
                assert(ast_var_decl.value == Node.ID.var_decl);
                const alloca_value = @intToPtr(*Value, ast_var_decl.value.var_decl.backend_ref);
                assert(alloca_value.id == Value.ID.Instruction);
                const alloca = @ptrCast(*Instruction, alloca_value);
                const alloca_type = alloca.value.alloca.type;
                assert(alloca_type.id == Type.ID.array);
                const array_type = @ptrCast(*ArrayType, alloca_type);
                const array_element_type = array_type.type;
                const zero_value = builder.context.get_constant_int(builder.context.get_integer_type(32), 0, false);
                var indices = [_]*Value
                {
                    @ptrCast(*Value, zero_value),
                    index_const,
                };
                const gep = builder.create_inbounds_GEP(allocator, array_element_type, alloca_value, indices[0..]);

                if (node.value_type == Node.ValueType.RValue)
                {
                    const load = builder.create_load(allocator, array_element_type, @ptrCast(*Value, gep));
                    result = @ptrCast(*Value, load);
                }
                else
                {
                    if (expected_value) |rvalue|
                    {
                        const store = builder.create_store(allocator, rvalue, @ptrCast(*Value, gep));
                    }
                    else
                    {
                        panic("Expected right value in array store\n", .{});
                    }
                }
            }
            else
            {
                panic("Couldn't get bytecode for index in array subscript_expr\n", .{});
            }
        },
        Node.ID.field_access_expr =>
        {
            assert(node.value_type == Node.ValueType.RValue);
            const ast_struct_expr = node.value.field_access_expr.expression;
            const struct_type = get_type(allocator, builder.context, ast_struct_expr.type, ast_types);
            // @Info: this should emit a struct load
            if (do_node(allocator, compiler, builder, ast_types, ast_struct_expr, struct_type, null)) |struct_expr|
            {
                const ast_field_expr = node.value.field_access_expr.field_expr;
                assert(ast_field_expr.value == Node.ID.field_expr);
                const ast_field = ast_field_expr.value.field_expr;
                const ast_field_type = ast_field.type;
                const field_type = get_type(allocator, builder.context, ast_field_type, ast_types);
                const index = ast_field.index;
                const zero_value = builder.context.get_constant_int(builder.context.get_integer_type(32), 0, false);
                const index_value = builder.context.get_constant_int(builder.context.get_integer_type(32), index, false);

                const indices = [_]*Value {
                    @ptrCast(*Value, zero_value),
                    @ptrCast(*Value, index_value),
                };
                const gep = builder.create_inbounds_GEP(allocator, field_type, struct_expr, indices[0..]);
                const gep_load = builder.create_load(allocator, field_type, @ptrCast(*Value, gep));
                result = @ptrCast(*Value, gep_load);
            }
            else
            {
                panic("Can't codegen struct expression\n", .{});
            }
        },
        Node.ID.function_decl => panic("Not implemented\n", .{}),
        Node.ID.type_identifier => panic("Type identifier should be resolved before IR generation\n", .{}),
        Node.ID.identifier_expr =>
        {
            panic("This shouldn't ever be reached\n", .{});
        },
        else => panic("Not implemented: {}\n", .{node.value}),
    }

    if (should_log)
    {
        const instruction_count_after: u64 = count_instructions(builder);
        const instruction_count = instruction_count_after - instruction_count_before;
        compiler.log(Log.debug, "Call to do_node for {s} generated {} instructions\n===========================\n", .{@tagName(node.value), instruction_count});
        const expand = false;
        if (expand)
        {
            compiler.log("Expression: {}\n===========================\n", .{node.value});
        }
    }

    return result;
}

/// This function gets the compiler (AST-Lexer) type and transform it into a "LLVM" type
fn get_type(allocator: *Allocator, context: *Context, ast_type: *Internal.Type, ast_types: *Internal.TypeBuffer) *Type
{
    switch (ast_type.value)
    {
        Internal.Type.ID.function => 
        {
            const ret_type = get_type(allocator, context, ast_type.value.function.ret_type, ast_types);
            const arg_count = ast_type.value.function.arg_types.items.len;

            // @Info: Iterate over the bucket array
            //
            for (context.function_types.list.items) |function_type_bucket|
            {
                var elem_index : u64 = 0;
                while (elem_index < function_type_bucket.len) : (elem_index += 1)
                {
                    const function_type = &function_type_bucket.items[elem_index];
                    if (ret_type != function_type.ret_type)
                    {
                        continue;
                    }

                    if (arg_count != function_type.arg_types.len)
                    {
                        continue;
                    }

                    var arg_i : u64 = 0;
                    while (arg_i < arg_count) : (arg_i += 1)
                    {
                        const ast_arg_type = ast_type.value.function.arg_types.items[arg_i];
                        const new_arg_type = get_type(allocator, context, ast_arg_type, ast_types);
                        const already_registered_arg_type = function_type.arg_types[arg_i];
                        if (new_arg_type == already_registered_arg_type)
                        {
                            continue;
                        }

                        break;
                    }

                    return @ptrCast(*Type, function_type);
                }
            }

            var function_type = FunctionType {
                .base = Type {
                    .name = undefined,
                    .id = Type.ID.@"function",
                },
                .arg_types = undefined,
                .ret_type = ret_type,
            };

            if (arg_count > 0)
            {
                var arg_types = ArrayList(*Type).init(allocator);
                arg_types.ensureCapacity(arg_count) catch |err| {
                    panic("Resize to {} elements failed for array slice for function argument types\n", .{arg_count});
                };

                for (ast_type.value.function.arg_types.items) |ast_arg_type|
                {
                    const new_arg_type = get_type(allocator, context, ast_arg_type, ast_types);
                    arg_types.append(new_arg_type) catch |err| {
                        panic("Failed to allocate memory for argument type\n", .{});
                    };
                }

                function_type.arg_types = arg_types.items;
            }

            const result = context.function_types.append(function_type) catch |err| {
                panic("Failed to allocate function type\n", .{});
            };
            return @ptrCast(*Type, result);
        },
        Internal.Type.ID.integer =>
        {
            const bits = ast_type.value.integer.bits;
            const result = context.get_integer_type(bits);
            return result;
        },
        Internal.Type.ID.void_type =>
        {
            const result = context.get_void_type();
            return result;
        },
        Internal.Type.ID.pointer =>
        {
            const ast_appointee = ast_type.value.pointer.type;
            const appointee = get_type(allocator, context, ast_appointee, ast_types);
            const result = context.get_pointer_type(appointee);
            return result;
        },
        Internal.Type.ID.array =>
        {
            const ast_array_type = ast_type.value.array.type;
            const ast_array_length = ast_type.value.array.count;
            const array_type = get_type(allocator, context, ast_array_type, ast_types);
            const result = context.get_array_type(array_type, ast_array_length);
            return result;
        },
        Internal.Type.ID.structure =>
        {
            const ast_struct_name = ast_type.value.structure.name;
            var field_types = ArrayList(*Type).initCapacity(allocator, ast_type.value.structure.fields.len) catch |err| {
                panic("Error allocating memory for field types\n", .{});
            };

            for (ast_type.value.structure.fields) |ast_struct_field|
            {
                const field_type = get_type(allocator, context, ast_struct_field.type, ast_types);
                field_types.append(field_type) catch |err| {
                    panic("Error allocating memory for field type\n", .{});
                };
            }

            const result = context.get_struct_type(field_types.items, ast_struct_name);
            return result;
        },
        Internal.Type.ID.unresolved =>
        {
            panic("Unreachable!\n", .{});
        },
    }
}

fn get_size(type_: *Type) usize
{
    switch (type_.id)
    {
        Type.ID.array =>
        {
            const array_type = @ptrCast(*ArrayType, type_);
            const array_size = array_type.count * get_size(array_type.type);
            return array_size;
        },
        Type.ID.integer =>
        {
            const integer_type = @ptrCast(*IntegerType, type_);
            assert(integer_type.bits % 8 == 0);
            return integer_type.bits / 8;
        },
        else =>
        {
            panic("Not implemented: {}\n", .{type_.id});
        }
    }
}

pub fn encode(allocator: *Allocator, compiler: *Compiler, semantics_result: *SemanticsResult) Module
{
    compiler.current_module = Compiler.Module.bytecode;
    compiler.log(Compiler.LogLevel.debug, "\n==============\nIR\n==============\n\n", .{});

    var module = Module {
        .base = Value {
            .id = Value.ID.Module,
            .type = undefined,
            // @TODO: not used
            .parent = undefined,
        },
        .functions = FunctionBuffer.init(allocator) catch |err| {
            panic("Failed to allocate function bucket array\n", .{});
        },
    };

    var context = Context.create(allocator);

    for (semantics_result.function_declarations.items) |ast_function|
    {
        assert(ast_function.value == Node.ID.function_decl);
        const ast_function_type = ast_function.type;
        assert(ast_function_type.value == Internal.Type.ID.function);
        // @TODO: get RNS function type
        const function_type = get_type(allocator, &context, ast_function_type, &semantics_result.types);
        Function.create(allocator, &module, function_type, ast_function.value.function_decl.name);
    }

    assert(semantics_result.function_declarations.items.len == module.functions.len());

    var basic_block_buffer = BlockBuffer.init(allocator) catch |block_err| {
        panic("Couldn't allocate big block buffer\n", .{});
    };
    var instruction_buffer = InstructionBuffer.init(allocator) catch |instr_err| {
        panic("Couldn't allocate big instruction buffer\n", .{});
    };

    for (semantics_result.function_declarations.items) |ast_function, function_index|
    {
        // @TODO: code this access faster
        var function = module.functions.get(function_index).?;

        var builder = Builder {
            .context = &context,
            .function = function,
            .basic_block_buffer = &basic_block_buffer,
            .instruction_buffer = &instruction_buffer,
            .module = &module,
            .next_alloca_index = 0,
            .conditional_alloca = false,
            .emitted_return = false,
            .explicit_return = false,
            .current = undefined,
            .return_alloca = null,
            .exit_block = null,
        };

        // Pointer to the function main block
        var entry_block = builder.create_block(allocator);
        builder.append_to_current_function(entry_block);
        builder.set_block(entry_block);

        const arg_count = ast_function.value.function_decl.arguments.items.len;
        const function_base_type = builder.function.type;
        const function_type = @ptrCast(*FunctionType, function_base_type);
        const ret_type = function_type.ret_type;

        const returns_something = ret_type.id != Type.ID.@"void";

        var ast_main_block = ast_function.*.value.function_decl.blocks.items[0];

        for (ast_main_block.value.block_expr.statements.items) |ast_statement|
        {
            switch (ast_statement.value)
            {
                Node.ID.branch_expr =>
                {
                    if (introspect_for_allocas(&builder, ast_statement.value.branch_expr.if_block))
                    {
                        builder.explicit_return = true;
                        break;
                    }
                    if (ast_statement.value.branch_expr.else_block) |ast_else_block|
                    {
                        if (introspect_for_allocas(&builder, ast_else_block))
                        {
                            builder.explicit_return = true;
                            break;
                        }
                    }
                },
                Node.ID.loop_expr =>
                {
                    if (introspect_for_allocas(&builder, ast_statement.value.loop_expr.body))
                    {
                        builder.explicit_return = true;
                        break;
                    }
                },
                else =>
                {
                    // @Warning: here we need to contemplate other cases which imply new blocks
                }
            }
        }

        builder.conditional_alloca = returns_something and builder.explicit_return;

        if (builder.explicit_return)
        {
            builder.exit_block = builder.create_block(allocator);

            if (builder.conditional_alloca)
            {
                builder.return_alloca = builder.create_alloca(ret_type, null);
            }
        }

        var argument_list = ArrayList(Function.Argument).init(allocator);
        if (arg_count > 0)
        {
            argument_list.ensureCapacity(arg_count) catch |err| {
                panic("Error resizing argument buffer\n", .{});
            };
            var arg_index: u64 = 0;

            for (ast_function.*.value.function_decl.arguments.items) |ast_arg|
            {
                assert(ast_arg.value == Node.ID.var_decl);
                assert(ast_arg.value.var_decl.is_function_arg);
                const ast_arg_type = ast_arg.type;
                const arg_type = get_type(allocator, &context, ast_arg_type, &semantics_result.types);

                const index = argument_list.items.len;
                const arg = Function.Argument {
                    .base = Value {
                        .type = arg_type,
                        .id = Value.ID.Argument,
                        .parent = @ptrCast(*Value, builder.function),
                    },
                    .arg_index = index,
                };

                const arg_alloca = builder.create_alloca(arg_type, null);
                ast_arg.value.var_decl.backend_ref = @ptrToInt(arg_alloca);

                argument_list.append(arg) catch |err| {
                    panic("Error allocating argument in function\n", .{});
                };
                const arg_ref = &argument_list.items[index];
                _ = builder.create_store(allocator, @ptrCast(*Value, arg_ref), @ptrCast(*Value, arg_alloca));
            }
        }

        builder.function.arguments = argument_list.items;

        _ = do_node(allocator, compiler, &builder, &semantics_result.types, ast_main_block, null, null);

        if (builder.conditional_alloca)
        {
            assert(builder.current.instructions.items.len > 0);
            assert(builder.exit_block != null);
            assert(builder.return_alloca != null);

            builder.append_to_current_function(builder.exit_block.?);
            builder.set_block(builder.exit_block.?);

            const loaded_return = builder.create_load(allocator, builder.return_alloca.?.value.alloca.type, @ptrCast(*Value, builder.return_alloca));
            _ = builder.create_ret(allocator, @ptrCast(*Value, loaded_return));
        }
        else if (!returns_something)
        {
            if (builder.explicit_return)
            {
                assert(builder.exit_block != null);

                const current_block = builder.current;
                if (current_block.instructions.items.len == 0)
                {
                    const saved_current = current_block;
                    builder.set_block(builder.exit_block.?);

                    var block_ptr : ?*BasicBlock = null;
                    var block_index : u64 = 0;
                    while (block_index < builder.function.basic_blocks.items.len) : (block_index += 1)
                    {
                        const block = builder.function.basic_blocks.items[block_index];
                        if (block == saved_current)
                        {
                            block_ptr = block;
                            break;
                        }
                    }

                    if (block_ptr) |block|
                    {
                        if (block_index == 0)
                        {
                            builder.function.basic_blocks.items[block_index] = builder.current;
                            builder.current.base.parent = @ptrCast(*Value, builder.function);
                        }
                        else
                        {
                            panic("This should be the main block\n", .{});
                        }
                    }
                    else
                    {
                        panic("Block is not found in the function block list\n", .{});
                    }
                }
                else
                {
                    builder.append_to_current_function(builder.exit_block.?);
                    builder.set_block(builder.exit_block.?);
                }
            }

            // @TODO: figure out how to return here without causing a panic
            _ = builder.create_ret_void(allocator);
        }
        print_function(compiler, allocator, builder.context, function);
    }

    return module;
}

const SlotTracker = struct
{
    next_id : u64,
    not_used: u64,
    starting_id: u64,
    map: ArrayList(*Value),

    fn find(self: *SlotTracker, value: *Value) u64
    {
        var i : u64 = 0;
        while (i < self.map.items.len) : (i += 1)
        {
            if (self.map.items[i] == value)
            {
                return i + starting_id;
            }
        }
    }

    fn new_id(self: *SlotTracker, value: *Value) u64
    {
        const id = self.next_id;
        self.next_id += 1;
        self.map.append(value) catch |err| {
            panic("Failed to allocate memory for printer map\n", .{});
        };

        return id;
    }
};

const BlockPrinter = struct
{
    // @TODO: does this need pointer stability?
    instruction_printers: ArrayList(InstructionPrinter),
    id: u64,
    block: *BasicBlock,
    block_printer_list: *ArrayList(BlockPrinter),

    pub fn format(self: *const BlockPrinter, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void
    {
        const function = @ptrCast(*Function, self.block.base.parent);
        if (function.basic_blocks.items[0] != self.block)
        {
            try std.fmt.format(writer, "{}:\n", .{self.id});
        }

        for (self.instruction_printers.items) |i_printer|
        {
            try std.fmt.format(writer, "    {}\n", .{i_printer});
        }
    }
};

const InstructionPrinter = struct
{
    ref: *Instruction,
    result: u64,
    // @TODO: does this need pointer stability
    block_ref: *BlockPrinter,
    context: *Context,

    // @TODO: add align
    pub fn format(self: *const InstructionPrinter, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void
    {
        switch (self.ref.id)
        {
            Instruction.ID.Ret =>
            {
                try writer.writeAll("return ");
                if (self.ref.operands.items.len != 0)
                {
                    const return_value_operand = self.ref.operands.items[0];
                    try self.value_format(return_value_operand, fmt, options, writer);
                }
                else
                {
                    try writer.writeAll("void");
                }
            },
            Instruction.ID.Alloca =>
            {
                try std.fmt.format(writer, "%{} = alloca {}", .{self.result, self.ref.value.alloca.type});
            },
            Instruction.ID.Store =>
            {
                try writer.writeAll("store ");
                const operands = self.ref.operands.items;
                assert(operands.len == 2);
                try self.value_format(operands[0], fmt, options, writer);
                try writer.writeAll(", ");
                try self.value_format(operands[1], fmt, options, writer);
            },
            Instruction.ID.Load =>
            {
                try std.fmt.format(writer, "%{} = load {}, ", .{self.result, self.ref.base.type});
                try self.value_format(self.ref.operands.items[0], fmt, options, writer);
            },
            Instruction.ID.Br =>
            {
                const operand_count = self.ref.operands.items.len;
                const conditional = operand_count == 3;
                if (conditional)
                {
                    try writer.writeAll("br ");
                    try self.value_format(self.ref.operands.items[0], fmt, options, writer);
                    try writer.writeAll(", ");
                    try self.value_format(self.ref.operands.items[1], fmt, options, writer);
                    try writer.writeAll(", ");
                    try self.value_format(self.ref.operands.items[2], fmt, options, writer);
                }
                else
                {
                    assert(operand_count == 1);
                    try writer.writeAll("br ");
                    try self.value_format(self.ref.operands.items[0], fmt, options, writer);
                }
            },
            Instruction.ID.Call =>
            {
                if (self.ref.base.type != self.context.get_void_type())
                {
                    try std.fmt.format(writer, "%{} = call ", .{self.result});
                }
                else
                {
                    try std.fmt.format(writer, "call ", .{});
                }
                try self.value_format(self.ref.operands.items[0], fmt, options, writer);
                try writer.writeAll("(");
                if (self.ref.operands.items.len > 1)
                {
                    var arg_index : u64 = 1;
                    while (arg_index < self.ref.operands.items.len - 1) : (arg_index += 1)
                    {
                        try self.value_format(self.ref.operands.items[arg_index], fmt, options, writer);
                        try writer.writeAll(", ");
                    }
                    try self.value_format(self.ref.operands.items[arg_index], fmt, options, writer);
                }
                try writer.writeAll(")");
            },
            Instruction.ID.ICmp =>
            {
                assert(self.ref.operands.items.len == 2);
                try std.fmt.format(writer, "%{} = icmp {} ", .{self.result, self.ref.value.compare_type});
                try self.value_format(self.ref.operands.items[0], fmt, options, writer);
                try writer.writeAll(", ");
                try self.value_format(self.ref.operands.items[1], fmt, options, writer);
            },
            Instruction.ID.Add =>
            {
                assert(self.ref.operands.items.len == 2);
                try std.fmt.format(writer, "%{} = add ", .{self.result}); 
                try self.value_format(self.ref.operands.items[0], fmt, options, writer);
                try writer.writeAll(", ");
                try self.value_format(self.ref.operands.items[1], fmt, options, writer);
            },
            Instruction.ID.Sub =>
            {
                assert(self.ref.operands.items.len == 2);
                try std.fmt.format(writer, "%{} = sub ", .{self.result}); 
                try self.value_format(self.ref.operands.items[0], fmt, options, writer);
                try writer.writeAll(", ");
                try self.value_format(self.ref.operands.items[1], fmt, options, writer);
            },
            Instruction.ID.Mul =>
            {
                assert(self.ref.operands.items.len == 2);
                try std.fmt.format(writer, "%{} = mul ", .{self.result}); 
                try self.value_format(self.ref.operands.items[0], fmt, options, writer);
                try writer.writeAll(", ");
                try self.value_format(self.ref.operands.items[1], fmt, options, writer);
            },
            Instruction.ID.BitCast =>
            {
                try std.fmt.format(writer, "%{} = bitcast ", .{self.result}); 
                try self.value_format(self.ref.operands.items[0], fmt, options, writer);
                try std.fmt.format(writer, " to {}", .{self.ref.base.type});
            },
            Instruction.ID.GetElementPtr =>
            {
                const first_operand = self.ref.operands.items[0];
                assert(first_operand.id == Value.ID.Instruction);
                const instruction = @ptrCast(*Instruction, first_operand);
                var gep_type : *Type = undefined;
                switch (instruction.id)
                {
                    Instruction.ID.Alloca =>
                    {
                        gep_type = instruction.value.alloca.type;
                    },
                    Instruction.ID.Load =>
                    {
                        const pointer_type = @ptrCast(*PointerType, instruction.base.type);
                        assert(pointer_type.base.id == Type.ID.pointer);

                        const base_type = pointer_type.type;
                        // @TODO: probably buggy?
                        gep_type = base_type;
                    },
                    else => panic("not implemented: {}\n", .{instruction.id}),
                }

                try std.fmt.format(writer, "%{} = getelementptr inbounds {}, ", .{self.result, gep_type}); 
                for (self.ref.operands.items) |operand, i|
                {
                    try self.value_format(operand, fmt, options, writer);
                    if (i != self.ref.operands.items.len - 1)
                    {
                        try writer.writeAll(", ");
                    }
                }
            },
            else =>
            {
                panic("Not implemented: {}\n", .{self.ref.id});
            }
        }
    }

    fn value_format(self: *const InstructionPrinter, value: *Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void
    {
        switch (value.id)
        {
            Value.ID.Instruction =>
            {
                const instruction = @ptrCast(*Instruction, value);
                const block_list = self.block_ref.block_printer_list;
                for (block_list.items) |block_printer|
                {
                    for (block_printer.instruction_printers.items) |instruction_printer|
                    {
                        if (instruction == instruction_printer.ref)
                        {
                            try std.fmt.format(writer, "{} %{}", .{instruction.base.type, instruction_printer.result});
                            return;
                        }
                    }
                }

                panic("FIX THIS BUG!!! Instruction was not found\n", .{});
            },
            Value.ID.BasicBlock =>
            {
                const block_list = self.block_ref.block_printer_list;
                const block = @ptrCast(*BasicBlock, value);
                for (block_list.items) |block_printer|
                {
                    if (block_printer.block == block)
                    {
                        try std.fmt.format(writer, "label %{}", .{block_printer.id});
                    }
                }
            },
            else =>
            {
                try std.fmt.format(writer, "{}", .{value});
            }
        }
    }
};

fn print_function(compiler: *Compiler, allocator: *Allocator, context: *Context, function: *Function) void
{
    if (compiler.should_log(Compiler.LogLevel.info))
    {
        // @TODO: change hardcoding (starting_id, next_id)
        var slot_tracker = SlotTracker {
            .next_id = 0,
            .not_used = 0,
            .starting_id = 0,
            .map = ArrayList(*Value).init(allocator),
        };

        for (function.arguments) |*argument|
        {
            _ = slot_tracker.new_id(@ptrCast(*Value, argument));
        }

        var foo_element : Value = undefined;
        _ = slot_tracker.new_id(&foo_element);

        var block_printers = ArrayList(BlockPrinter).initCapacity(allocator, function.basic_blocks.items.len) catch |err| {
            panic("Failed to allocate block printers array\n", .{});
        };

        const entry_block_id = 0xffffffffffffffff;
        for (function.basic_blocks.items) |basic_block|
        {
            var block_printer = BlockPrinter
            {
                .instruction_printers = ArrayList(InstructionPrinter).initCapacity(allocator, basic_block.instructions.items.len) catch |err| {
                    panic("Failed to allocate memory for instruction printer buffer\n", .{});
                },
                .id = block_id_label: {
                    var id: u64 = undefined;
                    const block_parent = @ptrCast(*Function, basic_block.base.parent);
                    if (block_parent.basic_blocks.items[0] != basic_block)
                    {
                        id = slot_tracker.new_id(@ptrCast(*Value, basic_block));
                    }
                    else
                    {
                        id = entry_block_id;
                    }
                    break :block_id_label id;
                },
                .block = basic_block,
                .block_printer_list = &block_printers,
            };

            for (basic_block.instructions.items) |instruction|
            {
                var instruction_printer = InstructionPrinter {
                    .ref = instruction,
                    .result = undefined,
                    .block_ref = &block_printer,
                    .context = context,
                };

                switch (instruction.id)
                {
                    Instruction.ID.Ret,
                    Instruction.ID.Alloca,
                    Instruction.ID.Load,
                    Instruction.ID.ICmp,
                    Instruction.ID.Add,
                    Instruction.ID.Sub,
                    Instruction.ID.Mul,
                    Instruction.ID.BitCast,
                    Instruction.ID.GetElementPtr,
                    =>
                    {
                        instruction_printer.result = slot_tracker.new_id(@ptrCast(*Value, instruction));
                    },
                    Instruction.ID.Call =>
                    {
                        if (instruction.base.type != context.get_void_type())
                        {
                            instruction_printer.result = slot_tracker.new_id(@ptrCast(*Value, instruction));
                        }
                    },
                    Instruction.ID.Br,
                    Instruction.ID.Store, => { },
                    else =>
                    {
                        panic("Not implemented: {}\n", .{instruction.id});
                    }
                }

                block_printer.instruction_printers.append(instruction_printer) catch |err| {
                    panic("Couldn't allocate memory for instruction printer\n", .{});
                };
            }

            block_printers.append(block_printer) catch |err| {
                panic("Error allocating a new block printer\n", .{});
            };
        }

        const function_type = @ptrCast(*FunctionType, function.type);
        const ret_type = function_type.ret_type;
        compiler.log(Compiler.LogLevel.info, "\ndefine dso_local {} @{s}(", .{ret_type, function.name});

        const arg_count = function.arguments.len;
        if (arg_count > 0)
        {
            var arg_i : u64 = 0;
            while (arg_i < arg_count - 1) : (arg_i += 1)
            {
                compiler.log(Compiler.LogLevel.info, "{}, ", .{function.arguments[arg_i]});
            }
            compiler.log(Compiler.LogLevel.info, "{}", .{function.arguments[arg_i]});
        }

        compiler.log(Compiler.LogLevel.info, ")\n{c}\n", .{'{'});

        for (block_printers.items) |*block_printer|
        {
            compiler.log(Compiler.LogLevel.info, "{}", .{block_printer});
        }

        compiler.log(Compiler.LogLevel.info, "{c}\n", .{'}'});
    }
}
