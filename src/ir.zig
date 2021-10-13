const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const panic = std.debug.panic;
const Format = std.fmt.format;

// @INFO: this is just for alloca variable references
const Entity = @import("entity.zig").Entity;
const Parser = @import("parser.zig");
const AST = Parser.AST;
const Semantics = @import("semantics.zig");
const Type = @import("type.zig");

const Compiler = @import("compiler.zig");

fn log(comptime format: []const u8, arguments: anytype) void
{
    Compiler.log(.ir, "[IR] " ++ format, arguments);
}

fn report_error(comptime format: []const u8, arguments: anytype) void
{
    panic(format, arguments);
}

pub const Constant = struct
{
    pub const ID = enum(u8)
    {
        array,
        @"struct",
        vector,
        aggregate_zero,
        data_array,
        data_vector,
        fp,
        int,
        null_pointer,

        const position = Reference.ID.position - @bitSizeOf(ID);
    };

    fn new(id: ID, index: u32) Reference
    {
        return .{ .value = (@as(u64, @enumToInt(Reference.ID.constant)) << Reference.ID.position) | (@as(u64, @enumToInt(id)) << ID.position) | index };
    }

    pub fn get_ID(reference: Reference) ID
    {
        return @intToEnum(ID, @intCast(u8, (reference.value & (std.math.maxInt(std.meta.Int(.unsigned, @bitSizeOf(ID))) << ID.position)) >> ID.position));
    }
};

const IntegerLiteral = Parser.IntegerLiteral;
const ArrayLiteral = Parser.ArrayLiteral;

pub const Reference = struct
{
    value: T,

    const T = u64;

    pub const Null = std.mem.zeroes(Reference);

    pub fn get_ID (self: Reference) ID
    {
        return @intToEnum(ID, @intCast(u8, (self.value & (std.math.maxInt(std.meta.Int(.unsigned, @bitSizeOf(ID))) << ID.position)) >> ID.position));
    }

    pub fn get_index(self: Reference) u32
    {
        return @truncate(u32, self.value);
    }

    pub fn get_size(self: Reference, program: *const Program) u64
    {
        assert(self.get_ID() == .instruction);
        assert(Instruction.get_ID(self) == .alloca);
        const instruction_index = self.get_index();
        const alloca = program.instructions.alloca[instruction_index];
        const alloca_type = alloca.base_type;
        const alloca_size = alloca_type.get_size_resolved(program);

        return alloca_size;
    }

    pub const ID = enum(u8)
    {
        none,
        module,
        argument,
        basic_block,
        constant,
        global_function,
        external_function,
        global_variable,
        instruction,
        intrinsic,
        operator,
        operator_bitcast,
        operator_gep,
        operator_pointer_to_int,
        operator_zero_extend,

        const position = @bitSizeOf(T) - @bitSizeOf(ID);
    };

    pub const LLVM_ID = enum
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
        Intrinsic, // In this we differ from llvm, since LLVM doesn't have a Intrinsic category, but they are call instruction children
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

const BasicBlock = struct
{
    instructions: ArrayList(Reference),
    uses: ArrayList(Reference),
    function_index: u32,

    fn new(allocator: *Allocator, builder: *Program.Builder) u32
    {
        const block_index = @intCast(u32, builder.basic_blocks.items.len);
        log("Creating new block #{}\n", .{block_index});

        builder.basic_blocks.append(
            .{
                .instructions = ArrayList(Reference).init(allocator),
                .uses = ArrayList(Reference).init(allocator),
                .function_index = 0,
            }) catch unreachable;


        return block_index;
    }

    fn get_ref(index: u32) Reference
    {
        return .{ .value = (@as(u64, @enumToInt(Reference.ID.basic_block)) << Reference.ID.position) | index };
    }

    fn is_terminated(self: *const BasicBlock) bool
    {
        if (self.instructions.items.len > 0)
        {
            const last_instruction_ID = Instruction.get_ID(self.instructions.items[self.instructions.items.len - 1]);

            if (last_instruction_ID == .br or last_instruction_ID == .ret)
            {
                return true;
            }
        }

        return false;
    }
};

pub const Instruction = struct
{
    const count = last_instruction_id_index + 1;
    const last_instruction_id_index = @enumToInt(ID.memcopy);

    fn new(allocator: *Allocator, builder: *Program.Builder, id: ID, index: u64) Reference
    {
        builder.instruction_uses[@enumToInt(id)].append(ArrayList(Reference).init(allocator)) catch unreachable;
        return .{ .value = (@as(u64, @enumToInt(Reference.ID.instruction)) << Reference.ID.position) | (@as(u64, @enumToInt(id)) << Instruction.ID.position) | index };
    }

    pub fn get_ID(reference: Reference) ID
    {
        assert(reference.get_ID() == .instruction);

        return @intToEnum(ID, @intCast(u8, (reference.value & (std.math.maxInt(std.meta.Int(.unsigned, @bitSizeOf(ID))) << ID.position)) >> ID.position));
    }

    pub const ID = enum(u8)
    {
        // Terminator
        //
        ret = 1,
        br = 2,
        @"switch" = 3,
        indirect_br = 4,
        invoke = 5,
        @"resume" = 6,
        @"unreachable" = 7,
        cleanup_ret = 8,
        catch_ret = 9,
        catch_switch = 10,
        call_br = 11,

        // unary
        fneg = 12,

        // binary
        add = 13,
        fadd = 14,
        sub = 15,
        fsub = 16,
        mul = 17,
        fmul = 18,
        udiv = 19,
        sdiv = 20,
        fdiv = 21,
        urem = 22,
        srem = 23,
        frem = 24,

        // logical
        shl = 25,
        lshr = 26,
        ashr = 27,
        @"and" = 28,
        @"or" = 29,
        xor = 30,

        // memory
        alloca = 31,
        load = 32,
        store = 33,
        get_element_ptr = 34,
        fence = 35,
        atomic_cmp_xchg = 36,
        atomic_rmw = 37,

        // cast
        trunc = 38,
        z_ext = 39,
        s_ext = 40,
        fp_to_ui = 41,
        fp_to_si = 42,
        ui_to_fp = 43,
        si_to_fp = 44,
        fp_trunc = 45,
        fp_ext = 46,
        ptr_to_int = 47,
        int_to_ptr = 48,
        bit_cast = 49,
        addr_space_cast = 50,

        // func_let_pad
        cleanup_pad = 51,
        catch_pad = 52,

        // other
        icmp = 53,
        fcmp = 54,
        phi = 55,
        call = 56,
        select = 57,
        user_op1 = 58,
        user_op2 = 59,
        va_arg = 60,
        extract_element = 61,
        insert_element = 62,
        shuffle_vector = 63,
        extract_value = 64,
        insert_value = 65,
        landing_pad = 66,
        freeze = 67,

        // CUSTOM operations. Why implement intrinsics when we can have instructions
        memcopy = 68,

        const position = Reference.ID.position - @bitSizeOf(Instruction.ID);
    };

    const Alloca = struct
    {
        pointer_type: Type,
        base_type: Type,
        reference: Reference,

        fn new(allocator: *Allocator, builder: *Program.Builder, reference: Reference, alloca_type: Type, array_size: ?*Reference) Reference
        {
            assert(array_size == null);

            const alloca_array_index = builder.instructions.alloca.items.len;
            builder.instructions.alloca.append(.{
                .pointer_type = builder.get_or_create_pointer_type(alloca_type),
                .base_type = alloca_type,
                .reference = reference,
            }) catch unreachable;

            const alloca_instruction = Instruction.new(allocator, builder, .alloca, alloca_array_index);
            
            var function_builder = &builder.function_builders.items[builder.current_function];
            const entry_block_index = function_builder.basic_blocks.items[0];
            builder.basic_blocks.items[entry_block_index].instructions.insert(function_builder.next_alloca_index, alloca_instruction) catch unreachable;
            function_builder.next_alloca_index += 1;

            return alloca_instruction;
        }
    };

    const Store = struct
    {
        value: Reference,
        pointer: Reference,
        type: Type,

        fn new(allocator: *Allocator, builder: *Program.Builder, value: Reference, pointer: Reference, store_type: Type) Reference
        {
            const store_array_index = builder.instructions.store.items.len;
            builder.instructions.store.append(.
                {
                    .value = value,
                    .pointer = pointer,
                    .type = store_type,
                }) catch unreachable;

            const store_instruction = Instruction.new(allocator, builder, .store, store_array_index);
            builder.append_use(pointer, store_instruction);
            builder.append_use(value, store_instruction);
            return builder.append_instruction_to_function(store_instruction);
        }
    };

    const Call = struct
    {
        type: Type,
        callee: Reference,
        arguments: []Reference,

        fn new(allocator: *Allocator, builder: *Program.Builder, return_type: Type, callee: Reference, arguments: []Reference) Reference
        {
            const call_array_index = builder.instructions.call.items.len;
            builder.instructions.call.append(
                .{
                    .type = return_type,
                    .callee = callee,
                    .arguments = arguments,
                }) catch unreachable;

            const call_instruction = Instruction.new(allocator, builder, .call, call_array_index);
            builder.append_use(callee, call_instruction);

            for (arguments) |argument|
            {
                builder.append_use(argument, call_instruction);
            }

            return builder.append_instruction_to_function(call_instruction);
        }
    };

    const Ret = struct
    {
        type: Type,
        value: Reference,

        fn new(allocator: *Allocator, builder: *Program.Builder, return_type: Type, maybe_value: ?Reference) Reference
        {
            const current_block = builder.get_current_basic_block();

            if (!current_block.is_terminated())
            {
                const ret_array_index = builder.instructions.ret.items.len;
                const ret_instruction = Instruction.new(allocator, builder, .ret, ret_array_index);

                const ret = blk:
                {
                    if (maybe_value) |value|
                    {
                        assert(return_type.value != Type.Builtin.void_type.value and return_type.value != Type.Builtin.noreturn_type.value);
                        builder.append_use(value, ret_instruction);

                        break :blk Ret
                        {
                            .type = return_type,
                            .value = value,
                        };
                    }
                    else
                    {
                        assert(return_type.value == Type.Builtin.void_type.value);
                        break :blk Ret
                        {
                            .type = return_type,
                            .value = Reference.Null,
                        };
                    }
                };

                builder.instructions.ret.append(ret) catch unreachable;
                return builder.append_instruction_to_function(ret_instruction);
            }
            else
            {
                // panic("Trying to create a ret in a terminated basic block\n", .{});
                 return undefined;
            }
        }
    };

    pub const Load = struct
    {
        type: Type,
        pointer: Reference,

        fn new(allocator: *Allocator, builder: *Program.Builder, load_type: Type, load_value: Reference) Reference
        {
            const load_array_index = builder.instructions.load.items.len;
            const load_instruction = Instruction.new(allocator, builder, .load, load_array_index);

            builder.instructions.load.append(
                .{
                    .type = load_type,
                    .pointer = load_value,
                }) catch unreachable;

            builder.append_use(load_value, load_instruction);

            return builder.append_instruction_to_function(load_instruction);
        }
    };

    const Br = struct
    {
        condition: ?Reference,
        dst_basic_block_false: ?u32,
        dst_basic_block: u32,

        fn new(allocator: *Allocator, builder: *Program.Builder, dst_basic_block: u32) void
        {
            const current_block = builder.function_builders.items[builder.current_function].current_block;
            if (!builder.is_block_terminated(current_block))
            {
                var list = &builder.instructions.br;
                const array_index = list.items.len;
                list.append(
                    .{
                        .condition = null,
                        .dst_basic_block_false = null,
                        .dst_basic_block = dst_basic_block,
                    }) catch unreachable;

                const instruction = Instruction.new(allocator, builder, .br, array_index);
                builder.basic_blocks.items[dst_basic_block].uses.append(instruction) catch unreachable;
                _ = builder.append_instruction_to_function(instruction);
            }
            else
            {
                log("Block #{} is already terminated, can't branch again to a new block\n", .{current_block});
            }
        }

        fn new_conditional(allocator: *Allocator, builder: *Program.Builder, condition: Reference, dst_basic_block: u32, dst_basic_block_false: u32) void
        {
            const current_block = builder.function_builders.items[builder.current_function].current_block;
            if (!builder.is_block_terminated(current_block))
            {
                var list = &builder.instructions.br;
                const array_index = list.items.len;
                list.append(
                    .{
                        .condition = condition,
                        .dst_basic_block_false = dst_basic_block_false,
                        .dst_basic_block = dst_basic_block,
                    }) catch unreachable;

                const instruction = Instruction.new(allocator, builder, .br, array_index);

                builder.append_use(condition, instruction);
                builder.basic_blocks.items[dst_basic_block].uses.append(instruction) catch unreachable;
                builder.basic_blocks.items[dst_basic_block_false].uses.append(instruction) catch unreachable;
                _ = builder.append_instruction_to_function(instruction);
            }
            else
            {
                log("Block #{} is already terminated, can't branch again to a new block\n", .{current_block});
            }
        }
    };

    pub const ICmp = struct
    {
        left: Reference,
        right: Reference,
        id: ICmpID,

        pub const ICmpID = enum(u8)
        {
            eq,
            ne,
            ugt,
            uge,
            ult,
            ule,
            sgt,
            sge,
            slt,
            sle,
        };

        fn new(allocator: *Allocator, builder: *Program.Builder, id: ICmpID, left: Reference, right: Reference) Reference
        {
            var list = &builder.instructions.icmp;
            const array_index = list.items.len;
            list.append(
                .{
                    .left = left,
                    .right = right,
                    .id = id,
                }) catch unreachable;

            const instruction = Instruction.new(allocator, builder, .icmp, array_index);
            builder.append_use(left, instruction);
            builder.append_use(right, instruction);

            return builder.append_instruction_to_function(instruction);
        }
    };

    pub const Add = struct
    {
        left: Reference,
        right: Reference,

        fn new(allocator: *Allocator, builder: *Program.Builder, left: Reference, right: Reference) Reference
        {
            var list = &builder.instructions.add;
            const array_index = list.items.len;
            list.append(
                .{
                    .left = left,
                    .right = right,
                }) catch unreachable;

            const instruction = Instruction.new(allocator, builder, .add, array_index);
            builder.append_use(left, instruction);
            builder.append_use(right, instruction);

            return builder.append_instruction_to_function(instruction);
        }
    };

    pub const Sub = struct
    {
        left: Reference,
        right: Reference,

        fn new(allocator: *Allocator, builder: *Program.Builder, left: Reference, right: Reference) Reference
        {
            var list = &builder.instructions.sub;
            const array_index = list.items.len;
            list.append(
                .{
                    .left = left,
                    .right = right,
                }) catch unreachable;

            const instruction = Instruction.new(allocator, builder, .sub, array_index);
            builder.append_use(left, instruction);
            builder.append_use(right, instruction);

            return builder.append_instruction_to_function(instruction);
        }
    };

    pub const Mul = struct
    {
        left: Reference,
        right: Reference,

        fn new(allocator: *Allocator, builder: *Program.Builder, left: Reference, right: Reference) Reference
        {
            var list = &builder.instructions.mul;
            const array_index = list.items.len;
            list.append(
                .{
                    .left = left,
                    .right = right,
                }) catch unreachable;

            const instruction = Instruction.new(allocator, builder, .mul, array_index);
            builder.append_use(left, instruction);
            builder.append_use(right, instruction);

            return builder.append_instruction_to_function(instruction);
        }
    };

    pub const GetElementPointer = struct
    {
        indices: []const i64,
        pointer: Reference,
        type: Type,

        fn new(allocator: *Allocator, builder: *Program.Builder, gep_type: Type, pointer: Reference, indices: []const i64) Reference
        {
            var list = &builder.instructions.gep;
            const array_index = list.items.len;
            list.append(
                .{
                    .indices = indices,
                    .pointer = pointer,
                    .type = builder.get_or_create_pointer_type(gep_type),
                }) catch unreachable;

            const instruction = Instruction.new(allocator, builder, .get_element_ptr, array_index);
            builder.append_use(pointer, instruction);

            return builder.append_instruction_to_function(instruction);
        }
    };

    // @TODO: implement bitcasts
    pub const MemCopy = struct
    {
        destination: Reference,
        source: Reference,
        size: u64,

        fn new(allocator: *Allocator, builder: *Program.Builder, dst: Reference, src: Reference, size: u64) Reference
        {
            var list = &builder.instructions.memcopy;
            const array_index = list.items.len;
            list.append(
                .{
                    .destination = dst,
                    .source = src,
                    .size = size,
                }) catch unreachable;
            const instruction = Instruction.new(allocator, builder, .memcopy, array_index);
            builder.append_use(dst, instruction);
            builder.append_use(src, instruction);

            return builder.append_instruction_to_function(instruction);
        }
    };
};

pub const Function = struct
{
    argument_allocas: []Reference,
    declaration: Parser.Function,
    basic_blocks: []u32,

    const Builder = struct
    {
        declaration: Parser.Function,
        argument_allocas: ArrayList(Reference),
        uses: Uses,
        basic_blocks: ArrayList(u32),
        return_alloca: Reference,
        current_block: u32,
        exit_block: u32,
        next_alloca_index: u32,
        scope_to_basic_block_map: ArrayList(u32),
        explicit_return: bool,
        emitted_return: bool,
        conditional_alloca: bool,

        fn new(allocator: *Allocator, function: *Parser.Function.Internal) Function.Builder
        {
            var builder = Builder
            {
                .declaration = function.declaration,
                .argument_allocas = ArrayList(Reference).initCapacity(allocator, function.declaration.argument_names.len) catch unreachable,
                .uses = Uses.init(allocator),
                .basic_blocks = ArrayList(u32).init(allocator),
                .return_alloca = Reference.Null,
                .current_block = 0,
                .exit_block = 0,
                .next_alloca_index = 0,
                .scope_to_basic_block_map = ArrayList(u32).initCapacity(allocator, function.scopes.len) catch unreachable,
                .emitted_return = false,
                .explicit_return = false,
                .conditional_alloca = false,
            };

            builder.scope_to_basic_block_map.items.len = function.scopes.len;

            return builder;
        }
    };

    pub const Argument = struct
    {
        pub fn new(index: u32) Reference
        {
            return .{ .value = (@as(u64, @enumToInt(Reference.ID.argument)) << Reference.ID.position) | index };
        }
    };

    const VariableDeclaration = struct
    {
        fn new(index: u32) Reference
        {
            // @TODO: we use instruction here but we know it is a variable declaration
            return .{ .value = (@as(u64, @enumToInt(Reference.ID.instruction)) << Reference.ID.position) | index };
        }
    };

    fn new(index: u32) Reference
    {
        return .{ .value = (@as(u64, @enumToInt(Reference.ID.global_function)) << Reference.ID.position) | index };
    }
};

pub const ExternalFunction = struct
{
    library_name: []const u8,
    type: Type,

    fn new(index: u32) Reference
    {
        return .{ .value = (@as(u64, @enumToInt(Reference.ID.external_function)) << Reference.ID.position) | index };
    }
};

const Uses = ArrayList(Reference);

var pointer_to_u8: Type = undefined;

pub const Program = struct
{
    instructions: struct
    {
        add: []Instruction.Add,
        alloca: []Instruction.Alloca,
        br: []Instruction.Br,
        call: []Instruction.Call,
        gep: []Instruction.GetElementPointer,
        icmp: []Instruction.ICmp,
        load: []Instruction.Load,
        memcopy: []Instruction.MemCopy,
        mul: []Instruction.Mul,
        ret: []Instruction.Ret,
        store: []Instruction.Store,
        sub: []Instruction.Sub,
    },
    instruction_uses: [Instruction.count][]Uses,
    basic_blocks: []BasicBlock,
    functions: []Function,
    external: Semantics.External,
    integer_literals: []IntegerLiteral,
    array_literals: []ArrayLiteral,
    pointer_types: []Type.Pointer,
    slice_types: []Type.Slice,
    function_types: []Type.Function,
    array_types: []Type.Array,
    struct_types: []Type.Struct,

    pub fn get_block_function_index(self: *const Program, block_index: u32) u32
    {
        const block = &self.basic_blocks[block_index];
        return block.function_index;
    }

    pub const Builder = struct
    {
        const Self = @This();

        instructions: struct
        {
            add: ArrayList(Instruction.Add),
            alloca: ArrayList(Instruction.Alloca),
            br: ArrayList(Instruction.Br),
            call: ArrayList(Instruction.Call),
            gep: ArrayList(Instruction.GetElementPointer),
            icmp: ArrayList(Instruction.ICmp),
            load: ArrayList(Instruction.Load),
            memcopy: ArrayList(Instruction.MemCopy),
            mul: ArrayList(Instruction.Mul),
            store: ArrayList(Instruction.Store),
            sub: ArrayList(Instruction.Sub),
            ret: ArrayList(Instruction.Ret),
        },
        basic_blocks: ArrayList(BasicBlock),
        integer_literals: ArrayList(IntegerLiteral),
        array_literals: ArrayList(ArrayLiteral),

        instruction_uses: [Instruction.count]ArrayList(Uses),
        integer_literal_uses: ArrayList(Uses),

        function_builders: ArrayList(Function.Builder),
        external: Semantics.External,
        pointer_types: ArrayList(Type.Pointer),
        slice_types: ArrayList(Type.Slice),
        function_types: ArrayList(Type.Function),
        array_types: ArrayList(Type.Array),
        struct_types: ArrayList(Type.Struct),

        current_function: u32,

        fn new(allocator: *Allocator, result: Semantics.Result) Self
        {
            var builder = Builder
            {
                .instructions = .
                {
                    .add = ArrayList(Instruction.Add).init(allocator),
                    .alloca = ArrayList(Instruction.Alloca).init(allocator),
                    .br = ArrayList(Instruction.Br).init(allocator),
                    .call = ArrayList(Instruction.Call).init(allocator),
                    .gep = ArrayList(Instruction.GetElementPointer).init(allocator),
                    .icmp = ArrayList(Instruction.ICmp).init(allocator),
                    .load = ArrayList(Instruction.Load).init(allocator),
                    .memcopy = ArrayList(Instruction.MemCopy).init(allocator),
                    .mul = ArrayList(Instruction.Mul).init(allocator),
                    .ret = ArrayList(Instruction.Ret).init(allocator),
                    .store = ArrayList(Instruction.Store).init(allocator),
                    .sub = ArrayList(Instruction.Sub).init(allocator),
                },
                .instruction_uses = blk:
                {
                    var instruction_uses: [Instruction.count]ArrayList(Uses) = undefined;
                    std.mem.set(ArrayList(Uses), instruction_uses[0..], ArrayList(Uses).init(allocator));
                    break :blk instruction_uses;
                },
                .basic_blocks = ArrayList(BasicBlock).init(allocator),
                .integer_literal_uses = ArrayList(Uses).initCapacity(allocator, result.integer_literals.len) catch unreachable,
                .function_builders = ArrayList(Function.Builder).initCapacity(allocator, result.functions.len) catch unreachable,
                .external = result.external,
                .integer_literals = ArrayList(IntegerLiteral).initCapacity(allocator, result.integer_literals.len) catch unreachable,
                .array_literals = ArrayList(ArrayLiteral).initCapacity(allocator, result.array_literals.len) catch unreachable,
                .pointer_types = ArrayList(Type.Pointer).initCapacity(allocator, result.pointer_types.len) catch unreachable,
                .slice_types = ArrayList(Type.Slice).initCapacity(allocator, result.slice_types.len) catch unreachable,
                .function_types = ArrayList(Type.Function).initCapacity(allocator, result.function_types.len) catch unreachable,
                .array_types = ArrayList(Type.Array).initCapacity(allocator, result.array_types.len) catch unreachable,
                .struct_types = ArrayList(Type.Struct).initCapacity(allocator, result.struct_types.len) catch unreachable,

                .current_function = 0,
            };

            
            //for (result.external_functions) |external_function|
            //{
                //builder.external_functions.append(.{
                    //.type = external_function.declaration.type,
                    //.library_name = external_function.library,
                //}) catch unreachable;
            //}
            // @TODO: Avoid all these copies

            builder.function_builders.items.len = result.functions.len;

            for (result.functions) |*ast_function, function_i|
            {
                var function_builder = &builder.function_builders.items[function_i];
                function_builder.* = Function.Builder.new(allocator, ast_function);
            }

            builder.integer_literals.appendSlice(result.integer_literals) catch unreachable;
            builder.integer_literal_uses.items.len = result.integer_literals.len;
            builder.array_literals.appendSlice(result.array_literals) catch unreachable;

            for (builder.integer_literal_uses.items) |*uses|
            {
                uses.* = Uses.init(allocator);
            }

            builder.pointer_types.appendSlice(result.pointer_types) catch unreachable;
            pointer_to_u8 = builder.get_or_create_pointer_type(Type.Integer.new(8, .unsigned));
            builder.slice_types.appendSlice(result.slice_types) catch unreachable;
            builder.function_types.appendSlice(result.function_types) catch unreachable;
            builder.array_types.appendSlice(result.array_types) catch unreachable;
            builder.struct_types.appendSlice(result.struct_types) catch unreachable;

            return builder;
        }

        fn is_block_terminated(self: *Program.Builder, basic_block_index: u32) bool
        {
            const block = &self.basic_blocks.items[basic_block_index];
            const instruction_count = block.instructions.items.len;
            if (instruction_count == 0) return false;

            const last_instruction_index = instruction_count - 1;
            const last_instruction = block.instructions.items[last_instruction_index];
            const last_instruction_id = Instruction.get_ID(last_instruction);

            return last_instruction_id == .ret or last_instruction_id == .br;
        }

        fn append_block_to_current_function(self: *Program.Builder, basic_block_index: u32, scope_index: ?u32) void
        {
            var function_builder = &self.function_builders.items[self.current_function];
            var block = &self.basic_blocks.items[basic_block_index];
            const function_block_index = @intCast(u32, function_builder.basic_blocks.items.len);
            block.function_index = function_block_index;

            function_builder.basic_blocks.append(basic_block_index) catch unreachable;

            if (scope_index) |scope|
            {
                function_builder.scope_to_basic_block_map.items[scope] = basic_block_index;
            }
        }

        fn get_or_create_pointer_type(self: *Self, p_type: Type) Type
        {
            const pointer_type = Type.Pointer.new(self.pointer_types.items.len, 0);
            self.pointer_types.append(.{ .type = p_type }) catch unreachable;
            return pointer_type;
        }

        fn introspect_branch_for_allocas(self: *Self, function: *const Parser.Function.Internal, scope_index: u32, branch_expression: Entity) bool
        {
            assert(branch_expression.get_level() == .scope);
            assert(branch_expression.get_array_id(.scope) == .branches);
            assert(branch_expression.get_array_index() == scope_index);

            var scope = function.scopes[scope_index];
            const branch = &scope.branches[branch_expression.get_index()];

            if (self.introspect_for_allocas(function, branch.if_scope))
            {
                return true;
            }

            if (branch.else_scope) |else_scope_index|
            {
                if (self.introspect_for_allocas(function, else_scope_index))
                {
                    return true;
                }
            }

            return false;
        }

        fn introspect_loop_for_allocas(self: *Self, function: *const Parser.Function.Internal, scope_index: u32, loop_expression: Entity) bool
        {
            assert(loop_expression.get_level() == .scope);
            assert(loop_expression.get_array_id(.scope) == .loops);
            assert(loop_expression.get_array_index() == scope_index);

            var scope = function.scopes[scope_index];
            const loop = &scope.loops[loop_expression.get_index()];

            if (self.introspect_for_allocas(function, loop.prefix_scope_index))
            {
                return true;
            }
            if (self.introspect_for_allocas(function, loop.body_scope_index))
            {
                return true;
            }
            if (self.introspect_for_allocas(function, loop.postfix_scope_index))
            {
                return true;
            }

            return false;
        }

        fn introspect_for_allocas(self: *Self, function: *const Parser.Function.Internal, scope_index: u32) bool
        {
            var scope = function.scopes[scope_index];

            for (scope.statements) |statement|
            {
                assert(statement.get_level() == .scope);
                const statement_id = statement.get_array_id(.scope);

                if (statement_id == .return_expressions)
                {
                    return true;
                }
                else if (statement_id == .branches)
                {
                    if (self.introspect_branch_for_allocas(function, scope_index, statement))
                    {
                        return true;
                    }
                }
                else if (statement_id == .loops)
                {
                    if (self.introspect_loop_for_allocas(function, scope_index, statement))
                    {
                        return true;
                    }
                }
            }

            return false;
        }

        fn find_expression_alloca(self: *Program.Builder, function_builder: *Function.Builder, expression: Entity) Reference
        {
            const entry_block_index = function_builder.basic_blocks.items[0];
            const entry_block = self.basic_blocks.items[entry_block_index];
            const alloca_instructions = entry_block.instructions.items[@boolToInt(function_builder.conditional_alloca)..function_builder.next_alloca_index];
            const expression_index = expression.get_index(); // + @boolToInt(function_builder.conditional_alloca);

            for (alloca_instructions) |function_alloca_ref|
            {
                assert(Instruction.get_ID(function_alloca_ref) == .alloca);
                const global_alloca_index = function_alloca_ref.get_index();

                const alloca = self.instructions.alloca.items[global_alloca_index];
                const alloca_reference_index = alloca.reference.get_index();

                if (alloca_reference_index == expression_index)
                {
                    return function_alloca_ref;
                }
            }

            panic("Not Found\n", .{});
        }

        fn append_instruction_to_function(self: *Program.Builder, instruction: Reference) Reference
        {
            const function_index = self.current_function;
            const current_block_index = self.function_builders.items[function_index].current_block;
            const current_block = &self.basic_blocks.items[current_block_index];
            log("Appending {} to basic block #{}, which is used in function #{}\n", .{Instruction.get_ID(instruction), current_block_index, function_index});
            current_block.instructions.append(instruction) catch unreachable;

            return instruction;
        }

        //fn initialize_aggregate_with_constant(self: *Self, alloca: Reference, rvalue: Reference, data_type: Type) void
        //{
            //unreachable;
        //}

        fn process_scope(self: *Builder, allocator: *Allocator, function_builder: *Function.Builder, scope_index: u32, result: Semantics.Result, existing_block: ?u32) void
        {
            function_builder.current_block = existing_block orelse blk:
            {
                 const block_index = BasicBlock.new(allocator, self);
                 self.append_block_to_current_function(block_index, scope_index);
                 break :blk block_index;
            };

            log("Processing new scope in basic block #{}\n", .{function_builder.current_block});
            var scope = result.functions[self.current_function].scopes[scope_index];

            const return_type = function_builder.declaration.type.return_type;

            for (scope.statements) |ast_statement|
            {
                // @INFO: Discard statements if the function has return in every existing branch
                if (!function_builder.emitted_return)
                {
                    const statement_index = ast_statement.get_index();
                    const statement_level = ast_statement.get_level();

                    switch (statement_level)
                    {
                        .scope =>
                        {
                            const statement_type = ast_statement.get_array_id(.scope);
                            log("\t====>> Statement type: {}\n", .{statement_type});
                            switch (statement_type)
                            {
                                .invoke_expressions =>
                                {
                                    _ = self.process_invoke_expression(allocator, function_builder, result, ast_statement);
                                },
                                .return_expressions =>
                                {
                                    assert(!function_builder.emitted_return);
                                    function_builder.emitted_return = true;

                                    const ast_return_expression = scope.return_expressions[statement_index];
                                    if (ast_return_expression.expression) |ast_expression_to_return|
                                    {
                                        const ret_expression = self.process_expression(allocator, function_builder, result, ast_expression_to_return);

                                        if (function_builder.conditional_alloca)
                                        {
                                            assert(function_builder.return_alloca.value != Reference.Null.value);
                                            assert(function_builder.exit_block != 0);

                                            const store_ref = Instruction.Store.new(allocator, self, ret_expression, function_builder.return_alloca, return_type);
                                            {
                                                _ = store_ref;
                                                // @TODO: debug assert that uses >= 1
                                                //const store = self.instructions.store[store_ref.get_index()];
                                            }

                                            Instruction.Br.new(allocator, self, function_builder.exit_block);
                                        }
                                        else
                                        {
                                            _ = Instruction.Ret.new(allocator, self, return_type, ret_expression);
                                        }
                                    }
                                    else
                                    {
                                        if (!function_builder.explicit_return)
                                        {
                                            _ = Instruction.Ret.new(allocator, self, return_type, null);
                                            function_builder.emitted_return = true;
                                        }
                                        else
                                        {
                                            unreachable;
                                        }
                                    }
                                },
                                .variable_declarations =>
                                {
                                    const variable_declaration = scope.variable_declarations[statement_index];
                                    const var_type = variable_declaration.type;
                                    _ = Instruction.Alloca.new(allocator, self, Function.VariableDeclaration.new(statement_index), var_type, null);
                                },
                                .assignments =>
                                {
                                    const assignment = scope.assignments[statement_index];

                                    var store_type: Type = undefined;

                                    assert(assignment.left.get_level() == .scope);
                                    const left_id = assignment.left.get_array_id(.scope);

                                    switch (left_id)
                                    {
                                        .variable_declarations =>
                                        {
                                            const alloca_ref = self.find_expression_alloca(function_builder, assignment.left);
                                            const alloca = self.instructions.alloca.items[alloca_ref.get_index()];
                                            store_type = alloca.base_type;
                                            switch (store_type.get_ID())
                                            {
                                                .array =>
                                                {
                                                    // bitcast to u8
                                                    // memcpy
                                                    const right_reference = self.process_expression(allocator, function_builder, result, assignment.right);
                                                    assert(right_reference.get_ID() == .constant);
                                                    const memcopy_size = store_type.get_size(self);
                                                    _ = Instruction.MemCopy.new(allocator, self, alloca_ref, right_reference, memcopy_size);
                                                },
                                                else =>
                                                {
                                                    const left_reference = alloca_ref;
                                                    const right_reference = self.process_expression(allocator, function_builder, result, assignment.right);
                                                    _ = Instruction.Store.new(allocator, self, right_reference, left_reference, store_type);
                                                }
                                            }
                                        },
                                        .dereference_expressions =>
                                        {
                                            const dereference_expression = &result.functions[self.current_function].scopes[scope_index].dereference_expressions[assignment.left.get_index()];
                                            const expression_alloca = self.find_expression_alloca(function_builder, dereference_expression.reference); 
                                            const alloca = self.instructions.alloca.items[expression_alloca.get_index()];
                                            const pointer_type = alloca.base_type;
                                            store_type = Type.Pointer.get_base_type(pointer_type, self.pointer_types.items);
                                            const pointer_load = Instruction.Load.new(allocator, self, pointer_type, expression_alloca);
                                            const left_reference = pointer_load;
                                            const right_reference = self.process_expression(allocator, function_builder, result, assignment.right);

                                            _ = Instruction.Store.new(allocator, self, right_reference, left_reference, store_type);
                                        },
                                        else => panic("NI: {}\n", .{left_id}),
                                    }
                                },
                                .compound_assignments =>
                                {
                                    const compound_assignment = scope.compound_assignments[statement_index];
                                    assert(compound_assignment.left.get_level() == .scope);
                                    assert(compound_assignment.right.get_level() == .scope);

                                    const right = self.process_expression(allocator, function_builder, result, compound_assignment.right);
                                    const left = self.process_expression(allocator, function_builder, result, compound_assignment.left);
                                    _ = left; _ = right;

                                    const operation = switch (compound_assignment.id)
                                    {
                                        .add => Instruction.Add.new(allocator, self, left, right),
                                        else => panic("ID: {}\n", .{compound_assignment.id}),
                                    };

                                    assert(compound_assignment.left.get_level() == .scope);
                                    const left_id = compound_assignment.left.get_array_id(.scope);
                                    const left_alloca = switch (left_id)
                                    {
                                        .variable_declarations => self.find_expression_alloca(function_builder, compound_assignment.left),
                                        else => panic("NI: {}\n", .{left_id}),
                                    };

                                    assert(Instruction.get_ID(left_alloca) == .alloca);
                                    const alloca = self.instructions.alloca.items[left_alloca.get_index()];
                                    const store_type = alloca.base_type;

                                    _ = Instruction.Store.new(allocator, self, operation, left_alloca, store_type);
                                },
                                .loops =>
                                {
                                    const ast_loop = scope.loops[statement_index];

                                    const prefix_block = BasicBlock.new(allocator, self);
                                    const body_block = BasicBlock.new(allocator, self);
                                    const postfix_block = BasicBlock.new(allocator, self);
                                    const end_block = BasicBlock.new(allocator, self);

                                    const ast_function = &result.functions[self.current_function];
                                    const ast_loop_prefix_scope = ast_function.scopes[ast_loop.prefix_scope_index];
                                    const ast_loop_prefix_scope_statement_count = ast_loop_prefix_scope.statements.len;
                                    if (ast_loop_prefix_scope_statement_count != 1)
                                    {
                                        report_error("More than one statement is not admitted\n", .{});
                                    }

                                    self.append_block_to_current_function(prefix_block, ast_loop.prefix_scope_index);
                                    Instruction.Br.new(allocator, self, prefix_block);
                                    function_builder.current_block = prefix_block;


                                    const ast_loop_condition = ast_loop_prefix_scope.statements[0];
                                    const loop_condition = self.process_comparison(allocator, function_builder, result, ast_loop_condition);
                                    Instruction.Br.new_conditional(allocator, self, loop_condition, body_block, end_block);

                                    self.append_block_to_current_function(body_block, ast_loop.body_scope_index);

                                    self.process_scope(allocator, function_builder, ast_loop.body_scope_index, result, body_block);

                                    Instruction.Br.new(allocator, self, postfix_block);

                                    self.append_block_to_current_function(postfix_block, ast_loop.postfix_scope_index);

                                    self.process_scope(allocator, function_builder, ast_loop.postfix_scope_index, result, postfix_block);

                                    if (!function_builder.emitted_return)
                                    {
                                        Instruction.Br.new(allocator, self, prefix_block);
                                        self.append_block_to_current_function(end_block, null);
                                        function_builder.current_block = end_block;
                                    }
                                },
                                .break_expressions =>
                                {
                                    const ast_break = scope.break_expressions[statement_index];
                                    const ast_loop_ref = ast_break.loop_to_break;
                                    const loop_index = ast_loop_ref.get_index();
                                    const loop_scope = ast_loop_ref.get_array_index();
                                    const ast_loop = &result.functions[self.current_function].scopes[loop_scope].loops[loop_index];
                                    const loop_prefix_basic_block_index = function_builder.scope_to_basic_block_map.items[ast_loop.prefix_scope_index];
                                    const loop_prefix_block = self.basic_blocks.items[loop_prefix_basic_block_index];
                                    const br_instruction = loop_prefix_block.instructions.items[loop_prefix_block.instructions.items.len - 1];
                                    assert(Instruction.get_ID(br_instruction) == .br);
                                    const br = self.instructions.br.items[br_instruction.get_index()];
                                    assert(br.dst_basic_block_false != null);
                                    const target_block = br.dst_basic_block_false.?;

                                    log("Branching from break\n", .{});
                                    Instruction.Br.new(allocator, self, target_block);
                                },
                                .branches =>
                                {
                                    const ast_branch = scope.branches[statement_index];

                                    const branch_condition = self.process_comparison(allocator, function_builder, result, ast_branch.condition);

                                    const if_block = BasicBlock.new(allocator, self);
                                    const exit_block = BasicBlock.new(allocator, self);
                                    const else_block = 
                                        if (ast_branch.else_scope) |_|
                                            BasicBlock.new(allocator, self)
                                        else
                                            exit_block;

                                    if (else_block == exit_block) log("Exit block same else\n", .{}) else log("Exit block is different than else\n", .{});

                                    var exit_block_in_use = true;
                                    // @TODO: care about this in the future when it gives errors
                                    log("Branching from if conditional\n", .{});
                                    Instruction.Br.new_conditional(allocator, self, branch_condition, if_block, else_block);

                                    self.append_block_to_current_function(if_block, ast_branch.if_scope);
                                    function_builder.emitted_return = false;
                                    self.process_scope(allocator, function_builder, ast_branch.if_scope, result, if_block);
                                    log("Current block after if block processing: {}\n", .{function_builder.current_block});
                                    const if_block_returned = function_builder.emitted_return;

                                    log("Branching from if to exit\n", .{});
                                    Instruction.Br.new(allocator, self, exit_block);

                                    function_builder.emitted_return = false;

                                    if (else_block != exit_block)
                                    {
                                        self.append_block_to_current_function(else_block, ast_branch.else_scope);
                                        self.process_scope(allocator, function_builder, ast_branch.else_scope.?, result, else_block);
                                        log("Current block after else scope: {}\n", .{function_builder.current_block});

                                        log("Branching from if-else to exit\n", .{});
                                        Instruction.Br.new(allocator, self, exit_block);
                                    }

                                    const else_block_returned = function_builder.emitted_return;

                                    function_builder.emitted_return = if_block_returned and else_block_returned;

                                    if (exit_block_in_use and !function_builder.emitted_return)
                                    {
                                        self.append_block_to_current_function(exit_block, null);
                                        function_builder.current_block = exit_block;
                                    }
                                },
                                else => panic("ni: {}\n", .{statement_type}),
                            }
                        },
                        else => panic("ni: {}\n", .{statement_level}),
                    }
                }
            }
        }

        fn process_comparison(self: *Builder, allocator: *Allocator, function_builder: *Function.Builder, result: Semantics.Result, ast_comparison_ref: Entity) Reference
        {
            assert(ast_comparison_ref.get_level() == .scope);
            assert(ast_comparison_ref.get_array_id(.scope) == Entity.ScopeID.comparisons);

            const scope_index = ast_comparison_ref.get_array_index();
            const comparison_index = ast_comparison_ref.get_index();

            const comparison = &result.functions[self.current_function].scopes[scope_index].comparisons[comparison_index];

            const comparison_left = self.process_expression(allocator, function_builder, result, comparison.left);
            const comparison_right = self.process_expression(allocator, function_builder, result, comparison.right);

            // @TODO: care about signedness
            const comparison_id: Instruction.ICmp.ICmpID = switch (comparison.id)
            {
                .less => .slt,
                .equal => .eq,
                .greater => .sgt,
                else => panic("NI: {}\n", .{comparison.id}),
            };

            return Instruction.ICmp.new(allocator, self, comparison_id, comparison_left, comparison_right);
        }

        fn process_invoke_expression(self: *Builder, allocator: *Allocator, function_builder: *Function.Builder, result: Semantics.Result, ast_invoke_expression_ref: Entity) Reference
        {
            assert(ast_invoke_expression_ref.get_level() == .scope);
            assert(ast_invoke_expression_ref.get_array_id(.scope) == Entity.ScopeID.invoke_expressions);

            const scope_index = ast_invoke_expression_ref.get_array_index();
            const invoke_expression_index = ast_invoke_expression_ref.get_index();

            const ast_invoke_expression = &result.functions[self.current_function].scopes[scope_index].invoke_expressions[invoke_expression_index];
            const expression = ast_invoke_expression.expression;
            assert(expression.get_level() == .global);

            var called_function_reference: Reference = undefined;
            var ast_called_function_declaration: Parser.Function = undefined;
            const called_function_index = expression.get_index();

            if (expression.get_array_id(.global) == .resolved_internal_functions)
            {
                called_function_reference = Function.new(called_function_index);
                ast_called_function_declaration = result.functions[called_function_index].declaration;
            }
            else if (expression.get_array_id(.global) == .resolved_external_functions)
            {
                called_function_reference = ExternalFunction.new(called_function_index);
                ast_called_function_declaration = result.external.functions[called_function_index].declaration;
            }
            else unreachable;

            //const called_function_type = result.function_types[ast_called_function_declaration.type.get_index()];
            const called_function_type = ast_called_function_declaration.type;

            const called_function_argument_count = ast_called_function_declaration.argument_names.len;
            if (called_function_argument_count > 0)
            {
                var argument_list = ArrayList(Reference).initCapacity(allocator, called_function_argument_count) catch unreachable;

                for (ast_invoke_expression.arguments) |ast_argument|
                {
                    const ast_arg_level = ast_argument.get_level();
                    const ast_index = ast_argument.get_index();

                    switch (ast_arg_level)
                    {
                        .scope =>
                        {
                            const argument_id = ast_argument.get_array_id(.scope);

                            switch (argument_id)
                            {
                                .integer_literals =>
                                {
                                    // @TODO: this can be buggy
                                    const int_literal = Constant.new(.int, ast_index);
                                    argument_list.append(int_literal) catch unreachable;
                                },
                                .variable_declarations =>
                                {
                                    const argument_alloca_ref = self.find_expression_alloca(function_builder, ast_argument);
                                    const argument_alloca = self.instructions.alloca.items[argument_alloca_ref.get_index()];
                                    const argument_load = Instruction.Load.new(allocator, self, argument_alloca.base_type, argument_alloca_ref);
                                    argument_list.append(argument_load) catch unreachable;
                                },
                                .address_of_expressions =>
                                {
                                    const address_of_expression = &result.functions[self.current_function].scopes[scope_index].address_of_expressions[ast_index];
                                    const expression_to_take_address_of = address_of_expression.reference;
                                    const expression_id = expression_to_take_address_of.get_array_id(.scope);
                                    assert(expression_id == .variable_declarations);

                                    const ast_argument_alloca = self.find_expression_alloca(function_builder, expression_to_take_address_of);
                                    argument_list.append(ast_argument_alloca) catch unreachable;
                                },
                                else => panic("{}\n", .{argument_id}),
                            }
                        },
                        else => panic("Level: {}\n", .{ast_arg_level}),
                    }
                }

                const call = Instruction.Call.new(allocator, self, called_function_type.return_type, called_function_reference, argument_list.items);
                // @TODO: we should be returning this? Yes, but...
                return call;
            }
            else
            {
                const call = Instruction.Call.new(allocator, self, called_function_type.return_type, called_function_reference, std.mem.zeroes([]Reference));
                // @TODO: we should be returning this? Yes, but...
                return call;
            }
        }

        fn get_constant_integer(self: *Builder, expression: Entity) i64
        {
            _ = self;
            const level = expression.get_level();
            switch (level)
            {
                .scope =>
                {
                    const array_id = expression.get_array_id(.scope);
                    switch (array_id)
                    {
                        .integer_literals =>
                        {
                            const int_lit = self.integer_literals.items[expression.get_index()];
                            if (int_lit.signed)
                            {
                                return -@intCast(i64, int_lit.value);
                            }
                            else
                            {
                                return @intCast(i64, int_lit.value);
                            }
                        },
                        else => panic("ni: {}\n", .{array_id}),
                    }
                },
                else => panic("ni: {}\n", .{level}),
            }
        }

        fn process_expression(self: *Builder, allocator: *Allocator, function_builder: *Function.Builder, result: Semantics.Result, ast_expression: Entity) Reference
        {
            const ast_expr_level = ast_expression.get_level();
            switch (ast_expr_level)
            {
                .scope =>
                {
                    const scope_index = ast_expression.get_array_index();
                    const expression_index = ast_expression.get_index();

                    const expression_id = ast_expression.get_array_id(.scope);

                    switch (expression_id)
                    {
                        .variable_declarations =>
                        {
                            const alloca_ref = self.find_expression_alloca(function_builder, ast_expression);
                            const alloca = self.instructions.alloca.items[alloca_ref.get_index()];
                            const load = Instruction.Load.new(allocator, self, alloca.base_type, alloca_ref);
                            return load;
                        },
                        .integer_literals =>
                        {
                            // @TODO: this can be buggy
                            const int_literal = Constant.new(.int, expression_index);
                            return int_literal;
                        },
                        .arithmetic_expressions =>
                        {
                            const arithmetic_expression = &result.functions[self.current_function].scopes[scope_index].arithmetic_expressions[expression_index];
                            const left = self.process_expression(allocator, function_builder, result, arithmetic_expression.left);
                            const right = self.process_expression(allocator, function_builder, result, arithmetic_expression.right);

                            return switch (arithmetic_expression.id)
                            {
                                .add => Instruction.Add.new(allocator, self, left, right),
                                .sub => Instruction.Sub.new(allocator, self, left, right),
                                .mul => Instruction.Mul.new(allocator, self, left, right),
                                else => panic("ID: {}\n", .{arithmetic_expression.id}),
                            };
                        },
                        .invoke_expressions => return self.process_invoke_expression(allocator, function_builder, result, ast_expression),
                        .address_of_expressions =>
                        {
                            const address_of_expression = &result.functions[self.current_function].scopes[scope_index].address_of_expressions[expression_index];
                            const expression_alloca = self.find_expression_alloca(function_builder, address_of_expression.reference); 
                            return expression_alloca;
                        },
                        .dereference_expressions =>
                        {
                            const dereference_expression = &result.functions[self.current_function].scopes[scope_index].dereference_expressions[expression_index];
                            const expression_alloca = self.find_expression_alloca(function_builder, dereference_expression.reference); 
                            const alloca = self.instructions.alloca.items[expression_alloca.get_index()];
                            const pointer_type_ref = alloca.base_type;
                            assert(pointer_type_ref.get_ID() == .pointer);
                            const pointer_load = Instruction.Load.new(allocator, self, pointer_type_ref, expression_alloca);
                            // @TODO: assert that this is true
                            // here we expect the dereference to be a rvalue expression
                            {
                                const dereference_type = Type.Pointer.get_base_type(pointer_type_ref, self.pointer_types.items);
                                const dereference_load = Instruction.Load.new(allocator, self, dereference_type, pointer_load);
                                return dereference_load;
                            }
                        },
                        .arguments =>
                        {
                            const argument_index = expression_index;
                            const alloca_ref = function_builder.argument_allocas.items[expression_index];
                            const argument_type = function_builder.declaration.type.argument_types[argument_index];
                            const argument_load = Instruction.Load.new(allocator, self, argument_type, alloca_ref);
                            return argument_load;
                        },
                        .array_literals =>
                        {
                            const array_literal = self.array_literals.items[expression_index];
                            log("Array literal type {}\n", .{array_literal.type.get_ID()});
                            const array_literal_element_count = array_literal.elements.len;
                            log("Array literal length: {}\n", .{array_literal_element_count});

                            return Constant.new(.array, expression_index);
                        },
                        .array_subscript_expressions =>
                        {
                            const array_subscript_expression = &result.functions[self.current_function].scopes[scope_index].array_subscript_expressions[expression_index];

                            const index_expression = self.get_constant_integer(array_subscript_expression.index);
                            log("Index expression: {}\n", .{index_expression});
                            const array_expression_alloca = self.find_expression_alloca(function_builder, array_subscript_expression.expression);
                            const alloca = self.instructions.alloca.items[array_expression_alloca.get_index()];
                            const array_type_ref = alloca.base_type;
                            assert(array_type_ref.get_ID() == .array);
                            const array_type = self.array_types.items[array_type_ref.get_index()];
                            const array_element_type = array_type.type;
                            // @TODO: should we dynamically allocate this? It can cause stack corruption problems
                            var indices = ArrayList(i64).initCapacity(allocator, 2) catch unreachable;
                            indices.appendAssumeCapacity(0);
                            indices.appendAssumeCapacity(index_expression);

                            const gep = Instruction.GetElementPointer.new(allocator, self, array_element_type, array_expression_alloca, indices.items);

                            // @TODO: this is always RVALUE

                            const gep_load = Instruction.Load.new(allocator, self, array_element_type, gep);
                            return gep_load;
                        },
                        else => panic("NI: {}\n", .{expression_id}),
                    }
                },
                .global =>
                {
                    const global_expr_id = ast_expression.get_array_id(.global);
                    switch (global_expr_id)
                    {
                        .resolved_internal_functions =>
                        {
                            unreachable;
                            //var function = &result.functions[expression_index];
                        },
                        else => panic("NI: {}\n", .{global_expr_id}),
                    }
                },
                else => panic("Ni: {}\n", .{ast_expr_level}),
            }
        }

        fn append_use(self: *Self, value: Reference, use: Reference) void
        {
            _ = use;
            _ = self;

            switch (value.get_ID())
            {
                .constant =>
                {
                    switch (Constant.get_ID(value))
                    {
                        .int =>
                        {
                            self.integer_literal_uses.items[value.get_index()].append(use) catch unreachable;
                        },
                        // @TODO: add in the future
                        .array => {},
                        else => panic("{}\n", .{Constant.get_ID(value)}),
                    }
                },
                .instruction =>
                {
                    self.instruction_uses[@enumToInt(Instruction.get_ID(value))].items[value.get_index()].append(use) catch unreachable;
                },
                .global_function =>
                {
                    self.function_builders.items[value.get_index()].uses.append(use) catch unreachable;
                },
                // @TODO:
                .external_function => {},
                .argument => {},
                else => panic("{}\n", .{value.get_ID()}),
            }
        }

        fn get_current_basic_block(self: *Builder) *BasicBlock
        {
            return &self.basic_blocks.items[self.function_builders.items[self.current_function].current_block];
        }

    };

    pub fn get_uses(self: *const Program, instruction: Instruction.ID, index: u32) []Reference
    {
        return self.instruction_uses[@enumToInt(instruction)][index].items;
    }
};

const ReturnKind = enum
{
    void,
    noreturn,
    something,
};

pub const Formatter = struct
{
    builder: *Program.Builder,
    allocator: *Allocator,

    const SlotTracker = struct
    {
        slots: ArrayList(Reference),
        next_id: u32,
        starting_id: u32,

        fn new_index(self: *SlotTracker, reference: Reference) u32
        {
            const index = self.next_id;
            self.slots.append(reference) catch unreachable;
            self.next_id += 1;
            return index;
        }
    };

    const InstructionPrinter = struct
    {
        ref: Reference,
        id: ?u32,
        block_ref: u32,
    };

    const BlockPrinter = struct
    {
        instructions: ArrayList(InstructionPrinter),
        ref: u32,
        id: u32,
        parent: *ArrayList(BlockPrinter),
    };

    fn new(allocator: *Allocator, builder: *Program.Builder) void
    {
        var formatter = Formatter { .builder = builder, .allocator = allocator };
        log("{}", .{formatter});
    }

    fn setup_block(self: *const Formatter, block_printers: *ArrayList(BlockPrinter), slot_tracker: *SlotTracker, basic_block_index: u32, block_id: u32) !void
    {
        const basic_block = &self.builder.basic_blocks.items[basic_block_index];
        const instruction_count = basic_block.instructions.items.len;
        //try Format(writer, "Basic block: {}. Instruction count: {}\n", .{basic_block_index, instruction_count});

        var block_printer = BlockPrinter
        {
            .instructions = ArrayList(InstructionPrinter).initCapacity(self.allocator, instruction_count) catch unreachable,
            .ref = basic_block_index,
            .id = block_id,
            .parent = block_printers,
        };

        for (basic_block.instructions.items) |instruction|
        {
            var instruction_printer = InstructionPrinter
            {
                .ref = instruction,
                .id = null,
                .block_ref = basic_block_index,
            };

            const instruction_id = Instruction.get_ID(instruction);

            switch (instruction_id)
            {
                .call =>
                {
                    const call = self.builder.instructions.call.items[instruction.get_index()];
                    if (call.type.value != Type.Builtin.void_type.value)
                    {
                        instruction_printer.id = slot_tracker.new_index(instruction);
                    }
                },
                .alloca,
                .load,
                .icmp,
                .mul,
                .add,
                .sub,
                .get_element_ptr,
                => instruction_printer.id = slot_tracker.new_index(instruction),
                .memcopy,
                .ret,
                .store,
                .br => {},
                else => panic("Ni: {}\n", .{instruction_id}),
            }

            block_printer.instructions.append(instruction_printer) catch unreachable;
        }

        block_printers.append(block_printer) catch unreachable;
    }

    fn get_index(writer: anytype, reference: Reference, slot_tracker: *SlotTracker) u32
    {
        for (slot_tracker.slots.items) |ref, ref_i|
        {
            if (ref.value == reference.value)
            {
                return @intCast(u32, ref_i);
            }
        }

        Format(writer, "Cant get: {}, {}\n", .{reference.get_ID(), reference.get_index()}) catch unreachable;
        unreachable;
    }

    pub fn format(self: *const Formatter, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void
    {
        _ = fmt;

        for (self.builder.function_builders.items) |*function, function_i|
        {
            var slot_tracker = SlotTracker
            {
                .slots = ArrayList(Reference).init(self.allocator),
                .next_id = 0,
                .starting_id = 0,
            };

            for (function.declaration.argument_names) |_, arg_i|
            {
                _ = slot_tracker.new_index(Function.Argument.new(@intCast(u32, arg_i)));
            }

            _ = slot_tracker.new_index(undefined);

            const basic_block_count: u64 = function.basic_blocks.items.len;
            var block_printers = ArrayList(BlockPrinter).initCapacity(self.allocator, basic_block_count) catch unreachable;

            const entry_block_index = function.basic_blocks.items[0];
            try self.setup_block(&block_printers, &slot_tracker, entry_block_index, 0xffffffff);

            if (basic_block_count > 1)
            {
                for (function.basic_blocks.items[1..]) |basic_block_index|
                {
                    const block_id = slot_tracker.new_index(BasicBlock.get_ref(basic_block_index));
                    try self.setup_block(&block_printers, &slot_tracker, basic_block_index, block_id);
                }
            }

            try Format(writer, "\ndefine {s} @{s}(", .{function.declaration.type.return_type.to_string(self), function.declaration.name});

            const argument_count = function.declaration.type.argument_types.len;
            if (argument_count > 0)
            {
                for (function.declaration.type.argument_types[0..argument_count - 1]) |argument_type, argument_i|
                {
                    try Format(writer, "{s} %{}, ", .{argument_type.to_string(self), argument_i});
                }

                try Format(writer, "{s} %{}", .{function.declaration.type.argument_types[argument_count - 1].to_string(self), argument_count - 1});
            }

            try Format(writer, ") #{}\n{c}\n", .{function_i, '{'});

            for (block_printers.items) |block_printer|
            {
                if (block_printer.id != 0xffffffff)
                {
                    try Format(writer, "{}:\n", .{block_printer.id});
                }

                for (block_printer.instructions.items) |instruction_printer|
                {
                    const instruction_ref = instruction_printer.ref;
                    const instruction_id = Instruction.get_ID(instruction_ref);
                    const instruction_str = @tagName(instruction_id);
                    const instruction_index = instruction_ref.get_index();

                    //if (true) try Format(writer, "Instruction index: {}. Block index: {}\n", .{instruction_index, block_printer.ref});
                        
                    if (instruction_printer.id) |ir_id|
                    {
                        try Format(writer, "\t%{} = ", .{ir_id});
                    }
                    else
                    {
                        try writer.writeAll("\t");
                    }


                    try Format(writer, "{s} ", .{instruction_str});

                    switch (instruction_id)
                    {
                        .call =>
                        {
                            const call = &self.builder.instructions.call.items[instruction_index];
                            const callee_return_type = call.type.to_string(self);
                            const callee = call.callee;
                            const callee_declaration = switch (callee.get_ID())
                            {
                                .global_function => blk:
                                {
                                    const callee_function = &self.builder.function_builders.items[callee.get_index()];
                                    break :blk callee_function.declaration;
                                },
                                .external_function => blk:
                                {
                                    const callee_function = &self.builder.external.functions[callee.get_index()];
                                    break :blk callee_function.declaration;
                                },
                                else => panic("NI: {}\n", .{callee.get_ID()}),
                            };

                            try Format(writer, "{s} @{s}(", .{callee_return_type, callee_declaration.name});
                            for (call.arguments) |argument, argument_i|
                            {
                                try self.format_reference(writer, function, argument, &slot_tracker, callee_declaration.type.argument_types[argument_i]);
                                if (argument_i < call.arguments.len - 1)
                                {
                                    try writer.writeAll(", ");
                                }
                            }
                            try writer.writeAll(")");
                        },
                        .alloca =>
                        {
                            const alloca = &self.builder.instructions.alloca.items[instruction_index];
                            try Format(writer, "{s}", .{alloca.base_type.to_string(self)});
                        },
                        .store =>
                        {
                            const store = &self.builder.instructions.store.items[instruction_index];
                            try self.format_reference(writer, function, store.value, &slot_tracker, store.type);
                            try writer.writeAll(", ");
                            try self.format_reference(writer, function, store.pointer, &slot_tracker, null);
                        },
                        .br =>
                        {
                            const br = &self.builder.instructions.br.items[instruction_index];
                            if (br.condition) |br_condition|
                            {
                                try self.format_reference(writer, function, br_condition, &slot_tracker, null);
                                try writer.writeAll(", ");
                                try self.format_reference(writer, function, BasicBlock.get_ref(br.dst_basic_block), &slot_tracker, null);
                                try writer.writeAll(", ");
                                try self.format_reference(writer, function, BasicBlock.get_ref(br.dst_basic_block_false.?), &slot_tracker, null);
                            }
                            else
                            {
                                try self.format_reference(writer, function, BasicBlock.get_ref(br.dst_basic_block), &slot_tracker, null);
                            }
                        },
                        .load =>
                        {
                            const load = &self.builder.instructions.load.items[instruction_ref.get_index()];
                            try Format(writer, "{s}, ", .{load.type.to_string(self)});
                            try self.format_reference(writer, function, load.pointer, &slot_tracker, load.type);
                        },
                        .icmp =>
                        {
                            const icmp = &self.builder.instructions.icmp.items[instruction_ref.get_index()];
                            try Format(writer, "{s} ", .{@tagName(icmp.id)});
                            try self.format_reference(writer, function, icmp.left, &slot_tracker, null);
                            try writer.writeAll(", ");
                            try self.format_reference(writer, function, icmp.right, &slot_tracker, null);
                        },
                        .mul =>
                        {
                            const mul = &self.builder.instructions.mul.items[instruction_ref.get_index()];
                            try self.format_reference(writer, function, mul.left, &slot_tracker, null);
                            try writer.writeAll(", ");
                            try self.format_reference(writer, function, mul.right, &slot_tracker, null);
                        },
                        .add =>
                        {
                            const add = &self.builder.instructions.add.items[instruction_ref.get_index()];
                            try self.format_reference(writer, function, add.left, &slot_tracker, null);
                            try writer.writeAll(", ");
                            try self.format_reference(writer, function, add.right, &slot_tracker, null);
                        },
                        .sub =>
                        {
                            const sub = &self.builder.instructions.sub.items[instruction_ref.get_index()];
                            try self.format_reference(writer, function, sub.left, &slot_tracker, null);
                            try writer.writeAll(", ");
                            try self.format_reference(writer, function, sub.right, &slot_tracker, null);
                        },
                        .ret =>
                        {
                            const ret = &self.builder.instructions.ret.items[instruction_ref.get_index()];
                            try self.format_reference(writer, function, ret.value, &slot_tracker, ret.type);
                        },
                        .memcopy =>
                        {
                            const memcopy = &self.builder.instructions.memcopy.items[instruction_ref.get_index()];
                            try self.format_reference(writer, function, memcopy.destination, &slot_tracker, null);
                            try writer.writeAll(", ");
                            try self.format_reference(writer, function, memcopy.source, &slot_tracker, null);
                            try writer.writeAll(", ");
                            try Format(writer, "i64 {}", .{memcopy.size});
                        },
                        .get_element_ptr =>
                        {
                            const gep = &self.builder.instructions.gep.items[instruction_ref.get_index()];
                            try Format(writer, "inbounds {s}, ", .{gep.type.to_string(self)});

                            try self.format_reference(writer, function, gep.pointer, &slot_tracker, null);
                            try writer.writeAll(", ");

                            for (gep.indices[0..gep.indices.len-1]) |index|
                            {
                                try Format(writer, "{}, ", .{index});
                            }

                            try Format(writer, "{}", .{gep.indices[gep.indices.len - 1]});
                        },
                        else =>
                        {
                            try Format(writer, "not implemented: {}\n", .{instruction_id});
                            unreachable;
                        }
                    }

                    try writer.writeAll("\n");
                }
            }

            try writer.writeAll("}\n");
        }
    }

    fn format_reference(self: *const Formatter, writer: anytype, current_function: *Function.Builder, reference: Reference, slot_tracker: *SlotTracker, expected_type: ?Type) !void
    {
        switch (reference.get_ID())
        {
            .constant =>
            {
                switch (Constant.get_ID(reference))
                {
                    .int =>
                    {
                        // @TODO: fix this
                        const type_str = 
                            if (expected_type) |exp_type|
                                exp_type.to_string(self)
                            else
                                "s32";

                        const int_literal = self.builder.integer_literals.items[reference.get_index()];
                        try Format(writer, "{s} {}", .{type_str, int_literal.value});
                    },
                    .array =>
                    {
                        //const array_literal = self.builder.array_literals.items[reference.get_index()];
                        //const array_type = array_literal.type;

                        try writer.writeAll("@array_lit_placeholder");
                    },
                    else => panic("NI: {}\n", .{Constant.get_ID(reference)}),
                }
            },
            .instruction =>
            {
                const index = get_index(writer, reference, slot_tracker);
                switch (Instruction.get_ID(reference))
                {
                    .alloca =>
                    {
                        const alloca = &self.builder.instructions.alloca.items[reference.get_index()];
                        const alloca_type_str = alloca.pointer_type.to_string(self);

                        try Format(writer, "{s} %{}", .{alloca_type_str, index});
                    },
                    .load =>
                    {
                        const load = &self.builder.instructions.load.items[reference.get_index()];
                        const load_type_str = load.type.to_string(self);

                        try Format(writer, "{s} %{}", .{load_type_str, index});
                    },
                    .icmp =>
                    {
                        try Format(writer, "i1 %{}", .{index});
                    },
                    .mul =>
                    {
                        try Format(writer, "s32 %{}", .{index});
                    },
                    .add =>
                    {
                        try Format(writer, "s32 %{}", .{index});
                    },
                    .sub =>
                    {
                        try Format(writer, "s32 %{}", .{index});
                    },
                    .call =>
                    {
                        const call = &self.builder.instructions.call.items[reference.get_index()];
                        try Format(writer, "{s} %{}", .{call.type.to_string(self), index});
                    },
                    .get_element_ptr =>
                    {
                        const gep = &self.builder.instructions.gep.items[reference.get_index()];
                        try Format(writer, "{s} %{}", .{gep.type.to_string(self), index});
                    },
                    else => panic("NI: {}\n", .{Instruction.get_ID(reference)}),
                }
            },
            .basic_block =>
            {
                try Format(writer, "label %{}", .{get_index(writer, reference, slot_tracker)});
            },
            .argument =>
            {
                const argument_index = reference.get_index();
                const argument_type = current_function.declaration.type.argument_types[argument_index];
                try Format(writer, "{s} %{}", .{argument_type.to_string(self), argument_index});
            },
            // @INFO: this is void type
            .none =>
            {
                try writer.writeAll("void");
            },
            else => panic("NI: {}\n", .{reference.get_ID()}),
        }
    }

    fn get_type(self: *const Formatter, reference: Reference) Type
    {
        _ = self;
        switch (reference.get_ID())
        {
            .instruction =>
            {
                switch (Instruction.get_ID(reference))
                {
                    .load =>
                    {
                        const load = &self.builder.instructions.load.items[reference.get_index()];
                        return load.type;
                    },
                    else => panic("NI: {}\n", .{Instruction.get_ID(reference)}),
                }
            },
            else => panic("NI: {}\n", .{reference.get_ID()}),
        }
    }
};

pub fn generate(allocator: *Allocator, result: Semantics.Result) Program
{
    // @TODO: we are not doing anything relevant here. Maybe look for symbols here in parallel, check if they exist?

    var builder = Program.Builder.new(allocator, result);

    for (result.functions) |*ast_function, ast_function_i|
    {
        log("Processing internal function: {s}\n", .{ast_function.declaration.name});

        const function_type = ast_function.declaration.type;
        builder.current_function = @intCast(u32, ast_function_i);
        var function_builder = &builder.function_builders.items[builder.current_function];

        // Create function entry block
        function_builder.current_block = BasicBlock.new(allocator, &builder);
        builder.append_block_to_current_function(function_builder.current_block, 0);

        // @TODO: dont pick argument names to take argument length?
        const argument_count = ast_function.declaration.argument_names.len;
        _ = argument_count;
        const return_type = function_type.return_type;
        const returns_something = blk:
        {
            if (return_type.get_ID() == .builtin)
            {
                if (return_type.value == Type.Builtin.void_type.value)
                {
                    break :blk ReturnKind.void;
                }
                else if (return_type.value == Type.Builtin.noreturn_type.value)
                {
                    break :blk ReturnKind.noreturn;
                }
                else
                {
                    unreachable;
                }
            }
            else
            {
                break :blk ReturnKind.something;
            }
        };

        for (ast_function.scopes[0].statements) |statement|
        {
            assert(statement.get_level() == .scope);
            const statement_id = statement.get_array_id(.scope);

            if (statement_id == .branches)
            {
                if (builder.introspect_branch_for_allocas(ast_function, 0, statement))
                {
                    function_builder.explicit_return = true;
                    break;
                }
            }
            else if (statement_id == .loops)
            {
                if (builder.introspect_loop_for_allocas(ast_function, 0, statement))
                {
                    function_builder.explicit_return = true;
                    break;
                }
            }
        }

        function_builder.conditional_alloca = returns_something == .something and function_builder.explicit_return;

        if (function_builder.explicit_return)
        {
            function_builder.exit_block = BasicBlock.new(allocator, &builder);

            if (function_builder.conditional_alloca)
            {
                function_builder.return_alloca = Instruction.Alloca.new(allocator, &builder, Reference.Null, return_type, null);
            }
        }

        //var argument_list = ArrayList(Function.
        log("Processing arguments...\n", .{});
        for (function_type.argument_types) |argument_type, argument_i|
        {
            const argument_ref = Function.Argument.new(@intCast(u32, argument_i));
            const argument_alloca = Instruction.Alloca.new(allocator, &builder, argument_ref, argument_type, null);
            function_builder.argument_allocas.append(argument_alloca) catch unreachable;
            _ = Instruction.Store.new(allocator, &builder, argument_ref, argument_alloca, argument_type);
        }

        // Process function body
        builder.process_scope(allocator, function_builder, 0, result, function_builder.current_block);

        if (function_builder.conditional_alloca)
        {
            assert(builder.basic_blocks.items[function_builder.current_block].instructions.items.len > 0);
            assert(function_builder.exit_block != 0);
            assert(function_builder.return_alloca.value != Reference.Null.value);

            builder.append_block_to_current_function(function_builder.exit_block, null);
            function_builder.current_block = function_builder.exit_block;

            const return_alloca = builder.instructions.alloca.items[function_builder.return_alloca.get_index()];
            const return_alloca_type = return_alloca.base_type;
            const loaded_return = Instruction.Load.new(allocator, &builder, return_alloca_type, function_builder.return_alloca);
            _ = Instruction.Ret.new(allocator, &builder, return_alloca_type, loaded_return);
        }
        else if (returns_something == .void)
        {
            if (function_builder.explicit_return)
            {
                unreachable;
            }

            // @TODO: this can cause bugs
            if (!function_builder.emitted_return)
            {
                _ = Instruction.Ret.new(allocator, &builder, return_type, null);
            }
        }
    }

    var functions = ArrayList(Function).initCapacity(allocator, result.functions.len) catch unreachable;

    for (builder.function_builders.items) |fb|
    {
        functions.append(.{
            .argument_allocas = fb.argument_allocas.items,
            .declaration = fb.declaration,
            .basic_blocks = fb.basic_blocks.items,
        }) catch unreachable;
    }

    Formatter.new(allocator, &builder);
    //if (true) std.os.exit(0);

    const ir_result = Program
    {
        .instructions = .
        {
            .add = builder.instructions.add.items,
            .alloca = builder.instructions.alloca.items,
            .br = builder.instructions.br.items,
            .call = builder.instructions.call.items,
            .gep = builder.instructions.gep.items,
            .icmp = builder.instructions.icmp.items,
            .load = builder.instructions.load.items,
            .memcopy = builder.instructions.memcopy.items,
            .mul = builder.instructions.mul.items,
            .ret = builder.instructions.ret.items,
            .store = builder.instructions.store.items,
            .sub = builder.instructions.sub.items,
        },
        .instruction_uses = blk: {
            var uses: [Instruction.count][]Uses = undefined;
            for (builder.instruction_uses) |use, use_i|
            {
                uses[use_i] = use.items;
            }

            break :blk uses;
        },
        .basic_blocks = builder.basic_blocks.items,
        .functions = functions.items,
        .external = builder.external,
        .integer_literals = builder.integer_literals.items,
        .array_literals = builder.array_literals.items,

        .pointer_types = builder.pointer_types.items,
        .slice_types = builder.slice_types.items,
        .function_types = builder.function_types.items, 
        .array_types = builder.array_types.items,
        .struct_types = builder.struct_types.items,
    };

    return ir_result;
}
