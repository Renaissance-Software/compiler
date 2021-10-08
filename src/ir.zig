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

pub const Reference = struct
{
    value: T,

    const T = u64;

    pub fn get_ID (self: Reference) ID
    {
        return @intToEnum(ID, @intCast(u8, (self.value & (std.math.maxInt(std.meta.Int(.unsigned, @bitSizeOf(ID))) << ID.position)) >> ID.position));
    }

    pub fn get_index(self: Reference) u32
    {
        return @truncate(u32, self.value);
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
        builder.basic_blocks.append(
            .{
                .instructions = ArrayList(Reference).init(allocator),
                .uses = ArrayList(Reference).init(allocator),
                .function_index = 0,
            }) catch unreachable;


        return block_index;
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
    const count = std.enums.values(ID).len;

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

        const position = Reference.ID.position - @bitSizeOf(Instruction.ID);
    };

    const Alloca = struct
    {
        type: Type,
        alloca_type: Type,
        reference: Reference,

        fn new(allocator: *Allocator, builder: *Program.Builder, reference: Reference, alloca_type: Type, array_size: ?*Reference) Reference
        {
            assert(array_size == null);

            const alloca_array_index = builder.instructions.alloca.items.len;
            builder.instructions.alloca.append(.{
                .type = builder.get_or_create_pointer_type(alloca_type),
                .alloca_type = alloca_type,
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

        fn new(allocator: *Allocator, builder: *Program.Builder, value: Reference, pointer: Reference) Reference
        {
            const store_array_index = builder.instructions.store.items.len;
            builder.instructions.store.append(.
                {
                    .value = value,
                    .pointer = pointer,
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
                            .value = std.mem.zeroes(Reference),
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

        fn new(allocator: *Allocator, builder: *Program.Builder, dst_basic_block: u32) Reference
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
            return builder.append_instruction_to_function(instruction);
        }

        fn new_conditional(allocator: *Allocator, builder: *Program.Builder, condition: Reference, dst_basic_block: u32, dst_basic_block_false: u32) Reference
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
            return builder.append_instruction_to_function(instruction);
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
        current_block: u32,
        next_alloca_index: u32,
        allocator: *Allocator,
        scope_to_basic_block_map: ArrayList(u32),
        explicit_return: bool,
        emitted_return: bool,

        fn new(allocator: *Allocator, function: *Parser.Function.Internal) Function.Builder
        {
            var builder = Builder
            {
                .declaration = function.declaration,
                .argument_allocas = ArrayList(Reference).initCapacity(allocator, function.declaration.argument_names.len) catch unreachable,
                .uses = Uses.init(allocator),
                .basic_blocks = ArrayList(u32).init(allocator),
                .allocator = allocator,
                .current_block = 0,
                .next_alloca_index = 0,
                .scope_to_basic_block_map = ArrayList(u32).initCapacity(allocator, function.scopes.len) catch unreachable,
                .emitted_return = false,
                .explicit_return = false,
            };

            builder.scope_to_basic_block_map.items.len = function.scopes.len;

            return builder;
        }
    };

    const Argument = struct
    {
        fn new(index: u32) Reference
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

pub const Program = struct
{
    instructions: struct
    {
        add: []Instruction.Add,
        alloca: []Instruction.Alloca,
        br: []Instruction.Br,
        call: []Instruction.Call,
        icmp: []Instruction.ICmp,
        load: []Instruction.Load,
        store: []Instruction.Store,
        ret: []Instruction.Ret,
    },
    instruction_uses: [std.enums.values(Instruction.ID).len][]Uses,
    basic_blocks: []BasicBlock,
    functions: []Function,
    external: Semantics.External,
    integer_literals: []IntegerLiteral,
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
            icmp: ArrayList(Instruction.ICmp),
            load: ArrayList(Instruction.Load),
            store: ArrayList(Instruction.Store),
            ret: ArrayList(Instruction.Ret),
        },
        basic_blocks: ArrayList(BasicBlock),
        integer_literals: ArrayList(IntegerLiteral),

        instruction_uses: [std.enums.values(Instruction.ID).len]ArrayList(Uses),
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
                    .icmp = ArrayList(Instruction.ICmp).init(allocator),
                    .load = ArrayList(Instruction.Load).init(allocator),
                    .ret = ArrayList(Instruction.Ret).init(allocator),
                    .store = ArrayList(Instruction.Store).init(allocator),
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

            for (builder.integer_literal_uses.items) |*uses|
            {
                uses.* = Uses.init(allocator);
            }

            builder.pointer_types.appendSlice(result.pointer_types) catch unreachable;
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
            const pointer_type = Type.Pointer.new(self.pointer_types.items.len);
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
            const alloca_instructions = entry_block.instructions.items[0..function_builder.next_alloca_index];

            for (alloca_instructions) |alloca|
            {
                const alloca_i = alloca.get_index();
                if (self.instructions.alloca.items[alloca_i].reference.get_index() == expression.get_index())
                {
                    return alloca;
                }
            }

            panic("Not Found\n", .{});
        }

        fn append_instruction_to_function(builder: *Program.Builder, instruction: Reference) Reference
        {
            //log("Appending {} to basic block #{}, which is used in function #{}\n", .{Instruction.get_ID(instruction), basic_block_index, function_index});
            var current_block = builder.get_current_basic_block();
            current_block.instructions.append(instruction) catch unreachable;

            return instruction;
        }

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
                const statement_index = ast_statement.get_index();
                const statement_level = ast_statement.get_level();

                switch (statement_level)
                {
                    .scope =>
                    {
                        const statement_type = ast_statement.get_array_id(.scope);
                        log("Statement type: {}\n", .{statement_type});
                        switch (statement_type)
                        {
                            .invoke_expressions =>
                            {
                                const ast_invoke_expression = scope.invoke_expressions[statement_index];
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
                                                const ast_arg_array_index = ast_argument.get_array_id(.scope);
                                                switch (ast_arg_array_index)
                                                {
                                                    .integer_literals =>
                                                    {
                                                        // @TODO: this can be buggy
                                                        const int_literal = Constant.new(.int, ast_index);
                                                        argument_list.append(int_literal) catch unreachable;
                                                    },
                                                    else => panic("{}\n", .{ast_arg_array_index}),
                                                }
                                            },
                                            else => panic("Level: {}\n", .{ast_arg_level}),
                                        }
                                    }

                                    const call = Instruction.Call.new(allocator, self, called_function_type.return_type, called_function_reference, argument_list.items);
                                    // @TODO: we should be returning this? Yes, but...
                                    _ = call;
                                }
                                else
                                {
                                    const call = Instruction.Call.new(allocator, self, called_function_type.return_type, called_function_reference, std.mem.zeroes([]Reference));
                                    // @TODO: we should be returning this? Yes, but...
                                    _ = call;
                                }
                            },
                            .return_expressions =>
                            {
                                assert(!function_builder.emitted_return);

                                const ast_return_expression = scope.return_expressions[statement_index];
                                if (ast_return_expression.expression) |ast_expression_to_return|
                                {
                                    const ret_expression = self.process_expression(allocator, function_builder, result, ast_expression_to_return);

                                    _ = Instruction.Ret.new(allocator, self, return_type, ret_expression);
                                    function_builder.emitted_return = true;
                                }
                                else
                                {
                                    if (!function_builder.emitted_return)
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

                                assert(assignment.left.get_level() == .scope);
                                const left_id = assignment.left.get_array_id(.scope);
                                const left_reference = switch (left_id)
                                {
                                    .variable_declarations => self.find_expression_alloca(function_builder, assignment.left),
                                    else => panic("NI: {}\n", .{left_id}),
                                };

                                const right_reference = self.process_expression(allocator, function_builder, result, assignment.right);

                                _ = Instruction.Store.new(allocator, self, right_reference, left_reference);
                                //const instruction_count = current.instructions.items.len;
                                //if (true) panic("IC: {}\n", .{instruction_count});
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

                                _ = Instruction.Store.new(allocator, self, operation, left_alloca);
                            },
                            .loops =>
                            {
                                const ast_loop = scope.loops[statement_index];

                                const prefix_block = BasicBlock.new(allocator, self);
                                const body_block = BasicBlock.new(allocator, self);
                                const postfix_block = BasicBlock.new(allocator, self);
                                const end_block = BasicBlock.new(allocator, self);

                                //const continue_block = postfix_block;
                                //const exit_block = end_block;
                                
                                const ast_function = &result.functions[self.current_function];
                                const ast_loop_prefix_scope = ast_function.scopes[ast_loop.prefix_scope_index];
                                const ast_loop_prefix_scope_statement_count = ast_loop_prefix_scope.statements.len;
                                if (ast_loop_prefix_scope_statement_count != 1)
                                {
                                    report_error("More than one statement is not admitted\n", .{});
                                }

                                self.append_block_to_current_function(prefix_block, ast_loop.prefix_scope_index);
                                _ = Instruction.Br.new(allocator, self, prefix_block);
                                function_builder.current_block = prefix_block;


                                const ast_loop_condition = ast_loop_prefix_scope.statements[0];
                                const loop_condition = self.process_comparison(allocator, function_builder, result, ast_loop_condition);
                                _ = Instruction.Br.new_conditional(allocator, self, loop_condition, body_block, end_block);

                                self.append_block_to_current_function(body_block, ast_loop.body_scope_index);

                                self.process_scope(allocator, function_builder, ast_loop.body_scope_index, result, body_block);

                                _ = Instruction.Br.new(allocator, self, postfix_block);

                                self.append_block_to_current_function(postfix_block, ast_loop.postfix_scope_index);

                                self.process_scope(allocator, function_builder, ast_loop.postfix_scope_index, result, postfix_block);

                                if (!function_builder.emitted_return)
                                {
                                    _ = Instruction.Br.new(allocator, self, prefix_block);
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
                                _ = Instruction.Br.new(allocator, self, target_block);
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


                                var exit_block_in_use = true;
                                // @TODO: care about this in the future when it gives errors
                                log("Branching from if conditional\n", .{});
                                _ = Instruction.Br.new_conditional(allocator, self, branch_condition, if_block, else_block);

                                self.append_block_to_current_function(if_block, ast_branch.if_scope);
                                function_builder.emitted_return = false;
                                self.process_scope(allocator, function_builder, ast_branch.if_scope, result, if_block);
                                const if_block_returned = function_builder.emitted_return;
                                
                                if (!self.is_block_terminated(if_block))
                                {
                                    log("Branching from if to exit\n", .{});
                                    _ = Instruction.Br.new(allocator, self, exit_block);
                                }

                                function_builder.emitted_return = false;

                                if (else_block != exit_block)
                                {
                                    self.append_block_to_current_function(else_block, ast_branch.else_scope);
                                    self.process_scope(allocator, function_builder, ast_branch.else_scope.?, result, else_block);

                                    log("Branching from if-else to exit\n", .{});
                                    _ = Instruction.Br.new(allocator, self, exit_block);
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
                else => panic("NI: {}\n", .{comparison.id}),
            };

            return Instruction.ICmp.new(allocator, self, comparison_id, comparison_left, comparison_right);
        }

        fn process_expression(self: *Builder, allocator: *Allocator, function_builder: *Function.Builder, result: Semantics.Result, ast_expression: Entity) Reference
        {
            _ = function_builder;
            _ = allocator;
            _ = result;
            _ = self;

            assert(ast_expression.get_level() == .scope);
            const scope_index = ast_expression.get_array_index();
            const expression_index = ast_expression.get_index();
            _ = expression_index;
            _ = scope_index;

            const expression_id = ast_expression.get_array_id(.scope);

            switch (expression_id)
            {
                .variable_declarations =>
                {
                    const alloca_ref = self.find_expression_alloca(function_builder, ast_expression);
                    const alloca = self.instructions.alloca.items[alloca_ref.get_index()];
                    const load = Instruction.Load.new(allocator, self, alloca.alloca_type, alloca_ref);
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
                        else => panic("ID: {}\n", .{arithmetic_expression.id}),
                    };
                },
                else => panic("NI: {}\n", .{expression_id}),
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
                .external_function => {},
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

    fn new(allocator: *Allocator, builder: *Program.Builder) void
    {
        var formatter = Formatter { .builder = builder, .allocator = allocator };
        log("{}", .{formatter});
    }

    pub fn format(self: *const Formatter, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void
    {
        _ = fmt;

        for (self.builder.function_builders.items) |*function|
        {
            if (function.declaration.argument_names.len > 0) unreachable;
            try Format(writer, "\ndefine {s} @{s}()\n", .{function.declaration.type.return_type.to_string(self), function.declaration.name});

            for (function.basic_blocks.items) |basic_block_index|
            {
                const basic_block = &self.builder.basic_blocks.items[basic_block_index];
                try Format(writer, "\n#{}:\n", .{basic_block_index});

                for (basic_block.instructions.items) |instruction_ref|
                {
                    //const instruction_i = instruction_ref.get_index();
                    const instruction_id = Instruction.get_ID(instruction_ref);
                    const instruction_str = @tagName(instruction_id);

                    switch (instruction_id)
                    {
                        .call =>
                        {
                            const call = &self.builder.instructions.call.items[instruction_ref.get_index()];
                            const callee_return_type = call.type.to_string(self);
                            const callee = call.callee;
                            const callee_name = switch (callee.get_ID())
                            {
                                .global_function => blk:
                                {
                                    const callee_function = &self.builder.function_builders.items[callee.get_index()];
                                    break :blk callee_function.declaration.name;
                                },
                                .external_function => blk:
                                {
                                    const callee_function = &self.builder.external.functions[callee.get_index()];
                                    break :blk callee_function.declaration.name;
                                },
                                else => panic("NI: {}\n", .{callee.get_ID()}),
                            };

                            try Format(writer, "\t{s} {s} @{s}(", .{instruction_str, callee_return_type, callee_name});
                            for (call.arguments) |argument, argument_i|
                            {
                                try self.format_reference(writer, argument);
                                if (argument_i < call.arguments.len - 1)
                                {
                                    try writer.writeAll(", ");
                                }
                            }
                            try writer.writeAll(")\n");

                        },
                        .alloca =>
                        {
                            const alloca = &self.builder.instructions.alloca.items[instruction_ref.get_index()];
                            try Format(writer, "\t{s} {s}\n", .{instruction_str, alloca.alloca_type.to_string(self)});
                        },
                        .store =>
                        {
                            const store = &self.builder.instructions.store.items[instruction_ref.get_index()];
                            const alloca = self.get_alloca(store.pointer);

                            try Format(writer, "\t{s} {s} %placeholder", .{instruction_str, alloca.alloca_type.to_string(self)});
                            //try self.format_reference(writer, store.value);

                            try Format(writer, ", {s} %placeholder\n", .{alloca.type.to_string(self)});
                        },
                        .br =>
                        {
                            const br = &self.builder.instructions.br.items[instruction_ref.get_index()];
                            try Format(writer, "\t{s} ", .{instruction_str});

                            if (br.dst_basic_block_false) |_|
                            {
                                try writer.writeAll("i1 %placeholder, ");
                                try writer.writeAll("label %placeholder, label %placeholder\n");
                            }
                            else
                            {
                                try writer.writeAll("label %placeholder\n");
                            }
                        },
                        .load =>
                        {
                            const load = &self.builder.instructions.load.items[instruction_ref.get_index()];
                            try Format(writer, "\t{s} {s}, ", .{instruction_str, load.type.to_string(self)});
                            const alloca = self.get_alloca(load.pointer);
                            try Format(writer, "{s} %placeholder\n", .{alloca.type.to_string(self)});
                        },
                        .icmp =>
                        {
                            const icmp = &self.builder.instructions.icmp.items[instruction_ref.get_index()];
                            try Format(writer, "\t{s} {s} %placeholder\n", .{instruction_str, @tagName(icmp.id)});
                        },
                        .add =>
                        {
                            const add = &self.builder.instructions.add.items[instruction_ref.get_index()];
                            try Format(writer, "\t{s} {s} %placeholder, %placeholder\n", .{instruction_str, self.get_type(add.left).to_string(self)});
                        },
                        .ret =>
                        {
                            const ret = &self.builder.instructions.ret.items[instruction_ref.get_index()];
                            try Format(writer, "\t{s} {s} %placeholder\n", .{instruction_str, ret.type.to_string(self)});
                        },
                        else => panic("NI: {}\n", .{instruction_id}),
                    }
                }
            }
        }
    }

    fn format_reference(self: *const Formatter, writer: anytype, reference: Reference) !void
    {
        switch (reference.get_ID())
        {
            .constant =>
            {
                switch (Constant.get_ID(reference))
                {
                    .int =>
                    {
                        const int_literal = self.builder.integer_literals.items[reference.get_index()];
                        try Format(writer, "{}", .{int_literal.value});
                    },
                    else => panic("NI: {}\n", .{Constant.get_ID(reference)}),
                }
            },
            //.instruction =>
            //{
                //switch (Instruction.get_ID(reference))
                //{
                    //.alloca =>
                    //{
                        //const alloca = &self.builder.instructions.alloca.items[reference.get_index()];
                    //},
                    //else => panic("NI: {}\n", .{Instruction.get_ID(reference)}),
                //}
            //},
            else => panic("NI: {}\n", .{reference.get_ID()}),
        }
    }

    fn get_alloca(self: *const Formatter, reference: Reference) *Instruction.Alloca
    {
        switch (reference.get_ID())
        {
            .instruction =>
            {
                switch (Instruction.get_ID(reference))
                {
                    .alloca =>
                    {
                        const alloca = &self.builder.instructions.alloca.items[reference.get_index()];
                        return alloca;
                    },
                    else => panic("NI: {}\n", .{Instruction.get_ID(reference)}),
                }
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
            log("Statement id: {}\n", .{statement_id});

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

        var conditional_alloca = returns_something == .something and function_builder.explicit_return;
        _ = conditional_alloca;

        if (function_builder.explicit_return)
        {
            unreachable;
        }

        //var argument_list = ArrayList(Function.
        log("Processing arguments...\n", .{});
        for (function_type.argument_types) |argument_type, argument_i|
        {
            const argument_ref = Function.Argument.new(@intCast(u32, argument_i));
            const argument_alloca = Instruction.Alloca.new(allocator, &builder, argument_ref, argument_type, null);
            function_builder.argument_allocas.append(argument_alloca) catch unreachable;
            _ = Instruction.Store.new(allocator, &builder, argument_ref, argument_alloca);
        }

        // Process function body
        builder.process_scope(allocator, function_builder, 0, result, function_builder.current_block);

        if (conditional_alloca)
        {
            unreachable;
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

    const ir_result = Program
    {
        .instructions = .
        {
            .add = builder.instructions.add.items,
            .alloca = builder.instructions.alloca.items,
            .br = builder.instructions.br.items,
            .call = builder.instructions.call.items,
            .icmp = builder.instructions.icmp.items,
            .load = builder.instructions.load.items,
            .store = builder.instructions.store.items,
            .ret = builder.instructions.ret.items,
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

        .pointer_types = builder.pointer_types.items,
        .slice_types = builder.slice_types.items,
        .function_types = builder.function_types.items, 
        .array_types = builder.array_types.items,
        .struct_types = builder.struct_types.items,
    };

    return ir_result;
}
