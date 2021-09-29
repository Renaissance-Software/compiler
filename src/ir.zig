const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const panic = std.debug.panic;

const Parser = @import("parser.zig");
const AST = Parser.AST;
const Semantics = @import("semantics.zig");
const Type = @import("type.zig");

const Compiler = @import("compiler.zig");

fn log(comptime format: []const u8, arguments: anytype) void
{
    Compiler.log(.ir, format, arguments);
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
        return @intToEnum(ID, (reference.value & (std.math.maxInt(std.meta.Int(.unsigned, @bitSizeOf(ID))) << ID.position)) >> ID.position);
    }
};

const IntegerLiteral = Parser.IntegerLiteral;

pub const Reference = struct
{
    value: T,

    const T = u64;

    pub fn get_ID (self: Reference) ID
    {
        return @intToEnum(ID, (self.value & (std.math.maxInt(std.meta.Int(.unsigned, @bitSizeOf(ID))) << ID.position)) >> ID.position);
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
    instructions: []Reference,

    const Builder = struct
    {
        instructions: ArrayList(Reference),
        index: u64,

        fn new(allocator: *Allocator, function_builder: *Function.Builder) *BasicBlock.Builder
        {
            const basic_block_index = function_builder.basic_blocks.items.len;
            function_builder.basic_blocks.append(
            .{
                .instructions = ArrayList(Reference).init(allocator),
                .index = basic_block_index,
            }) catch unreachable;

            return &function_builder.basic_blocks.items[basic_block_index];
        }
    };
};

pub const Instruction = struct
{
    fn new(id: ID, index: u64) Reference
    {
        return .{ .value = (@as(u64, @enumToInt(Reference.ID.instruction)) << Reference.ID.position) | (@as(u64, @enumToInt(id)) << Instruction.ID.position) | index };
    }

    pub fn get_ID(reference: Reference) ID
    {
        assert(reference.get_ID() == .instruction);
        return @intToEnum(ID, (reference.value & (std.math.maxInt(std.meta.Int(.unsigned, @bitSizeOf(ID))) << ID.position)) >> ID.position);
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

        fn create(builder: *Program.Builder, alloca_type: Type, array_size: ?*Reference) Reference
        {
            assert(array_size == null);

            const alloca_array_index = builder.instructions.alloca.items.len;
            builder.instructions.alloca.append(.{
                .type = builder.get_or_create_pointer_type(alloca_type),
                .alloca_type = alloca_type,
            }) catch unreachable;

            const alloca_instruction = Instruction.new(.alloca, alloca_array_index);
            
            var function_builder = &builder.function_builders.items[builder.current_function];
            function_builder.basic_blocks.items[0].instructions.insert(function_builder.next_alloca_index, alloca_instruction) catch unreachable;
            function_builder.next_alloca_index += 1;

            return alloca_instruction;
        }
    };

    const Store = struct
    {
        value: Reference,
        pointer: Reference,

        fn new(builder: *Program.Builder, value: Reference, pointer: Reference) Reference
        {
            const store_array_index = builder.instructions.store.items.len;
            builder.instructions.store.append(.
                {
                    .value = value,
                    .pointer = pointer,
                }) catch unreachable;

            const store_instruction = Instruction.new(.store, store_array_index);
            var function_builder = &builder.function_builders.items[builder.current_function];
            function_builder.basic_blocks.items[function_builder.current_block].instructions.append(store_instruction) catch unreachable;

            return store_instruction;
        }
    };

    const Call = struct
    {
        type: Type,
        callee: Reference,
        arguments: []Reference,

        fn new(builder: *Program.Builder, return_type: Type, callee: Reference, arguments: []Reference) Reference
        {
            const call_array_index = builder.instructions.call.items.len;
            builder.instructions.call.append(
                .{
                    .type = return_type,
                    .callee = callee,
                    .arguments = arguments,
                }) catch unreachable;

            const call_instruction = Instruction.new(.call, call_array_index);
            var function_builder = &builder.function_builders.items[builder.current_function];
            function_builder.basic_blocks.items[function_builder.current_block].instructions.append(call_instruction) catch unreachable;

            return call_instruction;
        }
    };

    const Ret = struct
    {
        type: Type,
        value: Reference,

        fn new(builder: *Program.Builder, return_type: Type, maybe_value: ?*Reference) Reference
        {
            var function_builder = &builder.function_builders.items[builder.current_function];
            if (!function_builder.is_terminated())
            {
                const ret = blk:
                {
                    if (maybe_value) |value|
                    {
                        assert(return_type.value != Type.Builtin.void_type.value and return_type.value != Type.Builtin.noreturn_type.value);
                        const value_id = value.get_ID();
                        panic("Value ID: {}\n", .{value_id});
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

                const ret_array_index = builder.instructions.ret.items.len;
                builder.instructions.ret.append(ret) catch unreachable;
                const ret_instruction = Instruction.new(.ret, ret_array_index);
                function_builder.basic_blocks.items[function_builder.current_block].instructions.append(ret_instruction) catch unreachable;

                return ret_instruction;
            }
            else
            {
                // panic("Trying to create a ret in a terminated basic block\n", .{});
                 return undefined;
            }
        }
    };
};

pub const Function = struct
{
    argument_allocas: []Reference,
    argument_names: [][]const u8,
    type: Type.Function,
    basic_blocks: []BasicBlock,
    instructions: []Reference,

    const Builder = struct
    {
        argument_allocas: ArrayList(Reference),
        argument_names: [][]const u8,
        type: Type.Function,
        basic_blocks: ArrayList(BasicBlock.Builder),
        instructions: ArrayList(Reference),
        current_block: u32,
        next_alloca_index: u32,
        allocator: *Allocator,

        fn new(allocator: *Allocator, function: Parser.Function) Function.Builder
        {
            return Builder
            {
                .argument_allocas = ArrayList(Reference).initCapacity(allocator, function.argument_names.len) catch unreachable,
                .argument_names = function.argument_names,
                .type = function.type,
                .basic_blocks = ArrayList(BasicBlock.Builder).init(allocator),
                .instructions = ArrayList(Reference).init(allocator),
                .allocator = allocator,
                .current_block = 0,
                .next_alloca_index = 0,
            };
        }

        fn is_terminated(self: *Builder) bool
        {
            var current_block = self.basic_blocks.items[self.current_block];

            if (current_block.instructions.items.len > 0)
            {
                const last_instruction_ID = Instruction.get_ID(current_block.instructions.items[current_block.instructions.items.len - 1]);

                if (last_instruction_ID == .br or last_instruction_ID == .ret)
                {
                    return true;
                }
            }

            return false;
        }
    };

    const Argument = struct
    {
        fn new(index: u32) Reference
        {
            return .{ .value = (@as(u64, @enumToInt(Reference.ID.argument)) << Reference.ID.position) | index };
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

pub const Program = struct
{
    instructions: struct
    {
        alloca: []Instruction.Alloca,
        store: []Instruction.Store,
        call: []Instruction.Call,
        ret: []Instruction.Ret,
    },
    functions: []Function,
    external: Semantics.External,
    integer_literals: []IntegerLiteral,
    pointer_types: []Type.Pointer,
    slice_types: []Type.Slice,
    function_types: []Type.Function,
    array_types: []Type.Array,
    struct_types: []Type.Struct,

    const Builder = struct
    {
        const Self = @This();

        instructions: struct
        {
            alloca: ArrayList(Instruction.Alloca),
            store: ArrayList(Instruction.Store),
            call: ArrayList(Instruction.Call),
            ret: ArrayList(Instruction.Ret),
        },

        function_builders: ArrayList(Function.Builder),
        external: Semantics.External,
        integer_literals: ArrayList(IntegerLiteral),
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
                    .alloca = ArrayList(Instruction.Alloca).init(allocator),
                    .store = ArrayList(Instruction.Store).init(allocator),
                    .call = ArrayList(Instruction.Call).init(allocator),
                    .ret = ArrayList(Instruction.Ret).init(allocator),
                },
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

            std.debug.print("AST semantics integer literal count: {}\n", .{result.integer_literals.len});
            
            //for (result.external_functions) |external_function|
            //{
                //builder.external_functions.append(.{
                    //.type = external_function.declaration.type,
                    //.library_name = external_function.library,
                //}) catch unreachable;
            //}
            // @TODO: Avoid all these copies

            builder.integer_literals.appendSlice(result.integer_literals) catch unreachable;

            builder.pointer_types.appendSlice(result.pointer_types) catch unreachable;
            builder.slice_types.appendSlice(result.slice_types) catch unreachable;
            builder.function_types.appendSlice(result.function_types) catch unreachable;
            builder.array_types.appendSlice(result.array_types) catch unreachable;
            builder.struct_types.appendSlice(result.struct_types) catch unreachable;

            return builder;
        }

        fn get_or_create_pointer_type(self: *Self, p_type: Type) Type
        {
            _ = self; _ = p_type;
            panic("not implemented\n", .{});
        }
    };
};

const ReturnKind = enum
{
    void,
    noreturn,
    something,
};

pub fn generate(allocator: *Allocator, result: Semantics.Result) Program
{
    // @TODO: we are not doing anything relevant here
    //for (result.libraries) |library, library_i|
    //{
        //log("Library [#{}]: {s}\n", .{library_i, result.library_names[library_i]});

        //for (library.functions) |function, function_i|
        //{
            //log("Symbol [#{}]: {s}\n", .{function_i, function.name});
        //}
    //}

    var builder = Program.Builder.new(allocator, result);

    for (result.functions) |ast_function, ast_function_i|
    {
        log("Processing internal function: {s}\n", .{ast_function.declaration.name});

        const function_type = ast_function.declaration.type;
        builder.function_builders.append(Function.Builder.new(allocator, ast_function.declaration)) catch unreachable;
        builder.current_function = @intCast(u32, ast_function_i);

        var function_builder = &builder.function_builders.items[ast_function_i];
        var entry_block = BasicBlock.Builder.new(allocator, function_builder);
        function_builder.current_block = @intCast(u32, entry_block.index);
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

        var explicit_return = false;
        // @TODO: loop to detect alloca count

        const ast_main_block = &ast_function.scopes[0];
        var conditional_alloca = returns_something == .something and explicit_return;
        _ = conditional_alloca;

        if (explicit_return)
        {
            unreachable;
        }

        //var argument_list = ArrayList(Function.
        log("Processing arguments...\n", .{});
        for (function_type.argument_types) |argument_type, argument_i|
        {
            const argument_alloca = Instruction.Alloca.create(&builder, argument_type, null);
            builder.function_builders.items[builder.current_function].argument_allocas.append(argument_alloca) catch unreachable;
            const argument_ref = Function.Argument.new(@intCast(u32, argument_i));
            _ = Instruction.Store.new(&builder, argument_ref, argument_alloca);
        }

        for (ast_main_block.statements) |ast_statement|
        {
            const statement_index = ast_statement.get_index();
            const statement_level = ast_statement.get_level();
            
            switch (statement_level)
            {
                .scope =>
                {
                    const array_index = ast_statement.get_array_index(.scope);
                    switch (array_index)
                    {
                        .invoke_expressions =>
                        {
                            const ast_invoke_expression = ast_main_block.invoke_expressions[statement_index];
                            const expression = ast_invoke_expression.expression;
                            assert(expression.get_level() == .global);

                            var called_function_reference: Reference = undefined;
                            var ast_called_function_declaration: Parser.Function = undefined;
                            const called_function_index = expression.get_index();

                            if (expression.get_array_index(.global) == .resolved_internal_functions)
                            {
                                std.debug.print("Internal function\n", .{});
                                called_function_reference = Function.new(called_function_index);
                                ast_called_function_declaration = result.functions[called_function_index].declaration;
                            }
                            else if (expression.get_array_index(.global) == .resolved_external_functions)
                            {
                                std.debug.print("External function\n", .{});
                                called_function_reference = ExternalFunction.new(called_function_index);
                                ast_called_function_declaration = result.external.functions[called_function_index].declaration;
                            }
                            else unreachable;

                            //const called_function_type = result.function_types[ast_called_function_declaration.type.get_index()];
                            const called_function_type = ast_called_function_declaration.type;

                            const called_function_argument_count = ast_called_function_declaration.argument_names.len;
                            std.debug.print("Argument count: {}\n", .{called_function_argument_count});
                            if (called_function_argument_count != 0)
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
                                            const ast_arg_array_index = ast_argument.get_array_index(.scope);
                                            switch (ast_arg_array_index)
                                            {
                                                .integer_literals =>
                                                {
                                                    const int_literal = Constant.new(.int, ast_index);
                                                    argument_list.append(int_literal) catch unreachable;
                                                },
                                                else => panic("{}\n", .{ast_arg_array_index}),
                                            }
                                        },
                                        else => panic("Level: {}\n", .{ast_arg_level}),
                                    }
                                }

                                const call = Instruction.Call.new(&builder, called_function_type.return_type, called_function_reference, argument_list.items);
                                // @TODO: we should be returning this? Yes, but...
                                _ = call;
                            }
                            else
                            {
                                const call = Instruction.Call.new(&builder, called_function_type.return_type, called_function_reference, std.mem.zeroes([]Reference));
                                // @TODO: we should be returning this? Yes, but...
                                _ = call;
                            }
                        },
                        else => panic("ni: {}\n", .{array_index}),
                    }
                },
                else => panic("ni: {}\n", .{statement_level}),
            }
        }

        if (conditional_alloca)
        {
            unreachable;
        }
        else if (returns_something == .void)
        {
            if (explicit_return)
            {
                unreachable;
            }
            _ = Instruction.Ret.new(&builder, return_type, null);
        }
        else
        // here noreturn type, do nothing at the moment
        {
            assert(returns_something == .noreturn);
        }
    }

    var functions = ArrayList(Function).initCapacity(allocator, result.functions.len) catch unreachable;

    for (builder.function_builders.items) |fb|
    {
        var bb_list = ArrayList(BasicBlock).initCapacity(allocator, fb.basic_blocks.items.len) catch unreachable;

        for (fb.basic_blocks.items) |bb|
        {
            bb_list.append(.{
                .instructions = bb.instructions.items,
            }) catch unreachable;
        }

        functions.append(.{
            .argument_allocas = fb.argument_allocas.items,
            .argument_names = fb.argument_names,
            .type = fb.type,
            .basic_blocks = bb_list.items,
            .instructions = fb.instructions.items,
        }) catch unreachable;
    }

    std.debug.print("Integer literal count: {}\n", .{builder.integer_literals.items.len});
    return Program
    {
        .instructions = .
        {
            .alloca = builder.instructions.alloca.items,
            .store = builder.instructions.store.items,
            .call = builder.instructions.call.items,
            .ret = builder.instructions.ret.items,
        },
        .functions = functions.items,
        .external = builder.external,
        .integer_literals = builder.integer_literals.items,

        .pointer_types = builder.pointer_types.items,
        .slice_types = builder.slice_types.items,
        .function_types = builder.function_types.items, 
        .array_types = builder.array_types.items,
        .struct_types = builder.struct_types.items,
    };
}
