const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const panic = std.debug.panic;

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

        fn create(builder: *Program.Builder, reference: Reference, alloca_type: Type, array_size: ?*Reference) Reference
        {
            assert(array_size == null);

            const alloca_array_index = builder.instructions.alloca.items.len;
            builder.instructions.alloca.append(.{
                .type = builder.get_or_create_pointer_type(alloca_type),
                .alloca_type = alloca_type,
                .reference = reference,
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

        fn new(builder: *Program.Builder, return_type: Type, maybe_value: ?Reference) Reference
        {
            var function_builder = &builder.function_builders.items[builder.current_function];
            if (!function_builder.is_terminated())
            {
                const ret = blk:
                {
                    if (maybe_value) |value|
                    {
                        assert(return_type.value != Type.Builtin.void_type.value and return_type.value != Type.Builtin.noreturn_type.value);
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

    const Load = struct
    {
        type: Type,
        value: Reference,

        fn new(builder: *Program.Builder, load_type: Type, load_value: Reference) Reference
        {
            const load_array_index = builder.instructions.load.items.len;
            builder.instructions.load.append(
                .{
                    .type = load_type,
                    .value = load_value,
                }) catch unreachable;

            const load_instruction = Instruction.new(.load, load_array_index);
            var function_builder = &builder.function_builders.items[builder.current_function];
            function_builder.basic_blocks.items[function_builder.current_block].instructions.append(load_instruction) catch unreachable;

            return load_instruction;
        }
    };
};

pub const Function = struct
{
    argument_allocas: []Reference,
    argument_names: [][]const u8,
    type: Type.Function,
    basic_blocks: []BasicBlock,

    const Builder = struct
    {
        argument_allocas: ArrayList(Reference),
        argument_names: [][]const u8,
        type: Type.Function,
        basic_blocks: ArrayList(BasicBlock.Builder),
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

pub const Program = struct
{
    instructions: struct
    {
        alloca: []Instruction.Alloca,
        load: []Instruction.Load,
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
            load: ArrayList(Instruction.Load),
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
                    .load = ArrayList(Instruction.Load).init(allocator),
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
            const pointer_type = Type.Pointer.new(self.pointer_types.items.len);
            self.pointer_types.append(.{ .type = p_type }) catch unreachable;
            return pointer_type;
        }
    };
};

const ReturnKind = enum
{
    void,
    noreturn,
    something,
};

fn find_expression_alloca(builder: *Program.Builder, function_builder: *Function.Builder, expression: Entity) Reference
{
    for (function_builder.basic_blocks.items[0].instructions.items[0..function_builder.next_alloca_index]) |alloca|
    {
        const alloca_i = alloca.get_index();
        if (builder.instructions.alloca.items[alloca_i].reference.get_index() == expression.get_index())
        {
            return alloca;
        }
    }

    panic("Not Found\n", .{});
}

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
        // @TODO: detect all this variables changes and write it here
        var emitted_return = false;
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
            const argument_ref = Function.Argument.new(@intCast(u32, argument_i));
            const argument_alloca = Instruction.Alloca.create(&builder, argument_ref, argument_type, null);
            builder.function_builders.items[builder.current_function].argument_allocas.append(argument_alloca) catch unreachable;
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
                                called_function_reference = Function.new(called_function_index);
                                ast_called_function_declaration = result.functions[called_function_index].declaration;
                            }
                            else if (expression.get_array_index(.global) == .resolved_external_functions)
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
                                            const ast_arg_array_index = ast_argument.get_array_index(.scope);
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
                        .return_expressions =>
                        {
                            assert(!emitted_return);

                            const ast_return_expression = ast_main_block.return_expressions[statement_index];
                            if (ast_return_expression.expression) |ast_expression_to_return|
                            {
                                const ret_lvl = ast_expression_to_return.get_level();
                                const ret_expr_index = ast_expression_to_return.get_index();

                                const ret_expression = switch (ret_lvl)
                                {
                                    .scope => blk:
                                    {
                                        const ret_array_index = ast_expression_to_return.get_array_index(.scope);
                                        switch (ret_array_index)
                                        {
                                            .integer_literals =>
                                            {
                                                const int_literal = Constant.new(.int, ret_expr_index);
                                                break :blk int_literal;
                                            },
                                            .variable_declarations =>
                                            {
                                                const alloca_ref = find_expression_alloca(&builder, function_builder, ast_expression_to_return);
                                                const alloca = builder.instructions.alloca.items[alloca_ref.get_index()];
                                                const load = Instruction.Load.new(&builder, alloca.alloca_type, alloca_ref);
                                                break :blk load;
                                            },
                                            else => panic("NI AI: {}\n", .{ret_array_index}),
                                        }
                                    },
                                    else => panic("NI: {}\n", .{ret_lvl}),
                                };

                                _ = Instruction.Ret.new(&builder, return_type, ret_expression);
                                emitted_return = true;
                            }
                            else
                            {
                                if (!emitted_return)
                                {
                                    _ = Instruction.Ret.new(&builder, return_type, null);
                                    emitted_return = true;
                                }
                                else
                                {
                                    unreachable;
                                }
                            }
                        },
                        .variable_declarations =>
                        {
                            const variable_declaration = ast_main_block.variable_declarations[statement_index];
                            const var_type = variable_declaration.type;
                            _ = Instruction.Alloca.create(&builder, Function.VariableDeclaration.new(statement_index), var_type, null);
                        },
                        .assignments =>
                        {
                            const assignment = ast_main_block.assignments[statement_index];
                            assert(assignment.left.get_level() == .scope);
                            const left_id = assignment.left.get_array_index(.scope);
                            const pointer_reference = switch (left_id)
                            {
                                .variable_declarations => find_expression_alloca(&builder, function_builder, assignment.left),
                                else => panic("NI: {}\n", .{left_id}),
                            };

                            assert(assignment.right.get_level() == .scope);
                            const right_id = assignment.right.get_array_index(.scope);
                            const value_reference = switch (right_id)
                            {
                                .integer_literals => Constant.new(.int, assignment.right.get_index()),
                                else => panic("Right ID: {}\n", .{right_id}),
                            };

                            _ = Instruction.Store.new(&builder, value_reference, pointer_reference);
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

            // @TODO: this can cause bugs
            if (!emitted_return)
            {
                _ = Instruction.Ret.new(&builder, return_type, null);
            }
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
        }) catch unreachable;
    }

    return Program
    {
        .instructions = .
        {
            .alloca = builder.instructions.alloca.items,
            .load = builder.instructions.load.items,
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
