const std = @import("std");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const print = std.debug.print;
const assert = std.debug.assert;
const panic = std.debug.panic;

const Parser = @import("parser.zig");
const TypeIdentifier = Parser.TypeIdentifier;
const BinaryExpression = Parser.BinaryExpression;
const UnaryExpression = Parser.UnaryExpression;

const Type = @import("type.zig");
usingnamespace @import("entity.zig");

const log = std.log.scoped(.semantics);

pub fn semantics_error(comptime format: []const u8, args: anytype) noreturn
{
    log.err(format, args);
    panic("Error in the semantics stage\n", .{});
}

fn analyze_type_declaration(allocator: *Allocator, type_in_analysis: *Node, types: *TypeBuffer, node_buffer: *NodeBuffer) *Type
{
    switch (type_in_analysis.value)
    {
        Node.ID.type_identifier =>
        {
            switch (type_in_analysis.value.type_identifier.value)
            {
                TypeIdentifier.ID.structure =>
                {
                    const name = type_in_analysis.value.type_identifier.value.structure.name;
                    if (Type.get_type_by_name(types, name)) |type_found|
                    {
                        log.debug("Type already found\n", .{});
                        return type_found;
                    }
                    else
                    {
                        log.debug("Type not found. Must create\n", .{});
                    }

                    var fields = ArrayList(Type.Struct.Field).init(allocator);

                    for (type_in_analysis.value.type_identifier.value.structure.fields.items) |ast_field, i|
                    {
                        // @Info: struct fields are var decls
                        assert(ast_field.value == Node.ID.var_decl);
                        const field_type = analyze_type_declaration(allocator, ast_field.value.var_decl.var_type, types, node_buffer);
                        const field_name = ast_field.value.var_decl.name;

                        const new_field = Type.Struct.Field
                        {
                            .name = field_name,
                            .type = field_type,
                            .parent = undefined,
                            .index =  i,
                        };

                        fields.append(new_field) catch {
                            panic("Failed to allocate memory for struct field\n", .{});
                        };
                    }

                    const result = Type.create_struct_type(types, fields.items, name);
                    return result;
                },
                TypeIdentifier.ID.function =>
                {
                    var function_type = Type.Function
                    {
                        .arg_types = TypeRefBuffer.init(allocator),
                        .ret_type = undefined,
                    };

                    if (type_in_analysis.value.type_identifier.value.function.return_type) |return_type_node|
                    {
                        const return_type = analyze_type_declaration(allocator, return_type_node, types, node_buffer);
                        function_type.ret_type = return_type;
                    }
                    else
                    {
                        const return_type = Type.get_void_type(types);
                        function_type.ret_type = return_type;
                    }

                    for (type_in_analysis.value.type_identifier.value.function.arg_types.items) |*arg_type_node|
                    {
                        const arg_type = analyze_type_declaration(allocator, arg_type_node.*, types, node_buffer);
                        function_type.arg_types.append(arg_type) catch {
                            panic("Error allocating argument type\n", .{});
                        };
                    }

                    const result = Type.get_function_type(types, function_type);

                    return result;
                },
                TypeIdentifier.ID.simple =>
                {
                    const name = type_in_analysis.value.type_identifier.value.simple;
                    if (Type.get_type_by_name(types, name)) |simple_type|
                    {
                        return simple_type;
                    }
                    else
                    {
                        semantics_error("Type {s} not found\n", .{name});
                    }
                },
                TypeIdentifier.ID.pointer =>
                {
                    const appointee_node = type_in_analysis.value.type_identifier.value.pointer.type;
                    const appointee_type = analyze_type_declaration(allocator, appointee_node, types, node_buffer);
                    const pointer_type = Type.get_pointer_type(appointee_type, types);
                    return pointer_type;
                },
                TypeIdentifier.ID.array =>
                {
                    const array_elem_type_node = type_in_analysis.value.type_identifier.value.array.type;
                    const array_length_node = type_in_analysis.value.type_identifier.value.array.len_expr;
                    const array_elem_type = analyze_type_declaration(allocator, array_elem_type_node, types, node_buffer);
                    const array_length_constant = resolve_compile_time_uint_constant(array_length_node);
                    const array_type = Type.get_array_type(array_elem_type, array_length_constant, types);
                    return array_type;
                },
                else => panic("not implemented: {}\n", .{type_in_analysis.value.type_identifier.value}),
            }
        },
        else => panic("Unreachable or ni: {}\n", .{type_in_analysis.value}),
    }
}

fn resolve_compile_time_uint_constant(node: *Node) u64
{
    switch (node.value)
    {
        Node.ID.int_lit =>
        {
            const int_lit_value = node.value.int_lit.value;
            if (node.value.int_lit.signed)
            {
                semantics_error("Expected unsigned element, value is signed: {}\n", .{- @intCast(i64, int_lit_value)});
            }

            return int_lit_value;
        },
        else => panic("ni: {}\n", .{node.value}),
    }
}

pub fn typecheck(lvalue_type: *Type, right: *Node, types: *TypeBuffer) *Type
{
    log.debug("Left type: {} --- Right type: {}\n", .{lvalue_type, right.type});

    switch (lvalue_type.value)
    {
        Type.ID.integer =>
        {
            switch (right.value)
            {
                Node.ID.int_lit =>
                {
                    assert(right.type.value == Type.ID.unresolved);
                    right.type = lvalue_type;
                    log.debug("Integer literal type resolved\n", .{});
                    // @TODO: make sure we have enough bytes in the lvalue type

                    return lvalue_type;
                },
                Node.ID.identifier_expr,
                Node.ID.binary_expr,
                Node.ID.resolved_identifier,
                Node.ID.invoke_expr,
                Node.ID.unary_expr,
                Node.ID.array_subscript_expr,
                Node.ID.field_access_expr,
                =>
                {
                    const rvalue_type = right.type;
                    log.debug("{}\n", .{rvalue_type});
                    if (lvalue_type == rvalue_type)
                    {
                        return lvalue_type;
                    }
                    else
                    {
                        panic("reached here\n", .{});
                    }
                },
                else => panic("ni: {}\n", .{right.value}),
            }
        },
        Type.ID.pointer =>
        {
            switch (right.value)
            {
                Node.ID.unary_expr =>
                {
                    const unary_expr = right.value.unary_expr.id;
                    switch (unary_expr)
                    {
                        UnaryExpression.ID.AddressOf =>
                        {
                            // @TODO: change base type for pointer type and get_type of pointer rvalue
                            const rvalue_base_type = right.value.unary_expr.node_ref.type;
                            const lvalue_base_type = lvalue_type.value.pointer.type;
                            if (lvalue_base_type == rvalue_base_type)
                            {
                                return lvalue_type;
                            }
                        },
                        else => panic("ni: {}\n", .{unary_expr}),
                    }
                },
                Node.ID.invoke_expr =>
                {
                    const rvalue_type = right.type;
                    if (rvalue_type == lvalue_type)
                    {
                        return lvalue_type;
                    }
                },
                Node.ID.field_access_expr =>
                {
                    const rvalue_type = right.type;
                    if (rvalue_type == lvalue_type)
                    {
                        return lvalue_type;
                    }
                },
                else => panic("ni: {}\n", .{right.value}),
            }
        },
        Type.ID.array =>
        {
            switch (right.value)
            {
                Node.ID.array_lit =>
                {
                    const result = typecheck(lvalue_type.value.array.type, right.value.array_lit.elements.items[0], types); 
                    assert(result.value != Type.ID.unresolved);
                    // @TODO: typecheck here
                    log.debug("Array literal resolved into {}\n", .{result});
                    right.type = lvalue_type;
                    return lvalue_type;
                },
                else => panic("ni: {}\n", .{right.value}),
            }
        },
        Type.ID.structure =>
        {
            switch (right.value)
            {
                Node.ID.struct_lit =>
                {
                    for (right.value.struct_lit.field_names.items) |field_id, field_index|
                    {
                        const field_expr = right.value.struct_lit.field_expressions.items[field_index];
                        _ = typecheck(field_id.type, field_expr, types);
                    }

                    right.type = lvalue_type;

                    return lvalue_type;
                },
                else => panic("ni: {}\n", .{right.value}),
            }
        },
        Type.ID.unresolved =>
        {
            switch (right.type.value)
            {
                Type.ID.unresolved =>
                {
                    return lvalue_type;
                },
                else => panic("ni: {}\n", .{right.type.value}),
            }
        },
        else => panic("ni: {}\n", .{lvalue_type.value}),
    }

    semantics_error("Typecheck failed!\nLeft: {}\nRight: {}\n", .{lvalue_type, right});
}

pub fn find_variable(current_function: *Node, name: []const u8) *Node
{
    for (current_function.value.function_decl.arguments.items) |arg|
    {
        if (std.mem.eql(u8, arg.value.var_decl.name, name))
        {
            return arg;
        }
    }

    for (current_function.value.function_decl.variables.items) |variable_decl|
    {
        if (std.mem.eql(u8, variable_decl.value.var_decl.name, name))
        {
            return variable_decl;
        }
    }

    semantics_error("Can't find variable name: {s}\n", .{name});
}

pub fn find_function_decl(name: []const u8, functions: *NodeRefBuffer) *Node
{
    for (functions.items) |function_decl|
    {
        if (std.mem.eql(u8, function_decl.value.function_decl.name, name))
        {
            return function_decl;
        }
    }

    semantics_error("Can't find function name: {s}\n", .{name});
}

fn find_field_from_struct(structure_type: *Type, field_name: []const u8) *Type.Struct.Field
{
    for (structure_type.value.structure.fields) |*field|
    {
        if (std.mem.eql(u8, field.name,  field_name))
        {
            return field;
        }
    }

    semantics_error("Couldn't match field expression with structure\n", .{});
}

fn find_field_from_resolved_identifier(resolved_id_node: *Node, name: []const u8) *Type.Struct.Field
{
    const decl_node = resolved_id_node.value.resolved_identifier;
    switch (decl_node.value)
    {
        Node.ID.var_decl =>
        {
            const decl_type = decl_node.type;
            switch (decl_type.value)
            {
                Type.ID.pointer =>
                {
                    const appointee_type = decl_type.value.pointer.type;
                    switch (appointee_type.value)
                    {
                        Type.ID.structure =>
                        {
                            const field = find_field_from_struct(appointee_type, name);
                            return field;
                        },
                        else => panic("ni: {}\n", .{appointee_type.value}),
                    }
                },
                Type.ID.structure =>
                {
                    const field = find_field_from_struct(decl_type, name);
                    return field;
                },
                else => panic("ni: {}\n", .{decl_type.value}),
            }
        },
        else => panic("ni: {}\n", .{decl_node.value}),
    }
}

fn new_field_node(node_buffer: *NodeBuffer, field: *Type.Struct.Field, parent_node: *Node) *Node
{
    const field_node = Node
    {
        .value = Node.Value {
            .field_expr = field,
        },
        .parent = parent_node,
        .value_type = Node.ValueType.RValue,
        .type = field.type,
    };

    const result = node_buffer.append(field_node) catch {
        panic("Error allocating memory for type node\n", .{});
    };

    return result;
}

pub fn explore_field_identifier_expression(node: *Node, node_buffer: *NodeBuffer) *Node
{
    switch (node.value)
    {
        Node.ID.identifier_expr =>
        {
            const name = node.value.identifier_expr.name;
            if (node.parent) |parent|
            {
                switch (parent.value)
                {
                    Node.ID.field_access_expr =>
                    {
                        const struct_var_node = parent.value.field_access_expr.expression;
                        switch (struct_var_node.value)
                        {
                            Node.ID.resolved_identifier =>
                            {
                                const field = find_field_from_resolved_identifier(struct_var_node, name);
                                const field_node = new_field_node(node_buffer, field, parent);
                                return field_node;
                            },
                            else => panic("ni: {}\n", .{struct_var_node.value}),
                        }
                    },
                    Node.ID.struct_lit =>
                    {
                        assert(parent.value_type == Node.ValueType.RValue);
                        const parent_of_parent = parent.parent.?;
                        switch (parent_of_parent.value)
                        {
                            Node.ID.binary_expr =>
                            {
                                assert(parent_of_parent.value.binary_expr.id == BinaryExpression.ID.Assignment);
                                const left = parent_of_parent.value.binary_expr.left;
                                const right = parent_of_parent.value.binary_expr.right;
                                assert(parent == right);

                                switch (left.value)
                                {
                                    Node.ID.resolved_identifier =>
                                    {
                                        const field = find_field_from_resolved_identifier(left, name);
                                        const field_node = new_field_node(node_buffer, field, parent);
                                        return field_node;
                                    },
                                    else => panic("left value: {}\n", .{left.value}),
                                }
                            },
                            else => panic("ni: {}\n", .{parent_of_parent.value}),
                        }
                    },
                    else => panic("ni: {}\n", .{parent.value}),
                }
            }
            else
            {
                panic("Field identifier must have a parent\n", .{});
            }
        },
        else => panic("ni: {}\n", .{node.value}),
    }
}

pub fn explore_expression(allocator: *Allocator, current_function: *Node, current_block: *Node, node: *Node, functions: *NodeRefBuffer, node_buffer: *NodeBuffer, types: *TypeBuffer) *Node
{
    switch (node.value)
    {
        Node.ID.var_decl =>
        {
            node.type = analyze_type_declaration(allocator, node.value.var_decl.var_type, types, node_buffer);
        },
        Node.ID.binary_expr =>
        {
            node.value.binary_expr.left = explore_expression(allocator, current_function, current_block, node.value.binary_expr.left, functions, node_buffer, types);
            node.value.binary_expr.right = explore_expression(allocator, current_function, current_block, node.value.binary_expr.right, functions, node_buffer, types);
            node.type = typecheck(node.value.binary_expr.left.type, node.value.binary_expr.right, types);
        },
        Node.ID.identifier_expr =>
        {
            const name = node.value.identifier_expr.name;
            const decl_node = find_variable(current_function, name);
            const new_node_value = Node
            {
                .value = Node.Value {
                    .resolved_identifier = decl_node,
                },
                .value_type = node.value_type,
                .parent = node.parent,
                .type = decl_node.type,
            };

            var new_node = node_buffer.append(new_node_value) catch {
                panic("Error allocating memory for resolved identifier node\n", .{});
            };

            return new_node;
        },
        Node.ID.return_expr =>
        {
            if (node.value.return_expr.expression) |return_expr|
            {
                node.value.return_expr.expression = explore_expression(allocator, current_function, current_block, return_expr, functions, node_buffer, types);
            }
            node.type = Type.get_void_type(types);
        },
        Node.ID.block_expr =>
        {
            for (node.value.block_expr.statements.items) |*statement|
            {
                const new_current_block = node;
                statement.* = explore_expression(allocator, current_function, new_current_block, statement.*, functions, node_buffer, types);
            }
            node.type = Type.get_void_type(types);
        },
        Node.ID.loop_expr =>
        {
            node.value.loop_expr.prefix = explore_expression(allocator, current_function, current_block, node.value.loop_expr.prefix, functions, node_buffer, types);
            node.value.loop_expr.body = explore_expression(allocator, current_function, current_block, node.value.loop_expr.body, functions, node_buffer, types);
            node.value.loop_expr.postfix = explore_expression(allocator, current_function, current_block, node.value.loop_expr.postfix, functions, node_buffer, types);
            node.type = Type.get_void_type(types);
        },
        Node.ID.branch_expr =>
        {
            node.value.branch_expr.condition = explore_expression(allocator, current_function, current_block, node.value.branch_expr.condition, functions, node_buffer, types);
            node.value.branch_expr.if_block = explore_expression(allocator, current_function, current_block, node.value.branch_expr.if_block, functions, node_buffer, types);
            if (node.value.branch_expr.else_block) |else_block|
            {
                node.value.branch_expr.else_block = explore_expression(allocator, current_function, current_block, else_block, functions, node_buffer, types);
            }
            node.type = Type.get_void_type(types);
        },
        Node.ID.invoke_expr =>
        {
            const function_call_id_node = node.value.invoke_expr.expression;
            assert(function_call_id_node.value == Node.ID.identifier_expr);
            const function_call_name = function_call_id_node.value.identifier_expr.name;
            log.debug("function call name: {s}\n", .{function_call_name});
            const function_decl = find_function_decl(function_call_name, functions);
            node.type = function_decl.type.value.function.ret_type;
            node.value.invoke_expr.expression = function_decl;

            for (node.value.invoke_expr.arguments.items) |*arg|
            {
                arg.* = explore_expression(allocator, current_function, current_block, arg.*, functions, node_buffer, types);
            }
        },
        Node.ID.unary_expr =>
        {
            // @TODO: check anything more with unary expression id?
            node.value.unary_expr.node_ref = explore_expression(allocator, current_function, current_block, node.value.unary_expr.node_ref, functions, node_buffer, types);
            switch (node.value.unary_expr.id)
            {
                UnaryExpression.ID.AddressOf =>
                {
                    node.type = Type.get_pointer_type(node.value.unary_expr.node_ref.type, types);
                },
                UnaryExpression.ID.Dereference =>
                {
                    node.type = node.value.unary_expr.node_ref.type.value.pointer.type;
                },
            }
        },
        Node.ID.break_expr =>
        {
            // @TODO: check if we are in a loop
        },
        Node.ID.int_lit,  =>
        {
            // @Info: this is a literal type which is resolved later
            node.type = Type.get_literal_type(types);
        },
        Node.ID.array_lit =>
        {
            const first_elem = node.value.array_lit.elements.items[0];
            const first_elem_analyzed = explore_expression(allocator, current_function, current_block, first_elem, functions, node_buffer, types);
            const first_elem_type = first_elem_analyzed.type;

            for (node.value.array_lit.elements.items) |*array_elem|
            {
                array_elem.* = explore_expression(allocator, current_function, current_block, array_elem.*, functions, node_buffer, types);
                _ = typecheck(first_elem_type, array_elem.*, types);
            }

            // @TODO: improve
            node.type = Type.get_literal_type(types);
        },
        Node.ID.struct_lit =>
        {
            for (node.value.struct_lit.field_names.items) |*name_node_ptr|
            {
                name_node_ptr.* = explore_field_identifier_expression(name_node_ptr.*, node_buffer);
            }

            for (node.value.struct_lit.field_expressions.items) |*expression_node_ptr|
            {
                expression_node_ptr.* = explore_expression(allocator, current_function, current_block, expression_node_ptr.*, functions, node_buffer, types);
            }
        },
        Node.ID.array_subscript_expr =>
        {
            node.value.array_subscript_expr.expression = explore_expression(allocator, current_function, current_block, node.value.array_subscript_expr.expression, functions, node_buffer, types);
            node.value.array_subscript_expr.index = explore_expression(allocator, current_function, current_block, node.value.array_subscript_expr.index, functions, node_buffer, types);
            node.type = node.value.array_subscript_expr.expression.type.value.array.type;
        },
        Node.ID.field_access_expr =>
        {
            node.value.field_access_expr.expression = explore_expression(allocator, current_function, current_block, node.value.field_access_expr.expression, functions, node_buffer, types);
            node.value.field_access_expr.field_expr = explore_field_identifier_expression(node.value.field_access_expr.field_expr, node_buffer);
            node.type = node.value.field_access_expr.field_expr.type;
        },
        else => panic("ni: {}\n", .{node.value}),
    }

    return node;
}


fn analyze_type(semantics_analyzer: *SemanticsAnalyzer, module_offsets: []ModuleStats, unresolved_type: Type) Type
{
    _ = semantics_analyzer;
    const type_id = unresolved_type.get_ID();
    switch (type_id)
    {
        // @INFO: builtin types are already resolved
        .builtin => return unresolved_type,
        .unresolved =>
        {
            const module_index = unresolved_type.get_module_index();
            const unresolved_type_module_offset = module_offsets[module_index].counters[@enumToInt(ModuleStats.ID.unresolved_types)];
            const index = unresolved_type.get_index();
            const unresolved_type_identifier = semantics_analyzer.unresolved_types.items[unresolved_type_module_offset + index];
            if (unresolved_type_identifier[0] == 'u')
            {
                
                if (std.fmt.parseUnsigned(u16, unresolved_type_identifier[1..], 10)) |bit_count|
                {
                    return Type.Integer.new(bit_count, .unsigned);
                }
                else |_| {}
            }
            if (unresolved_type_identifier[0] == 's')
            {
                if (std.fmt.parseUnsigned(u16, unresolved_type_identifier[1..], 10)) |bit_count|
                {
                    return Type.Integer.new(bit_count, .signed);
                }
                else |_| {}
            }

            unreachable;
        },
        else => panic("Type ID: {}\n", .{type_id}),
    }
    //const array_index = unresolved_type.get_array_index();
    //switch (type_level)
    //{
        //.builtin =>
        //{
            //const builtin_id = @intToEnum(Parser.Identifier.BuiltinID, array_index);
            //return switch (builtin_id)
            //{
                //.void_type => BuiltinType.void_type,
                //.noreturn_type => BuiltinType.noreturn_type,
                //else => unreachable,
            //};
        //},
        //.global =>
        //{
            //unreachable;
        //},
        //.module =>
        //{
            //const module_id = @intToEnum(Parser.Identifier.ModuleID, array_index);
            //switch (module_id)
            //{
                //.unresolved_types =>
                //{
                    //const base_index = unresolved_type.index;
                    //const t = ast.modules[module_index].unresolved_types[base_index];
                    //panic("T: {s}\n", .{t});
                //},
                //else => panic("ni: {}\n", .{module_id}),
            //}
        //},
        //else => panic("ni: {}\n", .{type_level}),
    //}
}

const TypeStore = struct
{
    function_types: []Type.Function,
    array_types: []Type.Array,
    pointer_types: []Type.Pointer,
    structure_types: []Type.Struct,
};

const ModuleStats = struct
{
    counters: [enum_values.len]u64,

    const ID = enum
    {
        internal_functions,
        external_functions,
        imported_modules,
        unresolved_types,
        pointer_types,
        slice_types,
        function_types,
        array_types,
        struct_types,
    };

    const enum_values = std.enums.values(ID);

    const Map = blk:
    {
        var array: [enum_values.len]type = undefined;

        inline for (enum_values) |enum_value, i|
        {
            array[i] = switch(enum_value)
            {
                .internal_functions => Parser.Function.Internal,
                .external_functions => Parser.Function.External,
                .imported_modules => Parser.ImportedModule,
                .unresolved_types => []const u8,
                .pointer_types => Type.Pointer,
                .slice_types => Type.Slice,
                .function_types => Type.Function,
                .array_types => Type.Array,
                .struct_types => Type.Struct,
            };

        }

        break :blk array;
    };
};


// @TODO: make this fast
pub fn get_module_item_slice_range(comptime module_stats_id: ModuleStats.ID, semantics_analyzer: *SemanticsAnalyzer, module_offsets: []ModuleStats, module_index: u64) struct { start: u64, end: u64 }
{
    const this_module_offsets = module_offsets[module_index];
    const internal_item_start = this_module_offsets.counters[@enumToInt(module_stats_id)];
    const next_module_index = module_index + 1;
    const internal_item_end =
        if (next_module_index < module_offsets.len)
            module_offsets[next_module_index].counters[@enumToInt(module_stats_id)]
        else
            semantics_analyzer.internal_functions.items.len;

    return .{ .start = internal_item_start, .end = internal_item_end };
}

pub fn analyze_scope(semantics_analyzer: *SemanticsAnalyzer, module_offsets: []ModuleStats, scope: *Parser.Scope, module_index: u64) void
{
    const statement_count = scope.statements.len;
    print("Statement count: {}\n", .{statement_count});
    for (scope.statements) |*statement|
    {
        const statement_index = statement.get_index();
        const statement_level = statement.get_level();
        assert(statement_level == .scope);
        const statement_type = statement.get_array_index(.scope);
        switch (statement_type)
        {
            .invoke_expressions =>
            {
                const invoke_expression = &scope.invoke_expressions[statement_index];
                const expression_to_invoke = invoke_expression.expression;
                print("{}\n", .{invoke_expression});
                const expression_to_invoke_index = expression_to_invoke.get_index();
                const expression_to_invoke_level = expression_to_invoke.get_level();
                assert(expression_to_invoke_level == .scope);
                const expression_to_invoke_id = expression_to_invoke.get_array_index(.scope);
                print("{}\n", .{expression_to_invoke_id});

                const resolved_expression_to_invoke: Entity = blk:
                {
                    if (expression_to_invoke_id == .identifier_expressions)
                    {
                        const invoke_expression_name = scope.identifier_expressions[expression_to_invoke_index].name;
                        // @TODO: stop hardcoding this and handle it the right way
                        if (std.mem.eql(u8, invoke_expression_name, "main"))
                        {
                            for (semantics_analyzer.internal_functions.items) |function, i|
                            {
                                print("Comparing {s} with {s}...\n", .{function.declaration.name, invoke_expression_name});
                                if (!std.mem.eql(u8, function.declaration.name, invoke_expression_name))
                                {
                                    continue;
                                }

                                break :blk Entity.new(i, Entity.GlobalID.resolved_internal_functions);
                            }

                            unreachable;
                        }
                        else
                        {
                            panic("{s}\n", .{invoke_expression_name});
                        }
                    }
                    else if (expression_to_invoke_id == .field_access_expressions)
                    {
                        const field_expression = scope.field_access_expressions[expression_to_invoke_index];
                        print("{}\n", .{field_expression});
                        const left = field_expression.left_expression;
                        const left_level = left.get_level();
                        assert(left_level == .scope);
                        const left_array_index = left.get_array_index(.scope);
                        assert(left_array_index == .identifier_expressions);
                        const left_index = left.get_index();
                        const left_name = scope.identifier_expressions[left_index].name;

                        const imported_module_range = get_module_item_slice_range(.imported_modules, semantics_analyzer, module_offsets, module_index);
                        const imported_modules = semantics_analyzer.imported_modules.items[imported_module_range.start..imported_module_range.end];

                        for (imported_modules) |imported_module|
                        {
                            assert(imported_module.alias != null);
                            if (!std.mem.eql(u8, imported_module.alias.?, left_name))
                            {
                                continue;
                            }

                            const field = field_expression.field_expression;
                            const field_level = field.get_level();
                            assert(field_level == .scope);
                            const field_array_index = field.get_array_index(.scope);
                            assert(field_array_index == .identifier_expressions);
                            const field_index = field.get_index();
                            const field_name = scope.identifier_expressions[field_index].name;

                            const imported_module_index = imported_module.module.get_index();
                            const internal_functions_range = get_module_item_slice_range(.internal_functions, semantics_analyzer, module_offsets, imported_module_index);
                            const internal_functions = semantics_analyzer.internal_functions.items[internal_functions_range.start..internal_functions_range.end];
                            print("Internal functions\n", .{});

                            for (internal_functions) |function|
                            {
                                print("Comparing {s} with {s}...\n", .{function.declaration.name, field_name});
                                if (!std.mem.eql(u8, function.declaration.name, field_name))
                                {
                                    continue;
                                }

                                panic("Found\n", .{});
                            }

                            const external_functions_range = get_module_item_slice_range(.external_functions, semantics_analyzer, module_offsets, imported_module_index);
                            const external_functions = semantics_analyzer.external_functions.items[external_functions_range.start..external_functions_range.end];
                            print("External functions\n", .{});
                            for (external_functions) |function, i|
                            {
                                print("Comparing {s} with {s}...\n", .{function.declaration.name, field_name});
                                if (!std.mem.eql(u8, function.declaration.name, field_name))
                                {
                                    continue;
                                }

                                const real_index = external_functions_range.start + i;
                                break :blk Entity.new(real_index, Entity.GlobalID.resolved_external_functions);
                            }
                        }

                        unreachable;
                    }
                    else unreachable;
                };

                invoke_expression.expression = resolved_expression_to_invoke;

                print("Argument count: {}\n", .{invoke_expression.arguments.len});
                if (invoke_expression.arguments.len > 0)
                {
                    const resolved_expression_array_index = resolved_expression_to_invoke.get_array_index(.global);
                    const function_type_id =
                        if (resolved_expression_array_index == Entity.GlobalID.resolved_internal_functions)
                            semantics_analyzer.internal_functions.items[resolved_expression_to_invoke.get_index()].declaration.type
                        else if (resolved_expression_array_index == Entity.GlobalID.resolved_external_functions)
                            semantics_analyzer.external_functions.items[resolved_expression_to_invoke.get_index()].declaration.type
                        else unreachable;
                    const function_type = semantics_analyzer.function_types.items[function_type_id.get_index()];
                    const argument_types = function_type.argument_types;

                    for (invoke_expression.arguments) |*arg, arg_i|
                    {
                        print("Argument: {}\n", .{arg});
                        const arg_level = arg.get_level();
                        assert(arg_level == .scope);
                        const array_index = arg.get_array_index(.scope);
                        assert(array_index == Entity.ScopeID.integer_literals);
                        // @TODO: typecheck properly
                        if (argument_types[arg_i].get_ID() != .integer)
                        {
                            panic("Argument type must be integer\n", .{});
                        }
                    }
                }
            },
            else => unreachable,
        }
    }
}

const SemanticsAnalyzer = Parser.ModuleParser.ModuleBuilder;
pub const Result = Parser.Module;

pub fn analyze(allocator: *Allocator, ast: Parser.AST) Result
{
    log.debug("\n==============\nSEMANTICS\n==============\n\n", .{});

    print("{}\n", .{ast});

    //var types = Types.init(allocator);
    //

    var module_array = ast.modules[0..ast.module_len];
    var total = std.mem.zeroes(ModuleStats);
    var module_offsets = ArrayList(ModuleStats).initCapacity(allocator, ast.module_len) catch unreachable;
    module_offsets.resize(ast.module_len) catch unreachable;

    for (module_array) |module, i|
    {
        print("Module imported module count: {}\n", .{module.imported_modules.len});
        module_offsets.items[i].counters[@enumToInt(ModuleStats.ID.internal_functions)] = total.counters[@enumToInt(ModuleStats.ID.internal_functions)];
        module_offsets.items[i].counters[@enumToInt(ModuleStats.ID.external_functions)] = total.counters[@enumToInt(ModuleStats.ID.external_functions)];
        module_offsets.items[i].counters[@enumToInt(ModuleStats.ID.imported_modules)] = total.counters[@enumToInt(ModuleStats.ID.imported_modules)];
        print("Imported module count: {}\n", .{module_offsets.items[i].counters[@enumToInt(ModuleStats.ID.imported_modules)]});
        module_offsets.items[i].counters[@enumToInt(ModuleStats.ID.unresolved_types)] = total.counters[@enumToInt(ModuleStats.ID.unresolved_types)];
        module_offsets.items[i].counters[@enumToInt(ModuleStats.ID.pointer_types)] = total.counters[@enumToInt(ModuleStats.ID.pointer_types)];
        module_offsets.items[i].counters[@enumToInt(ModuleStats.ID.slice_types)] = total.counters[@enumToInt(ModuleStats.ID.slice_types)];
        module_offsets.items[i].counters[@enumToInt(ModuleStats.ID.function_types)] = total.counters[@enumToInt(ModuleStats.ID.function_types)];
        module_offsets.items[i].counters[@enumToInt(ModuleStats.ID.array_types)] = total.counters[@enumToInt(ModuleStats.ID.array_types)];
        module_offsets.items[i].counters[@enumToInt(ModuleStats.ID.struct_types)] = total.counters[@enumToInt(ModuleStats.ID.struct_types)];

        total.counters[@enumToInt(ModuleStats.ID.internal_functions)] += module.internal_functions.len;
        total.counters[@enumToInt(ModuleStats.ID.external_functions)] += module.external_functions.len;
        total.counters[@enumToInt(ModuleStats.ID.imported_modules)] += module.imported_modules.len;
        total.counters[@enumToInt(ModuleStats.ID.unresolved_types)] += module.unresolved_types.len;
        total.counters[@enumToInt(ModuleStats.ID.pointer_types)] += module.pointer_types.len;
        total.counters[@enumToInt(ModuleStats.ID.slice_types)]+= module.slice_types.len;
        total.counters[@enumToInt(ModuleStats.ID.function_types)]+= module.function_types.len;
        total.counters[@enumToInt(ModuleStats.ID.array_types)]+= module.array_types.len;
        total.counters[@enumToInt(ModuleStats.ID.struct_types)] += module.struct_types.len;
    }

    var semantics_analyzer = SemanticsAnalyzer
    {
        .internal_functions = ArrayList(Parser.Function.Internal).initCapacity(allocator, total.counters[@enumToInt(ModuleStats.ID.internal_functions)]) catch unreachable,
        .external_functions = ArrayList(Parser.Function.External).initCapacity(allocator, total.counters[@enumToInt(ModuleStats.ID.external_functions)]) catch unreachable,
        .imported_modules = ArrayList(Parser.ImportedModule).initCapacity(allocator, total.counters[@enumToInt(ModuleStats.ID.imported_modules)]) catch unreachable,
        .unresolved_types = ArrayList([]const u8).initCapacity(allocator, total.counters[@enumToInt(ModuleStats.ID.unresolved_types)]) catch unreachable,
        .pointer_types = ArrayList(Type.Pointer).initCapacity(allocator, total.counters[@enumToInt(ModuleStats.ID.pointer_types)]) catch unreachable,
        .slice_types = ArrayList(Type.Slice).initCapacity(allocator, total.counters[@enumToInt(ModuleStats.ID.slice_types)]) catch unreachable,
        .function_types = ArrayList(Type.Function).initCapacity(allocator, total.counters[@enumToInt(ModuleStats.ID.function_types)]) catch unreachable,
        .array_types = ArrayList(Type.Array).initCapacity(allocator, total.counters[@enumToInt(ModuleStats.ID.array_types)]) catch unreachable,
        .struct_types = ArrayList(Type.Struct).initCapacity(allocator, total.counters[@enumToInt(ModuleStats.ID.struct_types)]) catch unreachable,
        .index = 0,
    };

    for (module_array) |module|
    {
        semantics_analyzer.internal_functions.appendSlice(module.internal_functions) catch unreachable;
        semantics_analyzer.external_functions.appendSlice(module.external_functions) catch unreachable;
        semantics_analyzer.imported_modules.appendSlice(module.imported_modules) catch unreachable;
        semantics_analyzer.unresolved_types.appendSlice(module.unresolved_types) catch unreachable;
        semantics_analyzer.pointer_types.appendSlice(module.pointer_types) catch unreachable;
        semantics_analyzer.slice_types.appendSlice(module.slice_types) catch unreachable;
        semantics_analyzer.function_types.appendSlice(module.function_types) catch unreachable;
        semantics_analyzer.array_types.appendSlice(module.array_types) catch unreachable;
        semantics_analyzer.struct_types.appendSlice(module.struct_types) catch unreachable;
    }

    print("Internal:\t{}\n", .{semantics_analyzer.internal_functions.items.len});
    print("External:\t{}\n", .{semantics_analyzer.external_functions.items.len});
    print("Imported:\t{}\n", .{semantics_analyzer.imported_modules.items.len});
    print("Unresolved:\t{}\n", .{semantics_analyzer.unresolved_types.items.len});
    print("Pointer:\t{}\n", .{semantics_analyzer.pointer_types.items.len});
    print("Slice:\t{}\n", .{semantics_analyzer.slice_types.items.len});
    print("Function:\t{}\n", .{semantics_analyzer.function_types.items.len});
    print("Array\t{}\n", .{semantics_analyzer.array_types.items.len});
    print("Struct\t{}\n", .{semantics_analyzer.struct_types.items.len});


    // @TODO: shouldn't we discard repeated function types?
    for (semantics_analyzer.function_types.items) |*function_type|
    {
        function_type.return_type = analyze_type(&semantics_analyzer, module_offsets.items, function_type.return_type);
        for (function_type.argument_types) |*argument_type|
        {
            argument_type.* = analyze_type(&semantics_analyzer, module_offsets.items, argument_type.*);
        }
    }

    for (semantics_analyzer.external_functions.items) |*function|
    {
        // @TODO: be certain that the type is really resolved
        const type_index = function.declaration.type.get_index();
        const type_module_index = function.declaration.type.get_module_index();
        const final_index = module_offsets.items[type_module_index].counters[@enumToInt(ModuleStats.ID.function_types)] + type_index;
        function.declaration.type.set_new_index(final_index);
        function.declaration.type.mark_as_resolved();
    }

    for (semantics_analyzer.internal_functions.items) |*function|
    {
        // @TODO: be certain that the type is really resolved
        const type_index = function.declaration.type.get_index();
        const type_module_index = function.declaration.type.get_module_index();
        const this_module_offsets = module_offsets.items[type_module_index];
        const final_index = this_module_offsets.counters[@enumToInt(ModuleStats.ID.function_types)] + type_index;
        function.declaration.type.set_new_index(final_index);
        function.declaration.type.mark_as_resolved();

        // @TODO: is this the best way to get the module index? Might not match in the future because the function type may be repeated and come from a module different than the one the function is declared
        const module_index = type_module_index;
        const main_block = &function.scopes[0];
        analyze_scope(&semantics_analyzer, module_offsets.items, main_block, module_index);
    }

    return Result
    {
        .internal_functions = semantics_analyzer.internal_functions.items,
        .external_functions = semantics_analyzer.external_functions.items,
        .imported_modules = semantics_analyzer.imported_modules.items,
        .unresolved_types = undefined,
        .pointer_types = semantics_analyzer.pointer_types.items,
        .slice_types = semantics_analyzer.slice_types.items,
        .function_types = semantics_analyzer.function_types.items,
        .array_types = semantics_analyzer.array_types.items,
        .struct_types = semantics_analyzer.struct_types.items,
    };
}
