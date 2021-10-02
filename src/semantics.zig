const std = @import("std");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const assert = std.debug.assert;
const panic = std.debug.panic;

const Parser = @import("parser.zig");
const TypeIdentifier = Parser.TypeIdentifier;
const BinaryExpression = Parser.BinaryExpression;
const UnaryExpression = Parser.UnaryExpression;

const Type = @import("type.zig");
usingnamespace @import("entity.zig");

const Compiler = @import("compiler.zig");

pub fn log(comptime format: []const u8, arguments: anytype) void
{
    Compiler.log(.semantics, "[SEMANTICS] " ++ format, arguments);
}

pub fn report_error(comptime format: []const u8, args: anytype) noreturn
{
    panic(format, args);
}

// @TODO: make this fast
pub fn get_module_item_slice_range(comptime module_stats_id: ModuleStats.ID, analyzer: *Analyzer, module_offsets: []ModuleStats, module_index: u64) struct { start: u64, end: u64 }
{
    const this_module_offsets = module_offsets[module_index];
    const internal_item_start = this_module_offsets.counters[@enumToInt(module_stats_id)];
    const next_module_index = module_index + 1;
    const internal_item_end =
        if (next_module_index < module_offsets.len)
            module_offsets[next_module_index].counters[@enumToInt(module_stats_id)]
        else switch (comptime module_stats_id)
        {
            .internal_functions => analyzer.functions.items.len,
            .external_functions => analyzer.external_functions.items.len,
            .integer_literals => analyzer.integer_literals.items.len,
            else => unreachable,
        };

    return .{ .start = internal_item_start, .end = internal_item_end };
}

pub fn typecheck(lvalue_type: *Type, right: *Node, types: *TypeBuffer) *Type
{
    log("Left type: {} --- Right type: {}\n", .{lvalue_type, right.type});

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
                    log("Array literal resolved into {}\n", .{result});
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

    report_error("Typecheck failed!\nLeft: {}\nRight: {}\n", .{lvalue_type, right});
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
            log("function call name: {s}\n", .{function_call_name});
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

fn analyze_type(analyzer: *Analyzer, module_offsets: []ModuleStats, unresolved_type: Type) Type
{
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
            const unresolved_type_identifier = analyzer.unresolved_types.items[unresolved_type_module_offset + index];
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
}

const ModuleStats = struct
{
    counters: [enum_values.len]u64,

    const ID = enum
    {
        internal_functions,
        external_functions,
        imported_modules,
        integer_literals,

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
                .integer_literals => Parser.IntegerLiteral,
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

pub fn resolve_entity_index(analyzer: *Analyzer, comptime module_stats_id: ModuleStats.ID, entity: *Entity, module_offsets: []ModuleStats, module_index: u64) void
{
    const item_range = get_module_item_slice_range(module_stats_id, analyzer, module_offsets, module_index);
    const real_index = @intCast(u32, item_range.start) + entity.get_index();
    entity.set_index(real_index);
}

// @TODO: make this more robust
pub fn find_variable_declaration(scope: *Parser.Scope, name: []const u8) Entity
{
    for (scope.variable_declarations) |variable_declaration, variable_declaration_i|
    {
        if (std.mem.eql(u8, variable_declaration.name, name))
        {
            return Entity.new(variable_declaration_i, Entity.ScopeID.variable_declarations);
        }
    }

    report_error("Variable declaration \"{s}\" not found\n", .{name});
}

pub fn analyze_scope(analyzer: *Analyzer, module_offsets: []ModuleStats, scope: *Parser.Scope, current_function: *Parser.Function.Internal, module_index: u64) void
{
    const statement_count = scope.statements.len;
    log("Statement count: {}\n", .{statement_count});
    for (scope.statements) |*statement|
    {
        const statement_index = statement.get_index();
        const statement_level = statement.get_level();
        assert(statement_level == .scope);
        const statement_id = statement.get_array_index(.scope);
        switch (statement_id)
        {
            .invoke_expressions =>
            {
                const invoke_expression = &scope.invoke_expressions[statement_index];
                const expression_to_invoke = invoke_expression.expression;
                const expression_to_invoke_index = expression_to_invoke.get_index();
                const expression_to_invoke_level = expression_to_invoke.get_level();
                assert(expression_to_invoke_level == .scope);
                const expression_to_invoke_id = expression_to_invoke.get_array_index(.scope);

                const resolved_expression_to_invoke: Entity = blk:
                {
                    if (expression_to_invoke_id == .identifier_expressions)
                    {
                        const invoke_expression_name = scope.identifier_expressions[expression_to_invoke_index];
                        for (analyzer.functions.items) |function, i|
                        {
                            if (!std.mem.eql(u8, function.declaration.name, invoke_expression_name))
                            {
                                continue;
                            }

                            break :blk Entity.new(i, Entity.GlobalID.resolved_internal_functions);
                        }

                        unreachable;
                    }
                    else if (expression_to_invoke_id == .field_access_expressions)
                    {
                        const field_expression = scope.field_access_expressions[expression_to_invoke_index];
                        const left = field_expression.left_expression;
                        const left_level = left.get_level();
                        assert(left_level == .scope);
                        const left_array_index = left.get_array_index(.scope);
                        assert(left_array_index == .identifier_expressions);
                        const left_index = left.get_index();
                        const left_name = scope.identifier_expressions[left_index];

                        const field = field_expression.field_expression;
                        const field_level = field.get_level();
                        assert(field_level == .scope);
                        const field_array_index = field.get_array_index(.scope);
                        assert(field_array_index == .identifier_expressions);
                        const field_index = field.get_index();
                        const field_name = scope.identifier_expressions[field_index];

                        const imported_module_range = get_module_item_slice_range(.imported_modules, analyzer, module_offsets, module_index);
                        const imported_modules = analyzer.imported_modules.items[imported_module_range.start..imported_module_range.end];

                        for (imported_modules) |imported_module|
                        {
                            assert(imported_module.alias != null);
                            if (!std.mem.eql(u8, imported_module.alias.?, left_name))
                            {
                                continue;
                            }

                            const imported_module_index = imported_module.module.get_index();

                            const internal_functions_range = get_module_item_slice_range(.internal_functions, analyzer, module_offsets, imported_module_index);
                            const internal_functions = analyzer.functions.items[internal_functions_range.start..internal_functions_range.end];

                            for (internal_functions) |function, function_i|
                            {
                                if (!std.mem.eql(u8, function.declaration.name, field_name))
                                {
                                    continue;
                                }

                                break :blk Entity.new(function_i + internal_functions_range.start, Entity.GlobalID.resolved_internal_functions);
                            }

                            const external_functions_range = get_module_item_slice_range(.external_functions, analyzer, module_offsets, imported_module_index);
                            const external_functions = analyzer.external_functions.items[external_functions_range.start..external_functions_range.end];

                            // @TODO: is this faster than double for loop [library_i, symbol_i]?
                            for (external_functions) |function, function_i|
                            {
                                if (!std.mem.eql(u8, function.declaration.name, field_name))
                                {
                                    continue;
                                }

                                break :blk Entity.new(function_i + external_functions_range.start, Entity.GlobalID.resolved_external_functions);
                            }
                        }

                        unreachable;
                        // not found
                    }
                    else unreachable;
                };

                invoke_expression.expression = resolved_expression_to_invoke;

                if (invoke_expression.arguments.len > 0)
                {
                    const resolved_expression_array_index = resolved_expression_to_invoke.get_array_index(.global);
                    const function_type =
                        if (resolved_expression_array_index == Entity.GlobalID.resolved_internal_functions)
                            analyzer.functions.items[resolved_expression_to_invoke.get_index()].declaration.type
                        else if (resolved_expression_array_index == Entity.GlobalID.resolved_external_functions)
                            analyzer.external_functions.items[resolved_expression_to_invoke.get_index()].declaration.type
                        else unreachable;
                    const argument_types = function_type.argument_types;

                    for (invoke_expression.arguments) |*arg, arg_i|
                    {
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
            .return_expressions =>
            {
                const return_expression = &scope.return_expressions[statement_index];
                const return_type = current_function.declaration.type.return_type;
                
                if (return_expression.expression) |expression_to_return|
                {
                    const ret_expr_lvl = expression_to_return.get_level();
                    assert(ret_expr_lvl == .scope);
                    const ret_expr_array_index = expression_to_return.get_array_index(.scope);

                    switch (ret_expr_array_index)
                    {
                        .integer_literals =>
                        {
                            if (return_type.get_ID() != .integer)
                            {
                                report_error("Expected return type: {s}\n", .{@tagName(return_type.get_ID())});
                            }

                            resolve_entity_index(analyzer, .integer_literals, &return_expression.expression.?, module_offsets, module_index);
                            // @TODO: do further checks?
                        },
                        .identifier_expressions =>
                        {
                            const identifier = scope.identifier_expressions[expression_to_return.get_index()];
                            return_expression.expression.? = find_variable_declaration(scope, identifier);
                        },
                        else => panic("NI: {}\n", .{ret_expr_array_index}),
                    }
                }
                else
                {
                    if (return_type.value != Type.Builtin.void_type.value or return_type.value != Type.Builtin.noreturn_type.value)
                    {
                        report_error("Expected void or noreturn type, have: {s}\n", .{@tagName(return_type.get_ID())});
                    }
                }
            },
            .variable_declarations =>
            {
                var variable_declaration = &scope.variable_declarations[statement_index];
                variable_declaration.type = analyze_type(analyzer, module_offsets, variable_declaration.type);
            },
            .assignments =>
            {
                var assignment = &scope.assignments[statement_index];
                assert(assignment.left.get_level() == .scope);
                const left_id = assignment.left.get_array_index(.scope);

                const left_type = switch (left_id)
                {
                    .variable_declarations => var_blk:
                    {
                        const variable_declaration = scope.variable_declarations[assignment.left.get_index()];
                        break :var_blk variable_declaration.type;
                    },
                    else => panic("NI: {}\n", .{left_id}),
                };

                assert(assignment.right.get_level() == .scope);
                const right_id = assignment.right.get_array_index(.scope);

                switch (right_id)
                {
                    .integer_literals =>
                    {
                        const left_type_id = left_type.get_ID();
                        if (left_type_id != .integer)
                        {
                            report_error("Expected type: {}\n", .{left_type_id});
                        }

                        resolve_entity_index(analyzer, .integer_literals, &assignment.right, module_offsets, module_index);
                    },
                    else => panic("NI: {}", .{right_id}),
                }
            },
            else => panic("NI: {}", .{statement_id}),
        }
    }
}

pub const Analyzer = struct
{
    functions: ArrayList(Parser.Function.Internal),
    external_functions: ArrayList(Parser.Function.External),
    external_library_names: ArrayList([]const u8),
    external_symbol_names: ArrayList([]const u8),
    external_libraries: ArrayList(External.Library),
    imported_modules: ArrayList(Parser.ImportedModule),
    integer_literals: ArrayList(Parser.IntegerLiteral),

    unresolved_types: ArrayList([]const u8),
    pointer_types: ArrayList(Type.Pointer),
    function_types: ArrayList(Type.Function),
    slice_types: ArrayList(Type.Slice),
    array_types: ArrayList(Type.Array),
    struct_types: ArrayList(Type.Struct),
};

pub const Result = struct
{
    functions: []Parser.Function.Internal,
    external: External,
    imported_modules: []Parser.ImportedModule,
    integer_literals: []Parser.IntegerLiteral,

    pointer_types: []Type.Pointer,
    slice_types: []Type.Slice,
    function_types: []Type.Function,
    array_types: []Type.Array,
    struct_types: []Type.Struct,
};


pub const External = struct
{
    functions: []Parser.Function.External,
    library_names: [][]const u8,
    symbol_names: [][]const u8,
    libraries: []Library,

    pub const Library = struct
    {
        symbols: []u32,
    };
};

pub fn analyze(allocator: *Allocator, ast: Parser.AST) Result
{
    var module_array = ast.modules[0..ast.module_len];
    var total = std.mem.zeroes(ModuleStats);
    var module_offsets = ArrayList(ModuleStats).initCapacity(allocator, ast.module_len) catch unreachable;
    module_offsets.resize(ast.module_len) catch unreachable;

    for (module_array) |module, i|
    {
        log("Module imported module count: {}\n", .{module.imported_modules.len});
        module_offsets.items[i].counters[@enumToInt(ModuleStats.ID.internal_functions)] = total.counters[@enumToInt(ModuleStats.ID.internal_functions)];
        module_offsets.items[i].counters[@enumToInt(ModuleStats.ID.external_functions)] = total.counters[@enumToInt(ModuleStats.ID.external_functions)];
        module_offsets.items[i].counters[@enumToInt(ModuleStats.ID.imported_modules)] = total.counters[@enumToInt(ModuleStats.ID.imported_modules)];
        log("Imported module count: {}\n", .{module_offsets.items[i].counters[@enumToInt(ModuleStats.ID.imported_modules)]});
        module_offsets.items[i].counters[@enumToInt(ModuleStats.ID.integer_literals)] = total.counters[@enumToInt(ModuleStats.ID.integer_literals)];

        module_offsets.items[i].counters[@enumToInt(ModuleStats.ID.unresolved_types)] = total.counters[@enumToInt(ModuleStats.ID.unresolved_types)];
        module_offsets.items[i].counters[@enumToInt(ModuleStats.ID.pointer_types)] = total.counters[@enumToInt(ModuleStats.ID.pointer_types)];
        module_offsets.items[i].counters[@enumToInt(ModuleStats.ID.slice_types)] = total.counters[@enumToInt(ModuleStats.ID.slice_types)];
        module_offsets.items[i].counters[@enumToInt(ModuleStats.ID.function_types)] = total.counters[@enumToInt(ModuleStats.ID.function_types)];
        module_offsets.items[i].counters[@enumToInt(ModuleStats.ID.array_types)] = total.counters[@enumToInt(ModuleStats.ID.array_types)];
        module_offsets.items[i].counters[@enumToInt(ModuleStats.ID.struct_types)] = total.counters[@enumToInt(ModuleStats.ID.struct_types)];

        total.counters[@enumToInt(ModuleStats.ID.internal_functions)] += module.internal_functions.len;
        total.counters[@enumToInt(ModuleStats.ID.external_functions)] += module.external_functions.len;
        total.counters[@enumToInt(ModuleStats.ID.imported_modules)] += module.imported_modules.len;
        total.counters[@enumToInt(ModuleStats.ID.integer_literals)] += module.integer_literals.len;

        total.counters[@enumToInt(ModuleStats.ID.unresolved_types)] += module.unresolved_types.len;
        total.counters[@enumToInt(ModuleStats.ID.pointer_types)] += module.pointer_types.len;
        total.counters[@enumToInt(ModuleStats.ID.slice_types)]+= module.slice_types.len;
        total.counters[@enumToInt(ModuleStats.ID.function_types)]+= module.function_types.len;
        total.counters[@enumToInt(ModuleStats.ID.array_types)]+= module.array_types.len;
        total.counters[@enumToInt(ModuleStats.ID.struct_types)] += module.struct_types.len;
    }

    var analyzer = Analyzer
    {
        .functions = ArrayList(Parser.Function.Internal).initCapacity(allocator, total.counters[@enumToInt(ModuleStats.ID.internal_functions)]) catch unreachable,
        .imported_modules = ArrayList(Parser.ImportedModule).initCapacity(allocator, total.counters[@enumToInt(ModuleStats.ID.imported_modules)]) catch unreachable,
        .integer_literals = ArrayList(Parser.IntegerLiteral).initCapacity(allocator, total.counters[@enumToInt(ModuleStats.ID.integer_literals)]) catch unreachable,

        .external_functions = ArrayList(Parser.Function.External).initCapacity(allocator, total.counters[@enumToInt(ModuleStats.ID.external_functions)]) catch unreachable,
        .external_library_names = ArrayList([]const u8).init(allocator),
        .external_symbol_names = ArrayList([]const u8).init(allocator),
        .external_libraries = ArrayList(External.Library).init(allocator),

        .unresolved_types = ArrayList([]const u8).initCapacity(allocator, total.counters[@enumToInt(ModuleStats.ID.unresolved_types)]) catch unreachable,
        .pointer_types = ArrayList(Type.Pointer).initCapacity(allocator, total.counters[@enumToInt(ModuleStats.ID.pointer_types)]) catch unreachable,
        .slice_types = ArrayList(Type.Slice).initCapacity(allocator, total.counters[@enumToInt(ModuleStats.ID.slice_types)]) catch unreachable,
        .function_types = ArrayList(Type.Function).initCapacity(allocator, total.counters[@enumToInt(ModuleStats.ID.function_types)]) catch unreachable,
        .array_types = ArrayList(Type.Array).initCapacity(allocator, total.counters[@enumToInt(ModuleStats.ID.array_types)]) catch unreachable,
        .struct_types = ArrayList(Type.Struct).initCapacity(allocator, total.counters[@enumToInt(ModuleStats.ID.struct_types)]) catch unreachable,
    };

    for (module_array) |*module|
    {
        next_lib_name: for (module.library_names) |new_library_name|
        {
            for (analyzer.external_library_names.items) |library_name|
            {
                if (std.mem.eql(u8, library_name, new_library_name))
                {
                    continue :next_lib_name;
                }
            }

            analyzer.external_library_names.append(new_library_name) catch unreachable;
        }
    }

    const library_count = analyzer.external_library_names.items.len;
    var libraries_symbols_indices = ArrayList(ArrayList(u32)).initCapacity(allocator, library_count) catch unreachable;
    libraries_symbols_indices.appendNTimesAssumeCapacity(ArrayList(u32).init(allocator), library_count);
    analyzer.external_libraries.ensureTotalCapacity(library_count) catch unreachable;

    for (module_array) |*module|
    {
        for (module.libraries) |library, library_i|
        {
            next_symbol_name: for (library.symbol_names.items) |new_symbol_name|
            {
                for (analyzer.external_symbol_names.items) |symbol_name, symbol_i|
                {
                    if (std.mem.eql(u8, symbol_name, new_symbol_name))
                    {
                        var library_symbol_list = &libraries_symbols_indices.items[library_i];
                        for (library_symbol_list.items) |symbol_index|
                        {
                            if (symbol_index == symbol_i)
                            {
                                continue :next_symbol_name;
                            }
                        }

                        analyzer.external_symbol_names.append(new_symbol_name) catch unreachable;
                        library_symbol_list.append(@intCast(u32, symbol_i)) catch unreachable;
                    }
                }

                const symbol_i = @intCast(u32, analyzer.external_symbol_names.items.len);
                analyzer.external_symbol_names.append(new_symbol_name) catch unreachable;
                libraries_symbols_indices.items[library_i].append(symbol_i) catch unreachable;
            }
        }
    }

    for (libraries_symbols_indices.items) |library_symbol_list|
    {
        analyzer.external_libraries.appendAssumeCapacity(. { .symbols = library_symbol_list.items });
    }

    //var library_names = ArrayList([]const u8).init(allocator);
    //for (module_array) |*module|
    //{
        //next_lib_name: for (module.library_names) |new_library_name|
        //{
            //for (library_names.items) |library_name|
            //{
                //if (std.mem.eql(u8, library_name, new_library_name))
                //{
                    //continue :next_lib_name;
                //}
            //}

            //library_names.append(new_library_name) catch unreachable;
        //}
    //}


    //for (library_names.items) |library_name|
    //{
        //var library_symbol_names = ArrayList([]const u8).init(allocator);

        //for (module_array) |*module|
        //{
            //for (module.library_names) |module_library_name, library_i|
            //{
                //if (std.mem.eql(u8, module_library_name, library_name))
                //{
                    //for (module.libraries[library_i].symbol_names.items) |new_symbol_name|
                    //{
                        //for (library_symbol_names.items) |symbol_name|
                        //{
                            //if (std.mem.eql(u8, symbol_name, new_symbol_name))
                            //{
                                //break;
                            //}
                        //}

                        //library_symbol_names.append(new_symbol_name) catch unreachable;
                    //}
                //}
            //}
        //}
    //}

    //var ArrayList(ArrayList([]const u8)).initCapacity(allocator, library_names.items.len);

    for (module_array) |*module|
    {
        for (module.external_functions) |*external_function|
        {
            const module_symbol_name = external_function.declaration.name;
            //const module_symbol_i = external_function.index.symbol;
            const module_library_i = external_function.index.function;

            const module_library_name = module.library_names[module_library_i];
            //const module_library = &module.libraries.items[module_library_i];

            external_function.index = index_blk:
            {
                for (analyzer.external_library_names.items) |library_name, library_i|
                {
                    if (std.mem.eql(u8, module_library_name, library_name))
                    {
                        const library = analyzer.external_libraries.items[library_i];

                        for (library.symbols) |symbol_i|
                        {
                            const symbol_name = analyzer.external_symbol_names.items[symbol_i];
                            if (std.mem.eql(u8, symbol_name, module_symbol_name))
                            {
                                const final_library_i = @intCast(u16, library_i);
                                const final_symbol_i = @intCast(u16, symbol_i);

                                break :index_blk .
                                {
                                    .library = final_library_i,
                                    .function = final_symbol_i,
                                };
                            }
                        }

                        unreachable;
                        //const final_library_i = @intCast(u16, library_i);
                        //const final_symbol_i = @intCast(u16, library.symbol_names.items.len);

                        //library.symbol_names.append(module_symbol_name) catch unreachable;

                        //break :index_blk .
                        //{
                            //.library = final_library_i,
                            //.function = final_symbol_i,
                        //};
                    }
                }

                unreachable;
                //const final_library_i = @intCast(u16, analyzer.library_names.items.len);
                //analyzer.library_names.append(module_library_name) catch unreachable;
                //analyzer.libraries.append(.{
                    //.symbol_names = blk:
                    //{
                        //var library_symbol_names = ArrayList([]const u8).init(allocator);
                        //library_symbol_names.append(module_symbol_name) catch unreachable;
                        //break :blk library_symbol_names;
                    //},
                    //}) catch unreachable;

                //const final_symbol_i: u16 = 0;

                //break :index_blk .
                //{
                    //.library = final_library_i,
                    //.function = final_symbol_i,
                //};
            };
        }
    }

    //var libs = Libraries
    //{
        //.library_names = library_names.items,
        //.symbol_names = symbol_names.items,
        //.libraries = libraries.items,
    //};

    for (module_array) |*module|
    {
        analyzer.functions.appendSlice(module.internal_functions) catch unreachable;
        analyzer.external_functions.appendSlice(module.external_functions) catch unreachable;
        analyzer.imported_modules.appendSlice(module.imported_modules) catch unreachable;
        analyzer.integer_literals.appendSlice(module.integer_literals) catch unreachable;
        analyzer.unresolved_types.appendSlice(module.unresolved_types) catch unreachable;
        analyzer.pointer_types.appendSlice(module.pointer_types) catch unreachable;
        analyzer.slice_types.appendSlice(module.slice_types) catch unreachable;
        analyzer.function_types.appendSlice(module.function_types) catch unreachable;
        analyzer.array_types.appendSlice(module.array_types) catch unreachable;
        analyzer.struct_types.appendSlice(module.struct_types) catch unreachable;
    }

    log("Internal:\t{}\n", .{analyzer.functions.items.len});
    log("External libraries:\t{}\n", .{analyzer.external_library_names.items.len});
    log("External symbols:\t{}\n", .{analyzer.external_symbol_names.items.len});
    log("Imported:\t{}\n", .{analyzer.imported_modules.items.len});
    log("Unresolved:\t{}\n", .{analyzer.unresolved_types.items.len});
    log("Pointer:\t{}\n", .{analyzer.pointer_types.items.len});
    log("Slice:\t{}\n", .{analyzer.slice_types.items.len});
    log("Function:\t{}\n", .{analyzer.function_types.items.len});
    log("Array\t{}\n", .{analyzer.array_types.items.len});
    log("Struct\t{}\n", .{analyzer.struct_types.items.len});


    // @TODO: shouldn't we discard repeated function types?
    for (analyzer.function_types.items) |*function_type|
    {
        function_type.return_type = analyze_type(&analyzer, module_offsets.items, function_type.return_type);
        for (function_type.argument_types) |*argument_type|
        {
            argument_type.* = analyze_type(&analyzer, module_offsets.items, argument_type.*);
        }
    }

    for (analyzer.external_functions.items) |*function|
    {
        function.declaration.type.return_type = analyze_type(&analyzer, module_offsets.items, function.declaration.type.return_type);

        for (function.declaration.type.argument_types) |*argument_type|
        {
            argument_type.* = analyze_type(&analyzer, module_offsets.items, argument_type.*);
        }
    }

    for (analyzer.functions.items) |*function|
    {
        function.declaration.type.return_type = analyze_type(&analyzer, module_offsets.items, function.declaration.type.return_type);

        for (function.declaration.type.argument_types) |*argument_type|
        {
            argument_type.* = analyze_type(&analyzer, module_offsets.items, argument_type.*);
        }
    }

    for (module_offsets.items) |_, module_index|
    {
        const function_range = get_module_item_slice_range(.internal_functions, &analyzer, module_offsets.items, module_index);
        for (analyzer.functions.items[function_range.start..function_range.end]) |*function|
        {
            log("Analyzing {s}()...\n", .{function.declaration.name});
            const main_block = &function.scopes[0];
            analyze_scope(&analyzer, module_offsets.items, main_block, function, module_index);
        }

    }

    return Result
    {
        .functions = analyzer.functions.items,
        .imported_modules = analyzer.imported_modules.items,
        .integer_literals = analyzer.integer_literals.items,

        .external = .
        {
            .functions = analyzer.external_functions.items,
            .library_names = analyzer.external_library_names.items,
            .symbol_names = analyzer.external_symbol_names.items,
            .libraries = analyzer.external_libraries.items,
        },

        .pointer_types = analyzer.pointer_types.items,
        .slice_types = analyzer.slice_types.items,
        .function_types = analyzer.function_types.items,
        .array_types = analyzer.array_types.items,
        .struct_types = analyzer.struct_types.items,
    };
}
