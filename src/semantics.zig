const std = @import("std");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const print = std.debug.print;
const assert = std.debug.assert;
const panic = std.debug.panic;

const Internal = @import("compiler.zig");

const Parser = @import("parser.zig");
const Node = Parser.Node;
const NodeBuffer = Parser.NodeBuffer;
const NodeRefBuffer = Parser.NodeRefBuffer;
const ParserResult = Parser.ParserResult;
const TypeIdentifier = Parser.TypeIdentifier;
const BinaryExpression = Parser.BinaryExpression;
const UnaryExpression = Parser.UnaryExpression;

const Compiler = Internal.Compiler;
const Log = Compiler.LogLevel;
const TypeBuffer = Internal.TypeBuffer;
const Type = Internal.Type;
const TypeRefBuffer = Internal.TypeRefBuffer;

fn analyze_type_declaration(compiler: *Compiler, allocator: *Allocator, type_in_analysis: *Node, types: *TypeBuffer, node_buffer: *NodeBuffer) *Type
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
                        compiler.log(Log.debug, "Type already found\n", .{});
                        return type_found;
                    }
                    else
                    {
                        compiler.log(Log.debug, "Type not found. Must create\n", .{});
                    }

                    const struct_name = type_in_analysis.value.type_identifier.value.structure.name;
                    var fields = ArrayList(Type.Struct.Field).init(allocator);

                    for (type_in_analysis.value.type_identifier.value.structure.fields.items) |ast_field, i|
                    {
                        // @Info: struct fields are var decls
                        assert(ast_field.value == Node.ID.var_decl);
                        const field_type = analyze_type_declaration(compiler, allocator, ast_field.value.var_decl.var_type, types, node_buffer);
                        const field_name = ast_field.value.var_decl.name;

                        const new_field = Type.Struct.Field
                        {
                            .name = field_name,
                            .type = field_type,
                            .parent = undefined,
                            .index =  i,
                        };

                        fields.append(new_field) catch |err| {
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
                        const return_type = analyze_type_declaration(compiler, allocator, return_type_node, types, node_buffer);
                        function_type.ret_type = return_type;
                    }
                    else
                    {
                        const return_type = Type.get_void_type(types);
                        function_type.ret_type = return_type;
                    }

                    const ast_arg_type_count = type_in_analysis.value.type_identifier.value.function.arg_types.items.len;

                    for (type_in_analysis.value.type_identifier.value.function.arg_types.items) |*arg_type_node|
                    {
                        const arg_type = analyze_type_declaration(compiler, allocator, arg_type_node.*, types, node_buffer);
                        function_type.arg_types.append(arg_type) catch |err| {
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
                        compiler.report_error("Type {s} not found\n", .{name});
                    }
                },
                TypeIdentifier.ID.pointer =>
                {
                    const appointee_node = type_in_analysis.value.type_identifier.value.pointer.type;
                    const appointee_type = analyze_type_declaration(compiler, allocator, appointee_node, types, node_buffer);
                    const pointer_type = Type.get_pointer_type(appointee_type, types);
                    return pointer_type;
                },
                TypeIdentifier.ID.array =>
                {
                    const array_elem_type_node = type_in_analysis.value.type_identifier.value.array.type;
                    const array_length_node = type_in_analysis.value.type_identifier.value.array.len_expr;
                    const array_elem_type = analyze_type_declaration(compiler, allocator, array_elem_type_node, types, node_buffer);
                    const array_length_constant = resolve_compile_time_uint_constant(compiler, array_length_node);
                    const array_type = Type.get_array_type(array_elem_type, array_length_constant, types);
                    return array_type;
                },
                else => panic("not implemented: {}\n", .{type_in_analysis.value.type_identifier.value}),
            }
        },
        else => panic("Unreachable or ni: {}\n", .{type_in_analysis.value}),
    }
}

fn resolve_compile_time_uint_constant(compiler: *Compiler, node: *Node) usize
{
    switch (node.value)
    {
        Node.ID.int_lit =>
        {
            const int_lit_value = node.value.int_lit.value;
            if (node.value.int_lit.signed)
            {
                compiler.report_error("Expected unsigned element, value is signed: {}\n", .{- @intCast(i64, int_lit_value)});
            }

            return int_lit_value;
        },
        else => panic("ni: {}\n", .{node.value}),
    }
}

pub fn typecheck(compiler: *Compiler, lvalue_type: *Type, right: *Node, types: *TypeBuffer) *Type
{
    compiler.log(Log.debug, "Left type: {} --- Right type: {}\n", .{lvalue_type, right.type});

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
                    compiler.log(Log.debug, "Integer literal type resolved\n", .{});
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
                    compiler.log(Log.debug, "{}\n", .{rvalue_type});
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
                    const result = typecheck(compiler, lvalue_type.value.array.type, right.value.array_lit.elements.items[0], types); 
                    assert(result.value != Type.ID.unresolved);
                    // @TODO: typecheck here
                    compiler.log(Log.debug, "Array literal resolved into {}\n", .{result});
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

    compiler.report_error("Typecheck failed!\nLeft: {}\nRight: {}\n", .{lvalue_type, right});
}

pub fn find_variable(compiler: *Compiler, allocator: *Allocator, current_function: *Node, current_block: *Node, name: []const u8, types: *TypeBuffer) *Node
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

    compiler.report_error("Can't find variable name: {s}\n", .{name});
}

pub fn find_function_decl(compiler: *Compiler, allocator: *Allocator, current_function: *Node, current_block: *Node, name: []const u8, types: *TypeBuffer, functions: *NodeRefBuffer) *Node
{
    for (functions.items) |function_decl|
    {
        if (std.mem.eql(u8, function_decl.value.function_decl.name, name))
        {
            return function_decl;
        }
    }

    compiler.report_error("Can't find function name: {s}\n", .{name});
}

pub fn explore_field_identifier_expression(compiler: *Compiler, allocator: *Allocator, current_function: *Node, current_block: *Node, node: *Node, types: *TypeBuffer, functions: *NodeRefBuffer, node_buffer: *NodeBuffer) *Node
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
                                const decl_node = struct_var_node.value.resolved_identifier;
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
                                                        for (appointee_type.value.structure.fields) |*field|
                                                        {
                                                            if (std.mem.eql(u8, field.name,  name))
                                                            {
                                                                const field_node = Node
                                                                {
                                                                    .value = Node.Value {
                                                                        .field_expr = field,
                                                                    },
                                                                    .parent = node.parent,
                                                                    .value_type = Node.ValueType.RValue,
                                                                    .type = field.type,
                                                                };

                                                                var result = node_buffer.append(field_node) catch |err| {
                                                                    panic("Error allocating memory for type node\n", .{});
                                                                };
                                                                return result;
                                                            }
                                                        }

                                                        compiler.report_error("Couldn't match field expression with structure\n", .{});
                                                    },
                                                    else => panic("ni: {}\n", .{appointee_type.value}),
                                                }
                                            },
                                            else => panic("ni: {}\n", .{decl_type.value}),
                                        }
                                    },
                                    else => panic("ni: {}\n", .{decl_node.value}),
                                }
                            },
                            else => panic("ni: {}\n", .{struct_var_node.value}),
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

pub fn explore_expression(compiler: *Compiler, allocator: *Allocator, current_function: *Node, current_block: *Node, node: *Node, types: *TypeBuffer, functions: *NodeRefBuffer, node_buffer: *NodeBuffer) *Node
{
    switch (node.value)
    {
        Node.ID.var_decl =>
        {
            node.type = analyze_type_declaration(compiler, allocator, node.value.var_decl.var_type, types, node_buffer);
        },
        Node.ID.binary_expr =>
        {
            const binary_op = node.value.binary_expr.id;
            node.value.binary_expr.left = explore_expression(compiler, allocator, current_function, current_block, node.value.binary_expr.left, types, functions, node_buffer);
            node.value.binary_expr.right = explore_expression(compiler, allocator, current_function, current_block, node.value.binary_expr.right, types, functions, node_buffer);
            node.type = typecheck(compiler, node.value.binary_expr.left.type, node.value.binary_expr.right, types);
        },
        Node.ID.identifier_expr =>
        {
            const name = node.value.identifier_expr.name;
            const decl_node = find_variable(compiler, allocator, current_function, current_block, name, types);
            const new_node_value = Node
            {
                .value = Node.Value {
                    .resolved_identifier = decl_node,
                },
                .value_type = node.value_type,
                .parent = node.parent,
                .type = decl_node.type,
            };

            var new_node = node_buffer.append(new_node_value) catch |err| {
                panic("Error allocating memory for resolved identifier node\n", .{});
            };

            return new_node;
        },
        Node.ID.return_expr =>
        {
            if (node.value.return_expr.expression) |return_expr|
            {
                node.value.return_expr.expression = explore_expression(compiler, allocator, current_function, current_block, return_expr, types, functions, node_buffer);
            }
            node.type = Type.get_void_type(types);
        },
        Node.ID.block_expr =>
        {
            for (node.value.block_expr.statements.items) |*statement|
            {
                const new_current_block = node;
                statement.* = explore_expression(compiler, allocator, current_function, new_current_block, statement.*, types, functions, node_buffer);
            }
            node.type = Type.get_void_type(types);
        },
        Node.ID.loop_expr =>
        {
            node.value.loop_expr.prefix = explore_expression(compiler, allocator, current_function, current_block, node.value.loop_expr.prefix, types, functions, node_buffer);
            node.value.loop_expr.body = explore_expression(compiler, allocator, current_function, current_block, node.value.loop_expr.body, types, functions, node_buffer);
            node.value.loop_expr.postfix = explore_expression(compiler, allocator, current_function, current_block, node.value.loop_expr.postfix, types, functions, node_buffer);
            node.type = Type.get_void_type(types);
        },
        Node.ID.branch_expr =>
        {
            node.value.branch_expr.condition = explore_expression(compiler, allocator, current_function, current_block, node.value.branch_expr.condition, types, functions, node_buffer);
            node.value.branch_expr.if_block = explore_expression(compiler, allocator, current_function, current_block, node.value.branch_expr.if_block, types, functions, node_buffer);
            if (node.value.branch_expr.else_block) |else_block|
            {
                node.value.branch_expr.else_block = explore_expression(compiler, allocator, current_function, current_block, else_block, types, functions, node_buffer);
            }
            node.type = Type.get_void_type(types);
        },
        Node.ID.invoke_expr =>
        {
            const function_call_id_node = node.value.invoke_expr.expression;
            assert(function_call_id_node.value == Node.ID.identifier_expr);
            const function_call_name = function_call_id_node.value.identifier_expr.name;
            compiler.log(Log.debug, "function call name: {s}\n", .{function_call_name});
            const function_decl = find_function_decl(compiler, allocator, current_function, current_block, function_call_name, types, functions);
            node.type = function_decl.type.value.function.ret_type;
            node.value.invoke_expr.expression = function_decl;

            for (node.value.invoke_expr.arguments.items) |*arg|
            {
                arg.* = explore_expression(compiler, allocator, current_function, current_block, arg.*, types, functions, node_buffer);
            }
        },
        Node.ID.unary_expr =>
        {
            // @TODO: check anything more with unary expression id?
            node.value.unary_expr.node_ref = explore_expression(compiler, allocator, current_function, current_block, node.value.unary_expr.node_ref, types, functions, node_buffer);
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
            const first_elem_analyzed = explore_expression(compiler, allocator, current_function, current_block, first_elem, types, functions, node_buffer);
            const first_elem_type = first_elem.type;

            for (node.value.array_lit.elements.items) |*array_elem|
            {
                array_elem.* = explore_expression(compiler, allocator, current_function, current_block, array_elem.*, types, functions, node_buffer);
                _ = typecheck(compiler, first_elem_type, array_elem.*, types);
            }

            // @TODO: improve
            node.type = Type.get_literal_type(types);
        },
        Node.ID.array_subscript_expr =>
        {
            node.value.array_subscript_expr.expression = explore_expression(compiler, allocator, current_function, current_block, node.value.array_subscript_expr.expression, types, functions, node_buffer);
            node.value.array_subscript_expr.index = explore_expression(compiler, allocator, current_function, current_block, node.value.array_subscript_expr.index, types, functions, node_buffer);
            node.type = node.value.array_subscript_expr.expression.type.value.array.type;
        },
        Node.ID.field_access_expr =>
        {
            node.value.field_access_expr.expression = explore_expression(compiler, allocator, current_function, current_block, node.value.field_access_expr.expression, types, functions, node_buffer);
            node.value.field_access_expr.field_expr = explore_field_identifier_expression(compiler, allocator, current_function, current_block, node.value.field_access_expr.field_expr, types, functions, node_buffer);
            node.type = node.value.field_access_expr.field_expr.type;
        },
        else => panic("ni: {}\n", .{node.value}),
    }

    return node;
}

pub const SemanticsResult = struct 
{
    types: TypeBuffer,
    function_declarations: NodeRefBuffer,
};

pub fn analyze(compiler: *Compiler, allocator: *Allocator, parser_result: *ParserResult) SemanticsResult
{
    compiler.current_module = Compiler.Module.semantics;
    compiler.log(Compiler.LogLevel.debug, "\n==============\nSEMANTICS\n==============\n\n", .{});

    var types = Type.init(allocator);

    for (parser_result.type_declarations.items) |typename|
    {
        _ = analyze_type_declaration(compiler, allocator, typename, &types, &parser_result.node_buffer);
    }

    // Check for function types
    for (parser_result.function_declarations.items) |function_decl|
    {
        const function_type = analyze_type_declaration(compiler, allocator, function_decl.value.function_decl.type, &types, &parser_result.node_buffer);
        function_decl.type = function_type;
        for (function_decl.value.function_decl.arguments.items) |arg, i|
        {
            arg.type = function_type.value.function.arg_types.items[i];
        }
    }

    for (parser_result.function_declarations.items) |function_decl|
    {
        const main_block = function_decl.value.function_decl.blocks.items[0];

        _ = explore_expression(compiler, allocator, function_decl, main_block, main_block, &types, &parser_result.function_declarations, &parser_result.node_buffer);
    }

    const semantics_result = SemanticsResult
    {
        .types = types,
        .function_declarations = parser_result.function_declarations,
    };

    return semantics_result;
}
