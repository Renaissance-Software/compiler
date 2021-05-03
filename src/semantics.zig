const std = @import("std");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const print = std.debug.print;
const assert = std.debug.assert;
const panic = std.debug.panic;

const Internal = @import("compiler.zig");

const Parser = @import("parser.zig");
const Node = Parser.Node;
const NodeBuffer = Parser.NodeRefBuffer;
const NodeRefBuffer = Parser.NodeRefBuffer;
const ParserResult = Parser.ParserResult;
const TypeIdentifier = Parser.TypeIdentifier;
const BinaryExpression = Parser.BinaryExpression;

const Compiler = Internal.Compiler;
const TypeBuffer = Internal.TypeBuffer;
const Type = Internal.Type;
const TypeRefBuffer = Internal.TypeRefBuffer;

const HashMap = std.AutoHashMap;

fn analyze_type_declaration(compiler: *Compiler, allocator: *Allocator, type_in_analysis: *Node, types: *TypeBuffer) *Type
{
    assert(type_in_analysis.value == Node.ID.type_identifier);

    const type_value = type_in_analysis.value.type_identifier.value;
    const name = switch (type_value)
    {
        TypeIdentifier.ID.structure => type_value.structure.name,
        TypeIdentifier.ID.simple =>type_value.simple,
        else => panic("ni: {}\n", .{type_value}),
    };

    if (Type.get_type_by_name(types, name)) |type_found|
    {
        print("Type already found\n", .{});
        return type_found;
    }
    else
    {
        print("Type not found. Must create\n", .{});
        if (type_value != TypeIdentifier.ID.structure)
        {
            panic("Type must be declared before being used\n", .{}); 
        }
    }

    const type_identifier = type_in_analysis.value.type_identifier;
    switch (type_identifier.value)
    {
        TypeIdentifier.ID.structure =>
        {
            const structure = type_identifier.value.structure;
            const struct_name = structure.name;
            var fields = ArrayList(Type.Struct.Field).init(allocator);

            for (structure.fields.items) |ast_field, i|
            {
                // @Info: struct fields are var decls
                assert(ast_field.value == Node.ID.var_decl);
                const field_type = analyze_type_declaration(compiler, allocator, ast_field.value.var_decl.var_type, types);
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
        else => panic("ni: {}\n", .{type_identifier.value}),
    }
}

pub fn create_type_node(allocator: *Allocator, node_type: *Type) *Node
{
    const node = Node {
        .value = Node.Value {
            .resolved_type = node_type,
        },
        .parent = null,
        .value_type = Node.ValueType.RValue,
    };

    var result = allocator.create(Node) catch |err| {
        panic("Error allocating memory for type node\n", .{});
    };
    result.* = node;
    return result;
}

pub fn analyze_variable_declaration(compiler: *Compiler, allocator: *Allocator, current_function: *Node, current_block: *Node, node: *Node, types: *TypeBuffer) void
{
    assert(node.value == Node.ID.var_decl);

    const resolved_type = analyze_type_declaration(compiler, allocator, node.value.var_decl.var_type, types);
    node.value.var_decl.var_type = create_type_node(allocator, resolved_type);
}

pub fn typecheck_binary(compiler: *Compiler, left: *Node, right: *Node, binary_op: BinaryExpression.ID) void
{
    switch (binary_op)
    {
        BinaryExpression.ID.Assignment =>
        {
            switch (left.value)
            {
                Node.ID.identifier_expr =>
                {
                    const decl = left.value.identifier_expr.reference;
                    const decl_type_node = decl.value.var_decl.var_type;
                    assert(decl_type_node.value == Node.ID.resolved_type);
                    const decl_type = decl_type_node.value.resolved_type;
                    panic("reached here\n", .{});
                },
                else => panic("ni: {}\n", .{left.value}),
            }
        },
        else => panic("ni: {}\n", .{binary_op}),
    }

    compiler.report_error("Typecheck failed!\nLeft: {}\nRight: {}\n", .{left, right});
}

pub fn analyze_binary_expression(compiler: *Compiler, allocator: *Allocator, current_function: *Node, current_block: *Node, node: *Node, types: *TypeBuffer) void
{
    const binary_op = node.value.binary_expr.id;
    const left = node.value.binary_expr.left;
    const right = node.value.binary_expr.right;
    explore_expression(compiler, allocator, current_function, current_block, left, types);
    explore_expression(compiler, allocator, current_function, current_block, right, types);
    typecheck_binary(compiler, left, right, binary_op);
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

pub fn explore_expression(compiler: *Compiler, allocator: *Allocator, current_function: *Node, current_block: *Node, node: *Node, types: *TypeBuffer) void
{
    switch (node.value)
    {
        Node.ID.var_decl =>
        {
            analyze_variable_declaration(compiler, allocator, current_function, current_block, node, types);
        },
        Node.ID.binary_expr =>
        {
            analyze_binary_expression(compiler, allocator, current_function, current_block, node, types);
        },
        Node.ID.identifier_expr =>
        {
            const name = node.value.identifier_expr.name;
            node.value.identifier_expr.reference = find_variable(compiler, allocator, current_function, current_block, name, types);
        },
        Node.ID.int_lit,  => { },
        else => panic("ni: {}\n", .{node.value}),
    }
}

pub fn explore_block(compiler: *Compiler, allocator: *Allocator, current_function: *Node, current_block: *Node, types: *TypeBuffer) void
{
    for (current_block.value.block_expr.statements.items) |statement|
    {
        switch (statement.value)
        {
            Node.ID.return_expr =>
            {
                if (statement.value.return_expr.expression) |return_expr|
                {
                    explore_expression(compiler, allocator, current_function, current_block, return_expr, types);
                }
            },
            Node.ID.var_decl =>
            {
                analyze_variable_declaration(compiler, allocator, current_function, current_block, statement, types);
            },
            Node.ID.binary_expr =>
            {
                analyze_binary_expression(compiler, allocator, current_function, current_block, statement, types);
            },
            else => panic("ni: {}\n", .{statement.value}),
        }
    }
}

pub fn analyze(compiler: *Compiler, allocator: *Allocator, function_declarations: NodeRefBuffer, typenames: NodeRefBuffer) void
{
    var types = Type.init(allocator);
    var ast_function_decl = function_declarations;
    var ast_typenames = typenames;

    for (ast_typenames.items) |typename|
    {
        _ = analyze_type_declaration(compiler, allocator, typename, &types);
    }

    for (function_declarations.items) |function_decl|
    {
        const function_declaration = function_decl.value.function_decl;
        const main_block = function_declaration.blocks.items[0];
        const arguments = function_declaration.arguments;
        const variables = function_declaration.variables;
        const function_name = function_declaration.name;
        const function_type = function_declaration.type;

        explore_block(compiler, allocator, function_decl, main_block, &types);
    }
}
