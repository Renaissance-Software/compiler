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
const Entity = @import("entity.zig").Entity;

const Compiler = @import("compiler.zig");

pub fn log(comptime format: []const u8, arguments: anytype) void
{
    Compiler.log(.semantics, "[SEMANTICS] " ++ format, arguments);
}

pub fn report_error(comptime format: []const u8, args: anytype) noreturn
{
    panic(format, args);
}

pub fn resolve_type(T: Type, new_index: u64) Type
{
    var old_type = T;
    old_type.set_new_index(new_index);
    old_type.mark_as_resolved();

    return old_type;
}

// @TODO: make this fast
pub fn get_module_item_slice_range(comptime module_stats_id: ModuleStats.ID, analyzer: *Analyzer, module_index: u64) struct { start: u64, end: u64 }
{
    const this_module_offsets = analyzer.module_offsets[module_index];
    const internal_item_start = this_module_offsets.counters[@enumToInt(module_stats_id)];
    const next_module_index = module_index + 1;
    const internal_item_end =
        if (next_module_index < analyzer.module_offsets.len)
            analyzer.module_offsets[next_module_index].counters[@enumToInt(module_stats_id)]
        else switch (comptime module_stats_id)
        {
            .internal_functions => analyzer.functions.items.len,
            .external_functions => analyzer.external_functions.items.len,
            .integer_literals => analyzer.integer_literals.items.len,
            .array_literals => analyzer.array_literals.items.len,
            .imported_modules => analyzer.imported_modules.items.len,
            else => panic("here: {}\n", .{module_stats_id}),
        };

    return .{ .start = internal_item_start, .end = internal_item_end };
}

// @INFO: we are logging types to see if there is any wrong-generated one
fn analyze_type(analyzer: *Analyzer, unresolved_type: Type) Type
{
    const type_id = unresolved_type.get_ID();
    const module_index = unresolved_type.get_module_index();

    switch (type_id)
    {
        // @INFO: types that are already resolved
        .builtin, .integer => return unresolved_type,
        .unresolved =>
        {
            const unresolved_type_module_offset = analyzer.module_offsets[module_index].counters[@enumToInt(ModuleStats.ID.unresolved_types)];
            const index = unresolved_type.get_index();
            const unresolved_type_identifier = analyzer.unresolved_types.items[unresolved_type_module_offset + index];

            log("Unresolved type identifier: {s}\n", .{unresolved_type_identifier});
            if (unresolved_type_identifier[0] == 'u')
            {
                if (std.fmt.parseUnsigned(u16, unresolved_type_identifier[1..], 10)) |bit_count|
                {
                    const unsigned_integer_type = Type.Integer.new(bit_count, .unsigned);
                    log("{}. Bit count: {}\n", .{unsigned_integer_type.get_ID(), bit_count});
                    return unsigned_integer_type;
                }
                else |_| {}
            }
            if (unresolved_type_identifier[0] == 's')
            {
                if (std.fmt.parseUnsigned(u16, unresolved_type_identifier[1..], 10)) |bit_count|
                {
                    const signed_integer_type = Type.Integer.new(bit_count, .signed);
                    log("{}. Bit count: {}\n", .{signed_integer_type.get_ID(), bit_count});
                    return signed_integer_type;
                }
                else |_| {}
            }

            for (analyzer.struct_types.items) |*struct_type, struct_i|
            {
                if (std.mem.eql(u8, struct_type.name, unresolved_type_identifier))
                {
                    const struct_type_module_offset = analyzer.module_offsets[module_index].counters[@enumToInt(ModuleStats.ID.struct_types)];
                    log("Module offset: {}. Unresolved index: {}\n", .{struct_type_module_offset, unresolved_type.get_index()});

                    const resolved_index = struct_i;
                    const resolved_struct_type = Type.Struct.new(resolved_index, module_index);

                    var target_struct_type = &analyzer.struct_types.items[resolved_index];
                    for (target_struct_type.types) |*field_type|
                    {
                        field_type.* = analyze_type(analyzer, field_type.*);
                    }
                
                    return resolved_struct_type;
                }
            }

            unreachable;
        },
        .pointer =>
        {
            const pointer_type_module_offset = analyzer.module_offsets[module_index].counters[@enumToInt(ModuleStats.ID.pointer_types)];
            const resolved_index = pointer_type_module_offset + unresolved_type.get_index();
            var resolved_pointer_type = resolve_type(unresolved_type, resolved_index);
            var pointer_type = &analyzer.pointer_types.items[resolved_index];
            pointer_type.type = analyze_type(analyzer, pointer_type.type);
            log("Pointer type base type: {}\n", .{pointer_type.type.get_ID()});

            log("{}\n", .{resolved_pointer_type.get_ID()});

            return resolved_pointer_type;
        },
        .array =>
        {
            const array_type_module_offset = analyzer.module_offsets[module_index].counters[@enumToInt(ModuleStats.ID.array_types)];
            const resolved_index = array_type_module_offset + unresolved_type.get_index();
            var resolved_array_type = resolve_type(unresolved_type, resolved_index);

            if (!analyzer.is_array_type_resolved(resolved_index))
            {
                var array_type = &analyzer.array_types.items[resolved_index];
                log("Array type resolved index: {}\n", .{resolved_index});
                array_type.type = analyze_type(analyzer, array_type.type);
                log("Array element type: {}\n", .{array_type.type});
                // resolve length
                {
                    var array_length_expression = Entity { .value = array_type.length_expression };
                    const array_length_expression_id = array_length_expression.get_array_id(.scope);
                    const array_length = switch (array_length_expression_id)
                    {
                        .integer_literals => blk:
                        {
                            resolve_entity_index(analyzer, .integer_literals, &array_length_expression, module_index);
                            const integer_literal = analyzer.integer_literals.items[array_length_expression.get_index()];
                            break :blk integer_literal.value;
                        },
                        else => panic("NI: {}\n", .{array_length_expression_id}),
                    };

                    array_type.length_expression = array_length;
                }

                log("Resolved array type: {}\n", .{array_type});

                analyzer.set_array_type_resolved(resolved_index);
            }

            return resolved_array_type;
        },
        .structure =>
        {
            const struct_type_module_offset = analyzer.module_offsets[module_index].counters[@enumToInt(ModuleStats.ID.struct_types)];
            const resolved_index = struct_type_module_offset + unresolved_type.get_index();
            var resolved_struct_type = resolve_type(unresolved_type, resolved_index);

            if (!analyzer.is_struct_type_resolved(resolved_index))
            {
                var struct_type = &analyzer.struct_types.items[resolved_index];
                log("struct type resolved index: {}\n", .{resolved_index});

                log("Alignment can't be computed here\n", .{});
                //var max_field_size: u32 = 0;
                //for (struct_type.types) |*field_type|
                //{
                    //field_type.* = analyze_type(analyzer, field_type.*);
                    //const field_size = field_type.get_size();
                    //if (field_size > max_field_size) max_field_size = field_size;
                //}
                //struct_type.alignment = max_field_size;

                log("Resolved struct type: {}\n", .{struct_type});

                analyzer.set_struct_type_resolved(resolved_index);
            }

            return resolved_struct_type;
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
        array_literals,
        struct_literals,

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

pub fn resolve_entity_index(analyzer: *Analyzer, comptime module_stats_id: ModuleStats.ID, entity: *Entity, module_index: u64) void
{
    const item_range = get_module_item_slice_range(module_stats_id, analyzer, module_index);
    const real_index = @intCast(u32, item_range.start) + entity.get_index();
    entity.set_index(real_index);
}

// @TODO: make this more robust
pub fn resolve_identifier_expression(analyzer: *Analyzer, current_function: *Parser.Function.Internal, expression: *Entity, scope_index: u32, name: []const u8) Type
{
    var current_scope_index = scope_index;

    for (current_function.declaration.argument_names) |argument_name, argument_i|
    {
        if (std.mem.eql(u8, argument_name, name))
        {
            const argument_id = Entity.new(argument_i, Entity.ScopeID.arguments, 0);
            expression.* = argument_id;
            const argument_type = current_function.declaration.type.argument_types[argument_i];
            return argument_type;
        }
    }
    _ = analyzer;
    //for (analyzer.functions.items) |function, function_i|
    //{
        //if (std.mem.eql(u8, function.declaration.name, name))
        //{
            //expression.* = Entity.new(function_i, Entity.GlobalID.resolved_internal_functions, 0);
            //return function.declaration.type.return_type;
        //}
    //}

    //for (analyzer.external_functions.items) |function, function_i|
    //{
        //if (std.mem.eql(u8, function.declaration.name, name))
        //{
            //expression.* = Entity.new(function_i, Entity.GlobalID.resolved_external_functions, 0);
            //return function.declaration.type.return_type;
        //}
    //}

    var scope_tree_explored = false;
    while (!scope_tree_explored)
    {
        var scope = &current_function.scopes[current_scope_index];

        for (scope.variable_declarations) |variable_declaration, variable_declaration_i|
        {
            if (std.mem.eql(u8, variable_declaration.name, name))
            {
                expression.* = Entity.new(variable_declaration_i, Entity.ScopeID.variable_declarations, current_scope_index);

                return variable_declaration.type;
            }
        }

        // @TODO: loop over other node types
        scope_tree_explored = scope_index == 0;
        if (!scope_tree_explored)
        {
            current_scope_index = scope.parent.scope;
        }
    }

    report_error("Identifier expression \"{s}\" not found\n", .{name});
}

fn analyze_integer_literal(analyzer: *Analyzer, entity: *Entity, expected_type: ?Type, module_index: u64) void
{
    assert(expected_type != null);
    const type_to_typecheck_against = expected_type.?;
    if (type_to_typecheck_against.get_ID() != .integer) report_error("Expected return type: {s}\n", .{@tagName(type_to_typecheck_against.get_ID())});

    resolve_entity_index(analyzer, .integer_literals, entity, module_index);
}

fn analyze_array_literal(analyzer: *Analyzer, expression: *Entity, module_index: u64, expected_type: ?Type) Type
{
    resolve_entity_index(analyzer, .array_literals, expression, module_index);
    const array_literal = &analyzer.array_literals.items[expression.get_index()];

    assert(expected_type != null);
    const type_to_typecheck_against = expected_type.?;
    assert(type_to_typecheck_against.get_ID() == .array);
    const array_type = &analyzer.array_types.items[type_to_typecheck_against.get_index()];
    if (array_type.length_expression != array_literal.elements.len) report_error("Array length differ. Expected: {}. Have: {}\n", .{array_type.length_expression, array_literal.elements.len});
    log("Array literal: {}. Element count: {}\n", .{array_literal, array_literal.elements.len});
    // @TODO: do further typechecking
    array_literal.type = type_to_typecheck_against;
    return type_to_typecheck_against;
}

fn analyze_array_subscript_expression(analyzer: *Analyzer, function: *Parser.Function.Internal, array_subscript_expression: *Parser.ArraySubscriptExpression, module_index: u64, expected_type: ?Type) Type
{
    const array_type_ref = analyze_expression_typed(analyzer, function, &array_subscript_expression.expression, module_index, null);
    assert(array_type_ref.get_ID() == .array);
    const array_type_index = array_type_ref.get_index();
    assert(analyzer.is_array_type_resolved(array_type_index));
    const array_type = &analyzer.array_types.items[array_type_index];


    const expected_type_hint = Type.Integer.new(64, .unsigned);
    const index_expression_type = analyze_expression_typed(analyzer, function, &array_subscript_expression.index, module_index, expected_type_hint);
    assert(index_expression_type.get_ID() == .integer);

    const actual = array_type.type;
    if (expected_type) |type_to_typecheck_against|
    {
        const expected = type_to_typecheck_against;
        log("Expected array subscript type: {}. Have: {}\n", .{expected.get_ID(), actual.get_ID()});
        if (expected.get_ID() != actual.get_ID())
        {
            report_error("Type mismatch\n", .{});
        }
    }

    return actual;
}

pub fn analyze_expression_typed(analyzer: *Analyzer, function: *Parser.Function.Internal, expression: *Entity, module_index: u64, expected_type: ?Type) Type
{
    const expression_level = expression.get_level();
    const expression_index = expression.get_index();
    const scope_index = expression.get_array_index();

    const expression_type = blk:
    {
        switch (expression_level)
        {
            .scope =>
            {
                const array_id = expression.get_array_id(.scope);

                switch (array_id)
                {
                    .integer_literals =>
                    {
                        analyze_integer_literal(analyzer, expression, expected_type, module_index);
                        if (expected_type) |type_to_typecheck_against|
                        {
                            const type_id = type_to_typecheck_against.get_ID();
                            if (type_id != .integer)
                            {
                                report_error("Expected: {}\n", .{type_id});
                            }

                            break :blk type_to_typecheck_against;
                        }
                        else unreachable;
                    },
                    .array_literals =>
                    {
                        break :blk analyze_array_literal(analyzer, expression, module_index, expected_type);
                    },
                    // @TODO: this is mostly a reference to a declaration, not a declaration in itself
                    .variable_declarations =>
                    {
                        var variable_declaration = &function.scopes[scope_index].variable_declarations[expression_index];
                        variable_declaration.type = analyze_type(analyzer, variable_declaration.type);
                        if (expected_type) |type_to_typecheck_against|
                        {
                            if (variable_declaration.type.value != type_to_typecheck_against.value)
                            {
                                report_error("Types don't match\n", .{});
                            }
                        }

                        break :blk variable_declaration.type;
                    },
                    .identifier_expressions =>
                    {
                        const identifier = function.scopes[scope_index].identifier_expressions[expression_index];
                        const expression_type = resolve_identifier_expression(analyzer, function, expression, scope_index, identifier);
                        break :blk expression_type;
                    },
                    .arithmetic_expressions =>
                    {
                        var scope_arithmetic_expressions = function.scopes[scope_index].arithmetic_expressions;
                        log("Scope #{} arithmetic expression count: {}\n", .{scope_index, scope_arithmetic_expressions.len});

                        var arithmetic_expression = &scope_arithmetic_expressions[expression_index];
                        break :blk analyze_arithmetic_expression(analyzer, function, arithmetic_expression, module_index);
                    },
                    .invoke_expressions =>
                    {
                        var expression_scope = &function.scopes[scope_index];
                        var invoke_expressions = expression_scope.invoke_expressions;
                        var invoke_expression = &invoke_expressions[expression_index];
                        break :blk analyze_invoke_expression(analyzer, function, expression_scope, invoke_expression, module_index);
                    },
                    .address_of_expressions =>
                    {
                        var expression_scope = &function.scopes[scope_index];
                        var address_of_expressions = expression_scope.address_of_expressions;
                        var address_of_expression = &address_of_expressions[expression_index];
                        break :blk analyze_address_of_expression(analyzer, function, module_index, address_of_expression);
                    },
                    .dereference_expressions =>
                    {
                        var expression_scope = &function.scopes[scope_index];
                        var dereference_expressions = expression_scope.dereference_expressions;
                        var dereference_expression = &dereference_expressions[expression_index];
                        break :blk analyze_dereference_expression(analyzer, function, module_index, dereference_expression);
                    },
                    .array_subscript_expressions =>
                    {
                        var array_subscript_expression = &function.scopes[scope_index].array_subscript_expressions[expression_index];
                        break :blk analyze_array_subscript_expression(analyzer, function, array_subscript_expression, module_index, expected_type);
                    },
                    .field_access_expressions =>
                    {
                        var field_access_expression = &function.scopes[scope_index].field_access_expressions[expression_index];
                        break :blk analyze_field_access_expression(analyzer, function, &function.scopes[scope_index], field_access_expression, module_index);
                    },
                    .struct_literals =>
                    {
                        const struct_type_ref = expected_type orelse unreachable;
                        assert(struct_type_ref.get_ID() == .structure);
                        assert(struct_type_ref.is_resolved());
                        assert(analyzer.is_struct_type_resolved(struct_type_ref.get_index()));

                        const resolved_index = analyzer.module_offsets[module_index].counters[@enumToInt(ModuleStats.ID.struct_literals)] + expression_index;
                        log("Resolved struct literal index: {}\n", .{resolved_index});
                        var struct_literal = &analyzer.struct_literals.items[resolved_index];
                        struct_literal.type = struct_type_ref;
                        const struct_type = &analyzer.struct_types.items[struct_type_ref.get_index()];

                        lit_field: for (struct_literal.fields.names) |field_name, field_literal_i|
                        {
                            for (struct_type.names) |field_name_candidate, field_index|
                            {
                                if (std.mem.eql(u8, field_name_candidate, field_name))
                                {
                                    _ = analyze_expression_typed(analyzer, function, &struct_literal.fields.initialization_expressions[field_literal_i], module_index, struct_type.types[field_index]);
                                    continue :lit_field;
                                }
                            }

                            report_error("No field {s} in struct type {s}\n", .{field_name, struct_type.name});
                        }

                        break :blk struct_type_ref;
                    },
                    else => panic("NI: {}\n", .{array_id}),
                }
            },
            else => panic("NI: {}\n", .{expression_level}),
        }
    };

    log("{}\n", .{expression_type.get_ID()});
    return expression_type;
}

pub fn analyze_address_of_expression(analyzer: *Analyzer, function: *Parser.Function.Internal, module_index: u64, address_of_expression: *Parser.UnaryExpression) Type
{
    const pointer_type_index = analyzer.pointer_types.items.len;
    const pointer_base_type = analyze_expression_typed(analyzer, function, &address_of_expression.reference, module_index, null);

    analyzer.pointer_types.append(.{ .type = pointer_base_type }) catch unreachable;

    const pointer_type = Type.Pointer.new(pointer_type_index, 0);

    return pointer_type;
}

pub fn analyze_dereference_expression(analyzer: *Analyzer, function: *Parser.Function.Internal, module_index: u64, dereference_expression: *Parser.UnaryExpression) Type
{
    const pointer_type_ref = analyze_expression_typed(analyzer, function, &dereference_expression.reference, module_index, null);
    assert(pointer_type_ref.get_ID() == .pointer);
    assert(pointer_type_ref.is_resolved());
    const pointer_type_index = pointer_type_ref.get_index();
    const pointer_type = analyzer.pointer_types.items[pointer_type_index];
    const base_type = pointer_type.type;

    return base_type;
}

pub fn analyze_binary_expression(analyzer: *Analyzer, function: *Parser.Function.Internal, left: *Entity, right: *Entity, module_index: u64) Type
{
    log("Left: {}. Right: {}\n", .{left.get_array_id(.scope), right.get_array_id(.scope)});
    const left_type = analyze_expression_typed(analyzer, function, left, module_index, null);
    const right_type = analyze_expression_typed(analyzer, function, right, module_index, left_type);

    if (left_type.value != right_type.value)
    {
        if (left_type.get_ID() == .integer and right_type.get_ID() == .integer)
        {
            const left_bits = Type.Integer.get_bit_count(left_type);
            const right_bits = Type.Integer.get_bit_count(right_type);

            if (left_bits == right_bits)
            {
                const left_signedness = Type.Integer.get_signedness(left_type);
                const right_signedness = Type.Integer.get_signedness(right_type);

                if (left_signedness == right_signedness)
                {
                    // no other aspect here to differ!
                    unreachable;
                }
                else
                {
                    // @TODO: correct this, we are bypassing signedness check
                    if (false) 
                        report_error("Integer of different signedness. Left: {}. Right: {}\n", .{left_signedness, right_signedness});
                }
            }
            else
            {
                report_error("Integer of different bit count. Left: {}. Right: {}\n", .{left_bits, right_bits});
            }
        }
        // @TODO: exhaustive typechecking
        else if (left_type.get_ID() == .pointer and right_type.get_ID() == .pointer)
        {
            return left_type;
        }
        else
        {
            report_error("Type mismatch. Left: {}. Right: {}\n", .{left_type.get_ID(), right_type.get_ID()}); 
        }
    }

    return left_type;
}

pub fn analyze_comparison(analyzer: *Analyzer, function: *Parser.Function.Internal, comparison: *Parser.Comparison, module_index: u64) Type
{
    _ = analyze_binary_expression(analyzer, function, &comparison.left, &comparison.right, module_index);
    return Type.Boolean;
}

pub fn analyze_arithmetic_expression(analyzer: *Analyzer, function: *Parser.Function.Internal, arithmetic_expression: *Parser.ArithmeticExpression, module_index: u64) Type
{
    return analyze_binary_expression(analyzer, function, &arithmetic_expression.left, &arithmetic_expression.right, module_index);
}

pub fn analyze_assignment_expression(analyzer: *Analyzer, function: *Parser.Function.Internal, assignment: *Parser.Assignment, module_index: u64) void
{
    log("Analyzing assignment expression\n", .{});
    _ = analyze_binary_expression(analyzer, function, &assignment.left, &assignment.right, module_index);
}
fn analyze_compound_assignment(analyzer: *Analyzer, function: *Parser.Function.Internal, compound_assignment: *Parser.CompoundAssignment, module_index: u64) void
{
    _ = analyze_binary_expression(analyzer, function, &compound_assignment.left, &compound_assignment.right, module_index);
}

fn analyze_field_access_expression(analyzer: *Analyzer, current_function: *Parser.Function.Internal, scope: *Parser.Scope, field_access_expression: *Parser.FieldAccessExpression, module_index: u64) Type
{
    _ = module_index;
    _ = current_function;

    const left = field_access_expression.left_expression;
    const left_level = left.get_level();
    assert(left_level == .scope);
    const left_array_id = left.get_array_id(.scope);
    assert(left_array_id == .identifier_expressions);
    const left_index = left.get_index();
    const left_name = scope.identifier_expressions[left_index];

    const field = field_access_expression.field_expression;
    const field_level = field.get_level();
    assert(field_level == .scope);
    const field_array_id = field.get_array_id(.scope);
    assert(field_array_id == .identifier_expressions);
    const field_index = field.get_index();
    const field_name = scope.identifier_expressions[field_index];

    const imported_module_range = get_module_item_slice_range(.imported_modules, analyzer, module_index);
    const imported_modules = analyzer.imported_modules.items[imported_module_range.start..imported_module_range.end];

    log("Looking for left side of field access expression: {s}\n", .{left_name});

    var left_type: Type = undefined;
    const left_part = blk:
    {
        for (current_function.declaration.argument_names) |argument_name, argument_i|
        {
            log("Comparing with argument name: {s}\n", .{argument_name});
            if (std.mem.eql(u8, argument_name, left_name)) 
            {
                log("argument #{}\n", .{argument_i});
                left_type = current_function.declaration.type.argument_types[argument_i];
                break :blk Entity.new(argument_i, Entity.ScopeID.arguments, module_index);
            }
        }

        for (scope.variable_declarations) |*variable_declaration|
        {
            log("Comparing with variable name: {s}\n", .{variable_declaration.name});
            if (std.mem.eql(u8, variable_declaration.name, left_name)) 
            {
                panic("Found\n", .{});
            }
        }

        if (true) unreachable;

        for (imported_modules) |imported_module|
        {
            assert(imported_module.alias != null);
            if (!std.mem.eql(u8, imported_module.alias.?, left_name))
            {
                continue;
            }

            const imported_module_index = imported_module.module.get_index();

            const internal_functions_range = get_module_item_slice_range(.internal_functions, analyzer, imported_module_index);
            const internal_functions = analyzer.functions.items[internal_functions_range.start..internal_functions_range.end];

            for (internal_functions) |function, function_i|
            {
                if (!std.mem.eql(u8, function.declaration.name, field_name))
                {
                    continue;
                }

                break :blk Entity.new(function_i + internal_functions_range.start, Entity.GlobalID.resolved_internal_functions, 0);
            }

            const external_functions_range = get_module_item_slice_range(.external_functions, analyzer, imported_module_index);
            const external_functions = analyzer.external_functions.items[external_functions_range.start..external_functions_range.end];

            // @TODO: is this faster than double for loop [library_i, symbol_i]?
            for (external_functions) |function, function_i|
            {
                if (!std.mem.eql(u8, function.declaration.name, field_name))
                {
                    continue;
                }

                break :blk Entity.new(function_i + external_functions_range.start, Entity.GlobalID.resolved_external_functions, 0);
            }
        }

        unreachable;
        // not found
    };
    _ = left_part;

    var right_type: Type = undefined;
    _ = right_type;

    const right_part = blk:
    {
        switch (left_type.get_ID())
        {
            // If we are accessing a pointer field, we have to get pointer base type and then get the field type from the child type
            .pointer =>
            {
                const pointer_base_type = Type.Pointer.get_base_type(left_type, analyzer.pointer_types.items);
                log("pointer base type id: {}\n", .{pointer_base_type.get_ID()});
                assert(pointer_base_type.get_ID() == .structure);
                const struct_type = analyzer.struct_types.items[pointer_base_type.get_index()];

                for (struct_type.names) |field_name_candidate, field_i|
                {
                    _ = field_i;

                    if (std.mem.eql(u8, field_name_candidate, field_name))
                    {
                        right_type = struct_type.types[field_i];
                        break :blk Entity.new(field_i, Entity.ScopeID.field, module_index);
                    }
                }

                unreachable;
            },
            else => panic("Ni: {}\n", .{left_type.get_ID()}),
        }
    };

    field_access_expression.type = right_type;
    field_access_expression.left_expression = left_part;
    field_access_expression.field_expression = right_part;

    return right_type;
}

fn analyze_invoke_expression(analyzer: *Analyzer, current_function: *Parser.Function.Internal, scope: *Parser.Scope, invoke_expression: *Parser.InvokeExpression, module_index: u64) Type
{
    const expression_to_invoke = invoke_expression.expression;
    const scope_index = expression_to_invoke.get_array_index();
    const expression_to_invoke_index = expression_to_invoke.get_index();
    const expression_to_invoke_level = expression_to_invoke.get_level();
    assert(expression_to_invoke_level == .scope);
    const expression_to_invoke_id = expression_to_invoke.get_array_id(.scope);

    var function_type: Type.Function = undefined;

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

                function_type = function.declaration.type;
                break :blk Entity.new(i, Entity.GlobalID.resolved_internal_functions, 0);
            }

            unreachable;
        }
        else if (expression_to_invoke_id == .field_access_expressions)
        {
            const field_expression = scope.field_access_expressions[expression_to_invoke_index];
            const left = field_expression.left_expression;
            const left_level = left.get_level();
            assert(left_level == .scope);
            const left_array_id = left.get_array_id(.scope);
            assert(left_array_id == .identifier_expressions);
            const left_index = left.get_index();
            const left_name = scope.identifier_expressions[left_index];

            const field = field_expression.field_expression;
            const field_level = field.get_level();
            assert(field_level == .scope);
            const field_array_id = field.get_array_id(.scope);
            assert(field_array_id == .identifier_expressions);
            const field_index = field.get_index();
            const field_name = scope.identifier_expressions[field_index];

            const imported_module_range = get_module_item_slice_range(.imported_modules, analyzer, module_index);
            const imported_modules = analyzer.imported_modules.items[imported_module_range.start..imported_module_range.end];

            for (imported_modules) |imported_module|
            {
                assert(imported_module.alias != null);
                if (!std.mem.eql(u8, imported_module.alias.?, left_name))
                {
                    continue;
                }

                const imported_module_index = imported_module.module.get_index();

                const internal_functions_range = get_module_item_slice_range(.internal_functions, analyzer, imported_module_index);
                const internal_functions = analyzer.functions.items[internal_functions_range.start..internal_functions_range.end];

                for (internal_functions) |function, function_i|
                {
                    if (!std.mem.eql(u8, function.declaration.name, field_name))
                    {
                        continue;
                    }

                    function_type = function.declaration.type;
                    break :blk Entity.new(function_i + internal_functions_range.start, Entity.GlobalID.resolved_internal_functions, 0);
                }

                const external_functions_range = get_module_item_slice_range(.external_functions, analyzer, imported_module_index);
                const external_functions = analyzer.external_functions.items[external_functions_range.start..external_functions_range.end];

                // @TODO: is this faster than double for loop [library_i, symbol_i]?
                for (external_functions) |function, function_i|
                {
                    if (!std.mem.eql(u8, function.declaration.name, field_name))
                    {
                        continue;
                    }

                    function_type = function.declaration.type;
                    break :blk Entity.new(function_i + external_functions_range.start, Entity.GlobalID.resolved_external_functions, 0);
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
        const argument_types = function_type.argument_types;

        for (invoke_expression.arguments) |*arg, arg_i|
        {
            const arg_level = arg.get_level();
            assert(arg_level == .scope);
            const array_id = arg.get_array_id(.scope);
            const argument_type = argument_types[arg_i];

            switch (array_id)
            {
                .integer_literals =>
                {
                    analyze_integer_literal(analyzer, arg, argument_type, module_index);
                },
                .identifier_expressions =>
                {
                    const identifier = scope.identifier_expressions[arg.get_index()];
                    const expression_type = resolve_identifier_expression(analyzer, current_function, arg, scope_index, identifier);
                    if (argument_type.value != expression_type.value)
                    {
                        report_error("Type mismatch\n", .{});
                    }
                },
                .address_of_expressions =>
                {
                    var address_of_expression = &scope.address_of_expressions[arg.get_index()];
                    const expression_type = analyze_address_of_expression(analyzer, current_function, module_index, address_of_expression);
                    if (argument_type.value != expression_type.value)
                    {
                        if (argument_type.get_ID() != expression_type.get_ID())
                        {
                            report_error("Type mismatch. Expected argument type: {}. Actual type: {}\n", .{argument_type.get_ID(), expression_type.get_ID()});
                        }
                    }
                },
                else => panic("Ni: {}\n", .{array_id}),
            }
        }
    }

    return function_type.return_type;
}


pub fn analyze_scope(analyzer: *Analyzer, scope: *Parser.Scope, current_function: *Parser.Function.Internal, module_index: u64) void
{
    const scope_index = @intCast(u32, (@ptrToInt(scope) - @ptrToInt(current_function.scopes.ptr)) / @sizeOf(Parser.Scope));
    log("Scope index: {}\n", .{scope_index});
    const statement_count = scope.statements.len;
    log("Statement count: {}\n", .{statement_count});
    log("Comparison count: {}\n", .{scope.comparisons.len});

    for (scope.statements) |*statement, statement_i|
    {
        const statement_index = statement.get_index();
        const statement_level = statement.get_level();
        assert(statement_level == .scope);
        const statement_id = statement.get_array_id(.scope);
        log("Analyzing statement #{}: {}\n", .{statement_i, statement_id});

        switch (statement_id)
        {
            .invoke_expressions =>
            {
                const invoke_expression = &scope.invoke_expressions[statement_index];
                _ = analyze_invoke_expression(analyzer, current_function, scope, invoke_expression, module_index);
            },
            .return_expressions =>
            {
                const return_expression = &scope.return_expressions[statement_index];
                const return_type = current_function.declaration.type.return_type;
                
                if (return_expression.expression) |expression_to_return|
                {
                    const ret_expr_lvl = expression_to_return.get_level();
                    assert(ret_expr_lvl == .scope);
                    const ret_expr_array_id = expression_to_return.get_array_id(.scope);

                    switch (ret_expr_array_id)
                    {
                        .integer_literals =>
                        {
                            analyze_integer_literal(analyzer, &return_expression.expression.?, return_type, module_index);
                        },
                        .identifier_expressions =>
                        {
                            const identifier = scope.identifier_expressions[expression_to_return.get_index()];
                            const expression_type = resolve_identifier_expression(analyzer, current_function, &return_expression.expression.?, scope_index, identifier);

                            log("Expected: {}. Got: {}\n", .{current_function.declaration.type.return_type.get_ID(), expression_type.get_ID()});
                            if (expression_type.value != current_function.declaration.type.return_type.value)
                            {
                                log("Expected: {}. Got: {}\n", .{current_function.declaration.type.return_type.get_ID(), expression_type.get_ID()});
                                if (current_function.declaration.type.return_type.get_ID() != expression_type.get_ID())
                                {
                                    report_error("Type mismatch\n", .{});
                                }
                            }
                        },
                        .arithmetic_expressions =>
                        {
                            const arithmetic_expression = &scope.arithmetic_expressions[expression_to_return.get_index()];
                            const expression_type = analyze_arithmetic_expression(analyzer, current_function, arithmetic_expression, module_index);
                            _ = expression_type;
                        },
                        .dereference_expressions =>
                        {
                            const dereference_expression = &scope.dereference_expressions[expression_to_return.get_index()];
                            const expression_type = analyze_dereference_expression(analyzer, current_function, module_index, dereference_expression);
                            _ = expression_type;
                        },
                        .invoke_expressions =>
                        {
                            var invoke_expression = &scope.invoke_expressions[expression_to_return.get_index()];
                            const invoke_expression_type = analyze_invoke_expression(analyzer, current_function, scope, invoke_expression, module_index);
                            _ = invoke_expression_type;
                        },
                        .array_subscript_expressions =>
                        {
                            var array_subscript_expression = &scope.array_subscript_expressions[expression_to_return.get_index()];
                            _ = analyze_array_subscript_expression(analyzer, current_function, array_subscript_expression, module_index, null);
                        },
                        else => panic("NI: {}\n", .{ret_expr_array_id}),
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
                variable_declaration.type = analyze_type(analyzer, variable_declaration.type);
            },
            .assignments =>
            {
                var assignment = &scope.assignments[statement_index];
                analyze_assignment_expression(analyzer, current_function, assignment, module_index);
            },
            .compound_assignments =>
            {
                var compound_assignment = &scope.compound_assignments[statement_index];
                analyze_compound_assignment(analyzer, current_function, compound_assignment, module_index);
            },
            .loops =>
            {
                var loop = &scope.loops[statement_index];
                var prefix_scope = &current_function.scopes[loop.prefix_scope_index];
                analyze_scope(analyzer, prefix_scope, current_function, module_index);
                var body_scope = &current_function.scopes[loop.body_scope_index];
                analyze_scope(analyzer, body_scope, current_function, module_index);
                var postfix_scope = &current_function.scopes[loop.postfix_scope_index];
                analyze_scope(analyzer, postfix_scope, current_function, module_index);
            },
            .branches =>
            {
                var branch = &scope.branches[statement_index];

                // @TODO: should we do this here?
                {
                    assert(branch.condition.get_level() == .scope);
                    const array_id = branch.condition.get_array_id(.scope);
                    if (array_id != .comparisons)
                    {
                        report_error("Expected a comparison as the branch condition\n", .{});
                    }
                    const branch_comparison_index = branch.condition.get_index();
                    const branch_comparison_array_index = branch.condition.get_array_index();
                    log("Branch comparison index: {}\n", .{branch_comparison_index});
                    log("Branch comparison array index: {}\n", .{branch_comparison_array_index});
                    var branch_comparison_scope = &current_function.scopes[branch_comparison_array_index];
                    const comparison_count = branch_comparison_scope.comparisons.len;
                    log("Comparison count: {}\n", .{comparison_count});
                    var branch_comparison = &branch_comparison_scope.comparisons[branch_comparison_index];
                    _ = analyze_comparison(analyzer, current_function, branch_comparison, module_index);
                }

                var if_scope = &current_function.scopes[branch.if_scope];
                analyze_scope(analyzer, if_scope, current_function, module_index);
                if (branch.else_scope) |else_scope_index|
                {
                    var else_scope = &current_function.scopes[else_scope_index];
                    analyze_scope(analyzer, else_scope, current_function, module_index);
                }
            },
            .comparisons =>
            {
                var comparison = &scope.comparisons[statement_index];
                const comparison_type = analyze_comparison(analyzer, current_function, comparison, module_index);
                assert(comparison_type.value == Type.Boolean.value);
            },
            .break_expressions =>
            {
                var break_expression = &scope.break_expressions[statement_index];
                const loop_scope_index = break_expression.loop_to_break.get_array_index();
                const loop_scope = &current_function.scopes[loop_scope_index];
                const loop_index = break_expression.loop_to_break.get_index();
                const loop_to_break = loop_scope.loops[loop_index];

                // @TODO: do something with this
                _ = loop_to_break;
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
    array_literals: ArrayList(Parser.ArrayLiteral),
    struct_literals: ArrayList(Parser.StructLiteral),

    unresolved_types: ArrayList([]const u8),
    pointer_types: ArrayList(Type.Pointer),
    function_types: ArrayList(Type.Function),
    slice_types: ArrayList(Type.Slice),
    array_types: ArrayList(Type.Array),
    array_type_resolution_bitsets: ArrayList([ResolutionBitsetSize]u1),
    struct_type_resolution_bitsets: ArrayList([ResolutionBitsetSize]u1),
    struct_types: ArrayList(Type.Struct),

    module_offsets: []ModuleStats,

    const ResolutionBitsetSize = 64;

    fn is_array_type_resolved(self: *Analyzer, resolved_array_type_index: u64) bool
    {
        const bitset_index = resolved_array_type_index / ResolutionBitsetSize;
        const array_type_index_in_bitset = resolved_array_type_index % ResolutionBitsetSize;

        return self.array_type_resolution_bitsets.items[bitset_index][array_type_index_in_bitset] == 1;
    }

    fn set_array_type_resolved(self: *Analyzer, resolved_array_type_index: u64) void
    {
        const bitset_index = resolved_array_type_index / ResolutionBitsetSize;
        const array_index_in_bitset = resolved_array_type_index % ResolutionBitsetSize;

        self.array_type_resolution_bitsets.items[bitset_index][array_index_in_bitset] = 1;
    }

    fn is_struct_type_resolved(self: *Analyzer, resolved_struct_type_index: u64) bool
    {
        const bitset_index = resolved_struct_type_index / ResolutionBitsetSize;
        const struct_type_index_in_bitset = resolved_struct_type_index % ResolutionBitsetSize;

        return self.struct_type_resolution_bitsets.items[bitset_index][struct_type_index_in_bitset] == 1;
    }

    fn set_struct_type_resolved(self: *Analyzer, resolved_struct_type_index: u64) void
    {
        const bitset_index = resolved_struct_type_index / ResolutionBitsetSize;
        const struct_index_in_bitset = resolved_struct_type_index % ResolutionBitsetSize;

        self.struct_type_resolution_bitsets.items[bitset_index][struct_index_in_bitset] = 1;
    }
};

pub const Result = struct
{
    functions: []Parser.Function.Internal,
    external: External,
    imported_modules: []Parser.ImportedModule,

    integer_literals: []Parser.IntegerLiteral,
    array_literals: []Parser.ArrayLiteral,
    struct_literals: []Parser.StructLiteral,

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
        module_offsets.items[i].counters[@enumToInt(ModuleStats.ID.array_literals)] = total.counters[@enumToInt(ModuleStats.ID.array_literals)];
        module_offsets.items[i].counters[@enumToInt(ModuleStats.ID.struct_literals)] = total.counters[@enumToInt(ModuleStats.ID.struct_literals)];

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
        total.counters[@enumToInt(ModuleStats.ID.array_literals)] += module.array_literals.len;
        total.counters[@enumToInt(ModuleStats.ID.struct_literals)] += module.struct_literals.len;

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
        .array_literals = ArrayList(Parser.ArrayLiteral).initCapacity(allocator, total.counters[@enumToInt(ModuleStats.ID.array_literals)]) catch unreachable,
        .struct_literals = ArrayList(Parser.StructLiteral).initCapacity(allocator, total.counters[@enumToInt(ModuleStats.ID.struct_literals)]) catch unreachable,

        .external_functions = ArrayList(Parser.Function.External).initCapacity(allocator, total.counters[@enumToInt(ModuleStats.ID.external_functions)]) catch unreachable,
        .external_library_names = ArrayList([]const u8).init(allocator),
        .external_symbol_names = ArrayList([]const u8).init(allocator),
        .external_libraries = ArrayList(External.Library).init(allocator),

        .unresolved_types = ArrayList([]const u8).initCapacity(allocator, total.counters[@enumToInt(ModuleStats.ID.unresolved_types)]) catch unreachable,
        .pointer_types = ArrayList(Type.Pointer).initCapacity(allocator, total.counters[@enumToInt(ModuleStats.ID.pointer_types)]) catch unreachable,
        .slice_types = ArrayList(Type.Slice).initCapacity(allocator, total.counters[@enumToInt(ModuleStats.ID.slice_types)]) catch unreachable,
        .function_types = ArrayList(Type.Function).initCapacity(allocator, total.counters[@enumToInt(ModuleStats.ID.function_types)]) catch unreachable,
        .array_types = ArrayList(Type.Array).initCapacity(allocator, total.counters[@enumToInt(ModuleStats.ID.array_types)]) catch unreachable,
        .array_type_resolution_bitsets = ArrayList([Analyzer.ResolutionBitsetSize]u1).initCapacity(allocator, (total.counters[@enumToInt(ModuleStats.ID.array_types)] / Analyzer.ResolutionBitsetSize) + @boolToInt(total.counters[@enumToInt(ModuleStats.ID.array_types)] % Analyzer.ResolutionBitsetSize != 0)) catch unreachable,
        .struct_types = ArrayList(Type.Struct).initCapacity(allocator, total.counters[@enumToInt(ModuleStats.ID.struct_types)]) catch unreachable,
        .struct_type_resolution_bitsets = ArrayList([Analyzer.ResolutionBitsetSize]u1).initCapacity(allocator, (total.counters[@enumToInt(ModuleStats.ID.struct_types)] / Analyzer.ResolutionBitsetSize) + @boolToInt(total.counters[@enumToInt(ModuleStats.ID.struct_types)] % Analyzer.ResolutionBitsetSize != 0)) catch unreachable,
        .module_offsets = module_offsets.items,
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
        analyzer.array_literals.appendSlice(module.array_literals) catch unreachable;
        analyzer.struct_literals.appendSlice(module.struct_literals) catch unreachable;
        analyzer.unresolved_types.appendSlice(module.unresolved_types) catch unreachable;
        analyzer.pointer_types.appendSlice(module.pointer_types) catch unreachable;
        analyzer.slice_types.appendSlice(module.slice_types) catch unreachable;
        analyzer.function_types.appendSlice(module.function_types) catch unreachable;
        analyzer.array_types.appendSlice(module.array_types) catch unreachable;
        analyzer.struct_types.appendSlice(module.struct_types) catch unreachable;
    }
    analyzer.array_type_resolution_bitsets.items.len = analyzer.array_types.items.len;
    std.mem.set([64]u1, analyzer.array_type_resolution_bitsets.items, std.mem.zeroes([64]u1));

    analyzer.struct_type_resolution_bitsets.items.len = analyzer.struct_types.items.len;
    std.mem.set([64]u1, analyzer.struct_type_resolution_bitsets.items, std.mem.zeroes([64]u1));

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
        function_type.return_type = analyze_type(&analyzer, function_type.return_type);
        for (function_type.argument_types) |*argument_type|
        {
            argument_type.* = analyze_type(&analyzer, argument_type.*);
        }
    }

    for (analyzer.external_functions.items) |*function, function_i|
    {
        log("External function #{}: {s}\n", .{function_i, function.declaration.name});
        function.declaration.type.return_type = analyze_type(&analyzer, function.declaration.type.return_type);

        for (function.declaration.type.argument_types) |*argument_type, argument_i|
        {
            argument_type.* = analyze_type(&analyzer, argument_type.*);
            log("Argument #{}: {}\n", .{argument_i, argument_type.get_ID()});
            assert(argument_type.get_ID() != .unresolved);
        }
    }

    for (analyzer.functions.items) |*function, function_i|
    {
        log("Function #{}: {s}\n", .{function_i, function.declaration.name});
        function.declaration.type.return_type = analyze_type(&analyzer, function.declaration.type.return_type);

        for (function.declaration.type.argument_types) |*argument_type, argument_i|
        {
            argument_type.* = analyze_type(&analyzer, argument_type.*);
            log("Argument #{}: {}\n", .{argument_i, argument_type.get_ID()});
            assert(argument_type.get_ID() != .unresolved);
        }
    }

    var module_index: u64 = 0;
    const module_count = analyzer.module_offsets.len;
    while (module_index < module_count) : (module_index += 1)
    {
        const function_range = get_module_item_slice_range(.internal_functions, &analyzer, module_index);
        for (analyzer.functions.items[function_range.start..function_range.end]) |*function|
        {
            log("\nAnalyzing {s}()...\n", .{function.declaration.name});
            const main_block = &function.scopes[0];
            analyze_scope(&analyzer, main_block, function, module_index);
        }
    }

    return Result
    {
        .functions = analyzer.functions.items,
        .imported_modules = analyzer.imported_modules.items,

        .integer_literals = analyzer.integer_literals.items,
        .array_literals = analyzer.array_literals.items,
        .struct_literals = analyzer.struct_literals.items,

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
