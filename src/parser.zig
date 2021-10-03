const std = @import("std");
const assert = std.debug.assert;
const panic = std.debug.panic;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Atomic = std.atomic.Atomic;

const Lexer = @import("lexer.zig");
const Operator = Lexer.Operator;
const KeywordID = Lexer.KeywordID;
const Token = Lexer.Token;
const LexerResult = Lexer.LexerResult;

const Compiler = @import("compiler.zig");

const Type = @import("type.zig");
const Entity = @import("entity.zig").Entity;

const IR = @import("ir.zig");

pub fn parser_error(comptime format: []const u8, args: anytype) noreturn
{
    panic(format, args);
}

pub fn log(comptime format: []const u8, arguments: anytype) void
{
    Compiler.log(.parser, "[PARSER] " ++ format, arguments);
}

pub const IntegerLiteral = struct
{
    value: u64,
    signed: bool,
    // padding
    //
    fn new(list: *ArrayList(IntegerLiteral), value: u64, signed: bool) Entity
    {
        const index = list.items.len;
        const id = Entity.new(index, Entity.ScopeID.integer_literals);
        list.append(.
            {
                .value = value,
                .signed = signed,
            }) catch unreachable;

        return id;
    }
};

const FieldAccessExpression = struct
{
    left_expression: Entity,
    field_expression: Entity,
    type: Type,
};

pub const VariableDeclaration = struct
{
    name: []const u8,
    type: Type,

    fn new(current_scope: *Scope.Builder, name: []const u8, var_type: Type) Entity
    {
        const var_decl_index = current_scope.variable_declarations.items.len;
        current_scope.variable_declarations.append(.
            {
                .name = name,
                .type = var_type,
            }) catch unreachable;

        const var_decl_id = Entity.new(var_decl_index, Entity.ScopeID.variable_declarations);
        current_scope.statements.append(var_decl_id) catch unreachable;

        return var_decl_id;
    }
};

pub const IdentifierExpression = []const u8;

fn new_identifier_expression(current_scope: *Scope.Builder, name: IdentifierExpression) Entity
{
    const index = current_scope.identifier_expressions.items.len;
    const identifier_expression = Entity.new(index, Entity.ScopeID.identifier_expressions);
    current_scope.identifier_expressions.append(name) catch unreachable;
    return identifier_expression;
}

pub const ReturnExpression = struct
{
    expression: ?Entity,
};

pub const Assignment = struct
{
    left: Entity,
    right: Entity,

    fn new(current_scope: *Scope.Builder, left: Entity, right: Entity) Entity
    {
        const index = current_scope.assignments.items.len;
        const assignment = Entity.new(index, Entity.ScopeID.assignments);
        current_scope.assignments.append(.{
            .left = left,
            .right = right,
        }) catch unreachable;

        // @TODO: think if assignments are always an statement
        current_scope.statements.append(assignment) catch unreachable;

        return assignment;
    }
};

pub const Operation = struct
{
    pub const ID = enum
    {
        add,
        sub,
    };
};

pub const ComposedAssignment = struct
{
    left: Entity,
    right: Entity,
    id: ID,

    const ID = enum(u8)
    {
        add,
        sub,
    };

    fn new(current_scope: *Scope.Builder, id: ID, left: Entity, right: Entity) Entity
    {
        const composed_assignment = ComposedAssignment
        {
            .left = left,
            .right = right,
            .id = id,
        };

        const index = current_scope.composed_assignments.items.len;
        current_scope.composed_assignments.append(composed_assignment) catch unreachable;
        const composed_assignment_id = Entity.new(index, Entity.ScopeID.composed_assignments);
        // @TODO: think if assignments are always an statement
        current_scope.statements.append(composed_assignment_id) catch unreachable;

        return composed_assignment_id;
    }
};

pub const Loop = struct
{
    prefix_scope_index: u32,
    body_scope_index: u32,
    postfix_scope_index: u32,
    exit_block: IR.Reference,
    continue_block: IR.Reference,
};

pub const Branch = struct
{
    condition: Entity,
    if_scope: u32,
    else_scope: ?u32,
    exit_block: IR.Reference,
};

pub const Comparison = struct
{
    left: Entity,
    right: Entity,
    id: ID,

    const ID = enum(u8)
    {
        equal,
        not_equal,
        less,
        greater,
        less_or_equal,
        greater_or_equal,
    };

    fn new(current_scope: *Scope.Builder, id: ID, left: Entity, right: Entity) Entity
    {
        const comparison = Comparison
        {
            .left = left,
            .right = right,
            .id = id,
        };

        const index = current_scope.comparisons.items.len;
        const comparison_id = Entity.new(index, Entity.ScopeID.comparisons);
        current_scope.comparisons.append(comparison) catch unreachable;
        log("Just appended a comparison here: {}\n", .{current_scope.comparisons.items.len});
        // @TODO: this may be just an expression and not an statement; remove

        return comparison_id;
    }
};

pub const ArithmeticExpression = struct
{
    left: Entity,
    right: Entity,
    id: ID,

    const ID = enum(u8)
    {
        add,
        sub,
        mul,
        div,
    };
};

pub const BreakExpression = struct
{
    loop_to_break: u32,
};

pub const Scope = struct
{
    statements: []Entity,
    variable_declarations: []VariableDeclaration,
    identifier_expressions: []IdentifierExpression,
    invoke_expressions: []InvokeExpression,
    field_access_expressions: []FieldAccessExpression,
    return_expressions: []ReturnExpression,
    assignments: []Assignment,
    comparisons: []Comparison,
    loops: []Loop,
    branches: []Branch,
    arithmetic_expressions: []ArithmeticExpression,
    break_expressions: []BreakExpression,
    parent: Parent,

    pub const Builder = struct
    {
        statements: ArrayList(Entity),
        variable_declarations: ArrayList(VariableDeclaration),
        identifier_expressions: ArrayList(IdentifierExpression),
        invoke_expressions: ArrayList(InvokeExpression),
        field_access_expressions: ArrayList(FieldAccessExpression),
        return_expressions: ArrayList(ReturnExpression),
        assignments: ArrayList(Assignment),
        comparisons: ArrayList(Comparison),
        composed_assignments: ArrayList(ComposedAssignment),
        loops: ArrayList(Loop),
        branches: ArrayList(Branch),
        arithmetic_expressions: ArrayList(ArithmeticExpression),
        break_expressions: ArrayList(BreakExpression),
        parent: Parent,
        last_loop_index: u32,

        fn new(allocator: *Allocator, builder: *Function.Builder, parent_expression: Entity, parent_scope: u32) u32
        {
            const last_loop_index = if (builder.scope_builders.items.len == 0) std.math.maxInt(u32) else builder.scope_builders.items[builder.current_scope].last_loop_index;

            builder.current_scope = @intCast(u32, builder.scope_builders.items.len);
            log("NEW SCOPE: {}\n", .{builder.current_scope});
            builder.scopes.append(undefined) catch unreachable;
            builder.scope_builders.append(Scope.Builder
                {
                    .statements = ArrayList(Entity).init(allocator),
                    .variable_declarations = ArrayList(VariableDeclaration).init(allocator),
                    .identifier_expressions = ArrayList(IdentifierExpression).init(allocator),
                    .invoke_expressions = ArrayList(InvokeExpression).init(allocator),
                    .field_access_expressions = ArrayList(FieldAccessExpression).init(allocator),
                    .return_expressions = ArrayList(ReturnExpression).init(allocator),
                    .assignments = ArrayList(Assignment).init(allocator),
                    .comparisons = ArrayList(Comparison).init(allocator),
                    .composed_assignments = ArrayList(ComposedAssignment).init(allocator),
                    .loops = ArrayList(Loop).init(allocator),
                    .branches = ArrayList(Branch).init(allocator),
                    .arithmetic_expressions = ArrayList(ArithmeticExpression).init(allocator),
                    .break_expressions = ArrayList(BreakExpression).init(allocator),
                    .parent = .{ .expression = parent_expression, .scope = parent_scope },
                    .last_loop_index = last_loop_index,
                }) catch unreachable;

            return builder.current_scope;
        }
    };

    const Parent = struct
    {
        expression: Entity,
        scope: u32,
    };
};

pub const Function = struct
{
    name: []const u8,
    argument_names: [][]const u8,
    type: Type.Function,

    pub const External = struct
    {
        declaration: Function,
        index: Index,

        pub const Index = struct
        {
            function: u16,
            library: u16,

            // @TODO: make this fast
            pub fn from_u32(index_int: u32) Index
            {
                return @ptrCast(* const Index, &index_int).*;
            }

            // @TODO: make this fast
            pub fn to_u32(self: *const Index) u32
            {
                return @ptrCast(*align (2) const u32, self).*;
            }
        };
    };

    pub const Internal = struct
    {
        declaration: Function,
        scopes: []Scope,
    };

    const Builder = struct
    {
        scope_builders: ArrayList(Scope.Builder),
        scopes: ArrayList(Scope),
        current_scope: u32,
    };
};

pub const Library = struct
{
    symbol_names: [][]const u8,

    pub const Builder = struct
    {
        symbol_names: ArrayList([]const u8),
    };
};

pub const TokenTypeMap = blk:
{
    var ttm: [TokenTypeCount]type = undefined;

    ttm[@enumToInt(Token.int_lit)] = Lexer.IntLiteral;
    ttm[@enumToInt(Token.char_lit)] = Lexer.CharLiteral;
    ttm[@enumToInt(Token.str_lit)] = Lexer.StringLiteral;
    ttm[@enumToInt(Token.identifier)] = Lexer.Identifier;
    ttm[@enumToInt(Token.keyword)] = Lexer.Keyword;
    ttm[@enumToInt(Token.sign)] = Lexer.Sign;
    ttm[@enumToInt(Token.operator)] = Lexer.Operator;

    break :blk ttm;
};


pub const TokenTypeCount = std.enums.values(Lexer.Token).len; 

pub const ModuleParser = struct
{
    const Self = @This();
    const CountersType = [TokenTypeCount]u32;

    lexer: TokenWalker,
    function_builder: Function.Builder,
    module_builder: Module.Builder,
    allocator: *Allocator,

    const TokenWalker = struct
    {
        next_index: u64,
        counters: CountersType,
        tokens: []Lexer.Token,
        int_literals: []Lexer.IntLiteral,
        char_literals: []Lexer.CharLiteral,
        string_literals: []Lexer.StringLiteral,
        identifiers: []Lexer.Identifier,
        keywords: []Lexer.Keyword,
        signs: []Lexer.Sign,
        operators: []Lexer.Operator,
    };

    fn get_and_consume_token(self: *Self, comptime token: Token) TokenTypeMap[@enumToInt(token)]
    {
        //log("Getting and consuming {}...\n", .{token});
        const token_to_consume = self.lexer.tokens[self.lexer.next_index];
        //log("Token to be consumed: {}\n", .{token_to_consume});
        assert(token_to_consume == token);
        const result = self.get_token(token);
        self.consume_token(token);

        return result;
    }

    fn get_token(self: *Self, comptime token: Token) callconv(.Inline) TokenTypeMap[@enumToInt(token)]
    {
        const index = self.lexer.counters[@enumToInt(token)];

        comptime switch (token)
        {
            .int_lit => return self.lexer.int_literals[index],
            .float_lit => return self.lexer.float_literals[index],
            .char_lit => return self.lexer.char_literals[index],
            .str_lit => return self.lexer.string_literals[index],
            .identifier => return self.lexer.identifiers[index],
            .keyword => return self.lexer.keywords[index],
            .sign => return self.lexer.signs[index],
            .operator => return self.lexer.operators[index],
        };
    }

    fn consume_token(self: *Self, comptime token: Token) callconv(.Inline) void
    {
        const token_value = self.get_token(token).value;
        switch (comptime token)
        {
            .str_lit, .identifier =>
            {
                log("Consuming {}: {s}\n", .{token, token_value});
            },
            .sign =>
            {
                log("Consuming {}: {c}\n", .{token, token_value});
            },
            else =>
            {
                log("Consuming {}: {}\n", .{token, token_value});
            }
        }

        self.lexer.counters[@enumToInt(token)] += 1;
        self.lexer.next_index += 1;
    }

    fn rectify(self: *Self, comptime token_to_rectify: Token) callconv(.Inline) void
    {
        //log("Rectifying {}...\n", .{token_to_rectify});
        self.lexer.counters[@enumToInt(token_to_rectify)] -= 1;
        self.lexer.next_index -= 1;
    }

    fn parse_expression_identifier(self: *Self) Entity
    {
        return self.parse_precedence_identifier(Precedence.Assignment);
    }

    fn parse_prefix_identifier(self: *Self) Entity
    {
        const identifier_name = self.get_and_consume_token(.identifier).value;
        const left_expression = new_identifier_expression(&self.function_builder.scope_builders.items[self.function_builder.current_scope], identifier_name);
        return left_expression;
    }

    fn parse_precedence_identifier(self: *Self, comptime precedence: Precedence) Entity
    {
        return self.parse_infix(precedence, self.parse_prefix_identifier());
    }

    fn parse_precedence(self: *Self, precedence: Precedence) Entity
    {
        // Parse prefix
        const prefix_token = self.lexer.tokens[self.lexer.next_index];
        const left_expression = blk:
        {
            switch (prefix_token)
            {
                .identifier => break :blk self.parse_prefix_identifier(),
                .int_lit =>
                {
                    // @TODO: implement signedness parsing
                    const signed = false;
                    const integer_value = self.get_and_consume_token(.int_lit).value;
                    break :blk IntegerLiteral.new(&self.module_builder.integer_literals, integer_value, signed);
                },
                .operator =>
                {
                    //const operator = token.value.operator;
                    //switch (operator)
                    //{
                    //// @Info: this is not array subscript, but array literal constant
                    //Operator.LeftBracket =>
                    //{
                    //const array_lit_expr = self.parse_array_literal(allocator, parser, parent_node);
                    //return array_lit_expr;
                    //},
                    //Operator.AddressOf,
                    //Operator.Dereference =>
                    //{
                    //const unary_expr = self.parse_unary_expression(allocator, parser, parent_node, operator);
                    //return unary_expr;
                    //},
                    //else => panic("ni: {}\n", .{operator}),
                    //}
                    unreachable;
                },
                .sign =>
                {
                    //const sign = token.value.sign;
                    //switch (sign)
                    //{
                    //'{' =>
                    //{
                    //const struct_lit_expr = self.parse_struct_literal(allocator, parser, parent_node);
                    //return struct_lit_expr;
                    //},
                    //else => panic("ni: {c}\n", .{sign}),
                    //}
                    unreachable;
                },
                else => panic("ni: {}\n", .{prefix_token}),
            }
        };

        return self.parse_infix(precedence, left_expression);
    }

    fn parse_expression(self: *Self) Entity
    {
        return self.parse_precedence(Precedence.Assignment);
    }

    fn parse_infix(self: *Self, precedence: Precedence, left_expr: Entity) Entity
    {
        var left_expression = left_expr;
        var has_less_precedence = true;

        while (has_less_precedence)
        {
            const token = self.lexer.tokens[self.lexer.next_index];
            const new_precedence = switch (token)
            {
                .sign => sign_block:
                {
                    const sign = self.get_token(.sign);
                    const sign_precedence = switch (sign.value)
                    {
                        '{', ';', ',', '}' => Precedence.None,
                        else => panic("Precedence not implemented for sign '{c}'\n", .{sign}),
                    };

                    break :sign_block sign_precedence;
                },
                .operator => operator_block:
                {
                    const operator = self.get_token(.operator).value;
                    const operator_precedence = switch (operator)
                    {
                        .RightParenthesis,
                        .RightBracket, => Precedence.None,

                        .Plus,
                        .Minus => Precedence.LightArithmetic,

                        .Multiplication,
                        .Division => Precedence.HeavyArithmetic,

                        .Equal,
                        .GreaterThan,
                        .LessThan => Precedence.Compare,

                        .Assignment => Precedence.Assignment,

                        .Declaration => Precedence.Declaration,

                        .LeftParenthesis,
                        .LeftBracket,
                        .Dot, => Precedence.Call,

                        else => panic("Precedence not implemented for {}\n", .{operator}),
                    };

                    break :operator_block operator_precedence;
                },
                else => panic("Precedence not implemented for token: {}\n", .{token}),
            };

            has_less_precedence = @enumToInt(precedence) <= @enumToInt(new_precedence);

            if (has_less_precedence)
            {
                assert(token == .operator);
                const operator = self.get_and_consume_token(.operator).value;
                left_expression = switch (operator)
                {
                    .Equal, .GreaterThan => self.parse_comparison(left_expr, operator, new_precedence),
                    .Assignment => self.parse_assignment(left_expr, new_precedence),
                    .Plus, .Minus, .Multiplication => self.parse_arithmetic_expression(left_expr, operator, new_precedence),
                    .LeftParenthesis => blk:
                    {
                        var arguments_left_to_parse = self.lexer.tokens[self.lexer.next_index] != .operator or self.get_token(.operator).value != .RightParenthesis;

                        var argument_list = ArrayList(Entity).init(self.allocator);

                        while (arguments_left_to_parse)
                        {
                            const argument_id = self.parse_expression();
                            argument_list.append(argument_id) catch unreachable;

                            arguments_left_to_parse = !(self.lexer.tokens[self.lexer.next_index] == .operator and self.get_token(.operator).value == .RightParenthesis);
                            if (arguments_left_to_parse and (self.lexer.tokens[self.lexer.next_index] != .sign or self.get_token(.sign).value != ','))
                            {
                                parser_error("Expected comma after argument in argument list\n", .{});
                            }
                        }

                        if (self.get_and_consume_token(.operator).value != .RightParenthesis)
                        {
                            parser_error("Expected right parenthesis to finish argument list\n", .{});
                        }

                        const invoke_expression_id = Entity.new(self.function_builder.scope_builders.items[self.function_builder.current_scope].invoke_expressions.items.len, Entity.ScopeID.invoke_expressions);
                        self.function_builder.scope_builders.items[self.function_builder.current_scope].invoke_expressions.append(.
                            {
                                .arguments = argument_list.items,
                                .expression = left_expression,
                            }) catch unreachable;

                        break :blk invoke_expression_id;
                    },
                    .Declaration => 
                    {
                        // break :blk self.parse_declaration(allocator, parser, parent_node, left_expr);
                        unreachable;
                    },
                    .LeftBracket =>
                    {
                        //break :blk self.parse_array_subscript(allocator, parser, parent_node, left_expr);
                        unreachable;
                    },
                    .Dot => blk:
                    {
                        const field_access_expression_id = Entity.new(self.function_builder.scope_builders.items[self.function_builder.current_scope].field_access_expressions.items.len, Entity.ScopeID.field_access_expressions);
                        self.function_builder.scope_builders.items[self.function_builder.current_scope].field_access_expressions.append(
                            .{
                                .left_expression = left_expression,
                                .field_expression = self.parse_precedence(comptime Precedence.Call.increment()),
                                .type = Type.unresolved_type,
                            }) catch unreachable;

                        break :blk field_access_expression_id;
                    },
                    else => panic("operator not implemented: {}\n", .{operator}),
                };
            }
        }

        return left_expression;
    }

    fn parse_arithmetic_expression(self: *Self, left_expression: Entity, operator: Operator.ID, precedence: Precedence) Entity
    {
        const arithmetic_id = switch (operator)
        {
            .Plus => ArithmeticExpression.ID.add,
            .Minus => ArithmeticExpression.ID.sub,
            .Multiplication => ArithmeticExpression.ID.mul,
            .Division => ArithmeticExpression.ID.div,
            else => panic("NI: {}\n", .{operator}),
        };

        const right_expression = self.parse_precedence(@intToEnum(Precedence, @enumToInt(precedence) + 1));
        const arithmetic_expression = ArithmeticExpression
        {
            .id = arithmetic_id,
            .left = left_expression,
            .right = right_expression,
        };

        var current_scope = self.function_builder.scope_builders.items[self.function_builder.current_scope];
        const arithmetic_expression_index = current_scope.arithmetic_expressions.items.len;
        current_scope.arithmetic_expressions.append(arithmetic_expression) catch unreachable;
        const arithmetic_expression_id = Entity.new(arithmetic_expression_index, Entity.ScopeID.arithmetic_expressions);

        return arithmetic_expression_id;
    }

    fn parse_comparison(self: *Self, left_expression: Entity, operator: Operator.ID, precedence: Precedence) Entity
    {
        const id = switch (operator)
        {
            .Equal => Comparison.ID.equal,
            .GreaterThan => Comparison.ID.greater,
            else => panic("NI: {}\n", .{operator}),
        };

        const right_expression = self.parse_precedence(@intToEnum(Precedence, @enumToInt(precedence) + 1));
        const expression = Comparison
        {
            .id = id,
            .left = left_expression,
            .right = right_expression,
        };

        var current_scope = self.function_builder.scope_builders.items[self.function_builder.current_scope];
        const expression_index = current_scope.comparisons.items.len;
        current_scope.comparisons.append(expression) catch unreachable;
        const expression_id = Entity.new(expression_index, Entity.ScopeID.comparisons);

        return expression_id;
    }

    fn parse_assignment(self: *Self, left_expression: Entity, precedence: Precedence) Entity
    {
        const right_expression = self.parse_precedence(@intToEnum(Precedence, @enumToInt(precedence) + 1));
        return Assignment.new(&self.function_builder.scope_builders.items[self.function_builder.current_scope], left_expression, right_expression);
    }

    fn parse_statement(self: *Self, parent_scope: u32) void
    {
        const next_token = self.lexer.tokens[self.lexer.next_index];
        log("Statement token: {}\n", .{next_token});

        var current_scope = &self.function_builder.scope_builders.items[parent_scope];

        switch (next_token)
        {
            .identifier =>
            {
                const identifier_name = self.get_token(.identifier).value;
                log("Identifier name:{s}\n", .{identifier_name});

                if (self.lexer.tokens[self.lexer.next_index + 1] == .operator and self.get_token(.operator).value == .Declaration)
                {
                    log("Parsing variable declaration...\n", .{});
                    self.consume_token(.identifier);
                    self.consume_token(.operator);

                    const var_decl_id = VariableDeclaration.new(current_scope, identifier_name, self.parse_type());
                    const var_next_token = self.lexer.tokens[self.lexer.next_index];

                    if (var_next_token == .operator)
                    {
                        if (self.get_token(.operator).value == Operator.ID.Assignment)
                        {
                            self.consume_token(.operator);

                            const var_init_expression = self.parse_expression();

                            const assignment_index = current_scope.assignments.items.len;
                            current_scope.assignments.append(.
                                {
                                    .left = var_decl_id,
                                    .right = var_init_expression,
                                }) catch unreachable;

                            const assignment_id = Entity.new(assignment_index, Entity.ScopeID.assignments);
                            current_scope.statements.append(assignment_id) catch unreachable;
                        }
                        else
                        {
                            unreachable;
                        }
                    }
                    else
                    {
                        unreachable;
                    }
                }
                else
                {
                    log("About to parse identifier expression\n", .{});
                    const identifier_expression = self.parse_expression_identifier();
                    log("Just parsed identifier expression\n", .{});
                    self.function_builder.scope_builders.items[self.function_builder.current_scope].statements.append(identifier_expression) catch unreachable;
                }
            },
            .operator =>
            {
                const operator = self.get_token(.operator).value;

                switch (operator)
                {
                    .Dereference =>
                    {
                        // @TODO: speed up this because we know we have a dereference operator
                        const dereference_statement = self.parse_expression();
                        if (self.lexer.tokens[self.lexer.next_index] != .sign or self.get_token(.sign).value != ';')
                        {
                            parser_error("Expected semicolon at the end of identifier expression\n", .{});
                        }

                        self.function_builder.scope_builders.items[self.function_builder.current_scope].statements.append(dereference_statement) catch unreachable;
                    },
                    .CompilerIntrinsic =>
                    {
                        self.consume_token(.operator);
                        const after_intrinsic_token = self.lexer.tokens[self.lexer.next_index];
                        assert(after_intrinsic_token == .keyword);
                        const keyword = self.get_and_consume_token(.keyword).value;
                        assert(keyword == .@"switch");
                        self.parse_compile_time_switch();
                        return;
                    },
                    else => panic("ni: {}\n", .{operator}),
                }
            },
            .keyword =>
            {
                const keyword = self.get_and_consume_token(.keyword).value;

                switch (keyword)
                {
                    .@"return" =>
                    {
                        const return_next_token = self.lexer.tokens[self.lexer.next_index];
                        const return_expression = blk:
                        {
                            if (return_next_token != .sign or self.get_token(.sign).value != ';')
                            {
                                const return_expression = self.parse_expression();
                                break :blk return_expression;
                            }
                            else
                            {
                                break :blk null;
                            }
                        };

                        const return_expression_index = current_scope.return_expressions.items.len;
                        const return_expression_id = Entity.new(return_expression_index, Entity.ScopeID.return_expressions);
                        current_scope.return_expressions.append(.{
                            .expression = return_expression,
                        }) catch unreachable;

                        current_scope.statements.append(return_expression_id) catch unreachable;
                    },
                    .@"for" =>
                    {
                        const for_loop_index = @intCast(u32, current_scope.loops.items.len);
                        current_scope.last_loop_index = for_loop_index;
                        current_scope.loops.append(undefined) catch unreachable;

                        const for_loop_id = Entity.new(for_loop_index, Entity.ScopeID.loops);
                        current_scope.statements.append(for_loop_id) catch unreachable;

                        const expected_identifier = self.lexer.tokens[self.lexer.next_index];
                        if (expected_identifier != .identifier)
                        {
                            parser_error("Expected identifier, found {s}\n", .{@tagName(expected_identifier)});
                        }

                        const it_decl_identifier = self.get_and_consume_token(.identifier).value;

                        const it_decl_value = IntegerLiteral.new(&self.module_builder.integer_literals, 0, false);
                        const it_decl_type = self.add_unresolved_type("u32");
                        const it_decl = VariableDeclaration.new(current_scope, it_decl_identifier, it_decl_type);

                        _ = Assignment.new(current_scope, it_decl, it_decl_value);

                        // Prefix block
                        const prefix_scope_index = Scope.Builder.new(self.allocator, &self.function_builder, for_loop_id, parent_scope);
                        current_scope = &self.function_builder.scope_builders.items[prefix_scope_index];
                        
                        if (self.lexer.tokens[self.lexer.next_index] != .operator or self.get_token(.operator).value != .Declaration)
                        {
                            parser_error("Expected declaration operator\n", .{});
                        }

                        self.consume_token(.operator);

                        const right_token = self.lexer.tokens[self.lexer.next_index];
                        const right_expression = blk:
                        {
                            switch (right_token)
                            {
                                .int_lit =>
                                {
                                    const int_lit = self.get_and_consume_token(.int_lit).value;
                                    break :blk IntegerLiteral.new(&self.module_builder.integer_literals, int_lit, false);
                                },
                                else => panic("NI: {}\n", .{right_token}),
                            }
                        };

                        const prefix_comparison = Comparison.new(current_scope, .less, it_decl, right_expression);
                        log("At {}, {}\n", .{self.function_builder.current_scope, prefix_scope_index});
                        current_scope.statements.append(prefix_comparison) catch unreachable;

                        self.end_scope(prefix_scope_index);

                        // Body
                        assert(self.function_builder.current_scope == parent_scope);
                        const body_scope_index = self.parse_scope(for_loop_id);

                        // Postfix
                        assert(self.function_builder.current_scope == parent_scope);
                        const postfix_scope_index = Scope.Builder.new(self.allocator, &self.function_builder, for_loop_id, parent_scope);
                        const postfix_increment_value = IntegerLiteral.new(&self.module_builder.integer_literals, 1, false);
                        _ = ComposedAssignment.new(current_scope, .add, it_decl, postfix_increment_value);

                        self.end_scope(postfix_scope_index);

                        assert(self.function_builder.current_scope == parent_scope);
                        current_scope = &self.function_builder.scope_builders.items[parent_scope];
                        var for_loop = &current_scope.loops.items[for_loop_index];
                        for_loop.* = .
                        {
                            .prefix_scope_index = prefix_scope_index,
                            .body_scope_index = body_scope_index,
                            .postfix_scope_index = postfix_scope_index,
                            .exit_block = std.mem.zeroes(IR.Reference),
                            .continue_block = std.mem.zeroes(IR.Reference),
                        };
                        // we don't want to consume ';' token
                        return;
                    },
                    .@"if" =>
                    {
                        const branch_index = current_scope.branches.items.len;
                        current_scope.branches.append(undefined) catch unreachable;

                        const branch_id = Entity.new(branch_index, Entity.ScopeID.branches);
                        current_scope.statements.append(branch_id) catch unreachable;
                        log("######## branch scope: {}\n", .{parent_scope});

                        const branch_condition = self.parse_expression();

                        const if_scope_index = self.parse_scope(branch_id);
                        assert(self.function_builder.current_scope == parent_scope);

                        const else_scope_index = blk:
                        {
                            if (self.lexer.tokens[self.lexer.next_index] == .keyword and self.get_token(.keyword).value == .@"else")
                            {
                                self.consume_token(.keyword);
                                break :blk self.parse_scope(branch_id);
                            }
                            else
                            {
                                break :blk null;
                            }
                        };

                        assert(self.function_builder.current_scope == parent_scope);
                        current_scope = &self.function_builder.scope_builders.items[parent_scope];
                        var branch = &current_scope.branches.items[branch_index];

                        branch.* = Branch
                        {
                            .condition = branch_condition,
                            .if_scope = if_scope_index,
                            .else_scope = else_scope_index,
                            .exit_block = std.mem.zeroes(IR.Reference),
                        };

                        // @INFO: skip consuming ';' token
                        return;
                    },
                    .@"break" =>
                    {
                        var scope = &self.function_builder.scope_builders.items[parent_scope];

                        const last_loop_index = scope.last_loop_index;
                        if (last_loop_index == std.math.maxInt(u32))
                        {
                            parser_error("No loops found\n", .{});
                        }

                        
                        //fn parse_break(self: *Parser, parser: *ModuleParser, parent_node: *Node) *Node
                        //{
                        //// consuming break keyword
                        //parser.consume();
                        //var target = self.current_block;

                        //while (target.value != Node.ID.loop_expr)
                        //{
                        //if (target.parent) |parent|
                        //{
                        //target = parent;
                        //}
                        //else
                        //{
                        //panic("Couldn't find any parent\n", .{});
                        //}
                        //}

                        //const break_value = Node
                        //{
                        //.value = Node.Value {
                        //.break_expr = BreakExpression {
                        //.target = target, 
                        //}
                        //},
                        //.parent = parent_node,
                        //.value_type = Node.ValueType.RValue,
                        //.type = undefined,
                        //};

                        //const result = self.append_and_get(break_value);
                        //return result;
                        //}
                    },
                    else => panic("Ni: {}\n", .{keyword}),
                }
            },
            else => panic("ni: {}\n", .{next_token}),
        }

        const expected_semicolon_token = self.lexer.tokens[self.lexer.next_index];
        log("Expected semicolon token: {}\n", .{expected_semicolon_token});
        if (expected_semicolon_token == .sign)
        {
            const sign = self.get_token(.sign).value;
            log("End of statement sign: {c}\n", .{sign});
            if (sign == ';')
            {
                self.consume_token(.sign);
                return;
            }
        }

        parser_error("Expected semicolon to end statement\n", .{});
    }

    fn end_scope(self: *Self, scope_index: u32) void
    {
        var scope_builder = &self.function_builder.scope_builders.items[scope_index];
        // @TODO: DEBUG THIS SHIT
        self.function_builder.scopes.items[scope_index] = .
        {
            .statements = scope_builder.statements.items,
            .variable_declarations = scope_builder.variable_declarations.items,
            .identifier_expressions = scope_builder.identifier_expressions.items,
            .invoke_expressions = scope_builder.invoke_expressions.items,
            .field_access_expressions = scope_builder.field_access_expressions.items,
            .return_expressions = scope_builder.return_expressions.items,
            .assignments = scope_builder.assignments.items,
            .comparisons = scope_builder.comparisons.items,
            .loops = scope_builder.loops.items,
            .branches = scope_builder.branches.items,
            .break_expressions = scope_builder.break_expressions.items,
            .arithmetic_expressions = scope_builder.arithmetic_expressions.items,
            .parent = scope_builder.parent,
        };

        log("[{}] Scope builder comparisons: {}\n", .{scope_index, scope_builder.comparisons.items.len});

        self.function_builder.current_scope = scope_builder.parent.scope;
    }

    fn parse_scope(self: *Self, parent_expression: Entity) u32
    {
        const previous_scope = self.function_builder.current_scope;

        const new_current_scope = Scope.Builder.new(self.allocator, &self.function_builder, parent_expression, previous_scope);

        // @TODO: should we move this outside the function?
        const expected_left_brace = self.lexer.tokens[self.lexer.next_index];
        if (expected_left_brace != .sign and self.get_token(.sign).value != '{')
        {
            parser_error("Expected left brace to open up the function body\n", .{});
        }

        self.consume_token(.sign);

        var next_token = self.lexer.tokens[self.lexer.next_index];
        var block_ends = next_token == .sign and self.get_token(.sign).value == '}';

        while (!block_ends)
        {
            self.parse_statement(self.function_builder.current_scope);
            next_token = self.lexer.tokens[self.lexer.next_index];
            block_ends = next_token == .sign and self.get_token(.sign).value == '}';
        }

        if (self.lexer.tokens[self.lexer.next_index] != .sign or self.get_and_consume_token(.sign).value != '}')
        {
            parser_error("Expected closing brace for scope\n", .{});
        }

        self.end_scope(new_current_scope);

        return new_current_scope;
    }

    fn add_unresolved_type(self: *Self, type_identifier: []const u8) Type
    {
        for (self.module_builder.unresolved_types.items) |t, i|
        {
            if (std.mem.eql(u8, type_identifier, t))
            {
                return Type.new_unresolved_type(i, self.module_builder.index);
            }
        }

        const i = self.module_builder.unresolved_types.items.len;
        log("INDEX: {}\n", .{i});
        self.module_builder.unresolved_types.append(type_identifier) catch unreachable;
        return Type.new_unresolved_type(i, self.module_builder.index);
    }

    fn parse_type(self: *Self) Type
    {
        const next_token = self.lexer.tokens[self.lexer.next_index];
        switch (next_token)
        {
            .identifier =>
            {
                const type_identifier = self.get_and_consume_token(.identifier).value;
                return self.add_unresolved_type(type_identifier);
            },
            else => panic("ni: {}\n", .{next_token}),
        }
        //fn parse_type(self: *Parser, allocator: *Allocator, parser: *ModuleParser, parent_node: ?*Node) *Node
        //{
        //log.debug("Parsing type...\n", .{});
        //const next_token = parser.peek();
        //parser.consume();

        //switch (next_token.value)
        //{
        //Token.ID.identifier =>
        //{
        //const type_name = next_token.value.identifier;
        //const type_node_value = Node {
        //.value = Node.Value {
        //.type_identifier = TypeIdentifier {
        //.value = TypeIdentifier.Value {
        //.simple = type_name,
        //},
        //},
        //},
        //.parent = parent_node,
        //.value_type = Node.ValueType.LValue,
        //.type = undefined,
        //};

        //const type_node = self.append_and_get(type_node_value);
        //return type_node;
        //},
        //Token.ID.keyword =>
        //{
        //const keyword = next_token.value.keyword;

        //switch (keyword)
        //const 
        //{
        //KeywordID.@"struct" =>
        //{
        //if (parser.expect_and_consume_sign('{') == null)
        //{
        //parser_error("Error: expected '{c}' at the beginning of the struct\n", .{'{'});
        //}

        //const struct_type_node = Node
        //{
        //.value = Node.Value {
        //.type_identifier = TypeIdentifier {
        //.value = TypeIdentifier.Value {
        //.structure = TypeIdentifier.Struct {
        //.fields = NodeRefBuffer.init(allocator),
        //.name = "",
        //},
        //},
        //},
        //},
        //.parent = parent_node,
        //.value_type = Node.ValueType.RValue,
        //.type = undefined,
        //};

        //var result = self.append_and_get(struct_type_node);

        //if (parser.expect_and_consume_sign('}') == null)
        //{
        //while (true)
        //{
        //const field = self.parse_expression(allocator, parser, result);
        //result.value.type_identifier.value.structure.fields.append(field) catch {
        //panic("Error allocating memory for struct field\n", .{});
        //};

        //const token = parser.peek();
        //parser.consume();

        //if (token.value == Token.ID.sign and token.value.sign == '}')
        //{
        //break;
        //}
        //else if (token.value == Token.ID.sign and token.value.sign != ',')
        //{
        //parser_error("Expected comma after argument. Found: {}\n", .{token.value});
        //}
        //else
        //{
        //if (parser.expect_and_consume_sign('}') != null)
        //{
        //break;
        //}
        //}
        //}
        //}
        //else
        //{
        //parser_error("Empty struct is not allowed\n", .{});
        //}

        //return result;
        //},
        //else => panic("ni: {}\n", .{keyword}),
        //}
        //},
        //Token.ID.operator =>
        //{
        //const operator = next_token.value.operator;
        //switch (operator)
        //{
        //Operator.AddressOf =>
        //{
        //const type_node_value = Node {
        //.value = Node.Value {
        //.type_identifier = TypeIdentifier {
        //.value = TypeIdentifier.Value {
        //.pointer = TypeIdentifier.Pointer {
        //.type = self.parse_type(allocator, parser, parent_node),
        //},
        //},
        //},
        //},
        //.parent = parent_node,
        //.value_type = Node.ValueType.LValue,
        //.type = undefined,
        //};

        //const type_node = self.append_and_get(type_node_value);
        //return type_node;
        //},
        //Operator.LeftBracket =>
        //{
        //if (parser.expect_and_consume_operator(Operator.RightBracket) == null)
        //{
        //var type_node_value = Node {
        //.value = Node.Value {
        //.type_identifier = TypeIdentifier {
        //.value = TypeIdentifier.Value {
        //.array = TypeIdentifier.Array {
        //.type = undefined,
        //.len_expr = undefined,
        //},
        //},
        //},
        //},
        //.parent = parent_node,
        //.value_type = Node.ValueType.LValue,
        //.type = undefined,
        //};

        //const type_node = self.append_and_get(type_node_value);

        //type_node.value.type_identifier.value.array.len_expr = self.parse_expression(allocator, parser, type_node);

        //if (parser.expect_and_consume_operator(Operator.RightBracket) == null)
        //{
        //parser_error("Expected ']' in array type\n", .{});
        //}

        //type_node.value.type_identifier.value.array.type = self.parse_type(allocator, parser, parent_node);

        //return type_node;
        //}
        //else
        //{
        //// Slice
        //const slice_type = self.parse_type(allocator, parser, parent_node);
        //panic("This is a slice of type: {}\n", .{slice_type});
        //}
        //},
        //else => panic("ni: {}\n", .{operator}),
        //}
        //},
        //else => panic("not implemented: {}\n", .{next_token.value}),
        //}
        //}
    }
    
    fn parse_compile_time_switch(self: *Self) void
    {
        if (self.lexer.tokens[self.lexer.next_index] != .operator or self.get_and_consume_token(.operator).value != .LeftParenthesis)
        {
            parser_error("Expected left parenthesis to open expression to switch on\n", .{});
        }

        switch (self.lexer.tokens[self.lexer.next_index])
        {
            .operator =>
            {
                const operator = self.get_and_consume_token(.operator).value;
                if (operator == .CompilerIntrinsic)
                {
                    const compile_time_expression = self.parse_compile_time_expression();
                    if (compile_time_expression.value == Entity.get_builtin_os().value)
                    {
                        if (self.lexer.tokens[self.lexer.next_index] != .operator or self.get_and_consume_token(.operator).value != .RightParenthesis)
                        {
                            parser_error("Expected left parenthesis to open expression to switch on\n", .{});
                        }

                        const os = std.builtin.target.os.tag;

                        if (self.lexer.tokens[self.lexer.next_index] != .sign or self.get_and_consume_token(.sign).value != '{')
                        {
                            parser_error("Expected left parenthesis to open expression to switch on\n", .{});
                        }

                        var right_brace_found = false;
                        var found_right_case = false;

                        while (!right_brace_found): (right_brace_found = self.lexer.tokens[self.lexer.next_index] == .sign and self.get_token(.sign).value == '}')
                        {
                            if (!found_right_case)
                            {
                                const first_case_token = self.lexer.tokens[self.lexer.next_index];
                                if (!(first_case_token == .operator and self.get_and_consume_token(.operator).value == .Dot))
                                {
                                    parser_error("Expected dot operator\n", .{});
                                }

                                assert(self.lexer.tokens[self.lexer.next_index] == .identifier);
                                const os_identifier = self.get_and_consume_token(.identifier).value;

                                const os_enum = std.meta.stringToEnum(std.Target.Os.Tag, os_identifier) orelse
                                {
                                    parser_error("Unknown operating system: {s}\n", .{os_identifier});
                                };


                                if (os_enum == os)
                                {
                                    found_right_case = true;
                                    const next_one = self.lexer.tokens[self.lexer.next_index];
                                    if (!(next_one == .operator and self.get_and_consume_token(.operator).value == .Declaration))
                                    {
                                        parser_error("Expected colon after switch case identifier\n", .{});
                                    }

                                    // @TODO: we are trapping here. We should be parsing a new scope but we are not ready to do it yet
                                    const found_lbrace = self.lexer.tokens[self.lexer.next_index] == .sign and self.get_token(.sign).value == '{';
                                    if (found_lbrace)
                                    {
                                        self.consume_token(.sign);

                                        var next_token = self.lexer.tokens[self.lexer.next_index];
                                        var block_ends = next_token == .sign and self.get_token(.sign).value == '}';
                                        while (!block_ends):
                                            ({
                                                next_token = self.lexer.tokens[self.lexer.next_index];
                                                block_ends = next_token == .sign and self.get_token(.sign).value == '}';
                                            })
                                        {
                                            // @TODO: this might be buggy
                                            self.parse_statement(self.function_builder.current_scope);
                                        }

                                        if (!(self.lexer.tokens[self.lexer.next_index] == .sign and self.get_and_consume_token(.sign).value == '}'))
                                        {
                                            parser_error("Expected right brace to close the switch case\n", .{});
                                        }

                                        if (!(self.lexer.tokens[self.lexer.next_index] == .sign and self.get_and_consume_token(.sign).value == ','))
                                        {
                                            parser_error("Expected right brace to close the switch case\n", .{});
                                        }
                                    }
                                    else
                                    {
                                        // @TODO: this might be buggy
                                        self.parse_statement(self.function_builder.current_scope);
                                    }
                                }
                                else
                                {
                                    unreachable;
                                }
                            }
                            else
                            {
                                // @TODO: this is cheating, we should make this robust
                                //
                                var open_braces: u32 = 1;
                                while (open_braces > 0)
                                {
                                    const next_token = self.lexer.tokens[self.lexer.next_index];
                                    switch (next_token)
                                    {
                                        .operator => self.consume_token(.operator),
                                        .identifier => self.consume_token(.identifier),
                                        .keyword => self.consume_token(.keyword),
                                        .sign =>
                                        {
                                            const sign = self.get_token(.sign).value;
                                            if (sign == '}')
                                            {
                                                log("Closing braces...\n", .{});
                                                open_braces -= 1;
                                                if (open_braces > 0)
                                                {
                                                    self.consume_token(.sign);
                                                }
                                            }
                                            else if (sign == '{')
                                            {
                                                log("Opening braces...\n", .{});
                                                open_braces += 1;
                                                self.consume_token(.sign);
                                            }
                                            else
                                            {
                                                self.consume_token(.sign);
                                            }
                                        },
                                        else => panic("ni Token: {}\n", .{next_token}),
                                    }
                                }
                            }
                        }

                        if (self.lexer.tokens[self.lexer.next_index] != .sign or self.get_and_consume_token(.sign).value != '}')
                        {
                            parser_error("Expected left parenthesis to open expression to switch on\n", .{});
                        }
                    }
                    else
                    {
                        unreachable;
                    }
                }
                else
                {
                    unreachable;
                }
            },
            else => unreachable,
        }
    }

    fn parse_compile_time_expression(self: *Self) Entity
    {
        switch (self.lexer.tokens[self.lexer.next_index])
        {
            .identifier =>
            {
                const identifier = self.get_and_consume_token(.identifier).value;
                if (std.mem.eql(u8, identifier, "os"))
                {
                    return Entity.get_builtin_os();
                }
                else unreachable;
            },
            else => unreachable,
        }
    }

    fn get_function_type(self: *Self, return_type: Type, argument_types: []Type, attributes: u64) Type
    {
        outer_loop: for (self.module_builder.function_types.items) |ft, i|
        {
            if (return_type.value != ft.return_type.value) continue;
            if (attributes != ft.attributes) continue;

            for (ft.argument_types) |arg_type, arg_i|
            {
                const arg_t = argument_types[arg_i];
                if (arg_type.value != arg_t.value)
                {
                    continue :outer_loop;
                }
            }

            return Type.Function.new(i, self.module_builder.index);
        }

        return Type.Function.append(&self.module_builder.function_types,
        .{
            .return_type = return_type,
            .argument_types = argument_types,
            .attributes = attributes,
        },
        self.module_builder.index);
    }
};

pub const InvokeExpression = struct
{
    arguments: []Entity,
    expression: Entity,
};

pub const ImportedModule = struct
{
    module: Entity,
    alias: ?[]const u8,
};

pub const Module = struct
{
    internal_functions: []Function.Internal,
    external_functions: []Function.External,
    library_names: []([]const u8),
    libraries: []Library.Builder,
    imported_modules: []ImportedModule,
    integer_literals: []IntegerLiteral,

    unresolved_types: [][]const u8,
    pointer_types: []Type.Pointer,
    slice_types: []Type.Slice,
    function_types: []Type.Function,
    array_types: []Type.Array,
    struct_types: []Type.Struct,

    pub const Builder = struct
    {
        internal_functions: ArrayList(Function.Internal),
        external_functions: ArrayList(Function.External),
        library_names: ArrayList([]const u8),
        libraries: ArrayList(Library.Builder),

        imported_modules: ArrayList(ImportedModule),
        integer_literals: ArrayList(IntegerLiteral),

        unresolved_types: ArrayList([]const u8),
        pointer_types: ArrayList(Type.Pointer),
        slice_types: ArrayList(Type.Slice),
        function_types: ArrayList(Type.Function),
        array_types: ArrayList(Type.Array),
        struct_types: ArrayList(Type.Struct),
        index: u32,
    };
};

pub const AST = struct
{
    modules: [*]Module,
    module_names: [*][]const u8, // @INFO: used to know if a module is already imported and get unique and atomic indices
    module_directories: [*][]const u8,
    module_len: u64,
    module_cap: u64,

    const Self = @This();

    pub fn parse(allocator: *Allocator, source_filename: []const u8, target: std.Target) Self
    {
        log("\n==============\nEntering PARSER stage\n==============\n\n", .{});

        const minimal_module_count = 3;
        var ast = AST
        {
            .modules = (allocator.alloc(Module, minimal_module_count) catch unreachable).ptr,
            .module_names = (allocator.alloc([]const u8, minimal_module_count) catch unreachable).ptr,
            .module_directories = (allocator.alloc([]const u8, minimal_module_count) catch unreachable).ptr,
            .module_len = 0,
            .module_cap = minimal_module_count,
        };

        _ = ast.lex_and_parse_module(allocator, "lib/runtime.rns", target, null);
        _ = ast.lex_and_parse_module(allocator, source_filename, target, null);

        return ast;
    }

    // @TODO: make this thread-safe
    pub fn lex_and_parse_module(self: *AST, allocator: *Allocator, source_file: []const u8, target: std.Target, parent_module: ?Entity) Entity
    {
        const module_index = self.module_len;
        self.module_len += 1;

        // @TODO: should this be atomic too?
        {
            // ensure total capacity
            const new_capacity = module_index + 1;
            var better_capacity = self.module_cap;
            if (new_capacity > better_capacity)
            {
                while (new_capacity > better_capacity)
                {
                    better_capacity += better_capacity / 2 + 8;
                }

                self.modules = (allocator.reallocAtLeast(self.modules[0..self.module_len], better_capacity) catch unreachable).ptr;
                self.module_names = (allocator.reallocAtLeast(self.module_names[0..self.module_len], better_capacity) catch unreachable).ptr;
                self.module_directories = (allocator.reallocAtLeast(self.module_directories[0..self.module_len], better_capacity) catch unreachable).ptr;
                self.module_cap = better_capacity;
            }
        }

        log("Parsing module #{} \"{s}\"\n", .{module_index, source_file});
        const module_id = Entity.new(module_index, Entity.GlobalID.modules);

        const file_content = if (parent_module) |parent_module_id| blk:
        {
            const parent_directory_name = self.module_directories[parent_module_id.get_index()];
            var parent_directory_handle = std.fs.openDirAbsolute(parent_directory_name, .{}) catch unreachable;
            defer parent_directory_handle.close();
            const absolute_path = parent_directory_handle.realpathAlloc(allocator, source_file) catch unreachable;
            self.module_directories[module_index] = std.fs.path.dirname(absolute_path).?;
            const file_handle = std.fs.openFileAbsolute(absolute_path, .{}) catch unreachable;
            defer file_handle.close();
            break :blk file_handle.readToEndAlloc(allocator, 0xffffffff) catch unreachable;
        }
        else blk:
        {
            const absolute_path = std.fs.realpathAlloc(allocator, source_file) catch unreachable;
            self.module_directories[module_index] = std.fs.path.dirname(absolute_path).?;
            const file_handle = std.fs.openFileAbsolute(absolute_path, .{}) catch unreachable;
            defer file_handle.close();
            break :blk file_handle.readToEndAlloc(allocator, 0xffffffff) catch unreachable;
        };

        const lexer = Lexer.lexical_analyze(allocator, file_content);

        log("\n==============\nEntering PARSER stage\n==============\n\n", .{});
        var parser = ModuleParser
        {
            .lexer = .
            {
                .next_index = 0,
                .counters = std.mem.zeroes(ModuleParser.CountersType),
                .tokens = lexer.tokens,
                .int_literals = lexer.int_literals,
                .char_literals = lexer.char_literals,
                .string_literals = lexer.string_literals,
                .identifiers = lexer.identifiers,
                .keywords = lexer.keywords,
                .signs = lexer.signs,
                .operators = lexer.operators,
            },
            // @TODO: substitute these for indices to concrete arrays
            .function_builder = undefined,
            // @TODO: substitute these for indices to concrete arrays
            .allocator = allocator,
            .module_builder = .
            {
                .internal_functions = ArrayList(Function.Internal).init(allocator),
                .external_functions = ArrayList(Function.External).init(allocator),
                .library_names = ArrayList([]const u8).init(allocator),
                .libraries = ArrayList(Library.Builder).init(allocator),
                .imported_modules = ArrayList(ImportedModule).init(allocator),
                .integer_literals = ArrayList(IntegerLiteral).init(allocator),

                .unresolved_types = ArrayList([]const u8).init(allocator),
                .pointer_types = ArrayList(Type.Pointer).init(allocator),
                .slice_types = ArrayList(Type.Slice).init(allocator),
                .function_types = ArrayList(Type.Function).init(allocator),
                .array_types = ArrayList(Type.Array).init(allocator),
                .struct_types = ArrayList(Type.Struct).init(allocator),
                .index = @intCast(u32, module_index),
            },
        };

        const token_count = parser.lexer.tokens.len;
        while (parser.lexer.next_index < token_count)
        {
            const tld_name_token = parser.lexer.tokens[parser.lexer.next_index];
            if (tld_name_token != .identifier)
            {
                parser_error("Top level declarations must start with an identifier/name\n", .{});
            }

            const tld_name = parser.get_and_consume_token(.identifier).value;

            if (parser.lexer.next_index + 2 >= parser.lexer.tokens.len)
            {
                parser_error("End of the file while parsing top level declaration\n", .{});
            }

            const next_token = parser.lexer.tokens[parser.lexer.next_index];
            if (next_token == .operator)
            {
                const operator = parser.get_and_consume_token(.operator);

                if (operator.value == .Constant)
                {
                    const after_const_token = parser.lexer.tokens[parser.lexer.next_index];
                    if (after_const_token == .operator)
                    {
                        const after_const_operator = parser.get_and_consume_token(.operator);
                        if (after_const_operator.value == .LeftParenthesis)
                        {
                            log("Parsing function {s}...\n", .{tld_name});
                            const left_parenthesis_next_token = parser.lexer.tokens[parser.lexer.next_index];
                            var argument_name_list = ArrayList([]const u8).init(parser.allocator);
                            var argument_type_list = ArrayList(Type).init(parser.allocator);
                            var arguments_left_to_parse = blk:
                            {
                                if (left_parenthesis_next_token == .operator)
                                {
                                    const left_parenthesis_next_operator = parser.get_token(.operator);
                                    if (left_parenthesis_next_operator.value == .RightParenthesis)
                                    {
                                        break :blk false;
                                    }
                                }

                                break :blk true;
                            };

                            while (arguments_left_to_parse)
                            {
                                const arg_first_token = parser.lexer.tokens[parser.lexer.next_index];
                                if (arg_first_token != .identifier)
                                {
                                    parser_error("Expected argument name; found: {}\n", .{arg_first_token});
                                }

                                const argument_name = parser.get_and_consume_token(.identifier).value;
                                const expected_colon = parser.lexer.tokens[parser.lexer.next_index];
                                if ((expected_colon == .operator and parser.get_token(.operator).value != .Declaration) or expected_colon != .operator)
                                {
                                    parser_error("Expected colon, found: {}\n", .{expected_colon});
                                }

                                parser.consume_token(.operator);

                                const argument_type = parser.parse_type();
                                argument_name_list.append(argument_name) catch unreachable;
                                argument_type_list.append(argument_type) catch unreachable;

                                const after_arg_token = parser.lexer.tokens[parser.lexer.next_index];
                                arguments_left_to_parse = !(after_arg_token == .operator and parser.get_token(.operator).value == .RightParenthesis);
                                if (arguments_left_to_parse)
                                {
                                    if (!(after_arg_token == .sign and parser.get_token(.sign).value == ','))
                                    {
                                        parser_error("Expected comma after function argument\n", .{});
                                    }
                                }
                            }

                            const expected_right_parenthesis = parser.lexer.tokens[parser.lexer.next_index];
                            if (!(expected_right_parenthesis == .operator and parser.get_token(.operator).value == .RightParenthesis))
                            {
                                parser_error("Expected right parenthesis after function argument list\n", .{});
                            }

                            parser.consume_token(.operator);

                            const arrow_or = parser.lexer.tokens[parser.lexer.next_index];
                            var return_type: Type = undefined;
                            if (arrow_or == .operator and parser.get_token(.operator).value == .Arrow)
                            {
                                parser.consume_token(.operator);
                                return_type = parser.parse_type();
                            }
                            else
                            {
                                return_type = Type.Builtin.void_type;
                            }
                            log("Return type: {}\n", .{return_type});

                            var next = parser.lexer.tokens[parser.lexer.next_index];
                            var attributes: u64 = 0;
                            var extern_library_name: []const u8 = undefined;

                            // Loop around a different premise, this is error-prone
                            while (next == .keyword) : (next = parser.lexer.tokens[parser.lexer.next_index])
                            {
                                const kw = parser.get_and_consume_token(.keyword).value;
                                switch (kw)
                                {
                                    .@"noreturn" => attributes |= 1 << @enumToInt(Type.Function.Attribute.@"noreturn"),
                                    .@"extern" =>
                                    {
                                        log("Parsing extern function\n", .{});
                                        attributes |= 1 << @enumToInt(Type.Function.Attribute.@"extern");
                                        if (parser.lexer.tokens[parser.lexer.next_index] != .operator or parser.get_and_consume_token(.operator).value != .LeftParenthesis)
                                        {
                                            parser_error("In this language external symbols must be defined along with the library name they belong to\n", .{});
                                        }

                                        if (parser.lexer.tokens[parser.lexer.next_index] != .str_lit)
                                        {
                                            parser_error("Expected library name after the function being declared extern, found: {}\n", .{parser.lexer.tokens[parser.lexer.next_index]});
                                        }

                                        extern_library_name = parser.get_and_consume_token(.str_lit).value;
                                        if (parser.lexer.tokens[parser.lexer.next_index] != .operator or parser.get_and_consume_token(.operator).value != .RightParenthesis)
                                        {
                                            parser_error("Expected right parenthesis after extern declaration\n", .{});
                                        }

                                        log("Library name: {s}\n", .{extern_library_name});
                                    },
                                    else => panic("ni: {}\n", .{kw}),
                                }
                            }

                            const has_body = (attributes & (1 << @enumToInt(Type.Function.Attribute.@"extern"))) >> @enumToInt(Type.Function.Attribute.@"extern") == 0;


                            const is_no_return = (attributes & (1 << @enumToInt(Type.Function.Attribute.@"noreturn")) >> @enumToInt(Type.Function.Attribute.@"noreturn")) != 0;
                            if (is_no_return) return_type = Type.Builtin.noreturn_type;
                            const function_type = Type.Function
                            {
                                .return_type = return_type,
                                .argument_types = argument_type_list.items,
                                .attributes = attributes,
                            };

                            log("Return type for \"{s}\": {}\n", .{tld_name, function_type.return_type});

                            if (has_body)
                            {
                                const current_function_index = parser.module_builder.internal_functions.items.len;
                                const function_id = Entity.new(current_function_index, Entity.ModuleID.internal_functions);
                                parser.function_builder = Function.Builder
                                {
                                    .scope_builders = ArrayList(Scope.Builder).init(allocator),
                                    .scopes = ArrayList(Scope).init(allocator),
                                    .current_scope = 0,
                                };

                                _ = parser.parse_scope(function_id);

                                parser.module_builder.internal_functions.append(Function.Internal
                                    {
                                        .declaration = .
                                        {
                                            .argument_names = argument_name_list.items,
                                            .name = tld_name,
                                            .type = function_type,
                                        },
                                        .scopes = parser.function_builder.scopes.items,
                                    }) catch unreachable;
                                log("[#############] Scope count: {}\n", .{parser.function_builder.scopes.items.len});
                            }
                            else
                            {
                                if (parser.lexer.tokens[parser.lexer.next_index] != .sign)
                                {
                                    parser_error("Expected semicolon after extern function declaration\n", .{});
                                }
                                const foo = parser.get_and_consume_token(.sign);
                                log("Foo value: {c}\n", .{foo.value});
                                if (foo.value != ';')
                                {
                                    parser_error("Expected semicolon after extern function declaration\n", .{});
                                }

                                parser.module_builder.external_functions.append(.
                                    {
                                        .declaration = .
                                        {
                                            .argument_names = argument_name_list.items,
                                            .name = tld_name,
                                            .type = function_type,
                                        },
                                        .index = blk:
                                        {
                                            const function_name = tld_name;

                                            for (parser.module_builder.library_names.items) |library_name, library_i|
                                            {
                                                if (std.mem.eql(u8, library_name, extern_library_name))
                                                {
                                                    var symbol_names = &parser.module_builder.libraries.items[library_i].symbol_names;
                                                    for (symbol_names.items) |symbol_name, symbol_i|
                                                    {
                                                        if (std.mem.eql(u8, symbol_name, function_name))
                                                        {
                                                            break :blk .
                                                            {
                                                                .function = @intCast(u16, symbol_i),
                                                                .library = @intCast(u16, library_i),
                                                            };
                                                        }
                                                    }

                                                    const symbol_index = symbol_names.items.len;
                                                    symbol_names.append(function_name) catch unreachable;

                                                    break :blk .
                                                    {
                                                        .function = @intCast(u16, symbol_index),
                                                        .library = @intCast(u16, library_i),
                                                    };
                                                }
                                            }

                                            const library_i = parser.module_builder.library_names.items.len;
                                            parser.module_builder.library_names.append(extern_library_name) catch unreachable;
                                            parser.module_builder.libraries.append(.
                                                {
                                                    .symbol_names = sym_blk:
                                                    {
                                                        var new_lib_symbol_names = ArrayList([]const u8).init(allocator);
                                                        new_lib_symbol_names.append(function_name) catch unreachable;
                                                        break :sym_blk new_lib_symbol_names;
                                                    },
                                                    }) catch unreachable;

                                            const symbol_i = 0;
                                            break :blk .
                                            {
                                                .function = @intCast(u16, symbol_i),
                                                .library = @intCast(u16, library_i),
                                            };
                                        }
                                    }) catch unreachable;
                            }
                        }
                        else if (after_const_operator.value == .CompilerIntrinsic)
                        {
                            const intrinsic_name_token = parser.lexer.tokens[parser.lexer.next_index];
                            if (intrinsic_name_token != .identifier)
                            {
                                parser_error("Expected identifier after intrinsic operator\n", .{});
                            }

                            const intrinsic_name = parser.get_and_consume_token(.identifier).value;
                            assert(std.mem.eql(u8, intrinsic_name, "import"));

                            const expected_left_parenthesis = parser.lexer.tokens[parser.lexer.next_index];
                            if (expected_left_parenthesis != .operator or parser.get_and_consume_token(.operator).value != .LeftParenthesis)
                            {
                                parser_error("Expected left parenthesis after import intrinsic\n", .{});
                            }

                            const file_name_token = parser.lexer.tokens[parser.lexer.next_index];
                            if (file_name_token != .str_lit)
                            {
                                parser_error("Expected string literal with the file name to be imported\n", .{});
                            }

                            const import_file_name = parser.get_and_consume_token(.str_lit).value;
                            log("File name to be imported: {s}\n", .{import_file_name});

                            if (parser.lexer.tokens[parser.lexer.next_index] != .operator or parser.get_and_consume_token(.operator).value != .RightParenthesis)
                            {
                                parser_error("Expected right parenthesis after import file name\n", .{});
                            }

                            if (parser.lexer.tokens[parser.lexer.next_index] != .sign or parser.get_and_consume_token(.sign).value != ';')
                            {
                                parser_error("Expected semicolon after import statement\n", .{});
                            }

                            const import_module_id = self.import_module(parser.allocator, import_file_name, target, module_id);
                            parser.module_builder.imported_modules.append(ImportedModule
                                {
                                    .module = import_module_id,
                                    .alias = tld_name,
                                }) catch unreachable;
                        }
                        else 
                        {
                            parser_error("Unexpected operator after constant declaration: {}\n", .{after_const_operator});
                        }
                    }
                    else if (after_const_token == .keyword)
                    {
                        const keyword = parser.get_and_consume_token(.keyword);
                        if (keyword.value == .@"struct")
                        {
                            //const struct_type = self.parse_type(parser.allocator, parser, null);
                            //struct_type.value.type_identifier.value.structure.name = name;
                            //self.type_declarations.append(struct_type) catch {
                            //panic("Error allocating memory for type declaration\n", .{});
                            //};
                            unreachable;
                        }
                        else
                        {
                            unreachable;
                        }
                    }
                    else
                    {
                        unreachable;
                    }
                }
                else if (operator.value == .Declaration)
                {
                    const decl_token = parser.lexer.tokens[parser.lexer.next_index];
                    if (decl_token != .operator)
                    {
                        parser_error("Expected operator\n",.{});
                    }
                    const decl_operator = parser.get_and_consume_token(.operator);
                    if (decl_operator.value != .CompilerIntrinsic)
                    {
                        parser_error("Expected compiler intrinsic operator\n", .{});
                    }
                    const intrinsic_token = parser.lexer.tokens[parser.lexer.next_index];
                    _ = intrinsic_token;
                    // @TODO: define the range of intrinsics which are valid here in order to get better performance
                    panic("Here we should be parsing the intrinsic\n", .{});
                }
                else
                {
                    unreachable;
                }
            }
            else
            {
                parser_error("unexpected token\n", .{});
            }
        }

        self.modules[module_index] = Module
        {
            .internal_functions = parser.module_builder.internal_functions.items,
            .external_functions = parser.module_builder.external_functions.items,
            .library_names = parser.module_builder.library_names.items,
            .libraries = parser.module_builder.libraries.items,
            .imported_modules = parser.module_builder.imported_modules.items,
            .integer_literals = parser.module_builder.integer_literals.items,
            .unresolved_types = parser.module_builder.unresolved_types.items,
            .pointer_types = parser.module_builder.pointer_types.items,
            .slice_types = parser.module_builder.slice_types.items,
            .function_types = parser.module_builder.function_types.items,
            .array_types = parser.module_builder.array_types.items,
            .struct_types = parser.module_builder.struct_types.items,
        };

        log("Module #{} just parsed: {}\n", .{module_index, self.modules[module_index].imported_modules.len});

        self.module_names[module_index] = source_file;

        return module_id;
    }

    pub fn import_module(self: *Self, allocator: *Allocator, source_file: []const u8, target: std.Target, parent_module: Entity) Entity
    {
        for (self.module_names[0..self.module_len]) |module_name, i|
        {
            if (std.mem.eql(u8, module_name, source_file))
            {
                log("Already import module \"{s}\" with ID {}\n", .{source_file, i});
                return Entity.new(i, Entity.GlobalID.modules);
            }
        }

        return self.lex_and_parse_module(allocator, source_file, target, parent_module);
    }
};

//pub const Node = struct
//{
    //value: Value,
    //parent: ?*Node,
    //value_type: ValueType,
    //type: *Type,

    //pub const Value = union(ID)
    //{
        //var_decl: VariableDeclaration,
        //function_decl: FunctionDeclaration,
        //syscall_decl: Intrinsic.Syscall,
        //int_lit: IntegerLiteral,
        //array_lit: ArrayLiteral,
        //struct_lit: StructLiteral,
        //unary_expr: UnaryExpression,
        //binary_expr: BinaryExpression,
        //return_expr: ReturnExpression,
        //identifier_expr: IdentifierExpression,
        //resolved_identifier: *Node,
        //field_expr: *Type.Struct.Field,
        //invoke_expr: InvokeExpression,
        //block_expr: BlockExpression,
        //branch_expr: BranchExpression,
        //loop_expr: LoopExpression,
        //break_expr: BreakExpression,
        //array_subscript_expr: ArraySubscriptExpression,
        //field_access_expr: FieldAccessExpression,
        //module: Module,
    //};

    //pub const ID = enum
    //{
        //var_decl,
        //function_decl,
        //syscall_decl,
        //int_lit,
        //array_lit,
        //struct_lit,
        //unary_expr,
        //binary_expr,
        //return_expr,
        //identifier_expr,
        //resolved_identifier,
        //field_expr,
        //invoke_expr,
        //block_expr,
        //branch_expr,
        //loop_expr,
        //break_expr,
        //array_subscript_expr,
        //field_access_expr,
        //module,
    //};

    //pub const ValueType = enum
    //{
        //RValue,
        //LValue,
    //};

    //pub fn format(self: *const Node, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void
    //{
        //_ = fmt;

        //switch (self.value)
        //{
            //Node.ID.function_decl =>
            //{
                //try std.fmt.format(writer, "{s} :: (", .{self.value.function_decl.name});
                //for (self.value.function_decl.arguments.items) |arg|
                //{
                    //try std.fmt.format(writer, "{}, ", .{arg});
                //}
                //try writer.writeAll(")");
                //const type_node = self.value.function_decl.type;
                //if (type_node.value.type_identifier.value.function.return_type) |return_type|
                //{
                    //try std.fmt.format(writer, " -> {}", .{return_type.value.type_identifier});
                //}
                //try writer.writeAll("\n{\n");
                //const block_count = self.value.function_decl.blocks.items.len;
                //if (block_count > 0)
                //{
                    //// @Info: we only need to log the first one since the others are dependent
                    //try std.fmt.format(writer, "{}", .{self.value.function_decl.blocks.items[0]});
                //}
                //try writer.writeAll("}\n");
            //},
            //Node.ID.syscall_decl =>
            //{
                //const syscall_decl = self.value.syscall_decl;
                //try std.fmt.format(writer, "{s} : syscall({}) : (", .{syscall_decl.name, syscall_decl.id});
                //for (syscall_decl.arg_bytes) |bytes|
                //{
                    //if (bytes == 0)
                    //{
                        //break;
                    //}

                    //try std.fmt.format(writer, "u{},", .{bytes * 8});
                //}

                //try writer.writeAll(");");
            //},
            //Node.ID.block_expr =>
            //{
                //for (self.value.block_expr.statements.items) |statement|
                //{
                    //try std.fmt.format(writer, "    {};\n", .{statement});
                //}
            //},
            //Node.ID.var_decl =>
            //{
                    //try std.fmt.format(writer, "{s}: {}", .{self.value.var_decl.name, self.value.var_decl.var_type});
            //},
            //Node.ID.int_lit =>
            //{
                //if (self.value.int_lit.signed)
                //{
                    //try writer.writeAll("-");
                //}
                //try std.fmt.format(writer, "{}", .{self.value.int_lit.value});
            //},
            //Node.ID.array_lit =>
            //{
                //try std.fmt.format(writer, "[", .{});
                //for (self.value.array_lit.elements.items) |array_lit_elem|
                //{
                    //try std.fmt.format(writer, "{}, ", .{array_lit_elem});
                //}
                //try writer.writeAll("]");
            //},
            //Node.ID.struct_lit =>
            //{
                //panic("Struct literal formatting not implemented:\n", .{});
            //},
            //Node.ID.binary_expr =>
            //{
                //if (self.value.binary_expr.parenthesis)
                //{
                    //try writer.writeAll("(");
                //}
                //switch (self.value.binary_expr.id)
                //{
                    //BinaryExpression.ID.Plus =>
                    //{
                        //try std.fmt.format(writer, "{} + {}", .{self.value.binary_expr.left, self.value.binary_expr.right});
                    //},
                    //BinaryExpression.ID.Minus =>
                    //{
                        //try std.fmt.format(writer, "{} - {}", .{self.value.binary_expr.left, self.value.binary_expr.right});
                    //},
                    //BinaryExpression.ID.Multiplication =>
                    //{
                        //try std.fmt.format(writer, "{} * {}", .{self.value.binary_expr.left, self.value.binary_expr.right});
                    //},
                    //BinaryExpression.ID.Assignment =>
                    //{
                        //try std.fmt.format(writer, "{} = {}", .{self.value.binary_expr.left, self.value.binary_expr.right});
                    //},
                    //BinaryExpression.ID.Compare_Equal =>
                    //{
                        //try std.fmt.format(writer, "{} == {}", .{self.value.binary_expr.left, self.value.binary_expr.right});
                    //},
                    //BinaryExpression.ID.Compare_LessThan =>
                    //{
                        //try std.fmt.format(writer, "{} < {}", .{self.value.binary_expr.left, self.value.binary_expr.right});
                    //},
                    //BinaryExpression.ID.Compare_GreaterThan =>
                    //{
                        //try std.fmt.format(writer, "{} > {}", .{self.value.binary_expr.left, self.value.binary_expr.right});
                    //},
                    //else => panic("Not implemented: {}\n", .{self.value.binary_expr.id}),
                //}
                //if (self.value.binary_expr.parenthesis)
                //{
                    //try writer.writeAll(")");
                //}
            //},
            //Node.ID.array_subscript_expr =>
            //{
                //try std.fmt.format(writer, "{}[{}]", .{self.value.array_subscript_expr.expression, self.value.array_subscript_expr.index});
            //},
            //Node.ID.identifier_expr =>
            //{
                //const id_reference_name = self.value.identifier_expr.name;
                //try std.fmt.format(writer, "{s}", .{id_reference_name});
            //},
            //Node.ID.return_expr =>
            //{
                //try std.fmt.format(writer, "return {}", .{self.value.return_expr.expression});
            //},
            //Node.ID.loop_expr =>
            //{
                //try writer.writeAll("while (");
                //for (self.value.loop_expr.prefix.value.block_expr.statements.items) |prefix_st|
                //{
                    //try std.fmt.format(writer, "{}", .{prefix_st});
                //}
                //try writer.writeAll(")\n{\n");
                //for (self.value.loop_expr.body.value.block_expr.statements.items) |loop_st|
                //{
                    //try std.fmt.format(writer, "{};\n", .{loop_st});
                //}
                //for (self.value.loop_expr.postfix.value.block_expr.statements.items) |postfix_st|
                //{
                    //try std.fmt.format(writer, "{};\n", .{postfix_st});
                //}
                //try writer.writeAll("}");
            //},
            //Node.ID.branch_expr =>
            //{
                //try writer.writeAll("if (");
                //try std.fmt.format(writer, "{}", .{self.value.branch_expr.condition});
                //try writer.writeAll(")\n{\n");
                //for (self.value.branch_expr.if_block.value.block_expr.statements.items) |if_st|
                //{
                    //try std.fmt.format(writer, "{};\n", .{if_st});
                //}
                //try writer.writeAll("}\n");
                //if (self.value.branch_expr.else_block) |else_block|
                //{
                    //try writer.writeAll("else\n{\n");
                    //for (else_block.value.block_expr.statements.items) |else_st|
                    //{
                        //try std.fmt.format(writer, "{};\n", .{else_st});
                    //}
                    //try writer.writeAll("}");
                //}
            //},
            //Node.ID.break_expr =>
            //{
                //try writer.writeAll("break");
            //},
            //Node.ID.invoke_expr =>
            //{
                //const invoke_expr = self.value.invoke_expr.expression;
                //switch (invoke_expr.value)
                //{
                    //Node.ID.function_decl =>
                    //{
                        //try std.fmt.format(writer, "{s}(", .{invoke_expr.value.function_decl.name});
                    //},
                    //Node.ID.identifier_expr =>
                    //{
                        //try std.fmt.format(writer, "{s}(", .{invoke_expr.value.identifier_expr.name});
                    //},
                    //else =>
                    //{
                        //panic("ni: {}\n", .{invoke_expr.value});
                    //}
                //}
                //for (self.value.invoke_expr.arguments.items) |arg|
                //{
                    //try std.fmt.format(writer, "{}, ", .{arg});
                //}
                //try writer.writeAll(")");
            //},
            //Node.ID.unary_expr =>
            //{
                //switch (self.value.unary_expr.id)
                //{
                    //UnaryExpression.ID.AddressOf =>
                    //{
                        //try std.fmt.format(writer, "&{}", .{self.value.unary_expr.node_ref});
                    //},
                    //UnaryExpression.ID.Dereference =>
                    //{
                        //try std.fmt.format(writer, "@{}", .{self.value.unary_expr.node_ref});
                    //},
                //}
            //},
            //Node.ID.field_access_expr =>
            //{
                //try std.fmt.format(writer, "{}.{}", .{self.value.field_access_expr.expression, self.value.field_access_expr.field_expr});
            //},
            //Node.ID.type_identifier =>
            //{
                //try std.fmt.format(writer, "{}", .{self.value.type_identifier});
            //},
            //Node.ID.resolved_identifier =>
            //{
                //panic("ni\n", .{});
            //},
            //Node.ID.field_expr =>
            //{
                //try std.fmt.format(writer, "{}", .{self.value.field_expr});
            //},
            //Node.ID.module =>
            //{
            //},
            ////else => panic("Not implemented: {}\n", .{self.value}),
        //}
    //}
//};

//const Intrinsic = struct
//{
    //const Value = union(ID)
    //{
        //syscall: Syscall,
    //};

    //const ID = enum
    //{
        //syscall,
    //};

    //const Syscall = struct
    //{
        //id: u64,
        //arg_bytes: [max_arg_count]u8,
        //name: []const u8,

        //const max_arg_count = 6;

        //const Invocation = struct
        //{
            //declaration: *Syscall,
            //arguments: [max_arg_count]Argument,
        //};
    //};
//};

const Precedence = enum
{
    None,
    Assignment,
    Logical,
    Compare,
    LightArithmetic, // add, sub
    HeavyArithmetic, // mul, div
    Unary,
    Call,
    Declaration,
    Primary,

    fn increment(comptime self: Precedence) Precedence
    {
        return comptime @intToEnum(Precedence, (@enumToInt(self) + 1));
    }
};

//const Parser = struct
//{
    //current_function: *Node,
    //current_block: *Node,

    //ast: *AST,

    //const Self = @This();


    //fn parse_unary_expression(self: *Parser, allocator: *Allocator, parser: *ModuleParser, parent_node: *Node, operator: Operator) *Node
    //{
        //const unary_expr_id = switch (operator)
        //{
            //Operator.AddressOf => UnaryExpression.ID.AddressOf,
            //Operator.Dereference => UnaryExpression.ID.Dereference,
            //else => panic("ni: {}\n", .{operator}),
        //};

        //const unary_expr_node = Node {
            //.value = Node.Value {
                //.unary_expr = UnaryExpression 
                //{
                    //.node_ref = undefined,
                    //.id = unary_expr_id,
                //},
            //},
            //.value_type = Node.ValueType.RValue,
            //.parent = parent_node,
            //.type = undefined,
        //};

        //const unary_expr = self.append_and_get(unary_expr_node);
        //unary_expr.value.unary_expr.node_ref = self.parse_precedence(allocator, parser, unary_expr, Precedence.Unary);

        //return unary_expr;
    //}

    //fn parse_array_literal(self: *Parser, allocator: *Allocator, parser: *ModuleParser, parent_node: *Node) *Node
    //{
        //const array_literal_node = Node
        //{
            //.value = Node.Value {
                //.array_lit = ArrayLiteral {
                    //.elements = NodeRefBuffer.init(allocator),
                //},
            //},
            //.value_type = Node.ValueType.RValue,
            //.parent = parent_node,
            //.type = undefined,
        //};

        //const array_literal = self.append_and_get(array_literal_node);
            
        //if (parser.expect_and_consume_operator(Operator.RightBracket) == null)
        //{
            //while (true)
            //{
                //const array_elem = self.parse_expression(allocator, parser, parent_node);
                //array_literal.value.array_lit.elements.append(array_elem) catch {
                    //panic("Error allocating memory for array literal element\n", .{});
                //};
                
                //const next_token = parser.peek();
                //parser.consume();

                //if (next_token.value == Token.ID.operator and next_token.value.operator == Operator.RightBracket)
                //{
                    //break;
                //}
                //else if (next_token.value == Token.ID.sign and next_token.value.sign != ',')
                //{
                    //parser_error("Expected comma after argument. Found: {}\n", .{next_token.value});
                //}
            //}
        //}
        //else
        //{
            //parser_error("Empty array literal is not allowed\n", .{});
        //}

        //return array_literal;
    //}

    //fn parse_struct_literal(self: *Parser, allocator: *Allocator, parser: *ModuleParser, parent_node: *Node) *Node
    //{
        //var struct_lit_node_value = Node
        //{
            //.value = Node.Value {
                //.struct_lit = StructLiteral {
                    //.field_names = NodeRefBuffer.init(allocator),
                    //.field_expressions = NodeRefBuffer.init(allocator),
                //},
            //},
            //.parent = parent_node,
            //.value_type = Node.ValueType.RValue,
            //.type = undefined,
        //};

        //var struct_lit_node = self.append_and_get(struct_lit_node_value);

        //if (parser.expect_and_consume_sign('}') != null)
        //{
            //panic("Empty struct initialization is not implemented yet\n", .{});
        //}

        //while (true)
        //{
            //if (parser.expect_and_consume_operator(Operator.Dot) != null)
            //{
                //if (parser.expect_and_consume(Token.ID.identifier)) |field_identifier_token|
                //{
                    //const field_identifier = field_identifier_token.value.identifier;
                    //const field_id_node = Node {
                        //.value = Node.Value {
                            //.identifier_expr = IdentifierExpression {
                                //.name = field_identifier,
                            //},
                        //},
                        //.parent = struct_lit_node,
                        //.value_type = Node.ValueType.RValue,
                        //.type = undefined,
                    //};

                    //struct_lit_node.value.struct_lit.field_names.append(self.append_and_get(field_id_node)) catch {
                        //panic("Error appending field identifier in struct literal node\n", .{});
                    //};

                    //if (parser.expect_and_consume_operator(Operator.Assignment) == null)
                    //{
                        //panic("Error: expected assignment token '='\n", .{});
                    //}

                    //struct_lit_node.value.struct_lit.field_expressions.append(self.parse_expression(allocator, parser, struct_lit_node)) catch {
                        //panic("Error appending field initialization expression in struct literal node\n", .{});
                    //};

                    //if (parser.expect_and_consume_sign('}') != null)
                    //{
                        //break;
                    //}
                    //else if (parser.expect_and_consume_sign(',') != null)
                    //{
                        //if (parser.expect_and_consume_sign('}') != null)
                        //{
                            //log("end of struct initializer\n", .{});
                            //break;
                        //}
                    //}
                    //else
                    //{
                        //panic("Not implemented\n", .{});
                    //}
                //}
                //else
                //{
                    //panic("Expecting identifier token\n", .{});
                //}
            //}
            //else
            //{
                //panic("No Dot\n", .{});
            //}
        //}

        //return struct_lit_node;
    //}


    //fn parse_array_subscript(self: *Parser, allocator: *Allocator, parser: *ModuleParser, parent_node: *Node, left_expr: *Node) *Node
    //{
        //const subscript_node_value = Node
        //{
            //.value = Node.Value {
                //.array_subscript_expr = ArraySubscriptExpression {
                    //.expression = left_expr,
                    //.index = undefined,
                //},
            //},
            //.parent = parent_node,
            //.value_type = Node.ValueType.RValue,
            //.type = undefined,
        //};

        //var subscript_node = self.append_and_get(subscript_node_value);
        //subscript_node.value.array_subscript_expr.index = self.parse_expression(allocator, parser, parent_node);

        //if (parser.expect_and_consume_operator(Operator.RightBracket) == null)
        //{
            //parser_error("Expected ']' in array subscript expression", .{});
        //}

        //return subscript_node;
    //}


    //fn parse_binary_expression(self: *Parser, allocator: *Allocator, parser: *ModuleParser, parent_node: *Node, left_expr: *Node, operator: Operator, precedence: Precedence) *Node
    //{
        //const binary_node_value = Node
        //{
            //.value = Node.Value {
                //.binary_expr = BinaryExpression {
                    //.left = left_expr,
                    //.right = undefined,
                    //.id = undefined,
                    //.parenthesis = false,
                //},
                //},
            //.parent = parent_node,
            //.value_type = Node.ValueType.RValue,
            //.type = undefined,
        //};

        //const binary_node = self.append_and_get(binary_node_value);

        //const right_expr = self.parse_precedence(allocator, parser, binary_node, precedence.increment());
        //log("Right expr: {}\n", .{right_expr});
        //binary_node.value.binary_expr.right = right_expr;

        //const binary_op: BinaryExpression.ID = switch (operator)
        //{
            //Operator.Assignment => BinaryExpression.ID.Assignment,
            //Operator.Plus => BinaryExpression.ID.Plus,
            //Operator.Minus => BinaryExpression.ID.Minus,
            //Operator.Multiplication => BinaryExpression.ID.Multiplication,
            //Operator.Equal => BinaryExpression.ID.Compare_Equal,
            //Operator.GreaterThan => BinaryExpression.ID.Compare_GreaterThan,
            //else => panic("not implemented: {}\n", .{operator}),
        //};
        //binary_node.value.binary_expr.id = binary_op;

        //if (binary_op == BinaryExpression.ID.Assignment)
        //{
            //left_expr.value_type = Node.ValueType.LValue;
        //}

        //log("New binary expression: {}\n", .{binary_node});
        //return binary_node;
    //}






    //fn parse_intrinsic(self: *Parser, allocator: *Allocator, parser: *ModuleParser, intrinsic_name: []const u8) void
    //{
        //if (std.meta.stringToEnum(Intrinsic.ID, intrinsic_name)) |intrinsic_id|
        //{
            //log("Intrinsic found: {}\n", .{intrinsic_id});
            //switch (intrinsic_id)
            //{
                //Intrinsic.ID.syscall =>
                //{
                    //if (parser.expect_and_consume_operator(Operator.LeftParenthesis) == null)
                    //{
                        //panic("expected left parenthesis to indicate system call id\n", .{});
                    //}

                    //if (parser.expect_and_consume(Token.ID.int_lit)) |syscall_id_token|
                    //{
                        //const syscall_id = syscall_id_token.value.int_lit;
                        //if (parser.expect_and_consume_operator(Operator.RightParenthesis) == null)
                        //{
                            //panic("expected right parenthesis\n", .{});
                        //}

                        //if (parser.expect_and_consume_operator(Operator.Declaration) == null)
                        //{
                            //panic("expected colon\n", .{});
                        //}

                        //if (parser.expect_and_consume_operator(Operator.LeftParenthesis) == null)
                        //{
                            //panic("expected left parenthesis\n", .{});
                        //}

                        //var syscall = std.mem.zeroes(Intrinsic.Syscall);
                        //syscall.id = syscall_id;
                        //var argument_count: u64 = 0;

                        //while (true) : (argument_count += 1)
                        //{
                            //if (argument_count >= 6)
                            //{
                                //panic("Too many arguments for syscall {}: {}\n", .{syscall_id, argument_count});
                            //}
                            //if (parser.expect_and_consume(Token.ID.identifier) == null)
                            //{
                                //panic("expected identifier\n", .{});
                            //}

                            //if (parser.expect_and_consume_operator(Operator.Declaration) == null)
                            //{
                                //panic("expected colon\n", .{});
                            //}

                            //const arg_type = self.parse_type(allocator, parser, null);
                            //const byte_count: u8 = blk:
                            //{
                                //switch (arg_type.value)
                                //{
                                    //Node.ID.type_identifier =>
                                    //{
                                        //const type_identifier = arg_type.value.type_identifier;
                                        //const type_kind = type_identifier.value;
                                        //switch (type_kind)
                                        //{
                                            //TypeIdentifier.ID.simple =>
                                            //{
                                                //const type_str = type_kind.simple;
                                                //if (std.mem.eql(u8, type_str, "u8") or std.mem.eql(u8, type_str, "s8"))
                                                //{
                                                    //break :blk 1;
                                                //}
                                                //else if (std.mem.eql(u8, type_str, "u16") or std.mem.eql(u8, type_str, "s16"))
                                                //{
                                                    //break :blk 2;
                                                //}
                                                //else if (std.mem.eql(u8, type_str, "u32") or std.mem.eql(u8, type_str, "s32"))
                                                //{
                                                    //break :blk 4;
                                                //}
                                                //else if (std.mem.eql(u8, type_str, "u64") or std.mem.eql(u8, type_str, "s64"))
                                                //{
                                                    //break :blk 8;
                                                //}
                                            //},
                                            //TypeIdentifier.ID.pointer =>
                                            //{
                                                //break :blk 8;
                                            //},
                                            //else => panic("ni: {}\n", .{type_kind}),
                                        //}
                                    //},
                                    //else => panic("ni: {}\n", .{arg_type.value}),
                                //}

                                //panic("byte count couldn't be figured out\n", .{});
                            //};

                            //syscall.arg_bytes[argument_count] = byte_count;
                            //log("Byte count: {}\n", .{byte_count});

                            //const next_token = parser.peek();
                            //parser.consume();
                            //switch (next_token.value)
                            //{
                                //Token.ID.sign =>
                                //{
                                    //const sign = next_token.value.sign;
                                    //switch (sign)
                                    //{
                                        //',' => {},
                                        //else => panic("ni: {c}\n", .{sign}),
                                    //}
                                //},
                                //Token.ID.operator =>
                                //{
                                    //const operator = next_token.value.operator;
                                    //switch (operator)
                                    //{
                                        //Operator.RightParenthesis =>
                                        //{
                                            //break;
                                        //},
                                        //else => panic("ni: {}\n", .{operator}),
                                    //}
                                //},
                                //else => panic("ni: {}\n", .{next_token.value}),
                            //}
                        //}

                        //if (parser.expect_and_consume_sign(';') == null)
                        //{
                            //panic("expected ';' at the end of syscall statement", .{});
                        //}

                        //const syscall_value = Node
                        //{
                            //.value = Node.Value {
                                //.syscall_decl = syscall,
                            //},
                            //.parent = null,
                            //.value_type = Node.ValueType.RValue,
                            //.type = undefined,
                        //};

                        //const syscall_node = self.append_and_get(syscall_value);

                        //self.syscalls.append(syscall_node) catch {
                            //panic("Error appending syscall\n", .{});
                        //};
                    //}
                    //else
                    //{
                        //panic("Expected syscall id\n", .{});
                    //}
                //},
            //}
        //}
        //else
        //{
            //panic("Intrinsic not found: {s}\n", .{intrinsic_name});
        //}
    //}



//};

//pub const UnaryExpression = struct
//{
    //node_ref: *Node,
    //id: ID,

    //pub const ID = enum
    //{
        //AddressOf,
        //Dereference,
    //};
//};


//const BlockExpression = struct
//{
    //statements: NodeRefBuffer,
    //id: ID,

    //const ID = enum {
        //LoopPrefix,
        //LoopBody,
        //LoopPostfix,
        //IfBlock,
        //ElseBlock,
        //Function,
    //};
//};

//const ArraySubscriptExpression = struct
//{
    //expression: *Node,
    //index: *Node,
//};

//const ArrayLiteral = struct
//{
    //elements: NodeRefBuffer,
//};

//const StructLiteral = struct
//{
    //field_names: NodeRefBuffer,
    //field_expressions: NodeRefBuffer,
//};
