const std = @import("std");
const assert = std.debug.assert;
const print = std.debug.print;
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
usingnamespace @import("entity.zig");

const log = std.log.scoped(.parser);

pub fn parser_error(comptime format: []const u8, args: anytype) noreturn
{
    panic(format, args);
}

const IntegerLiteral = struct
{
    value: u64,
    signed: bool,
    // padding
};

const FieldAccessExpression = struct
{
    left_expression: Entity,
    field_expression: Entity,
};

pub const VariableDeclaration = struct
{
    name: []const u8,
    type: Type,
//const VariableDeclaration = struct
//{
    //name: []const u8,
    //var_type: *Node,
    //var_scope: *Node,
    //backend_ref: usize,
    //is_function_arg: bool,
//};
};

pub const IdentifierExpression = struct
{
    name: []const u8,
};

pub const Scope = struct
{
    statements: []Entity,
    variable_declarations: []VariableDeclaration,
    identifier_expressions: []IdentifierExpression,
    invoke_expressions: []InvokeExpression,
    field_access_expressions: []FieldAccessExpression,
    integer_literals: []IntegerLiteral,
};

pub const Function = struct
{
    argument_names: [][]const u8,
    name: []const u8,
    function_type: Type,

    pub const Attribute = enum
    {
        @"noreturn",
        @"extern",
    };

    pub const External = struct
    {
        base: Function,
        library: []const u8,
    };

    pub const Internal = struct
    {
        declaration: Function,
        scopes: []Scope,
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
    lexer: TokenWalker,
    function_builder: FunctionBuilder,
    module_builder: ModuleBuilder,
    allocator: *Allocator,

    const Self = @This();
    const CountersType = [TokenTypeCount]u32;

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

    const ScopeBuilder = struct
    {
        statements: ArrayList(Entity),
        variable_declarations: ArrayList(VariableDeclaration),
        identifier_expressions: ArrayList(IdentifierExpression),
        invoke_expressions: ArrayList(InvokeExpression),
        field_access_expressions: ArrayList(FieldAccessExpression),
        integer_literals: ArrayList(IntegerLiteral),
    };

    const FunctionBuilder = struct
    {
        scope_builders: ArrayList(ScopeBuilder),
        scopes: ArrayList(Scope),
        current_scope: u32,
    };

    const ModuleBuilder = struct
    {
        internal_functions: ArrayList(Function.Internal),
        external_functions: ArrayList(Function.External),
        imported_modules: ArrayList(ImportedModule),
        unresolved_types: ArrayList([]const u8),
        pointer_types: ArrayList(Type.Pointer),
        slice_types: ArrayList(Type.Slice),
        function_types: ArrayList(Type.Function),
        array_types: ArrayList(Type.Array),
        struct_types: ArrayList(Type.Struct),
    };

    fn get_and_consume_token(self: *Self, comptime token: Token) TokenTypeMap[@enumToInt(token)]
    {
        //print("Getting and consuming {}...\n", .{token});
        const token_to_consume = self.lexer.tokens[self.lexer.next_index];
        //print("Token to be consumed: {}\n", .{token_to_consume});
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
        //print("Consuming {}...\n", .{token});
        self.lexer.counters[@enumToInt(token)] += 1;
        self.lexer.next_index += 1;
    }

    fn rectify(self: *Self, comptime token_to_rectify: Token) callconv(.Inline) void
    {
        //print("Rectifying {}...\n", .{token_to_rectify});
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
        const left_expression = Entity.new(self.function_builder.scope_builders.items[self.function_builder.current_scope].identifier_expressions.items.len, Entity.ScopeID.identifier_expressions);
        self.function_builder.scope_builders.items[self.function_builder.current_scope].identifier_expressions.append(.{ .name = identifier_name }) catch unreachable;
        return left_expression;
    }

    fn parse_precedence_identifier(self: *Self, comptime precedence: Precedence) Entity
    {
        const identifier_name = self.get_and_consume_token(.identifier).value;
        const left_expression = Entity.new(self.function_builder.scope_builders.items[self.function_builder.current_scope].identifier_expressions.items.len, Entity.ScopeID.identifier_expressions);
        self.function_builder.scope_builders.items[self.function_builder.current_scope].identifier_expressions.append(.{ .name = identifier_name }) catch unreachable;
        return self.parse_infix(precedence, left_expression);
    }

    fn parse_precedence(self: *Self, comptime precedence: Precedence) Entity
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
                    const integer_value = self.get_and_consume_token(.int_lit).value;
                    const integer_literal_id = Entity.new(self.function_builder.scope_builders.items[self.function_builder.current_scope].integer_literals.items.len, Entity.ScopeID.integer_literals);
                    self.function_builder.scope_builders.items[self.function_builder.current_scope].integer_literals.append(.
                        {
                            .value = integer_value,
                            // @TODO: implement signedness parsing
                            .signed = false,
                        }) catch unreachable;

                    break :blk integer_literal_id;
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

    fn parse_infix(self: *Self, comptime precedence: Precedence, left_expr: Entity) Entity
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
                left_expression = blk:
                {
                    switch (operator)
                    {
                        .Equal,
                        .GreaterThan,
                        .Assignment,
                        .Plus,
                        .Minus,
                        .Multiplication =>
                        {
                            //break :blk self.parse_binary_expression(allocator, self, parent_node, left_expr, operator, new_precedence);
                            unreachable;
                        },
                        .LeftParenthesis =>
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
                        .Dot =>
                        {
                            const field_access_expression_id = Entity.new(self.function_builder.scope_builders.items[self.function_builder.current_scope].field_access_expressions.items.len, Entity.ScopeID.field_access_expressions);
                            self.function_builder.scope_builders.items[self.function_builder.current_scope].field_access_expressions.append(
                                .{
                                    .left_expression = left_expression,
                                    .field_expression = self.parse_precedence(comptime Precedence.Call.increment()),
                                }) catch unreachable;

                            break :blk field_access_expression_id;
                        },
                        else => panic("operator not implemented: {}\n", .{operator}),
                    }
                };
            }
        }

        return left_expression;
    }

    //fn parse_precedence(self: *Parser, allocator: *Allocator, parser: *ModuleParser, parent_node: *Node, precedence: Precedence) *Node
    //{
        //const prefix_node = self.parse_prefix(allocator, parser, parent_node);
        //var left_expr = prefix_node;

        //while (true)
        //{
            //const token = parser.peek();
            //const new_precedence = switch (token.value)
            //{
                //Token.ID.sign => sign_block:
                //{
                    //const sign = token.value.sign;
                    //const sign_precedence = switch (sign)
                    //{
                        //'{' => Precedence.None,
                        //';' => Precedence.None,
                        //',' => Precedence.None,
                        //'}' => Precedence.None,
                        //else => panic("Precedence not implemented for sign {c}\n", .{sign}),
                    //};

                    //break :sign_block sign_precedence;
                //},
                //else => panic("Precedence not implemented for {}\n", .{token.value}),
            //};

            //log.debug("Old precedence: {}, new precedence: {}\n", .{precedence, new_precedence});
        //}

        //return left_expr;
    //}
    
    //fn parse_prefix(self: *Parser, allocator: *Allocator, parser: *ModuleParser, parent_node: *Node) *Node
    //{
        //const token = parser.peek();
        //parser.consume();

        //switch (token.value)
        //{
            //Token.ID.identifier =>
            //{
                //const identifier_name = token.value.identifier;
                //const id_expr_node_value = Node
                //{
                    //.value = Node.Value {
                        //.identifier_expr = IdentifierExpression {
                            //.name = identifier_name,
                        //},
                    //},
                    //.parent = parent_node,
                    //.value_type = Node.ValueType.RValue,
                    //.type = undefined,
                //};
                //const id_expr_node = self.append_and_get(id_expr_node_value);
                //return id_expr_node;
            //},
            //Token.ID.int_lit =>
            //{
            //},
            //Token.ID.operator =>
            //{
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
            //},
            //Token.ID.sign =>
            //{
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
            //},
            //else => panic("Prefix functionality not implemented for {}\n", .{token.value}),
        //}
    //}

    fn parse_statement(self: *Self) void
    {
        const next_token = self.lexer.tokens[self.lexer.next_index];
        switch (next_token)
        {
            .identifier =>
            {
                const token_to_maybe_rectify = Token.identifier;
                const identifier_name = self.get_and_consume_token(token_to_maybe_rectify).value;

                if (self.lexer.tokens[self.lexer.next_index] == .operator and self.get_token(.operator).value == .Declaration)
                {
                    _ = identifier_name;
                    self.consume_token(.operator);


                    //var var_decl_node_value = Node
                    //{
                    //.value = Node.Value {
                    //.var_decl = VariableDeclaration {
                    //.name = identifier_name,
                    //// Info: These are set later 
                    //.var_type =  undefined,
                    //.var_scope =  block_node,
                    //.backend_ref =  0,
                    //.is_function_arg = false,
                    //},
                    //},
                    //.parent = block_node,
                    //.value_type = Node.ValueType.LValue,
                    //.type = undefined,
                    //};

                    //var var_decl_node = self.append_and_get(var_decl_node_value);
                    //if (self.expect_and_consume_operator(Operator.Assignment) == null)
                    //{
                    //var_decl_node.value.var_decl.var_type = self.parse_type(allocator, self, block_node);

                    //if (self.expect_and_consume_operator(Operator.Assignment) == null)
                    //{
                    //panic("expected assignment after type in variable declaration\n", .{});
                    //}
                    //}
                    //else
                    //{
                    //// no type information
                    //panic("not implemented: var declaration without type information\n", .{});
                    //}

                    //var initialization_assignment: ?*Node = null;
                    //// @Info: separate variable declaration and initialization
                    //if (self.expect_sign(';') == null)
                    //{
                    //const id_expr = Node 
                    //{
                    //.value = Node.Value {
                    //.identifier_expr = IdentifierExpression {
                    //.name = identifier_name,
                    //},
                    //},
                    //.value_type = Node.ValueType.LValue,
                    //.parent = block_node,
                    //.type = undefined,
                    //};

                    //const id_expr_node = self.append_and_get(id_expr);

                    //initialization_assignment = self.parse_binary_expression(allocator, self, block_node, id_expr_node, Operator.Assignment, Precedence.Assignment);
                    //}
                    //else
                    //{
                    //// no initialization
                    //panic("not implemented: var declaration without initialization\n", .{});
                    //}

                    //if (parser.expect_and_consume_sign(';') == null)
                    //{
                    //parser_error("Expected semicolon at the end of the statement\n", .{});
                    //}
                    //block_node.value.block_expr.statements.append(var_decl_node) catch {
                    //panic("Failed to allocate memory for statement", .{});
                    //};
                    //self.current_function.value.function_decl.variables.append(var_decl_node) catch {
                    //panic("Error allocating variable reference to function variable list\n", .{});
                    //};
                    //if (initialization_assignment) |assignment|
                    //{
                    //block_node.value.block_expr.statements.append(assignment) catch {
                    //panic("Failed to allocate memory for statement", .{});
                    //};
                    //}

                    unreachable;
                }
                else
                {
                    self.rectify(token_to_maybe_rectify);
                    const identifier_expression = self.parse_expression_identifier();
                    _ = identifier_expression;
                    if (self.lexer.tokens[self.lexer.next_index] != .sign or self.get_and_consume_token(.sign).value != ';')
                    {
                        parser_error("Expected semicolon at the end of identifier expression\n", .{});
                    }

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
                        if (self.lexer.tokens[self.lexer.next_index] != .sign or self.get_and_consume_token(.sign).value != ';')
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
                    },
                    else => panic("ni: {}\n", .{operator}),
                }
            },
            else => panic("ni: {}\n", .{next_token}),
        }
        //while (should_keep_parsing)
        //{
            //const token = parser.peek();

            //switch (token.value)
            //{
                //Token.ID.keyword =>
                //{
                    //const keyword = token.value.keyword;
                    //switch (keyword)
                    //{
                        //KeywordID.@"return" =>
                        //{
                            //const return_statement = self.parse_return(allocator, parser, block_node);
                            //if (parser.expect_and_consume_sign(';') == null)
                            //{
                                //parser_error("Expected semicolon at the end of the statement\n", .{});
                            //}
                            //block_node.value.block_expr.statements.append(return_statement) catch {
                                //panic("Failed to allocate memory for statement", .{});
                            //};
                        //},
                        //KeywordID.@"for" =>
                        //{
                            //const for_st = self.parse_for(allocator, parser, block_node);
                            //block_node.value.block_expr.statements.append(for_st) catch {
                                //panic("Failed to allocate memory for statement", .{});
                            //};

                        //},
                        //KeywordID.@"if" =>
                        //{
                            //const if_st = self.parse_if(allocator, parser, block_node);
                            //block_node.value.block_expr.statements.append(if_st) catch {
                                //panic("Failed to allocate memory for statement", .{});
                            //};
                        //},
                        //KeywordID.@"break" =>
                        //{
                            //const break_st = self.parse_break(parser, block_node);
                            //if (parser.expect_and_consume_sign(';') == null)
                            //{
                                //parser_error("Expected semicolon at the end of the statement\n", .{});
                            //}
                            //block_node.value.block_expr.statements.append(break_st) catch {
                                //panic("Failed to allocate memory for statement", .{});
                            //};
                        //},
                        //else =>
                        //{
                            //panic("Keyword unhandled: {}\n", .{keyword});
                        //},
                    //}

                //},
                //else =>
                //{
                    //panic("ni: {}\n", .{token.value});
                //},
            //}

            //should_keep_parsing = has_braces and parser.expect_sign('}') == null;
        //}

        //if (has_braces)
        //{
            //const end = parser.expect_and_consume_sign('}');
            //if (end == null)
            //{
                //parser_error("Expected end sign at the end of the block", .{});
            //}
        //}
    //}
    }

    fn parse_scope(self: *Self) void
    {
        const previous_scope = self.function_builder.current_scope;

        self.function_builder.current_scope = @intCast(u32, self.function_builder.scope_builders.items.len);
        self.function_builder.scopes.append(undefined) catch unreachable;
        self.function_builder.scope_builders.append(ScopeBuilder
        {
            .statements = ArrayList(Entity).init(self.allocator),
            .variable_declarations = ArrayList(VariableDeclaration).init(self.allocator),
            .identifier_expressions = ArrayList(IdentifierExpression).init(self.allocator),
            .invoke_expressions = ArrayList(InvokeExpression).init(self.allocator),
            .field_access_expressions = ArrayList(FieldAccessExpression).init(self.allocator),
            .integer_literals = ArrayList(IntegerLiteral).init(self.allocator),
        }) catch unreachable;

        // @TODO: should we move this outside the function?
        const expected_left_brace = self.lexer.tokens[self.lexer.next_index];
        if (expected_left_brace != .sign and self.get_token(.sign).value != '{')
        {
            parser_error("Expected left brace to open up the function body\n", .{});
        }

        self.consume_token(.sign);

        var next_token = self.lexer.tokens[self.lexer.next_index];
        var block_ends = next_token == .sign and self.get_token(.sign).value == '}';

        while (!block_ends):
        ({
            next_token = self.lexer.tokens[self.lexer.next_index];
            block_ends = next_token == .sign and self.get_token(.sign).value == '}';
        })
        {
            self.parse_statement();
        }

        if (self.lexer.tokens[self.lexer.next_index] != .sign or self.get_and_consume_token(.sign).value != '}')
        {
            parser_error("Expected closing brace for scope\n", .{});
        }

        var scope_builder = &self.function_builder.scope_builders.items[self.function_builder.current_scope];
        // @TODO: DEBUG THIS SHIT
        self.function_builder.scopes.items[self.function_builder.current_scope] = .
        {
            .statements = scope_builder.statements.items,
            .variable_declarations = scope_builder.variable_declarations.items,
            .identifier_expressions = scope_builder.identifier_expressions.items,
            .invoke_expressions = scope_builder.invoke_expressions.items,
            .field_access_expressions = scope_builder.field_access_expressions.items,
            .integer_literals = scope_builder.integer_literals.items,
        };

        self.function_builder.current_scope = previous_scope;
    }
    
    fn add_unresolved_type(self: *Self, type_identifier: []const u8) Type
    {
        for (self.module_builder.unresolved_types.items) |t, i|
        {
            if (std.mem.eql(u8, type_identifier, t))
            {
                return Type.new_unresolved_type(i);
            }
        }

        const i = self.module_builder.unresolved_types.items.len;
        print("INDEX: {}\n", .{i});
        self.module_builder.unresolved_types.append(type_identifier) catch unreachable;
        return Type.new_unresolved_type(i);
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
                        _ = os;

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
                                            self.parse_statement();
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
                                        self.parse_statement();
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
                                                print("Closing braces...\n", .{});
                                                open_braces -= 1;
                                                if (open_braces > 0)
                                                {
                                                    self.consume_token(.sign);
                                                }
                                            }
                                            else if (sign == '{')
                                            {
                                                print("Opening braces...\n", .{});
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

            return Type.Function.new(i);
        }

        return Type.Function.append(&self.module_builder.function_types, .{
            .return_type = return_type,
            .argument_types = argument_types,
            .attributes = attributes,
        });
    }
};

pub const InvokeExpression = struct
{
    arguments: []Entity,
    expression: Entity,
};

pub const ImportedModule = Entity;

pub const Module = struct
{
    internal_functions: []Function.Internal,
    external_functions: []Function.External,
    imported_modules: []ImportedModule,
    unresolved_types: [][]const u8,
    pointer_types: []Type.Pointer,
    slice_types: []Type.Slice,
    function_types: []Type.Function,
    array_types: []Type.Array,
    struct_types: []Type.Struct,
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
        log.debug("\n==============\nPARSER\n==============\n\n", .{});

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

        print("Parsing module #{} \"{s}\"\n", .{module_index, source_file});
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
                .imported_modules = ArrayList(ImportedModule).init(allocator),
                .unresolved_types = ArrayList([]const u8).init(allocator),
                .pointer_types = ArrayList(Type.Pointer).init(allocator),
                .slice_types = ArrayList(Type.Slice).init(allocator),
                .function_types = ArrayList(Type.Function).init(allocator),
                .array_types = ArrayList(Type.Array).init(allocator),
                .struct_types = ArrayList(Type.Struct).init(allocator),
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
            _ = tld_name;

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
                            print("Parsing function {s}...\n", .{tld_name});
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
                            const return_type = if (arrow_or == .operator and parser.get_token(.operator).value == .Arrow) parser.parse_type() else Type.Builtin.void_type;
                            print("Return type: {}\n", .{return_type});

                            var next = parser.lexer.tokens[parser.lexer.next_index];
                            var attributes: u64 = 0;
                            var extern_library_name: []const u8 = undefined;

                            // Loop around a different premise, this is error-prone
                            while (next == .keyword) : (next = parser.lexer.tokens[parser.lexer.next_index])
                            {
                                const kw = parser.get_and_consume_token(.keyword).value;
                                switch (kw)
                                {
                                    .@"noreturn" => attributes |= 1 << @enumToInt(Function.Attribute.@"noreturn"),
                                    .@"extern" =>
                                    {
                                        print("Parsing extern function\n", .{});
                                        attributes |= 1 << @enumToInt(Function.Attribute.@"extern");
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

                                        print("Library name: {s}\n", .{extern_library_name});
                                    },
                                    else => panic("ni: {}\n", .{kw}),
                                }
                            }

                            const has_body = (attributes & (1 << @enumToInt(Function.Attribute.@"extern"))) >> @enumToInt(Function.Attribute.@"extern") == 0;


                            const is_no_return = (attributes & (1 << @enumToInt(Function.Attribute.@"noreturn")) >> @enumToInt(Function.Attribute.@"noreturn")) != 0;
                            const function_type = parser.get_function_type(if (is_no_return) Type.Builtin.noreturn_type else return_type, argument_type_list.items, attributes);

                            print("Return type for \"{s}\": {}\n", .{tld_name, function_type});

                            if (has_body)
                            {
                                //const current_function_index = parser.module_builder.internal_functions.items.len;
                                //_ = current_function_index;
                                parser.function_builder = ModuleParser.FunctionBuilder
                                {
                                    .scope_builders = ArrayList(ModuleParser.ScopeBuilder).init(allocator),
                                    .scopes = ArrayList(Scope).init(allocator),
                                    .current_scope = 0,
                                };
                                parser.parse_scope();

                                parser.module_builder.internal_functions.append(Function.Internal
                                    {
                                        .declaration = .
                                        {
                                            .argument_names = argument_name_list.items,
                                            .name = tld_name,
                                            .function_type = function_type,
                                        },
                                        .scopes = parser.function_builder.scopes.items,
                                    }) catch unreachable;
                                print("[#############] Scope count: {}\n", .{parser.function_builder.scopes.items.len});
                            }
                            else
                            {
                                if (parser.lexer.tokens[parser.lexer.next_index] != .sign)
                                {
                                    parser_error("Expected semicolon after extern function declaration\n", .{});
                                }
                                const foo = parser.get_and_consume_token(.sign);
                                print("Foo value: {c}\n", .{foo.value});
                                if (foo.value != ';')
                                {
                                    parser_error("Expected semicolon after extern function declaration\n", .{});
                                }

                                parser.module_builder.external_functions.append(.{
                                    .base = .
                                    {
                                        .argument_names = argument_name_list.items,
                                        .name = tld_name,
                                        .function_type = function_type,
                                    },
                                    .library = extern_library_name,
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
                            print("File name to be imported: {s}\n", .{import_file_name});

                            if (parser.lexer.tokens[parser.lexer.next_index] != .operator or parser.get_and_consume_token(.operator).value != .RightParenthesis)
                            {
                                parser_error("Expected right parenthesis after import file name\n", .{});
                            }

                            if (parser.lexer.tokens[parser.lexer.next_index] != .sign or parser.get_and_consume_token(.sign).value != ';')
                            {
                                parser_error("Expected semicolon after import statement\n", .{});
                            }

                            const import_module_id = self.import_module(parser.allocator, import_file_name, target, module_id);
                            parser.module_builder.imported_modules.append(import_module_id) catch unreachable;
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
            .imported_modules = parser.module_builder.imported_modules.items,
            .unresolved_types = parser.module_builder.unresolved_types.items,
            .pointer_types = parser.module_builder.pointer_types.items,
            .slice_types = parser.module_builder.slice_types.items,
            .function_types = parser.module_builder.function_types.items,
            .array_types = parser.module_builder.array_types.items,
            .struct_types = parser.module_builder.struct_types.items,
        };

        self.module_names[module_index] = source_file;

        return module_id;
    }

    pub fn import_module(self: *Self, allocator: *Allocator, source_file: []const u8, target: std.Target, parent_module: Entity) Entity
    {
        for (self.module_names[0..self.module_len]) |module_name, i|
        {
            if (std.mem.eql(u8, module_name, source_file))
            {
                print("Already import module \"{s}\" with ID {}\n", .{source_file, i});
                return Entity.new(i, Entity.GlobalID.modules);
            }
        }

        return self.lex_and_parse_module(allocator, source_file, target, parent_module);
    }
};

//pub const Identifier = packed struct
//{
    //index: u32 = 0,
    //features: u32 = 0,

    //const Self = @This();
    
    //pub const resolved_position = @bitSizeOf(u32) - 1;

    //pub const Level = enum(IntType)
    //{
        //builtin,
        //global,
        //module,
        ////function,
        //scope,

        //const position = resolved_position - @bitSizeOf(Level);
        //const IntType = u2;
    //};

    //pub const ArrayIDEnumType = std.meta.Int(std.builtin.Signedness.unsigned, Level.position);
    //pub const BuiltinID = enum(ArrayIDEnumType)
    //{
        //void_type,
        //noreturn_type,
        //os,
    //};

    //pub const GlobalID = enum(ArrayIDEnumType)
    //{
        //modules,
    //};

    //pub const ModuleID = enum(ArrayIDEnumType)
    //{
        //internal_functions,
        //external_functions,
        //imported_modules,
        //unresolved_types,
        //pointer_types,
        //slice_types,
        //function_types,
        //array_types,
        //struct_types,
    //};

    //pub const ScopeID = enum(ArrayIDEnumType)
    //{
        //statements,
        //variable_declarations,
        //identifier_expressions,
        //invoke_expressions,
        //field_access_expressions,
        //integer_literals,
    //};

    //comptime
    //{
        //assert(@sizeOf(Identifier) == @sizeOf(u64));
    //}

    //fn new(base_index: u64, comptime array_id: anytype) Identifier
    //{
        //const level = comptime switch(@TypeOf(array_id))
        //{
            //BuiltinID => Level.builtin,
            //GlobalID => Level.global,
            //ModuleID => Level.module,
            //ScopeID => Level.scope,
            //else => unreachable,
        //};

        //const result = Identifier
        //{
            //.index = @intCast(u32, base_index),
            //.features = (@as(u32, @enumToInt(level)) << Level.position) | @enumToInt(array_id),
        //};

        //const type_match = (@TypeOf(array_id) == ModuleID);
        //if (type_match)
        //{
            //if (array_id == Identifier.ModuleID.unresolved_types)
            //{
                //print("[#] New identifier: {} {} {}\n{}\n", .{base_index, level, array_id, result});
            //}
        //}

        //return result;
    //}

    //fn compare(self: Self, other: Self) bool
    //{
        //return self.index == other.index and self.features == other.features;
    //}

    //// @TODO: make this compile time and fast
    //fn from_builtin_id(comptime id: BuiltinID) Identifier
    //{
        //return comptime Identifier
        //{
            //.index = @intCast(u32, id_integer),
            //.features = (@enumToInt(Level.builtin) << Level.position) | @as(u32, @enumToInt(id)),
        //};
    //}

    //pub fn get_void_type() Type
    //{
        //return Identifier.new(0, Identifier.BuiltinID.void_type);
    //}

    //pub fn get_noreturn_type() Type
    //{
        //return Identifier.new(0, Identifier.BuiltinID.noreturn_type);
    //}

    //pub fn get_builtin_os() Identifier
    //{
        //return Identifier.new(0, Identifier.BuiltinID.os);
    //}

    //pub fn get_level(self: Self) Level
    //{
        //return @intToEnum(Level, (self.features & (std.math.maxInt(Level.IntType) << Level.position)) >> Level.position);
    //}

    //pub fn get_array_index(self: Self) ArrayIDEnumType
    //{
        //return @truncate(ArrayIDEnumType, self.features);
    //}

    //pub fn is_resolved(self: Self) callconv(.Inline) bool
    //{
        //return ((self.features & @as(u32, 0x80000000)) >> 31) != 0;
    //}
//};

pub const Node = struct
{
    value: Value,
    parent: ?*Node,
    value_type: ValueType,
    type: *Type,

    pub const Value = union(ID)
    {
        var_decl: VariableDeclaration,
        function_decl: FunctionDeclaration,
        syscall_decl: Intrinsic.Syscall,
        int_lit: IntegerLiteral,
        array_lit: ArrayLiteral,
        struct_lit: StructLiteral,
        unary_expr: UnaryExpression,
        binary_expr: BinaryExpression,
        return_expr: ReturnExpression,
        identifier_expr: IdentifierExpression,
        resolved_identifier: *Node,
        field_expr: *Type.Struct.Field,
        invoke_expr: InvokeExpression,
        block_expr: BlockExpression,
        branch_expr: BranchExpression,
        loop_expr: LoopExpression,
        break_expr: BreakExpression,
        array_subscript_expr: ArraySubscriptExpression,
        field_access_expr: FieldAccessExpression,
        module: Module,
    };

    pub const ID = enum
    {
        var_decl,
        function_decl,
        syscall_decl,
        int_lit,
        array_lit,
        struct_lit,
        unary_expr,
        binary_expr,
        return_expr,
        identifier_expr,
        resolved_identifier,
        field_expr,
        invoke_expr,
        block_expr,
        branch_expr,
        loop_expr,
        break_expr,
        array_subscript_expr,
        field_access_expr,
        module,
    };

    pub const ValueType = enum
    {
        RValue,
        LValue,
    };

    pub fn format(self: *const Node, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void
    {
        _ = fmt;

        switch (self.value)
        {
            Node.ID.function_decl =>
            {
                try std.fmt.format(writer, "{s} :: (", .{self.value.function_decl.name});
                for (self.value.function_decl.arguments.items) |arg|
                {
                    try std.fmt.format(writer, "{}, ", .{arg});
                }
                try writer.writeAll(")");
                const type_node = self.value.function_decl.type;
                if (type_node.value.type_identifier.value.function.return_type) |return_type|
                {
                    try std.fmt.format(writer, " -> {}", .{return_type.value.type_identifier});
                }
                try writer.writeAll("\n{\n");
                const block_count = self.value.function_decl.blocks.items.len;
                if (block_count > 0)
                {
                    // @Info: we only need to print the first one since the others are dependent
                    try std.fmt.format(writer, "{}", .{self.value.function_decl.blocks.items[0]});
                }
                try writer.writeAll("}\n");
            },
            Node.ID.syscall_decl =>
            {
                const syscall_decl = self.value.syscall_decl;
                try std.fmt.format(writer, "{s} : syscall({}) : (", .{syscall_decl.name, syscall_decl.id});
                for (syscall_decl.arg_bytes) |bytes|
                {
                    if (bytes == 0)
                    {
                        break;
                    }

                    try std.fmt.format(writer, "u{},", .{bytes * 8});
                }

                try writer.writeAll(");");
            },
            Node.ID.block_expr =>
            {
                for (self.value.block_expr.statements.items) |statement|
                {
                    try std.fmt.format(writer, "    {};\n", .{statement});
                }
            },
            Node.ID.var_decl =>
            {
                    try std.fmt.format(writer, "{s}: {}", .{self.value.var_decl.name, self.value.var_decl.var_type});
            },
            Node.ID.int_lit =>
            {
                if (self.value.int_lit.signed)
                {
                    try writer.writeAll("-");
                }
                try std.fmt.format(writer, "{}", .{self.value.int_lit.value});
            },
            Node.ID.array_lit =>
            {
                try std.fmt.format(writer, "[", .{});
                for (self.value.array_lit.elements.items) |array_lit_elem|
                {
                    try std.fmt.format(writer, "{}, ", .{array_lit_elem});
                }
                try writer.writeAll("]");
            },
            Node.ID.struct_lit =>
            {
                panic("Struct literal formatting not implemented:\n", .{});
            },
            Node.ID.binary_expr =>
            {
                if (self.value.binary_expr.parenthesis)
                {
                    try writer.writeAll("(");
                }
                switch (self.value.binary_expr.id)
                {
                    BinaryExpression.ID.Plus =>
                    {
                        try std.fmt.format(writer, "{} + {}", .{self.value.binary_expr.left, self.value.binary_expr.right});
                    },
                    BinaryExpression.ID.Minus =>
                    {
                        try std.fmt.format(writer, "{} - {}", .{self.value.binary_expr.left, self.value.binary_expr.right});
                    },
                    BinaryExpression.ID.Multiplication =>
                    {
                        try std.fmt.format(writer, "{} * {}", .{self.value.binary_expr.left, self.value.binary_expr.right});
                    },
                    BinaryExpression.ID.Assignment =>
                    {
                        try std.fmt.format(writer, "{} = {}", .{self.value.binary_expr.left, self.value.binary_expr.right});
                    },
                    BinaryExpression.ID.Compare_Equal =>
                    {
                        try std.fmt.format(writer, "{} == {}", .{self.value.binary_expr.left, self.value.binary_expr.right});
                    },
                    BinaryExpression.ID.Compare_LessThan =>
                    {
                        try std.fmt.format(writer, "{} < {}", .{self.value.binary_expr.left, self.value.binary_expr.right});
                    },
                    BinaryExpression.ID.Compare_GreaterThan =>
                    {
                        try std.fmt.format(writer, "{} > {}", .{self.value.binary_expr.left, self.value.binary_expr.right});
                    },
                    else => panic("Not implemented: {}\n", .{self.value.binary_expr.id}),
                }
                if (self.value.binary_expr.parenthesis)
                {
                    try writer.writeAll(")");
                }
            },
            Node.ID.array_subscript_expr =>
            {
                try std.fmt.format(writer, "{}[{}]", .{self.value.array_subscript_expr.expression, self.value.array_subscript_expr.index});
            },
            Node.ID.identifier_expr =>
            {
                const id_reference_name = self.value.identifier_expr.name;
                try std.fmt.format(writer, "{s}", .{id_reference_name});
            },
            Node.ID.return_expr =>
            {
                try std.fmt.format(writer, "return {}", .{self.value.return_expr.expression});
            },
            Node.ID.loop_expr =>
            {
                try writer.writeAll("while (");
                for (self.value.loop_expr.prefix.value.block_expr.statements.items) |prefix_st|
                {
                    try std.fmt.format(writer, "{}", .{prefix_st});
                }
                try writer.writeAll(")\n{\n");
                for (self.value.loop_expr.body.value.block_expr.statements.items) |loop_st|
                {
                    try std.fmt.format(writer, "{};\n", .{loop_st});
                }
                for (self.value.loop_expr.postfix.value.block_expr.statements.items) |postfix_st|
                {
                    try std.fmt.format(writer, "{};\n", .{postfix_st});
                }
                try writer.writeAll("}");
            },
            Node.ID.branch_expr =>
            {
                try writer.writeAll("if (");
                try std.fmt.format(writer, "{}", .{self.value.branch_expr.condition});
                try writer.writeAll(")\n{\n");
                for (self.value.branch_expr.if_block.value.block_expr.statements.items) |if_st|
                {
                    try std.fmt.format(writer, "{};\n", .{if_st});
                }
                try writer.writeAll("}\n");
                if (self.value.branch_expr.else_block) |else_block|
                {
                    try writer.writeAll("else\n{\n");
                    for (else_block.value.block_expr.statements.items) |else_st|
                    {
                        try std.fmt.format(writer, "{};\n", .{else_st});
                    }
                    try writer.writeAll("}");
                }
            },
            Node.ID.break_expr =>
            {
                try writer.writeAll("break");
            },
            Node.ID.invoke_expr =>
            {
                const invoke_expr = self.value.invoke_expr.expression;
                switch (invoke_expr.value)
                {
                    Node.ID.function_decl =>
                    {
                        try std.fmt.format(writer, "{s}(", .{invoke_expr.value.function_decl.name});
                    },
                    Node.ID.identifier_expr =>
                    {
                        try std.fmt.format(writer, "{s}(", .{invoke_expr.value.identifier_expr.name});
                    },
                    else =>
                    {
                        panic("ni: {}\n", .{invoke_expr.value});
                    }
                }
                for (self.value.invoke_expr.arguments.items) |arg|
                {
                    try std.fmt.format(writer, "{}, ", .{arg});
                }
                try writer.writeAll(")");
            },
            Node.ID.unary_expr =>
            {
                switch (self.value.unary_expr.id)
                {
                    UnaryExpression.ID.AddressOf =>
                    {
                        try std.fmt.format(writer, "&{}", .{self.value.unary_expr.node_ref});
                    },
                    UnaryExpression.ID.Dereference =>
                    {
                        try std.fmt.format(writer, "@{}", .{self.value.unary_expr.node_ref});
                    },
                }
            },
            Node.ID.field_access_expr =>
            {
                try std.fmt.format(writer, "{}.{}", .{self.value.field_access_expr.expression, self.value.field_access_expr.field_expr});
            },
            Node.ID.type_identifier =>
            {
                try std.fmt.format(writer, "{}", .{self.value.type_identifier});
            },
            Node.ID.resolved_identifier =>
            {
                panic("ni\n", .{});
            },
            Node.ID.field_expr =>
            {
                try std.fmt.format(writer, "{}", .{self.value.field_expr});
            },
            Node.ID.module =>
            {
            },
            //else => panic("Not implemented: {}\n", .{self.value}),
        }
    }
};


const Intrinsic = struct
{
    const Value = union(ID)
    {
        syscall: Syscall,
    };

    const ID = enum
    {
        syscall,
    };

    const Syscall = struct
    {
        id: u64,
        arg_bytes: [max_arg_count]u8,
        name: []const u8,

        const max_arg_count = 6;

        const Invocation = struct
        {
            declaration: *Syscall,
            arguments: [max_arg_count]Argument,
        };
    };
};

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



    //fn parse_expression(self: *Parser, allocator: *Allocator, parser: *ModuleParser, parent_node: *Node) *Node
    //{
        //const result = self.parse_precedence(allocator, parser, parent_node, Precedence.Assignment);
        //return result;
    //}

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
                            //log.debug("end of struct initializer\n", .{});
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



    //fn parse_declaration(self: *Parser, allocator: *Allocator, parser: *ModuleParser, parent_node: *Node, left_expr: *Node) *Node
    //{
        //const type_expr = self.parse_type(allocator, parser, parent_node);

        //assert(left_expr.value == Node.ID.identifier_expr);
        //const var_name = left_expr.value.identifier_expr.name;

        //const declaration_node = Node
        //{
            //.value = Node.Value {
                //.var_decl = VariableDeclaration 
                //{
                    //.name = var_name,
                    //.var_type = type_expr,
                    //.var_scope = parent_node,
                    //.backend_ref = 0,
                    //.is_function_arg = false,
                //},
            //},
            //.value_type = Node.ValueType.LValue,
            //.parent = parent_node,
            //.type = undefined,
        //};

        //const declaration = self.append_and_get(declaration_node);
        //return declaration;
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


    //fn parse_infix(self: *Parser, allocator: *Allocator, parser: *ModuleParser, parent_node: *Node, left_expr: *Node, operator: Operator, precedence: Precedence) *Node
    //{
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
        //log.debug("Right expr: {}\n", .{right_expr});
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

        //log.debug("New binary expression: {}\n", .{binary_node});
        //return binary_node;
    //}

    //fn parse_return(self: *Parser, allocator: *Allocator, parser: *ModuleParser, parent_node: *Node) *Node
    //{
        //parser.consume();
        //var return_node_value = Node{
            //.value = Node.Value{
                //.return_expr = ReturnExpression{
                    //.expression = null,
                //},
            //},
            //.value_type = Node.ValueType.RValue,
            //.parent = parent_node,
            //.type = undefined,
        //};

        //var return_node = self.append_and_get(return_node_value);

        //if (parser.expect_sign(';') == null)
        //{
            //const ret_expr = self.parse_expression(allocator, parser, return_node);
            //return_node.value.return_expr.expression = ret_expr;
        //}

        //return return_node;
    //}

    //fn _create_loop_block(self: *Parser, allocator: *Allocator, for_node: *Node, block_type : BlockExpression.ID) *Node
    //{
        //const loop_block_value = Node
        //{
            //.value = Node.Value {
                //.block_expr = BlockExpression {
                    //.statements = NodeRefBuffer.init(allocator),
                    //.id = block_type,
                //},
                //},
            //.parent = for_node,
            //.value_type = Node.ValueType.RValue,
            //.type = undefined,
        //};
        //const loop_block_node = self.append_and_get(loop_block_value);
        //self.current_function.value.function_decl.blocks.append(loop_block_node) catch {
            //panic("Failed to allocate a block reference to function block list\n", .{});
        //};
        //return loop_block_node;
    //}

    //fn parse_for(self: *Parser, allocator: *Allocator, parser: *ModuleParser, parent_node: *Node) *Node
    //{
        //if (parser.expect_and_consume_keyword(KeywordID.@"for") == null)
        //{
            //panic("Internal compiler error: expected 'for' keyword\n", .{});
        //}

        //const for_loop_node_value = Node
        //{
            //.value = Node.Value {
                //.loop_expr = LoopExpression {
                    //.prefix = undefined,
                    //.body = undefined,
                    //.postfix = undefined,
                    //.exit_block_ref = undefined,
                    //.continue_block_ref = undefined,
                //},
            //},
            //.parent = parent_node,
            //.value_type = Node.ValueType.RValue,
            //.type = undefined,
        //};

        //var for_node = self.append_and_get(for_loop_node_value);
        //const parent_scope = self.current_block;

        //for_node.value.loop_expr.prefix = self._create_loop_block(allocator, for_node, BlockExpression.ID.LoopPrefix);
        //for_node.value.loop_expr.body = self._create_loop_block(allocator, for_node, BlockExpression.ID.LoopBody);
        //for_node.value.loop_expr.postfix = self._create_loop_block(allocator, for_node, BlockExpression.ID.LoopPostfix);

        //if (parser.expect_and_consume(Token.ID.identifier)) |it_identifier|
        //{
            //const it_decl_decl_value = Node
            //{
                //.value = Node.Value {
                    //.int_lit = IntegerLiteral {
                        //.value = 0,
                        //.signed = false,
                    //}
                //},
                //// @Info: this is set later
                //.parent = undefined,
                //.value_type = Node.ValueType.RValue,
                //.type = undefined,
            //};

            //const value_type = Node
            //{
                //.value = Node.Value {
                    //.type_identifier = TypeIdentifier {
                        //.value = TypeIdentifier.Value {
                            //.simple = "s32",
                        //},
                    //},
                //},
                //.parent = parent_node,
                //.value_type = Node.ValueType.RValue,
                //.type = undefined,
            //};

            //var it_decl_literal_node = self.append_and_get(it_decl_decl_value);
            //const it_decl_type_node = self.append_and_get(value_type);
            //const it_decl_value = Node
            //{
                //.value = Node.Value {
                    //.var_decl = VariableDeclaration {
                        //.name = it_identifier.value.identifier,
                        //.is_function_arg = false,
                        //.var_scope = self.current_block,
                        //.var_type = it_decl_type_node,
                        //.backend_ref = 0,
                    //},
                //},
                //.parent = parent_node,
                //.value_type = Node.ValueType.LValue,
                //.type = undefined,
            //};

            //const it_decl_node = self.append_and_get(it_decl_value);
            //it_decl_literal_node.parent = it_decl_node;

            //self.current_function.value.function_decl.variables.append(it_decl_node) catch {
                //panic("Error allocating variable reference to function variable list\n", .{});
            //};
            //self.current_block.value.block_expr.statements.append(it_decl_node) catch {
                //panic("Error allocating statement reference to block statement list\n", .{});
            //};

            //const it_decl_ref = Node {
                //.value = Node.Value {
                    //.identifier_expr = IdentifierExpression {
                        //.name = it_decl_node.value.var_decl.name,
                    //},
                //},
                //.value_type = Node.ValueType.LValue,
                //.parent = parent_node,
                //.type = undefined,
            //};

            //const it_decl_var_ref_node = self.append_and_get(it_decl_ref);

            //const assignment = Node
            //{
                //.value = Node.Value {
                    //.binary_expr = BinaryExpression {
                        //.left = it_decl_var_ref_node,
                        //.right = it_decl_literal_node,
                        //.id = BinaryExpression.ID.Assignment,
                        //.parenthesis = false,
                    //}
                //},
                //.value_type = Node.ValueType.RValue,
                //.parent = parent_node,
                //.type = undefined,
            //};

            //const assignment_node = self.append_and_get(assignment);
            //self.current_block.value.block_expr.statements.append(assignment_node) catch {
                //panic("Error allocating statement reference to block statement list\n", .{});
            //};

            //// Prefix
            //{
                //self.current_block = for_node.value.loop_expr.prefix;
                //if (parser.expect_and_consume_operator(Operator.Declaration) == null)
                //{
                    //panic("Expected colon after a loop variable declaration\n", .{});
                //}
                //const right_token = parser.peek();
                //parser.consume();

                //var right_node : *Node = undefined;

                //switch (right_token.value)
                //{
                    //Token.ID.int_lit =>
                    //{
                        //const literal_value = right_token.value.int_lit;
                        //const literal_node_value = Node {
                            //.value = Node.Value {
                                //.int_lit = IntegerLiteral {
                                    //.value = literal_value,
                                    //.signed = false,
                                //}
                            //},
                            //.parent = self.current_block,
                            //.value_type = Node.ValueType.RValue,
                            //.type = undefined,
                        //};

                        //right_node = self.append_and_get(literal_node_value);
                    //},
                    //else =>
                    //{
                        //panic("Right token not implemented: {}\n", .{right_token.value});
                    //}
                //}

                //const iterator_ref_expr_value = Node
                //{
                    //.value = Node.Value {
                        //.identifier_expr = IdentifierExpression {
                            //.name = it_identifier.value.identifier,
                        //}
                    //},
                    //.parent = self.current_block,
                    //.value_type = Node.ValueType.RValue,
                    //.type = undefined,
                //};

                //const iterator_ref_expr = self.append_and_get(iterator_ref_expr_value);

                //const prefix_comparison_value = Node
                //{
                    //.value = Node.Value {
                        //.binary_expr = BinaryExpression {
                            //.left = iterator_ref_expr,
                            //.right =  right_node,
                            //.id = BinaryExpression.ID.Compare_LessThan,
                            //.parenthesis = false,
                        //}
                    //},
                    //.parent = self.current_block,
                    //.value_type = Node.ValueType.RValue,
                    //.type = undefined,
                //};

                //const prefix_comparison_node = self.append_and_get(prefix_comparison_value);
                //// @Info: in other kind of loops we should support multiple statements
                //self.current_block.value.block_expr.statements.append(prefix_comparison_node) catch {
                    //panic("Couldn't allocate prefix statement\n", .{});
                //};
            //}

            //// Block
            //{
                //// @Info: this sets the target block as current block, so no need here to set it in advance
                //self.block(allocator, parser, for_node.value.loop_expr.body, true);
            //}

            //// Postfix
            //{
                //self.current_block = for_node.value.loop_expr.postfix;
                //const identifier_expr_lvalue = Node
                //{
                    //.value = Node.Value {
                        //.identifier_expr = IdentifierExpression {
                            //.name = it_identifier.value.identifier,
                        //}
                    //},
                    //.parent = self.current_block,
                    //.value_type = Node.ValueType.LValue,
                    //.type = undefined,
                //};
                //var identifier_expr_rvalue = identifier_expr_lvalue;
                //identifier_expr_rvalue.value_type = Node.ValueType.RValue;
                //const identifier_lvalue = self.append_and_get(identifier_expr_lvalue);
                //const identifier_rvalue = self.append_and_get(identifier_expr_rvalue);

                //const one_lit_value = Node
                //{
                    //.value = Node.Value {
                        //.int_lit = IntegerLiteral {
                            //.value = 1,
                            //.signed = false,
                        //}
                    //},
                    //.parent = self.current_block,
                    //.value_type = Node.ValueType.RValue,
                    //.type = undefined,
                //};
                //const one_lit_node = self.append_and_get(one_lit_value);

                //const postfix_increment_value = Node
                //{
                    //.value = Node.Value {
                        //.binary_expr = BinaryExpression {
                            //.left = identifier_rvalue,
                            //.right =  one_lit_node,
                            //.id =  BinaryExpression.ID.Plus,
                            //.parenthesis = false,
                        //},
                        //},
                    //.parent = self.current_block,
                    //.value_type = Node.ValueType.RValue,
                    //.type = undefined,
                //};

                //const postfix_increment_node = self.append_and_get(postfix_increment_value);

                //const postfix_assignment_value = Node
                //{
                    //.value = Node.Value {
                        //.binary_expr = BinaryExpression {
                            //.left = identifier_lvalue,
                            //.right =  postfix_increment_node,
                            //.id =  BinaryExpression.ID.Assignment,
                            //.parenthesis = false,
                        //},
                        //},
                    //.parent = self.current_block,
                    //.value_type = Node.ValueType.RValue,
                    //.type = undefined,
                //};

                //const postfix_assignment_node = self.append_and_get(postfix_assignment_value);
                //self.current_block.value.block_expr.statements.append(postfix_assignment_node) catch {
                    //panic("Couldn't allocate postfix statement\n", .{});
                //};
            //}

            //self.current_block = parent_scope;
            //return for_node;
        //}
        //else
        //{
            //panic("Expected identifier declaration for loop iteration\n", .{});
        //}
    //}

    //fn parse_if(self: *Parser, allocator: *Allocator, parser: *ModuleParser, parent_node: *Node) *Node
    //{
        //// consume if keyword
        //parser.consume();

        //const branch_node_value = Node
        //{
            //.value = Node.Value {
                //.branch_expr = BranchExpression {
                    //.condition = undefined,
                    //.if_block = undefined,
                    //.else_block = null,
                    //.exit_block_ref = 0,
                //}
            //},
            //.parent = parent_node,
            //.value_type = Node.ValueType.RValue,
            //.type = undefined,
        //};

        //var branch_node = self.append_and_get(branch_node_value);
        //const condition_node = self.parse_expression(allocator, parser, parent_node);
        //branch_node.value.branch_expr.condition = condition_node;
        //const if_block_value = Node
        //{
            //.value = Node.Value {
                //.block_expr = BlockExpression {
                    //.statements = NodeRefBuffer.init(allocator),
                    //.id = BlockExpression.ID.IfBlock,
                //}
            //},
            //.parent = branch_node,
            //.value_type = Node.ValueType.RValue,
            //.type = undefined,
        //};
        //const if_block_node = self.append_and_get(if_block_value);
        //self.block(allocator, parser, if_block_node, true);
        //branch_node.value.branch_expr.if_block = if_block_node;

        //self.current_block = parent_node;

        //if (parser.expect_and_consume_keyword(KeywordID.@"else") != null)
        //{
            //const else_block_value = Node
            //{
                //.value = Node.Value {
                    //.block_expr = BlockExpression {
                        //.statements = NodeRefBuffer.init(allocator),
                        //.id = BlockExpression.ID.ElseBlock,
                    //}
                //},
                //.parent = branch_node,
                //.value_type = Node.ValueType.RValue,
                //.type = undefined,
            //};
            //const else_block_node = self.append_and_get(else_block_value);
            //self.block(allocator, parser, else_block_node, true);
            //branch_node.value.branch_expr.else_block = else_block_node;
        //}

        //return branch_node;
    //}

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


    //fn parse_intrinsic(self: *Parser, allocator: *Allocator, parser: *ModuleParser, intrinsic_name: []const u8) void
    //{
        //if (std.meta.stringToEnum(Intrinsic.ID, intrinsic_name)) |intrinsic_id|
        //{
            //print("Intrinsic found: {}\n", .{intrinsic_id});
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
                            //print("Byte count: {}\n", .{byte_count});

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

//pub const BinaryExpression = struct
//{
    //left: *Node,
    //right: *Node,
    //id: ID,
    //parenthesis: bool,

    //pub const ID = enum
    //{
        //Plus,
        //Minus,
        //Multiplication,
        //VariableDeclaration,
        //Assignment,
        //Compare_Equal,
        //Compare_NotEqual,
        //Compare_LessThan,
        //Compare_GreaterThan,
        //Compare_LessThanOrEqual,
        //Compare_GreaterThanOrEqual,
    //};
//};

//const ReturnExpression = struct
//{
    //expression: ?*Node,
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


//const BranchExpression = struct
//{
    //condition: *Node,
    //if_block: *Node,
    //else_block: ?*Node,
    //exit_block_ref: usize,
//};

//const LoopExpression = struct
//{
    //prefix: *Node,
    //body: *Node,
    //postfix: *Node,
    //exit_block_ref: usize,
    //continue_block_ref: usize,
//};

//const BreakExpression = struct
//{
    //target: *Node,
//};

//const FunctionDeclaration = struct
//{
    //blocks: NodeRefBuffer,
    //arguments: NodeRefBuffer,
    //variables: NodeRefBuffer,
    //name: []const u8,
    //type: *Node,
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
