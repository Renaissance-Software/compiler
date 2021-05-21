const std = @import("std");
const assert = std.debug.assert;
const print = std.debug.print;
const panic = std.debug.panic;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Types = @import("ast_types.zig");
const BucketArrayList = @import("bucket_array.zig").BucketArrayList;
const Type = Types.Type;

const Lexer = @import("lexer.zig");
const Operator = Lexer.Operator;
const KeywordID = Lexer.KeywordID;
const Token = Lexer.Token;
const LexerResult = Lexer.LexerResult;

const log = std.log.scoped(.parser);

pub fn parser_error(comptime format: []const u8, args: anytype) noreturn
{
    log.err(format, args);
    panic("Error in the parser\n", .{});
}

pub const NodeRefBuffer = ArrayList(*Node);
pub const NodeBuffer = BucketArrayList(Node, 64);

const IntegerLiteral = struct
{
    value: u64,
    signed: bool,
    // padding
};

pub const UnaryExpression = struct
{
    node_ref: *Node,
    id: ID,

    pub const ID = enum
    {
        AddressOf,
        Dereference,
    };
};

pub const BinaryExpression = struct
{
    left: *Node,
    right: *Node,
    id: ID,
    parenthesis: bool,

    pub const ID = enum
    {
        Plus,
        Minus,
        Multiplication,
        VariableDeclaration,
        Assignment,
        Compare_Equal,
        Compare_NotEqual,
        Compare_LessThan,
        Compare_GreaterThan,
        Compare_LessThanOrEqual,
        Compare_GreaterThanOrEqual,
    };
};

const ReturnExpression = struct
{
    expression: ?*Node,
};

const IdentifierExpression = struct
{
    name: []const u8,
};

pub const TypeIdentifier = struct
{
    value: Value,

    pub const Value = union(ID)
    {
        void,
        simple: []const u8,
        pointer: Pointer,
        array: Array,
        structure: Struct,
        function: Function,
    };

    pub const ID = enum
    {
        void,
        simple,
        pointer,
        array,
        structure,
        function,
    };

    pub const Function = struct
    {
        arg_types: NodeRefBuffer,
        return_type: ?*Node,
    };

    pub const Pointer = struct
    {
        type: *Node,
    };

    pub const Array = struct
    {
        type: *Node,
        len_expr: *Node,
    };

    pub const Struct = struct
    {
        // @Info: struct fields are of type var_decl
        fields: NodeRefBuffer,
        name: []const u8,
    };

    pub fn format(self: TypeIdentifier, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void
    {
        switch (self.value)
        {
            TypeIdentifier.ID.simple => try std.fmt.format(writer, "{s}", .{self.value.simple}),
            TypeIdentifier.ID.pointer => try std.fmt.format(writer, "&{}", .{self.value.pointer.type.value.type_identifier}),
            TypeIdentifier.ID.array => try std.fmt.format(writer, "[]{}", .{self.value.array.type.value.type_identifier}),
            else => panic("ni: {}\n", .{self.value}),
        }
    }
};

const BlockExpression = struct
{
    statements: NodeRefBuffer,
    id: ID,

    const ID = enum {
        LoopPrefix,
        LoopBody,
        LoopPostfix,
        IfBlock,
        ElseBlock,
        Function,
    };
};

const VariableDeclaration = struct
{
    name: []const u8,
    var_type: *Node,
    var_scope: *Node,
    backend_ref: usize,
    is_function_arg: bool,
};

const BranchExpression = struct
{
    condition: *Node,
    if_block: *Node,
    else_block: ?*Node,
    exit_block_ref: usize,
};

const LoopExpression = struct
{
    prefix: *Node,
    body: *Node,
    postfix: *Node,
    exit_block_ref: usize,
    continue_block_ref: usize,
};

const BreakExpression = struct
{
    target: *Node,
};

const FunctionDeclaration = struct
{
    blocks: NodeRefBuffer,
    arguments: NodeRefBuffer,
    variables: NodeRefBuffer,
    name: []const u8,
    type: *Node,
};

const InvokeExpression = struct
{
    arguments: NodeRefBuffer,
    expression: *Node,
};

const ArraySubscriptExpression = struct
{
    expression: *Node,
    index: *Node,
};

const ArrayLiteral = struct
{
    elements: NodeRefBuffer,
};

const FieldAccessExpression = struct
{
    expression: *Node,
    field_expr: *Node,
};

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
        int_lit: IntegerLiteral,
        array_lit: ArrayLiteral,
        unary_expr: UnaryExpression,
        binary_expr: BinaryExpression,
        return_expr: ReturnExpression,
        type_identifier: TypeIdentifier,
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
    };

    pub const ID = enum
    {
        var_decl,
        function_decl,
        int_lit,
        array_lit,
        unary_expr,
        binary_expr,
        return_expr,
        identifier_expr,
        type_identifier,
        resolved_identifier,
        field_expr,
        invoke_expr,
        block_expr,
        branch_expr,
        loop_expr,
        break_expr,
        array_subscript_expr,
        field_access_expr,
    };

    pub const ValueType = enum
    {
        RValue,
        LValue,
    };

    pub fn format(self: *const Node, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void
    {
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
            Node.ID.array_lit =>
            {
                try std.fmt.format(writer, "[", .{});
                for (self.value.array_lit.elements.items) |array_lit_elem|
                {
                    try std.fmt.format(writer, "{}, ", .{array_lit_elem});
                }
                try writer.writeAll("]");
            },
            Node.ID.int_lit =>
            {
                if (self.value.int_lit.signed)
                {
                    try writer.writeAll("-");
                }
                try std.fmt.format(writer, "{}", .{self.value.int_lit.value});
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
            //else => panic("Not implemented: {}\n", .{self.value}),
        }
    }
};

const TokenConsumer = struct
{
    tokens: []const Token,
    next_index: usize,

    fn peek(self: *TokenConsumer) Token
    {
        const token = self.tokens[self.next_index];
        return token;
    }

    fn consume(self: *TokenConsumer) void 
    {
        const consumed_token = self.tokens[self.next_index];
        log.debug("Consuming {}\n", .{consumed_token});
        self.next_index += 1;
    }

    fn expect_and_consume(self: *TokenConsumer, id: Token.ID) ?Token
    {
        const token = self.tokens[self.next_index];

        if (token.value == id)
        {
            self.consume();
            return token;
        }

        return null;
    }

    fn expect_operator(self: *TokenConsumer, operator: Operator) ?Token
    {
        const token = self.tokens[self.next_index];
        if (token.value == Token.ID.operator and token.value.operator == operator)
        {
            return token;
        }

        return null;
    }

    fn expect_sign(self: *TokenConsumer, sign: u8) ?Token
    {
        const token = self.tokens[self.next_index];
        if (token.value == Token.ID.sign and token.value.sign == sign)
        {
            return token;
        }

        return null;
    }

    fn expect_keyword(self: *TokenConsumer, keyword: KeywordID) ?Token
    {
        const token = self.tokens[self.next_index];
        if (token.value == Token.ID.keyword and token.value.keyword == keyword)
        {
            return token;
        }

        return null;
    }

    fn expect_and_consume_sign(self: *TokenConsumer, sign: u8) ?Token
    {
        const token = self.expect_sign(sign);
        if (token != null)
        {
            self.consume();
        }

        return token;
    }


    fn expect_and_consume_keyword(self: *TokenConsumer, keyword: KeywordID) ?Token
    {
        const token = self.expect_keyword(keyword);
        if (token != null)
        {
            self.consume();
        }

        return token;
    }

    fn expect_and_consume_operator(self: *TokenConsumer, operator: Operator) ?Token
    {
        const token = self.expect_operator(operator);
        if (token != null)
        {
            self.consume();
        }

        return token;
    }
};

const Parser = struct
{
    nb: NodeBuffer,
    current_function: *Node,
    current_block: *Node,

    function_declarations: NodeRefBuffer,
    type_declarations: NodeRefBuffer,

    fn append_and_get(self: *Parser, node: Node) *Node
    {
        const result = self.nb.append(node) catch |err| {
            panic("Couldn't allocate memory for node", .{});
        };
        log.debug("new node:\n\n{s}\n\n", .{@tagName(result.value)});
        return result;
    }

    fn parse_type(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer, parent_node: ?*Node) *Node
    {
        log.debug("Parsing type...\n", .{});
        const next_token = consumer.peek();
        consumer.consume();

        switch (next_token.value)
        {
            Token.ID.identifier =>
            {
                const type_name = next_token.value.identifier;
                const type_node_value = Node {
                    .value = Node.Value {
                        .type_identifier = TypeIdentifier {
                            .value = TypeIdentifier.Value {
                                .simple = type_name,
                            },
                        },
                    },
                    .parent = parent_node,
                    .value_type = Node.ValueType.LValue,
                    .type = undefined,
                };

                const type_node = self.append_and_get(type_node_value);
                return type_node;
            },
            Token.ID.keyword =>
            {
                const keyword = next_token.value.keyword;

                switch (keyword)
                {
                    KeywordID.@"struct" =>
                    {
                        if (consumer.expect_and_consume_sign('{') == null)
                        {
                            parser_error("Error: expected '{c}' at the beginning of the struct\n", .{'{'});
                        }

                        const struct_type_node = Node
                        {
                            .value = Node.Value {
                                .type_identifier = TypeIdentifier {
                                    .value = TypeIdentifier.Value {
                                        .structure = TypeIdentifier.Struct {
                                            .fields = NodeRefBuffer.init(allocator),
                                            .name = "",
                                        },
                                    },
                                },
                            },
                            .parent = parent_node,
                            .value_type = Node.ValueType.RValue,
                            .type = undefined,
                        };

                        var result = self.append_and_get(struct_type_node);

                        if (consumer.expect_and_consume_sign('}') == null)
                        {
                            while (true)
                            {
                                const field = self.parse_expression(allocator, consumer, result);
                                result.value.type_identifier.value.structure.fields.append(field) catch |err| {
                                    panic("Error allocating memory for struct field\n", .{});
                                };

                                const token = consumer.peek();
                                consumer.consume();

                                if (token.value == Token.ID.sign and token.value.sign == '}')
                                {
                                    break;
                                }
                                else if (token.value == Token.ID.sign and token.value.sign != ',')
                                {
                                    parser_error("Expected comma after argument. Found: {}\n", .{token.value});
                                }
                                else
                                {
                                    if (consumer.expect_and_consume_sign('}') != null)
                                    {
                                        break;
                                    }
                                }
                            }
                        }
                        else
                        {
                            parser_error("Empty struct is not allowed\n", .{});
                        }

                        return result;
                    },
                    else => panic("ni: {}\n", .{keyword}),
                }
            },
            Token.ID.operator =>
            {
                const operator = next_token.value.operator;
                switch (operator)
                {
                    Operator.AddressOf =>
                    {
                        const type_node_value = Node {
                            .value = Node.Value {
                                .type_identifier = TypeIdentifier {
                                    .value = TypeIdentifier.Value {
                                        .pointer = TypeIdentifier.Pointer {
                                            .type = self.parse_type(allocator, consumer, parent_node),
                                        },
                                    },
                                },
                            },
                            .parent = parent_node,
                            .value_type = Node.ValueType.LValue,
                            .type = undefined,
                        };

                        const type_node = self.append_and_get(type_node_value);
                        return type_node;
                    },
                    Operator.LeftBracket =>
                    {
                        var type_node_value = Node {
                            .value = Node.Value {
                                .type_identifier = TypeIdentifier {
                                    .value = TypeIdentifier.Value {
                                        .array = TypeIdentifier.Array {
                                            .type = undefined,
                                            .len_expr = undefined,
                                        },
                                    },
                                },
                            },
                            .parent = parent_node,
                            .value_type = Node.ValueType.LValue,
                            .type = undefined,
                        };

                        const type_node = self.append_and_get(type_node_value);

                        type_node.value.type_identifier.value.array.len_expr = self.parse_expression(allocator, consumer, type_node);

                        if (consumer.expect_and_consume_operator(Operator.RightBracket) == null)
                        {
                            parser_error("Expected ']' in array type\n", .{});
                        }

                        type_node.value.type_identifier.value.array.type = self.parse_type(allocator, consumer, parent_node);

                        return type_node;
                    },
                    else => panic("ni: {}\n", .{operator}),
                }
            },
            else => panic("not implemented: {}\n", .{next_token.value}),
        }
    }

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

        fn increment(self: Precedence) Precedence
        {
            return @intToEnum(Precedence, (@enumToInt(self) + 1));
        }
    };

    fn parse_expression(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer, parent_node: *Node) *Node
    {
        const result = self.parse_precedence(allocator, consumer, parent_node, Precedence.Assignment);
        return result;
    }

    fn parse_unary_expression(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer, parent_node: *Node, operator: Operator) *Node
    {
        const unary_expr_id = switch (operator)
        {
            Operator.AddressOf => UnaryExpression.ID.AddressOf,
            Operator.Dereference => UnaryExpression.ID.Dereference,
            else => panic("ni: {}\n", .{operator}),
        };

        const unary_expr_node = Node {
            .value = Node.Value {
                .unary_expr = UnaryExpression 
                {
                    .node_ref = undefined,
                    .id = unary_expr_id,
                },
            },
            .value_type = Node.ValueType.RValue,
            .parent = parent_node,
            .type = undefined,
        };

        const unary_expr = self.append_and_get(unary_expr_node);
        unary_expr.value.unary_expr.node_ref = self.parse_precedence(allocator, consumer, unary_expr, Precedence.Unary);

        return unary_expr;
    }

    fn parse_array_literal(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer, parent_node: *Node) *Node
    {
        const array_literal_node = Node
        {
            .value = Node.Value {
                .array_lit = ArrayLiteral {
                    .elements = NodeRefBuffer.init(allocator),
                },
            },
            .value_type = Node.ValueType.RValue,
            .parent = parent_node,
            .type = undefined,
        };

        const array_literal = self.append_and_get(array_literal_node);
            
        if (consumer.expect_and_consume_operator(Operator.RightBracket) == null)
        {
            while (true)
            {
                const array_elem = self.parse_expression(allocator, consumer, parent_node);
                array_literal.value.array_lit.elements.append(array_elem) catch |err| {
                    panic("Error allocating memory for array literal element\n", .{});
                };
                
                const next_token = consumer.peek();
                consumer.consume();

                if (next_token.value == Token.ID.operator and next_token.value.operator == Operator.RightBracket)
                {
                    break;
                }
                else if (next_token.value == Token.ID.sign and next_token.value.sign != ',')
                {
                    parser_error("Expected comma after argument. Found: {}\n", .{next_token.value});
                }
            }
        }
        else
        {
            parser_error("Empty array literal is not allowed\n", .{});
        }

        return array_literal;
    }

    fn parse_prefix(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer, parent_node: *Node) *Node
    {
        const token = consumer.peek();
        consumer.consume();

        switch (token.value)
        {
            Token.ID.int_lit =>
            {
                const int_value = token.value.int_lit;
                //print("Found int literal: {}\n", .{int_value});
                const int_lit_node = Node
                {
                    .value = Node.Value{
                        .int_lit = IntegerLiteral{
                            .value = int_value,
                            .signed = false,
                        },
                    },
                    .value_type = Node.ValueType.RValue,
                    .parent = parent_node,
                    .type = undefined,
                };

                return self.append_and_get(int_lit_node);
            },
            Token.ID.identifier =>
            {
                const identifier_name = token.value.identifier;
                const id_expr_node_value = Node
                {
                    .value = Node.Value {
                        .identifier_expr = IdentifierExpression {
                            .name = identifier_name,
                        },
                    },
                    .parent = parent_node,
                    .value_type = Node.ValueType.RValue,
                    .type = undefined,
                };
                const id_expr_node = self.append_and_get(id_expr_node_value);
                return id_expr_node;
            },
            Token.ID.operator =>
            {
                const operator = token.value.operator;
                switch (operator)
                {
                    // @Info: this is not array subscript, but array literal constant
                    Operator.LeftBracket =>
                    {
                        const array_lit_expr = self.parse_array_literal(allocator, consumer, parent_node);
                        return array_lit_expr;
                    },
                    Operator.AddressOf,
                    Operator.Dereference =>
                    {
                        const unary_expr = self.parse_unary_expression(allocator, consumer, parent_node, operator);
                        return unary_expr;
                    },
                    else => panic("ni: {}\n", .{operator}),
                }
            },
            else => panic("Prefix functionality not implemented for {}\n", .{token.value}),
        }
    }

    fn parse_invoke_expr(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer, parent_node: *Node, left_expr: *Node, operator: Operator, precedence: Precedence) *Node
    {
        log.debug("Parsing argument list\n", .{});
        var args_left_to_parse = consumer.expect_and_consume_operator(Operator.RightParenthesis) == null;
        var arg_list = NodeRefBuffer.init(allocator);
        var is_function_declaration = false;

        if (args_left_to_parse)
        {
            while (true)
            {
                const arg = self.parse_expression(allocator, consumer, parent_node);
                arg_list.append(arg) catch |err| {
                    panic("Error allocating memory for argument node\n", .{});
                };

                const next_token = consumer.peek();
                consumer.consume();

                if (next_token.value == Token.ID.operator and next_token.value.operator == Operator.RightParenthesis)
                {
                    break;
                }
                else if (next_token.value == Token.ID.sign and next_token.value.sign != ',')
                {
                    parser_error("Expected comma after argument. Found: {}\n", .{next_token.value});
                }
                const first_arg = arg_list.items[0];
                is_function_declaration = first_arg.value == Node.ID.var_decl;
            }
        }

        var return_type_node: ?*Node = null;
        // @Info: in case this is a function declaration
        if (consumer.expect_and_consume_operator(Operator.Arrow) != null)
        {
            is_function_declaration = true;
            return_type_node = self.parse_type(allocator, consumer, parent_node);
        }

        var result: *Node = undefined;
        if (consumer.expect_and_consume_sign('{') != null)
        {
            log.debug("Parsing function declaration\n", .{});
            panic("not implemented yet\n", .{});
        }
        else
        {
            log.debug("Parsing function call expression\n", .{});
            const function_call = Node
            {
                .value = Node.Value {
                    .invoke_expr = InvokeExpression {
                        .arguments = arg_list,
                        .expression = left_expr,
                    },
                },
                .value_type = Node.ValueType.RValue,
                .parent = parent_node,
                .type = undefined,
            };

            result = self.append_and_get(function_call);
        }

        return result;
    }

    fn parse_declaration(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer, parent_node: *Node, left_expr: *Node, operator: Operator, precedence: Precedence) *Node
    {
        const type_expr = self.parse_type(allocator, consumer, parent_node);

        assert(left_expr.value == Node.ID.identifier_expr);
        const var_name = left_expr.value.identifier_expr.name;

        const declaration_node = Node
        {
            .value = Node.Value {
                .var_decl = VariableDeclaration 
                {
                    .name = var_name,
                    .var_type = type_expr,
                    .var_scope = parent_node,
                    .backend_ref = 0,
                    .is_function_arg = false,
                },
            },
            .value_type = Node.ValueType.LValue,
            .parent = parent_node,
            .type = undefined,
        };

        const declaration = self.append_and_get(declaration_node);
        return declaration;
    }

    fn parse_array_subscript(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer, parent_node: *Node, left_expr: *Node, operator: Operator, precedence: Precedence) *Node
    {
        const subscript_node_value = Node
        {
            .value = Node.Value {
                .array_subscript_expr = ArraySubscriptExpression {
                    .expression = left_expr,
                    .index = undefined,
                },
            },
            .parent = parent_node,
            .value_type = Node.ValueType.RValue,
            .type = undefined,
        };

        var subscript_node = self.append_and_get(subscript_node_value);
        subscript_node.value.array_subscript_expr.index = self.parse_expression(allocator, consumer, parent_node);

        if (consumer.expect_and_consume_operator(Operator.RightBracket) == null)
        {
            parser_error("Expected ']' in array subscript expression", .{});
        }

        return subscript_node;
    }

    fn parse_field_access(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer, parent_node: *Node, left_expr: *Node, operator: Operator, precedence: Precedence) *Node
    {
        const field_acces_value = Node
        {
            .value = Node.Value {
                .field_access_expr = FieldAccessExpression {
                    .expression = left_expr,
                    .field_expr = undefined,
                },
            },
            .parent = parent_node,
            .value_type = Node.ValueType.RValue,
            .type = undefined,
        };

        var field_access_node = self.append_and_get(field_acces_value);
        field_access_node.value.field_access_expr.field_expr = self.parse_precedence(allocator, consumer, field_access_node, Precedence.Call.increment());
        assert(field_access_node.value.field_access_expr.field_expr.value == Node.ID.identifier_expr);

        return field_access_node;
    }

    fn parse_infix(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer, parent_node: *Node, left_expr: *Node, operator: Operator, precedence: Precedence) *Node
    {
        switch (operator)
        {
            Operator.Equal,
            Operator.GreaterThan,
            Operator.Assignment,
            Operator.Plus,
            Operator.Minus,
            Operator.Multiplication =>
            {
                const binary_expr = self.parse_binary_expression(allocator, consumer, parent_node, left_expr, operator, precedence);
                return binary_expr;
            },

            Operator.LeftParenthesis =>
            {
                const call_expr = self.parse_invoke_expr(allocator, consumer, parent_node, left_expr, operator, precedence);
                return call_expr;
            },
            Operator.Declaration => 
            {
                const declaration_expr = self.parse_declaration(allocator, consumer, parent_node, left_expr, operator, precedence);
                return declaration_expr;
            },
            Operator.LeftBracket =>
            {
                const array_subscript_expr = self.parse_array_subscript(allocator, consumer, parent_node, left_expr, operator, precedence);
                return array_subscript_expr;
            },
            Operator.Dot =>
            {
                const field_access_expr = self.parse_field_access(allocator, consumer, parent_node, left_expr, operator, precedence);
                return field_access_expr;
            },
            else => panic("operator not implemented: {}\n", .{operator}),
        }
    }

    fn parse_precedence(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer, parent_node: *Node, precedence: Precedence) *Node
    {
        const prefix_node = self.parse_prefix(allocator, consumer, parent_node);
        var left_expr = prefix_node;

        while (true)
        {
            const token = consumer.peek();
            const new_precedence = switch (token.value)
            {
                Token.ID.sign => sign_block:
                {
                    const sign = token.value.sign;
                    const sign_precedence = switch (sign)
                    {
                        '{' => Precedence.None,
                        ';' => Precedence.None,
                        ',' => Precedence.None,
                        else => panic("Precedence not implemented for sign {c}\n", .{sign}),
                    };

                    break :sign_block sign_precedence;
                },
                Token.ID.operator => operator_block:
                {
                    const operator = token.value.operator;
                    const operator_precedence = switch (operator)
                    {
                        Operator.RightParenthesis,
                        Operator.RightBracket, => Precedence.None,

                        Operator.Plus,
                        Operator.Minus => Precedence.LightArithmetic,

                        Operator.Multiplication,
                        Operator.Division => Precedence.HeavyArithmetic,

                        Operator.Equal,
                        Operator.GreaterThan,
                        Operator.LessThan => Precedence.Compare,

                        Operator.Assignment => Precedence.Assignment,

                        Operator.Declaration => Precedence.Declaration,

                        Operator.LeftParenthesis,
                        Operator.LeftBracket,
                        Operator.Dot, => Precedence.Call,

                        else => panic("Precedence not implemented for {}\n", .{operator}),
                    };

                    break :operator_block operator_precedence;
                },
                else => panic("Precedence not implemented for {}\n", .{token.value}),
            };

            log.debug("Old precedence: {}, new precedence: {}\n", .{precedence, new_precedence});
            if (@enumToInt(precedence) <= @enumToInt(new_precedence))
            {
                consumer.consume();
                assert(token.value == Token.ID.operator);
                const operator = token.value.operator;
                left_expr = self.parse_infix(allocator, consumer, parent_node, left_expr, operator, new_precedence);
            }
            else
            {
                break;
            }
        }

        return left_expr;
    }

    fn parse_binary_expression(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer, parent_node: *Node, left_expr: *Node, operator: Operator, precedence: Precedence) *Node
    {
        const binary_node_value = Node
        {
            .value = Node.Value {
                .binary_expr = BinaryExpression {
                    .left = left_expr,
                    .right = undefined,
                    .id = undefined,
                    .parenthesis = false,
                },
                },
            .parent = parent_node,
            .value_type = Node.ValueType.RValue,
            .type = undefined,
        };

        const binary_node = self.append_and_get(binary_node_value);

        const right_expr = self.parse_precedence(allocator, consumer, binary_node, precedence.increment());
        log.debug("Right expr: {}\n", .{right_expr});
        binary_node.value.binary_expr.right = right_expr;

        const binary_op: BinaryExpression.ID = switch (operator)
        {
            Operator.Assignment => BinaryExpression.ID.Assignment,
            Operator.Plus => BinaryExpression.ID.Plus,
            Operator.Minus => BinaryExpression.ID.Minus,
            Operator.Multiplication => BinaryExpression.ID.Multiplication,
            Operator.Equal => BinaryExpression.ID.Compare_Equal,
            Operator.GreaterThan => BinaryExpression.ID.Compare_GreaterThan,
            else => panic("not implemented: {}\n", .{operator}),
        };
        binary_node.value.binary_expr.id = binary_op;

        if (binary_op == BinaryExpression.ID.Assignment)
        {
            left_expr.value_type = Node.ValueType.LValue;
        }

        log.debug("New binary expression: {}\n", .{binary_node});
        return binary_node;
    }

    fn parse_return(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer, parent_node: *Node) *Node
    {
        consumer.consume();
        var return_node_value = Node{
            .value = Node.Value{
                .return_expr = ReturnExpression{
                    .expression = null,
                },
            },
            .value_type = Node.ValueType.RValue,
            .parent = parent_node,
            .type = undefined,
        };

        var return_node = self.append_and_get(return_node_value);

        if (consumer.expect_sign(';') == null)
        {
            const ret_expr = self.parse_expression(allocator, consumer, return_node);
            return_node.value.return_expr.expression = ret_expr;
        }

        return return_node;
    }

    fn _create_loop_block(self: *Parser, allocator: *Allocator, for_node: *Node, block_type : BlockExpression.ID) *Node
    {
        const loop_block_value = Node
        {
            .value = Node.Value {
                .block_expr = BlockExpression {
                    .statements = NodeRefBuffer.init(allocator),
                    .id = block_type,
                },
                },
            .parent = for_node,
            .value_type = Node.ValueType.RValue,
            .type = undefined,
        };
        const loop_block_node = self.append_and_get(loop_block_value);
        self.current_function.value.function_decl.blocks.append(loop_block_node) catch |err| {
            panic("Failed to allocate a block reference to function block list\n", .{});
        };
        return loop_block_node;
    }

    fn parse_for(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer, parent_node: *Node) *Node
    {
        if (consumer.expect_and_consume_keyword(KeywordID.@"for") == null)
        {
            panic("Internal compiler error: expected 'for' keyword\n", .{});
        }

        const for_loop_node_value = Node
        {
            .value = Node.Value {
                .loop_expr = LoopExpression {
                    .prefix = undefined,
                    .body = undefined,
                    .postfix = undefined,
                    .exit_block_ref = undefined,
                    .continue_block_ref = undefined,
                },
            },
            .parent = parent_node,
            .value_type = Node.ValueType.RValue,
            .type = undefined,
        };

        var for_node = self.append_and_get(for_loop_node_value);
        const parent_scope = self.current_block;

        for_node.value.loop_expr.prefix = self._create_loop_block(allocator, for_node, BlockExpression.ID.LoopPrefix);
        for_node.value.loop_expr.body = self._create_loop_block(allocator, for_node, BlockExpression.ID.LoopBody);
        for_node.value.loop_expr.postfix = self._create_loop_block(allocator, for_node, BlockExpression.ID.LoopPostfix);

        if (consumer.expect_and_consume(Token.ID.identifier)) |it_identifier|
        {
            const it_decl_decl_value = Node
            {
                .value = Node.Value {
                    .int_lit = IntegerLiteral {
                        .value = 0,
                        .signed = false,
                    }
                },
                // @Info: this is set later
                .parent = undefined,
                .value_type = Node.ValueType.RValue,
                .type = undefined,
            };

            const value_type = Node
            {
                .value = Node.Value {
                    .type_identifier = TypeIdentifier {
                        .value = TypeIdentifier.Value {
                            .simple = "s32",
                        },
                    },
                },
                .parent = parent_node,
                .value_type = Node.ValueType.RValue,
                .type = undefined,
            };

            var it_decl_literal_node = self.append_and_get(it_decl_decl_value);
            const it_decl_type_node = self.append_and_get(value_type);
            const it_decl_value = Node
            {
                .value = Node.Value {
                    .var_decl = VariableDeclaration {
                        .name = it_identifier.value.identifier,
                        .is_function_arg = false,
                        .var_scope = self.current_block,
                        .var_type = it_decl_type_node,
                        .backend_ref = 0,
                    },
                },
                .parent = parent_node,
                .value_type = Node.ValueType.LValue,
                .type = undefined,
            };

            const it_decl_node = self.append_and_get(it_decl_value);
            it_decl_literal_node.parent = it_decl_node;

            self.current_function.value.function_decl.variables.append(it_decl_node) catch |err| {
                panic("Error allocating variable reference to function variable list\n", .{});
            };
            self.current_block.value.block_expr.statements.append(it_decl_node) catch |err| {
                panic("Error allocating statement reference to block statement list\n", .{});
            };

            const it_decl_ref = Node {
                .value = Node.Value {
                    .identifier_expr = IdentifierExpression {
                        .name = it_decl_node.value.var_decl.name,
                    },
                },
                .value_type = Node.ValueType.LValue,
                .parent = parent_node,
                .type = undefined,
            };

            const it_decl_var_ref_node = self.append_and_get(it_decl_ref);

            const assignment = Node
            {
                .value = Node.Value {
                    .binary_expr = BinaryExpression {
                        .left = it_decl_var_ref_node,
                        .right = it_decl_literal_node,
                        .id = BinaryExpression.ID.Assignment,
                        .parenthesis = false,
                    }
                },
                .value_type = Node.ValueType.RValue,
                .parent = parent_node,
                .type = undefined,
            };

            const assignment_node = self.append_and_get(assignment);
            self.current_block.value.block_expr.statements.append(assignment_node) catch |err| {
                panic("Error allocating statement reference to block statement list\n", .{});
            };

            // Prefix
            {
                self.current_block = for_node.value.loop_expr.prefix;
                if (consumer.expect_and_consume_operator(Operator.Declaration) == null)
                {
                    panic("Expected colon after a loop variable declaration\n", .{});
                }
                const right_token = consumer.peek();
                consumer.consume();

                var right_node : *Node = undefined;

                switch (right_token.value)
                {
                    Token.ID.int_lit =>
                    {
                        const literal_value = right_token.value.int_lit;
                        const literal_node_value = Node {
                            .value = Node.Value {
                                .int_lit = IntegerLiteral {
                                    .value = literal_value,
                                    .signed = false,
                                }
                            },
                            .parent = self.current_block,
                            .value_type = Node.ValueType.RValue,
                            .type = undefined,
                        };

                        right_node = self.append_and_get(literal_node_value);
                    },
                    else =>
                    {
                        panic("Right token not implemented: {}\n", .{right_token.value});
                    }
                }

                const iterator_ref_expr_value = Node
                {
                    .value = Node.Value {
                        .identifier_expr = IdentifierExpression {
                            .name = it_identifier.value.identifier,
                        }
                    },
                    .parent = self.current_block,
                    .value_type = Node.ValueType.RValue,
                    .type = undefined,
                };

                const iterator_ref_expr = self.append_and_get(iterator_ref_expr_value);

                const prefix_comparison_value = Node
                {
                    .value = Node.Value {
                        .binary_expr = BinaryExpression {
                            .left = iterator_ref_expr,
                            .right =  right_node,
                            .id = BinaryExpression.ID.Compare_LessThan,
                            .parenthesis = false,
                        }
                    },
                    .parent = self.current_block,
                    .value_type = Node.ValueType.RValue,
                    .type = undefined,
                };

                const prefix_comparison_node = self.append_and_get(prefix_comparison_value);
                // @Info: in other kind of loops we should support multiple statements
                self.current_block.value.block_expr.statements.append(prefix_comparison_node) catch |err| {
                    panic("Couldn't allocate prefix statement\n", .{});
                };
            }

            // Block
            {
                // @Info: this sets the target block as current block, so no need here to set it in advance
                self.block(allocator, consumer, for_node.value.loop_expr.body, true);
            }

            // Postfix
            {
                self.current_block = for_node.value.loop_expr.postfix;
                const identifier_expr_lvalue = Node
                {
                    .value = Node.Value {
                        .identifier_expr = IdentifierExpression {
                            .name = it_identifier.value.identifier,
                        }
                    },
                    .parent = self.current_block,
                    .value_type = Node.ValueType.LValue,
                    .type = undefined,
                };
                var identifier_expr_rvalue = identifier_expr_lvalue;
                identifier_expr_rvalue.value_type = Node.ValueType.RValue;
                const identifier_lvalue = self.append_and_get(identifier_expr_lvalue);
                const identifier_rvalue = self.append_and_get(identifier_expr_rvalue);

                const one_lit_value = Node
                {
                    .value = Node.Value {
                        .int_lit = IntegerLiteral {
                            .value = 1,
                            .signed = false,
                        }
                    },
                    .parent = self.current_block,
                    .value_type = Node.ValueType.RValue,
                    .type = undefined,
                };
                const one_lit_node = self.append_and_get(one_lit_value);

                const postfix_increment_value = Node
                {
                    .value = Node.Value {
                        .binary_expr = BinaryExpression {
                            .left = identifier_rvalue,
                            .right =  one_lit_node,
                            .id =  BinaryExpression.ID.Plus,
                            .parenthesis = false,
                        },
                        },
                    .parent = self.current_block,
                    .value_type = Node.ValueType.RValue,
                    .type = undefined,
                };

                const postfix_increment_node = self.append_and_get(postfix_increment_value);

                const postfix_assignment_value = Node
                {
                    .value = Node.Value {
                        .binary_expr = BinaryExpression {
                            .left = identifier_lvalue,
                            .right =  postfix_increment_node,
                            .id =  BinaryExpression.ID.Assignment,
                            .parenthesis = false,
                        },
                        },
                    .parent = self.current_block,
                    .value_type = Node.ValueType.RValue,
                    .type = undefined,
                };

                const postfix_assignment_node = self.append_and_get(postfix_assignment_value);
                self.current_block.value.block_expr.statements.append(postfix_assignment_node) catch |err| {
                    panic("Couldn't allocate postfix statement\n", .{});
                };
            }

            self.current_block = parent_scope;
            return for_node;
        }
        else
        {
            panic("Expected identifier declaration for loop iteration\n", .{});
        }
    }

    fn parse_if(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer, parent_node: *Node) *Node
    {
        // consume if keyword
        consumer.consume();

        const branch_node_value = Node
        {
            .value = Node.Value {
                .branch_expr = BranchExpression {
                    .condition = undefined,
                    .if_block = undefined,
                    .else_block = null,
                    .exit_block_ref = 0,
                }
            },
            .parent = parent_node,
            .value_type = Node.ValueType.RValue,
            .type = undefined,
        };

        var branch_node = self.append_and_get(branch_node_value);
        const condition_node = self.parse_expression(allocator, consumer, parent_node);
        branch_node.value.branch_expr.condition = condition_node;
        const if_block_value = Node
        {
            .value = Node.Value {
                .block_expr = BlockExpression {
                    .statements = NodeRefBuffer.init(allocator),
                    .id = BlockExpression.ID.IfBlock,
                }
            },
            .parent = branch_node,
            .value_type = Node.ValueType.RValue,
            .type = undefined,
        };
        const if_block_node = self.append_and_get(if_block_value);
        self.block(allocator, consumer, if_block_node, true);
        branch_node.value.branch_expr.if_block = if_block_node;

        self.current_block = parent_node;

        if (consumer.expect_and_consume_keyword(KeywordID.@"else") != null)
        {
            const else_block_value = Node
            {
                .value = Node.Value {
                    .block_expr = BlockExpression {
                        .statements = NodeRefBuffer.init(allocator),
                        .id = BlockExpression.ID.ElseBlock,
                    }
                },
                .parent = branch_node,
                .value_type = Node.ValueType.RValue,
                .type = undefined,
            };
            const else_block_node = self.append_and_get(else_block_value);
            self.block(allocator, consumer, else_block_node, true);
            branch_node.value.branch_expr.else_block = else_block_node;
        }

        return branch_node;
    }

    fn parse_break(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer, parent_node: *Node) *Node
    {
        // consuming break keyword
        consumer.consume();
        var target = self.current_block;

        while (target.value != Node.ID.loop_expr)
        {
            if (target.parent) |parent|
            {
                target = parent;
            }
            else
            {
                panic("Couldn't find any parent\n", .{});
            }
        }

        const break_value = Node
        {
            .value = Node.Value {
                .break_expr = BreakExpression {
                    .target = target, 
                }
            },
            .parent = parent_node,
            .value_type = Node.ValueType.RValue,
            .type = undefined,
        };

        const result = self.append_and_get(break_value);
        return result;
    }

    fn block(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer, block_node: *Node, allow_no_braces: bool) void
    {
        self.current_block = block_node;

        const has_braces = consumer.expect_and_consume_sign('{') != null;

        if (!has_braces and !allow_no_braces)
        {
            parser_error("Expected braces in block\n", .{});
        }

        var should_keep_parsing = true;
        if (has_braces)
        {
            should_keep_parsing = consumer.expect_sign('}') == null;
        }

        while (should_keep_parsing)
        {
            const token = consumer.peek();

            switch (token.value)
            {
                Token.ID.keyword =>
                {
                    const keyword = token.value.keyword;
                    switch (keyword)
                    {
                        KeywordID.@"return" =>
                        {
                            const return_statement = self.parse_return(allocator, consumer, block_node);
                            if (consumer.expect_and_consume_sign(';') == null)
                            {
                                parser_error("Expected semicolon at the end of the statement\n", .{});
                            }
                            block_node.value.block_expr.statements.append(return_statement) catch |err| {
                                panic("Failed to allocate memory for statement", .{});
                            };
                        },
                        KeywordID.@"for" =>
                        {
                            const for_st = self.parse_for(allocator, consumer, block_node);
                            block_node.value.block_expr.statements.append(for_st) catch |err| {
                                panic("Failed to allocate memory for statement", .{});
                            };

                        },
                        KeywordID.@"if" =>
                        {
                            const if_st = self.parse_if(allocator, consumer, block_node);
                            block_node.value.block_expr.statements.append(if_st) catch |err| {
                                panic("Failed to allocate memory for statement", .{});
                            };
                        },
                        KeywordID.@"break" =>
                        {
                            const break_st = self.parse_break(allocator, consumer, block_node);
                            if (consumer.expect_and_consume_sign(';') == null)
                            {
                                parser_error("Expected semicolon at the end of the statement\n", .{});
                            }
                            block_node.value.block_expr.statements.append(break_st) catch |err| {
                                panic("Failed to allocate memory for statement", .{});
                            };
                        },
                        else =>
                        {
                            panic("Keyword unhandled: {}\n", .{keyword});
                        },
                    }

                },
                Token.ID.identifier =>
                {
                    const identifier_name = token.value.identifier;
                    consumer.consume();
                    if (consumer.expect_and_consume_operator(Operator.Declaration) != null)
                    {
                        var var_decl_node_value = Node
                        {
                            .value = Node.Value {
                                .var_decl = VariableDeclaration {
                                    .name = identifier_name,
                                    // Info: These are set later 
                                    .var_type =  undefined,
                                    .var_scope =  block_node,
                                    .backend_ref =  0,
                                    .is_function_arg = false,
                                },
                            },
                            .parent = block_node,
                            .value_type = Node.ValueType.LValue,
                            .type = undefined,
                        };

                        var var_decl_node = self.append_and_get(var_decl_node_value);
                        if (consumer.expect_and_consume_operator(Operator.Assignment) == null)
                        {
                            var_decl_node.value.var_decl.var_type = self.parse_type(allocator, consumer, block_node);

                            if (consumer.expect_and_consume_operator(Operator.Assignment) == null)
                            {
                                panic("expected assignment after type in variable declaration\n", .{});
                            }
                        }
                        else
                        {
                            // no type information
                            panic("not implemented: var declaration without type information\n", .{});
                        }

                        var initialization_assignment: ?*Node = null;
                        // @Info: separate variable declaration and initialization
                        if (consumer.expect_sign(';') == null)
                        {
                            const id_expr = Node 
                            {
                                .value = Node.Value {
                                    .identifier_expr = IdentifierExpression {
                                        .name = identifier_name,
                                    },
                                },
                                .value_type = Node.ValueType.LValue,
                                .parent = block_node,
                                .type = undefined,
                            };

                            const id_expr_node = self.append_and_get(id_expr);

                            initialization_assignment = self.parse_binary_expression(allocator, consumer, block_node, id_expr_node, Operator.Assignment, Precedence.Assignment);
                        }
                        else
                        {
                            // no initialization
                            panic("not implemented: var declaration without initialization\n", .{});
                        }

                        if (consumer.expect_and_consume_sign(';') == null)
                        {
                            parser_error("Expected semicolon at the end of the statement\n", .{});
                        }
                        block_node.value.block_expr.statements.append(var_decl_node) catch |err| {
                            panic("Failed to allocate memory for statement", .{});
                        };
                        self.current_function.value.function_decl.variables.append(var_decl_node) catch |err| {
                            panic("Error allocating variable reference to function variable list\n", .{});
                        };
                        if (initialization_assignment) |assignment|
                        {
                            block_node.value.block_expr.statements.append(assignment) catch |err| {
                                panic("Failed to allocate memory for statement", .{});
                            };
                        }
                    }
                    else
                    {
                        log.debug("Rectifying and parsing identifier expression...\n", .{});
                        consumer.next_index -= 1;
                        const identifier_expr = self.parse_expression(allocator, consumer, block_node);
                        if (consumer.expect_and_consume_sign(';') == null)
                        {
                            parser_error("Expected semicolon at the end of the statement\n", .{});
                        }
                        block_node.value.block_expr.statements.append(identifier_expr) catch |err| {
                            panic("Failed to allocate memory for statement", .{});
                        };
                    }
                },
                Token.ID.operator =>
                {
                    const operator = token.value.operator;
                    switch (operator)
                    {
                        Operator.Dereference =>
                        {
                            const deref_st = self.parse_expression(allocator, consumer, block_node);
                            if (consumer.expect_and_consume_sign(';') == null)
                            {
                                parser_error("Expected semicolon at the end of the statement\n", .{});
                            }
                            block_node.value.block_expr.statements.append(deref_st) catch |err| {
                                panic("Failed to allocate memory for statement", .{});
                            };
                        },
                        else => panic("ni: {}\n", .{token.value}),
                    }
                },
                else =>
                {
                    panic("ni: {}\n", .{token.value});
                },
            }

            const next_token = consumer.peek();
            should_keep_parsing = has_braces and consumer.expect_sign('}') == null;
        }

        if (has_braces)
        {
            const end = consumer.expect_and_consume_sign('}');
            if (end == null)
            {
                parser_error("Expected end sign at the end of the block", .{});
            }
        }
    }

    fn parse_function_declaration(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer) !?*Node
    {
        var function_node_value = Node
        {
            .value = Node.Value
            {
                .function_decl = FunctionDeclaration
                {
                    .name = "",
                    .arguments = ArrayList(*Node).init(allocator),
                    .blocks = ArrayList(*Node).init(allocator),
                    .variables = ArrayList(*Node).init(allocator),
                    .type = undefined,
                },
            },
            .parent = null,
            .value_type = Node.ValueType.LValue,
            .type = undefined,
        };

        var function_node = self.append_and_get(function_node_value);
        self.current_function = function_node;

        var function_type = TypeIdentifier
        {
            .value = TypeIdentifier.Value
            {
                .function = TypeIdentifier.Function {
                    .arg_types = NodeRefBuffer.init(allocator),
                    .return_type = null,
                }
            }
        };

        var next_token = consumer.tokens[consumer.next_index];
        var arg_types = NodeRefBuffer.init(allocator);
        var args_left_to_parse = !(next_token.value == Token.ID.operator and next_token.value.operator == Operator.RightParenthesis);

        while (args_left_to_parse)
        {
            const arg_node = self.parse_expression(allocator, consumer, function_node);
            if (arg_node.value != Node.ID.var_decl)
            {
                parser_error("Error parsing argument\n", .{});
            }

            arg_types.append(arg_node.value.var_decl.var_type) catch |err| {
                panic("Error allocating memory for argument type\n", .{});
            };

            arg_node.value.var_decl.is_function_arg = true;
            try function_node.value.function_decl.arguments.append(arg_node);

            next_token = consumer.tokens[consumer.next_index];
            args_left_to_parse = !(next_token.value == Token.ID.operator and next_token.value.operator == Operator.RightParenthesis);

            if (args_left_to_parse)
            {
                const comma = consumer.expect_and_consume_sign(',');
                if (comma == null)
                {
                    // print error
                    parser_error("Expected comma after function argument", .{});
                }
            }
        }

        if (consumer.expect_and_consume_operator(Operator.RightParenthesis) == null)
        {
            parser_error("Expected end of argument list\n", .{});
        }

        var return_type: ?*Node = null;
        if (consumer.expect_and_consume_operator(Operator.Arrow) != null)
        {
            return_type = self.parse_type(allocator, consumer, null);
        }

        var function_type_node_value = Node
        {
            .value = Node.Value {
                .type_identifier = TypeIdentifier {
                    .value = TypeIdentifier.Value {
                        .function = TypeIdentifier.Function {
                            .arg_types = arg_types,
                            .return_type = return_type,
                        },
                    },
                },
            },
            .parent = function_node,
            .value_type = Node.ValueType.RValue,
            .type = undefined,
        };

        log.debug("arg type count in the parser: {}\n", .{arg_types.items.len});

        function_node.value.function_decl.type = self.append_and_get(function_type_node_value);

        var block_node_value = Node
        {
            .value = Node.Value
            {
                .block_expr = BlockExpression
                {
                    .statements = NodeRefBuffer.init(allocator),
                    .id = BlockExpression.ID.Function,
                },
            },
            .parent = function_node,
            .value_type = Node.ValueType.RValue,
            .type = undefined,
        };

        var block_node = self.append_and_get(block_node_value);
        self.current_function.value.function_decl.blocks.append(block_node) catch |err| {
            panic("Failed to allocate memory for block node reference\n", .{});
        };

        self.block(allocator, consumer, block_node, false);

        return function_node;
    }

    fn tld(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer) !void
    {
        const name_token = consumer.expect_and_consume(Token.ID.identifier);
        if (name_token == null)
        {
            parser_error("Top level declarations must start with an identifier/name\n", .{});
        }
        const name = name_token.?.value.identifier;

        //print("Name found: {s}\n", .{function_name.?.value.identifier});

        if (consumer.next_index + 2 >= consumer.tokens.len)
        {
            parser_error("End of the file while parsing top level declaration\n", .{});
        }

        const constant = consumer.expect_and_consume_operator(Operator.Constant);
        if (constant == null)
        {
            panic("not implemented: top level declaration with type in the middle specified\n", .{});
        }

        if (consumer.expect_and_consume_operator(Operator.LeftParenthesis) != null)
        {
            const function_result = self.parse_function_declaration(allocator, consumer) catch |err| {
                parser_error("Couldn't parse the function\n", .{});
            };
            if (function_result) |function_node|
            {
                function_node.value.function_decl.name = name;
                self.function_declarations.append(function_node) catch |err| {
                    panic("Failed to allocate node reference for top-level declaration\n", .{});
                };
            }
            else
            {
                parser_error("Couldn't parse the function\n", .{});
            }
        }
        else if (consumer.expect_keyword(KeywordID.@"struct") != null)
        {
            const struct_type = self.parse_type(allocator, consumer, null);
            struct_type.value.type_identifier.value.structure.name = name;
            self.type_declarations.append(struct_type) catch |err| {
                panic("Error allocating memory for type declaration\n", .{});
            };
        }
        else
        {
            panic("Couldn't parse top level declaration\n", .{});
        }
    }
};

pub const ParserResult = struct
{
    function_declarations: NodeRefBuffer,
    node_buffer: NodeBuffer,
    type_declarations: NodeRefBuffer,
};

pub fn parse(allocator: *Allocator, lexer_result: LexerResult) ParserResult
{
    log.debug("\n==============\nPARSER\n==============\n\n", .{});

    const token_count = lexer_result.tokens.len;
    assert(token_count > 0);

    var token_consumer = TokenConsumer
    {
        .tokens = lexer_result.tokens,
        .next_index = 0,
    };

    var parser = Parser
    {
        .nb = NodeBuffer.init(allocator) catch |err| {
            panic("Couldn't allocate the bucket node buffer\n", .{});
        },
        .current_function = undefined,
        .current_block = undefined,
        .function_declarations = NodeRefBuffer.init(allocator),
        .type_declarations = NodeRefBuffer.init(allocator),
    };

    while (token_consumer.next_index < token_count)
    {
        parser.tld(allocator, &token_consumer) catch |err| {
            panic("Couldn't parse TLD\n", .{});
        };
    }

    const result = ParserResult
    {
        .function_declarations = parser.function_declarations,
        .type_declarations = parser.type_declarations,
        .node_buffer = parser.nb,
    };

    log.debug("Printing AST:\n\n", .{});
    for (result.function_declarations.items) |function|
    {
        log.info("{}", .{function});
    }

    return result;
}
