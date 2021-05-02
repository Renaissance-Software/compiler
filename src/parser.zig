// @TODO: fix up all the parent nodes
// @TODO: separate variable declaration and initialization value
const std = @import("std");
const assert = std.debug.assert;
const print = std.debug.print;
const panic = std.debug.panic;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const BucketArrayList = @import("bucket_array.zig").BucketArrayList;
const TypeBuffer = BucketArrayList(TypeIdentifier, 64);
const Internal = @import("compiler.zig");

const Operator = Internal.Operator;
const Compiler = Internal.Compiler;
const KeywordID = Internal.KeywordID;
const Lexer = @import("lexer.zig");
const Token = Lexer.Token;
const LexerResult = Lexer.LexerResult;
const NodeRefBuffer = ArrayList(*Node);
const NodeBuffer = BucketArrayList(Node, 64);

const IntegerLiteral = struct
{
    value: u64,
    bit_count: u16,
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

        fn get_importance(self: BinaryExpression.ID) OperatorPrecedenceImportance
        {
            switch (self)
            {
                BinaryExpression.ID.Plus => return OperatorPrecedenceImportance.Binary_Addition_Substraction,
                BinaryExpression.ID.Minus => return OperatorPrecedenceImportance.Binary_Addition_Substraction,
                BinaryExpression.ID.Multiplication => return OperatorPrecedenceImportance.Binary_Multiplication_Division_Remainder,
                BinaryExpression.ID.Assignment => return OperatorPrecedenceImportance.Binary_AssignmentOperators,
                BinaryExpression.ID.Compare_Equal => return OperatorPrecedenceImportance.Binary_RelationalOperators_EqualNotEqual,
                BinaryExpression.ID.Compare_NotEqual => return OperatorPrecedenceImportance.Binary_RelationalOperators_EqualNotEqual,
                BinaryExpression.ID.Compare_LessThan => return OperatorPrecedenceImportance.Binary_RelationalOperators,
                BinaryExpression.ID.Compare_GreaterThan => return OperatorPrecedenceImportance.Binary_RelationalOperators,
                BinaryExpression.ID.Compare_LessThanOrEqual => return OperatorPrecedenceImportance.Binary_RelationalOperators,
                BinaryExpression.ID.Compare_GreaterThanOrEqual => return OperatorPrecedenceImportance.Binary_RelationalOperators,
                else => panic("Not implemented: {}\n", .{self}),
            }
        }
    };

    const OperatorPrecedenceImportance = enum(u8)
    {
        Unary_ArraySubscript_FunctionCall_MemberAccess_CompoundLiteral,
        Unary_LogicalNot_BitwiseNot_Cast_Dereference_AddressOf_SizeOf_AlignOf,
        Binary_Multiplication_Division_Remainder,
        Binary_Addition_Substraction,
        Binary_BitwiseLeftShit_BitwiseRightShift,
        Binary_RelationalOperators,
        Binary_RelationalOperators_EqualNotEqual,
        Binary_BitwiseAND,
        Binary_BitwiseXOR,
        Binary_BitwiseOR,
        Binary_LogicalAND,
        Binary_LogicalOR,
        Binary_AssignmentOperators,
    };
};

const ReturnExpression = struct
{
    expression: ?*Node,
};

// @Info: Variable expression must reference a variable declaration (in which function arguments are included)
// @Warning: this is not true anymore dgm 29-04-2021
const IdentifierExpression = struct
{
    name: []const u8,
    reference: ?*Node,
};

const TypeIdentifier = struct
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
        // @TODO: change?
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
    var_value: ?*Node,
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

// @TODO: improve this one
const BreakExpression = struct
{
    target: *Node,
    origin: *Node,
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
                if (self.value.var_decl.is_function_arg)
                {
                    try std.fmt.format(writer, "{s}: {}", .{self.value.var_decl.name, self.value.var_decl.var_type});
                }
                else
                {
                    try std.fmt.format(writer, "{s}: {} = {}", .{self.value.var_decl.name, self.value.var_decl.var_type, self.value.var_decl.var_value});
                }
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
            //else => panic("Not implemented: {}\n", .{self.value}),
        }
    }
};

const TokenConsumer = struct
{
    tokens: []const Token,
    next_index: usize,
    compiler: *Compiler,

    fn peek(self: *TokenConsumer) Token
    {
        const token = self.tokens[self.next_index];
        return token;
    }

    fn consume(self: *TokenConsumer) void 
    {
        const consumed_token = self.tokens[self.next_index];
        self.compiler.log("Consuming {}\n", .{consumed_token});
        self.next_index += 1;
    }

    fn expect_and_consume(self: *TokenConsumer, id: Token.ID) ?Token
    {
        const token = self.tokens[self.next_index];
        if (token.value == id) {
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

    compiler: *Compiler,

    parsing_function_args: bool,

    fn append_and_get(self: *Parser, node: Node) *Node
    {
        const result = self.nb.append(node) catch |err| {
            panic("Couldn't allocate memory for node", .{});
        };
        return result;
    }

    fn _find_existing_invoke_expression(self: *Parser, invoke_expr_name: []const u8) *Node
    {
        for (self.function_declarations.items) |function_node|
        {
            if (std.mem.eql(u8, function_node.value.function_decl.name, invoke_expr_name))
            {
                return function_node;
            }
        }

        if (std.mem.eql(u8, self.current_function.value.function_decl.name, invoke_expr_name))
        {
            return self.current_function;
        }

        panic("Couldn't find any fitting invoke expression\n", .{});
    }

    fn _find_existing_variable(self: *Parser, existing_variable_name: []const u8) *Node
    {
        for (self.current_function.value.function_decl.arguments.items) |arg_node|
        {
            assert(arg_node.value == Node.ID.var_decl);
            if (std.mem.eql(u8, arg_node.value.var_decl.name, existing_variable_name))
            {
                return arg_node;
            }
        }

        for (self.current_function.value.function_decl.variables.items) |var_node|
        {
            assert(var_node.value == Node.ID.var_decl);
            if (std.mem.eql(u8, var_node.value.var_decl.name, existing_variable_name))
            {
                return var_node;
            }
        }

        panic("Variable name \"{s}\" not found\n", .{existing_variable_name});
    }

    fn parse_type(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer, parent_node: ?*Node) *Node
    {
        self.compiler.log("Parsing type...\n", .{});
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
                            self.compiler.report_error("Error: expected '{c}' at the beginning of the struct\n", .{'{'});
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
                            .parent = null,
                            .value_type = Node.ValueType.RValue,
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
                                    self.compiler.report_error("Expected comma after argument. Found: {}\n", .{token.value});
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
                            self.compiler.report_error("Empty struct is not allowed\n", .{});
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
                                            .len_expr = self.parse_expression(allocator, consumer, parent_node.?),
                                        },
                                    },
                                },
                            },
                            .parent = parent_node,
                            .value_type = Node.ValueType.LValue,
                        };

                        if (consumer.expect_and_consume_operator(Operator.RightBracket) == null)
                        {
                            self.compiler.report_error("Expected ']' in array type\n", .{});
                        }

                        type_node_value.value.type_identifier.value.array.type = self.parse_type(allocator, consumer, parent_node);

                        const type_node = self.append_and_get(type_node_value);
                        return type_node;
                    },
                    else => panic("ni: {}\n", .{operator}),
                }
            },
            else => panic("not implemented: {}\n", .{next_token.value}),
        }
    }

    fn primary_expression(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer) ?*Node
    {
        const token = consumer.peek();
        switch (token.value)
        {
            Token.ID.int_lit =>
            {
                consumer.consume();
                const int_value = token.value.int_lit;
                //print("Found int literal: {}\n", .{int_value});
                const int_lit_node = Node
                {
                    .value = Node.Value{
                        .int_lit = IntegerLiteral{
                            .value = int_value,
                            .bit_count = 32,
                            .signed = false,
                        },
                    },
                    .value_type = Node.ValueType.RValue,
                    .parent = parent_node,
                };

                return self.append_and_get(int_lit_node);
            },
            Token.ID.identifier =>
            {
                consumer.consume();
                const identifier_name = token.value.identifier;
                if (consumer.expect_and_consume_operator(Operator.Declaration) != null)
                {
                    var var_decl_node = Node
                    {
                        .value = Node.Value {
                            .var_decl = VariableDeclaration {
                                .name = identifier_name,
                                // Info: These are set later 
                                .var_type =  undefined,
                                .var_value =  undefined,
                                .var_scope =  undefined,
                                .backend_ref =  undefined,
                                .is_function_arg =  undefined,
                            },
                        },
                        .parent = parent_node,
                        .value_type = Node.ValueType.LValue,
                    };
                    const result = self.append_and_get(var_decl_node);
                    return result;
                }
                else if (consumer.expect_and_consume_operator(Operator.LeftParenthesis) != null)
                {
                    const invoke_expr_node_value = Node
                    {
                        .value = Node.Value {
                            .invoke_expr = InvokeExpression {
                                .arguments = NodeRefBuffer.init(allocator),
                                .expression = self.find_existing_invoke_expression(identifier_name),
                            },
                        },
                        .parent = parent_node,
                        .value_type = Node.ValueType.RValue,
                    };
                    var invoke_expr_node = self.append_and_get(invoke_expr_node_value);

                    var args_left_to_parse = consumer.expect_and_consume_operator(Operator.RightParenthesis) == null;

                    while (args_left_to_parse)
                    {
                        if (self.parse_expression(allocator, consumer, types, invoke_expr_node)) |new_arg|
                        {
                            invoke_expr_node.value.invoke_expr.arguments.append(new_arg) catch |err| {
                                panic("Failed to append argument for invoke expression\n", .{});
                            };
                            args_left_to_parse = consumer.expect_and_consume_operator(Operator.RightParenthesis) == null;
                            if (args_left_to_parse)
                            {
                                if (consumer.expect_and_consume_sign(',') == null)
                                {
                                    panic("Expected comma after invoke expression argument\n", .{});
                                }
                            }
                        }
                        else
                        {
                            // @TODO: convert into a compiler error
                            panic("Couldn't parse argument for invoke expression: {s}\n", .{identifier_name});
                        }
                    }

                    return invoke_expr_node;
                }
                else
                {
                    const var_expr_node_value = Node
                    {
                        .value = Node.Value {
                            .var_expr = VariableExpression {
                                .declaration = self.find_existing_variable(identifier_name),
                            },
                            },
                        .parent = parent_node,
                        .value_type = Node.ValueType.RValue,
                    };
                    const var_expr_node = self.append_and_get(var_expr_node_value);
                    return var_expr_node;
                }
            },
            Token.ID.sign =>
            {
                const sign = token.value.sign;
                switch (sign)
                {
                    ';' =>
                    {
                        return null;
                    },
                    '-' =>
                    {
                        panic("Not implemented\n", .{});
                    },
                    '(' =>
                    {
                        consumer.consume();
                        if (self.parse_expression(allocator, consumer, types, parent_node)) |expr_in_parenthesis|
                        {
                            if (consumer.expect_and_consume_operator(Operator.RightParenthesis) == null)
                            {
                                panic("Expected closing parenthesis after opening parenthesis\n", .{});
                            }

                            assert(expr_in_parenthesis.value == Node.ID.binary_expr);
                            expr_in_parenthesis.value.binary_expr.parenthesis = true;
                            return expr_in_parenthesis;
                        }
                        else
                        {
                            panic("Unable to parse parenthesis expression\n", .{});
                        }
                    },
                    '@' =>
                    {
                        consumer.consume();
                        const unary_expr_value = Node {
                            .value = Node.Value {
                                .unary_expr = UnaryExpression {
                                    .node_ref = undefined,
                                    .id = UnaryExpression.ID.Dereference,
                                    .location = UnaryExpression.Location.Prefix,
                                }
                            },
                            .parent = parent_node,
                            .value_type = Node.ValueType.RValue,
                        };

                        var pointer_deref = self.append_and_get(unary_expr_value);
                        if (self.primary_expression(allocator, consumer, types, pointer_deref)) |pointer|
                        {
                            pointer_deref.value.unary_expr.node_ref = pointer;
                            return pointer_deref;
                        }
                        else
                        {
                            panic("Unable to parse pointer dereference\n", .{});
                        }
                    },
                    '&' =>
                    {
                        consumer.consume();
                        const addressof_node_value = Node
                        {
                            .value = Node.Value {
                                .unary_expr = UnaryExpression {
                                    .node_ref = undefined,
                                    .id = UnaryExpression.ID.AddressOf,
                                    .location = UnaryExpression.Location.Prefix,
                                }
                            },
                            .parent = parent_node,
                            .value_type = Node.ValueType.RValue,
                        };

                        var addressof_node = self.append_and_get(addressof_node_value);
                        if (self.parse_expression(allocator, consumer, types, addressof_node)) |variable|
                        {
                            addressof_node.value.unary_expr.node_ref = variable;
                            return addressof_node;
                        }
                        else
                        {
                            panic("Can't parse address-of expression\n", .{});
                        }
                    },
                    '[' =>
                    {
                        consumer.consume();
                        const array_lit_node_value = Node 
                        {
                            .value = Node.Value {
                                .array_lit = ArrayLiteral {
                                    .elements = NodeRefBuffer.init(allocator),
                                    .type_expression = undefined,
                                }
                            },
                            .parent = parent_node,
                            .value_type = Node.ValueType.RValue,
                        };

                        var array_lit_node = self.append_and_get(array_lit_node_value);
                        if (consumer.expect_and_consume_operator(Operator.RightBracket) == null)
                        {
                            while (true)
                            {
                                if (self.parse_expression(allocator, consumer, types, array_lit_node)) |literal_node|
                                {
                                    array_lit_node.value.array_lit.elements.append(literal_node) catch |err| {
                                        panic("Couldn't allocate memory for element of array literal\n", .{});
                                    };

                                    const elements_to_parse = consumer.expect_and_consume_operator(Operator.RightBracket) == null;
                                    if (elements_to_parse)
                                    {
                                        if (consumer.expect_and_consume_sign(',') == null)
                                        {
                                            panic("Expected comma after array literal element\n", .{});
                                        }
                                    }
                                    else
                                    {
                                        break;
                                    }
                                }
                                else
                                {
                                    panic("Couldn't parse array literal expression\n", .{});
                                }
                            }

                            const first_element = array_lit_node.value.array_lit.elements.items[0];
                            assert(parent_node != null);
                            const expected_type = get_type(parent_node.?, null);
                            const element_type = get_type(first_element, expected_type);
                            const array_length = array_lit_node.value.array_lit.elements.items.len;
                            array_lit_node.value.array_lit.type_expression = Type.get_array_type(element_type, array_length, types);

                            return array_lit_node;
                        }
                        else
                        {
                            panic("Not implemented: [], empty array literal\n", .{});
                        }
                    },
                    else =>
                    {
                        panic("Not implemented: {c}\n", .{sign});
                    }
                }
            },
            else =>
            {
                panic("Not implemented: {}\n", .{token.value});
            },
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

    // List of expressions
    // -------------------
    // - grouping
    // - unary
    //
    //
    //
    //
    //
    
    // @TODO: define this when we have a clear sense of what's going on
    //const precedence_rules = PrecedenceRule[_]
    //{

    //};
    
    const ParseFn = fn (self: *Parser, allocator: *Allocator, consumer: *TokenConsumer) void;

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
                    .node_ref = self.parse_precedence(allocator, consumer, parent_node, Precedence.Unary),
                    .id = unary_expr_id,
                },
            },
            .value_type = Node.ValueType.RValue,
            .parent = parent_node,
        };

        const unary_expr = self.append_and_get(unary_expr_node);
        return unary_expr;
    }

    fn parse_array_literal(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer, parent_node: *Node) *Node
    {
        var array_list = NodeRefBuffer.init(allocator);
            
        if (consumer.expect_and_consume_operator(Operator.RightBracket) == null)
        {
            while (true)
            {
                const array_elem = self.parse_expression(allocator, consumer, parent_node);
                array_list.append(array_elem) catch |err| {
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
                    self.compiler.report_error("Expected comma after argument. Found: {}\n", .{next_token.value});
                }
            }
        }
        else
        {
            self.compiler.log("Empty array literal is not allowed\n", .{});
        }

        const array_literal_node = Node
        {
            .value = Node.Value {
                .array_lit = ArrayLiteral {
                    .elements = array_list,
                },
            },
            .value_type = Node.ValueType.RValue,
            .parent = parent_node,
        };

        const array_literal = self.append_and_get(array_literal_node);
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
                            .bit_count = 32,
                            .signed = false,
                        },
                    },
                    .value_type = Node.ValueType.RValue,
                    .parent = parent_node,
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
                            // .reference = self.find_existing_variable(identifier_name),
                            .reference = null,
                            .name = identifier_name,
                        },
                        },
                    .parent = parent_node,
                    .value_type = Node.ValueType.RValue,
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

    fn get_infix_rule(token: Token) ParseFn
    {
        panic("foo\n", .{});
    }

    const OperatorResult = struct 
    {
        precedence: Precedence,
        binop: BinaryExpression.ID,
    };

    fn get_precedence_for_operator(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer, operator: Operator) Precedence
    {
        switch (operator)
        {
            Operator.RightParenthesis,
            Operator.RightBracket,
                => return Precedence.None,

            Operator.Plus,
            Operator.Minus
                => return Precedence.LightArithmetic,

            Operator.Multiplication,
            Operator.Division
                => return Precedence.HeavyArithmetic,

            Operator.Equal,
            Operator.GreaterThan,
            Operator.LessThan
                => return Precedence.Compare,


            Operator.Assignment => return Precedence.Assignment,

            Operator.Declaration => return Precedence.Declaration,

            Operator.LeftParenthesis,
            Operator.LeftBracket,
            Operator.Dot,
                => return Precedence.Call,

            else => panic("Precedence not implemented for {}\n", .{operator}),
        }
    }

    fn get_precedence(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer, token: Token) Precedence
    {
        switch (token.value)
        {
            Token.ID.sign =>
            {
                const sign = token.value.sign;
                switch (sign)
                {
                    '{' => return Precedence.None,
                    ';' => return Precedence.None,
                    ',' => return Precedence.None,
                    else => panic("Precedence not implemented for sign {c}\n", .{sign}),
                }
            },
            Token.ID.operator =>
            {
                const operator = token.value.operator;
                return self.get_precedence_for_operator(allocator, consumer, operator);
            },
            else => panic("Precedence not implemented for {}\n", .{token.value}),
        }
    }

    fn parse_anonymous_function_with_arg_list(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer, parent_node: *Node, left_expr: *Node, operator: Operator, precedence: Precedence, arg_list: NodeRefBuffer, return_type_node: ?*Node) *Node
    {
        panic("not implemented yet\n", .{});
    }

    fn parse_invoke_expr(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer, parent_node: *Node, left_expr: *Node, operator: Operator, precedence: Precedence) *Node
    {
        self.compiler.log("Parsing argument list\n", .{});
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
                    self.compiler.report_error("Expected comma after argument. Found: {}\n", .{next_token.value});
                }
                const first_arg = arg_list.items[0];
                is_function_declaration = first_arg.value == Node.ID.var_decl;
            }
        }

        self.compiler.log("Argument count in argument list: {}\n", .{arg_list.items.len});

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
            self.compiler.log("Parsing function declaration\n", .{});
            result = self.parse_anonymous_function_with_arg_list(allocator, consumer, parent_node, left_expr, operator, precedence, arg_list, return_type_node);
        }
        else
        {
            self.compiler.log("Parsing function call expression\n", .{});
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
                    .var_value = null,
                    .var_scope = parent_node,
                    .backend_ref = 0,
                    .is_function_arg = false,
                },
            },
            .value_type = Node.ValueType.LValue,
            .parent = parent_node,
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
                    .index = self.parse_expression(allocator, consumer, parent_node),
                },
                },
            .parent = parent_node,
            .value_type = Node.ValueType.RValue,
        };

        var subscript_node = self.append_and_get(subscript_node_value);
        if (consumer.expect_and_consume_operator(Operator.RightBracket) == null)
        {
            self.compiler.report_error("Expected ']' in array subscript expression", .{});
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
                    .field_expr = self.parse_expression(allocator, consumer, parent_node),
                },
                },
            .parent = parent_node,
            .value_type = Node.ValueType.RValue,
        };

        var field_access_node = self.append_and_get(field_acces_value);

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
            const new_precedence = self.get_precedence(allocator, consumer, token);

            self.compiler.log("Old precedence: {}, new precedence: {}\n", .{precedence, new_precedence});
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

    // @TODO: Rectify signature if needed
    fn parse_binary_expression(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer, parent_node: *Node, left_expr: *Node, operator: Operator, precedence: Precedence) *Node
    {
        const right_expr = self.parse_precedence(allocator, consumer, parent_node, precedence.increment());
        self.compiler.log("Right expr: {}\n", .{right_expr});
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

        if (binary_op == BinaryExpression.ID.Assignment)
        {
            left_expr.value_type = Node.ValueType.LValue;
        }

        const binary_node_value = Node
        {
            .value = Node.Value {
                .binary_expr = BinaryExpression {
                    .left = left_expr,
                    .right = right_expr,
                    .id = binary_op,
                    .parenthesis = false,
                },
                },
            .parent = parent_node,
            .value_type = Node.ValueType.RValue,
        };

        const binary_node = self.append_and_get(binary_node_value);

        self.compiler.log("New binary expression: {}\n", .{binary_node});
        return binary_node;
    }

    fn _expression(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer) ?*Node
    {
        if (self.primary_expression(allocator, consumer, parent_node)) |left_expr|
        {
            var left = left_expr;
            if (left.value == Node.Value.var_decl)
            {
                var var_decl_node = left;
                var_decl_node.value.var_decl.var_type = parser.parse_type(allocator, consumer, );

                if (consumer.expect_and_consume_operator(Operator.Assignment) != null)
                {
                    if (self.parse_expression(allocator, consumer, var_decl_node)) |var_value|
                    {
                        var_decl_node.value.var_decl.var_value = var_value;
                        self.current_function.value.function_decl.variables.append(var_decl_node) catch |err| {
                            panic("Couldn't append reference to the variable in the function variable list\n", .{});
                };
                        return var_decl_node;
                    }
                    else
                    {
                        // @TODO: turn into a compiler error
                        panic("Variable assignment followed by an equal must be an assignment\n", .{});
                    }
                }
                else
                {
                    // No value
                    return var_decl_node;
                }
            }
            else
            {
                const result = self.right_expression(allocator, consumer, parent_node, &left);
                if (Internal.should_log)
                {
                    if (result) |result_expr|
                    {
                        if (result_expr.value == Node.ID.binary_expr)
                        {
                            print("\nGenerated binary expression:\nLEFT: {} ({s})\n{}\nRIGHT: {} ({s})\n\n", .{result_expr.value.binary_expr.left, @tagName(result_expr.value.binary_expr.left.value), result_expr.value.binary_expr.id, result_expr.value.binary_expr.right, @tagName(result_expr.value.binary_expr.right.value)});
                        }
                    }
                }
                panic("@TODO: we should try to rewrite the tree here taking into account operator precedence\n", .{});

                //return result;
            }
        }
        else
        {
            return null;
        }
    }

    fn right_expression(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer) ?*Node
    {
        binop_loop: while (true)
        {
            var token = consumer.peek();
            var binop: BinaryExpression.ID = undefined;

            var left_expr = left_ptr.*;

            switch (token.value)
            {
                Token.ID.sign =>
                {
                    const sign = token.value.sign;
                    switch (sign)
                    {
                        ':' =>
                        {
                            consumer.consume();
                            if (consumer.expect_and_consume_operator(Operator.Declaration) != null)
                            {
                                if (consumer.expect_operator(Operator.LeftParenthesis) != null)
                                {
                                    panic("Unreachable. Unexpected function here\n", .{});
                                }
                                else
                                {
                                    break :binop_loop;
                                }
                            }
                            else
                            {
                                binop = BinaryExpression.ID.VariableDeclaration;
                            }
                        },
                        '=' =>
                        {
                            // unreachable;
                            if (true)
                                panic("foo\n", .{});
                            consumer.consume();
                            if (consumer.expect_and_consume_sign('=') != null)
                            {
                                binop = BinaryExpression.ID.Compare_Equal;
                            }
                            else
                            {
                                binop = BinaryExpression.ID.Assignment;
                            }
                        },
                        '+' =>
                        {
                            consumer.consume();
                            binop = BinaryExpression.ID.Plus;
                        },
                        '-' =>
                        {
                            consumer.consume();
                            binop = BinaryExpression.ID.Minus;
                        },
                        '<' =>
                        {
                            consumer.consume();
                            if (consumer.expect_and_consume_sign('=') != null)
                            {
                                binop = BinaryExpression.ID.Compare_LessThanOrEqual;
                            }
                            else
                            {
                                binop = BinaryExpression.ID.Compare_LessThan;
                            }
                        },
                        '>' =>
                        {
                            consumer.consume();
                            if (consumer.expect_and_consume_sign('=') != null)
                            {
                                binop = BinaryExpression.ID.Compare_GreaterThanOrEqual;
                            }
                            else
                            {
                                binop = BinaryExpression.ID.Compare_GreaterThan;
                            }
                        },
                        '*' =>
                        {
                            consumer.consume();
                            binop = BinaryExpression.ID.Multiplication;
                        },
                        '[' =>
                        {
                            consumer.consume();
                            // do array subscript stuff
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
                            };

                            var subscript_node = self.append_and_get(subscript_node_value);
                            if (self.parse_expression(allocator, consumer, parent_node)) |index_node|
                            {
                                subscript_node.value.array_subscript_expr.index = index_node;

                                if (consumer.expect_and_consume_operator(Operator.RightBracket) == null)
                                {
                                    panic("Expected array subscript termination\n", .{});
                                }

                                left_ptr.* = subscript_node;
                                continue;
                            }
                            else
                            {
                                panic("Couldn't parse index node\n", .{});
                            }
                        },
                        '/' =>
                        {
                            consumer.consume();
                            panic("/ to binary expression sign not implemented\n", .{});
                        },
                        '.' =>
                        {
                            consumer.consume();
                            // do struct subscript stuff
                            const subscript_node_value = Node
                            {
                                .value = Node.Value {
                                    .struct_subscript_expr = StructSubscriptExpression {
                                        .expression = left_expr,
                                        .field_expr = undefined,
                                    },
                                    },
                                .parent = parent_node,
                                .value_type = Node.ValueType.RValue,
                            };

                            var subscript_node = self.append_and_get(subscript_node_value);

                            if (consumer.expect_and_consume(Token.ID.identifier)) |field_name_token|
                            {
                                const field_name = field_name_token.value.identifier;

                                print("SUBSCRIPT WRONG:\nleft: {s} = {}\n", .{@tagName(left_expr.value), left_expr.value});
                                assert(left_expr.value == Node.ID.var_expr);
                                const var_decl = left_expr.value.var_expr.declaration;
                                const var_type = var_decl.value.var_decl.var_type;
                                const struct_type = switch (var_type.value)
                                {
                                    Type.ID.structure => var_type,
                                    Type.ID.pointer => var_type.value.pointer.p_type,
                                    else => panic("not implemented: {}\n", .{var_type.value}),
                                };
                                assert(struct_type.value == Type.ID.structure);

                                for (struct_type.value.structure.fields) |field, i|
                                {
                                    if (std.mem.eql(u8, field.name, field_name))
                                    {
                                        subscript_node.value.struct_subscript_expr.field_expr = &struct_type.value.structure.fields[i];
                                        left_ptr.* = subscript_node;
                                        continue :binop_loop;
                                    }
                                }

                                panic("Struct field name not found: {s}\n", .{field_name});
                            }
                            else
                            {
                                panic("Unable to parse struct field name\n", .{});
                            }
                        },
                        else =>
                        {
                            break :binop_loop;
                        },
                    }
                },
                else =>
                {
                    break :binop_loop;
                },
            }

            assert(binop != BinaryExpression.ID.VariableDeclaration);

            self.compiler.log("Real binop: {}\n", .{binop});
            if (binop == BinaryExpression.ID.Assignment)
            {
                left_expr.value_type = Node.ValueType.LValue;
            }

            if (self.primary_expression(allocator, consumer, parent_node)) |right_expr|
            {
                print("Right expression: {}\n", .{right_expr});
                var right_precedes_left = false;
                const is_left_binary_expr = left_expr.value == Node.ID.binary_expr;
                const is_right_binary_expr = right_expr.value == Node.ID.binary_expr;
                self.compiler.log("Left binary expr: {}\n", .{is_left_binary_expr});
                self.compiler.log("Right binary expr: {}\n", .{is_right_binary_expr});

                if (is_left_binary_expr)
                    //if (is_right_binary_expr)
                {
                    print("Left is a binary expression\n", .{});
                    const left_bin_op = left_expr.value.binary_expr.id;
                    const right_bin_op = binop;
                    self.compiler.log("Left binop = {}. Right binop = {}\n", .{left_bin_op, right_bin_op});

                    const left_importance = left_bin_op.get_importance();
                    const right_importance = right_bin_op.get_importance();
                    const left_expr_operator_precedence = @enumToInt(left_importance);
                    const right_expr_operator_precedence = @enumToInt(right_importance);

                    self.compiler.log("Left importance = {} ({}). Right binop = {} ({})\n", .{left_importance, left_expr_operator_precedence, right_importance, right_expr_operator_precedence});
                    const right_over_left = right_expr_operator_precedence < left_expr_operator_precedence;
                    const left_has_parenthesis = left_expr.value.binary_expr.parenthesis;
                    const right_has_parenthesis = is_right_binary_expr and !right_expr.value.binary_expr.parenthesis;

                    right_precedes_left = is_left_binary_expr and right_over_left and !left_has_parenthesis and (!is_right_binary_expr or right_has_parenthesis);
                    self.compiler.log("Right over left: {}\nLeft has parenthesis: {}\nRight has parenthesis: {}\nFINAL: Right precedes left: {}\n", .{right_over_left, left_has_parenthesis, right_has_parenthesis, right_precedes_left});
                }

                if (right_precedes_left)
                {
                    const right_operand_of_left_binary_expression = left_expr.value.binary_expr.right;
                    self.compiler.log("RIGHT OPERAND OF LEFT BINARY EXPRESSION: {}\n", .{right_operand_of_left_binary_expression.value});

                    const new_prioritized_expression = Node
                    {
                        .value = Node.Value {
                            .binary_expr = BinaryExpression {
                                .left = right_operand_of_left_binary_expression,
                                .right = right_expr,
                                .id = binop,
                                .parenthesis = false,
                            }
                        },
                        .parent = parent_node,
                        .value_type = Node.ValueType.RValue,
                    };

                    left_expr.value.binary_expr.right = self.append_and_get(new_prioritized_expression);
                    left_ptr.* = left_expr;
                }
                else
                {
                    const new_node = Node
                    {
                        .value = Node.Value {
                            .binary_expr = BinaryExpression {
                                .left = left_expr,
                                .right = right_expr,
                                .id = binop,
                                .parenthesis = false,
                            }
                        },
                        .parent = parent_node,
                        .value_type = Node.ValueType.RValue,
                    };

                    left_ptr.* = self.append_and_get(new_node);
                }
            }
            else
            {
                return null;
            }
        }

        return left_ptr.*;
    }

    fn parse_return(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer, parent_node: *Node) ?*Node
    {
        consumer.consume();
        var return_node_value = Node{
            .value = Node.Value{
                .return_expr = ReturnExpression{
                    .expression = null,
                },
            },
            .value_type = Node.ValueType.LValue,
            .parent = parent_node,
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
        };
        const loop_block_node = self.append_and_get(loop_block_value);
        self.current_function.value.function_decl.blocks.append(loop_block_node) catch |err| {
            panic("Failed to allocate a block reference to function block list\n", .{});
        };
        return loop_block_node;
    }

    fn parse_for(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer, parent_node: *Node) ?*Node
    {
        if (consumer.expect_and_consume_keyword(KeywordID.@"for") == null)
        {
            // @TODO: improve error messages
            panic("This is not a for\n", .{});
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
        };

        var for_node = self.append_and_get(for_loop_node_value);
        const parent_scope = self.current_block;

        for_node.value.loop_expr.prefix = self._create_loop_block(allocator, for_node, BlockExpression.ID.LoopPrefix);
        for_node.value.loop_expr.body = self._create_loop_block(allocator, for_node, BlockExpression.ID.LoopBody);
        for_node.value.loop_expr.postfix = self._create_loop_block(allocator, for_node, BlockExpression.ID.LoopPostfix);

        if (consumer.expect_and_consume(Token.ID.identifier)) |it_identifier|
        {
            // @TODO: we should match it to the right operand
            const it_decl_decl_value = Node
            {
                .value = Node.Value {
                    .int_lit = IntegerLiteral {
                        .value = 0,
                        .bit_count = 32,
                        .signed = false,
                    }
                },
                // @Info: this is set later
                .parent = undefined,
                .value_type = Node.ValueType.RValue,
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
                .value_type = Node.ValueType.LValue,
            };

            var it_decl_literal_node = self.append_and_get(value_type);
            const it_decl_type_node = self.append_and_get(it_decl_decl_value);
            const it_decl_value = Node
            {
                .value = Node.Value {
                    .var_decl = VariableDeclaration {
                        .name = it_identifier.value.identifier,
                        .is_function_arg = false,
                        .var_scope = self.current_block,
                        .var_type = it_decl_type_node,
                        .var_value = it_decl_literal_node,
                        .backend_ref = 0,
                    },
                    },
                .parent = parent_node,
                .value_type = Node.ValueType.LValue,
            };

            const it_decl_node = self.append_and_get(it_decl_value);
            it_decl_literal_node.parent = it_decl_node;

            self.current_function.value.function_decl.variables.append(it_decl_node) catch |err| {
                panic("Error allocating variable reference to function variable list\n", .{});
            };
            self.current_block.value.block_expr.statements.append(it_decl_node) catch |err| {
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
                                    // @TODO: stop hardcoding this
                                    .value = literal_value,
                                    .bit_count = 32,
                                    .signed = false,
                                }
                            },
                            .parent = self.current_block,
                            .value_type = Node.ValueType.RValue,
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
                            .reference = it_decl_node,
                            .name = it_identifier.value.identifier,
                        }
                    },
                    .parent = self.current_block,
                    .value_type = Node.ValueType.RValue,
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
                };

                const prefix_comparison_node = self.append_and_get(prefix_comparison_value);
                // @TODO: support multiple statements
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
                const identifier_expr_value = Node
                {
                    .value = Node.Value {
                        .identifier_expr = IdentifierExpression {
                            .reference = it_decl_node,
                            .name = it_identifier.value.identifier,
                        }
                    },
                    .parent = self.current_block,
                    .value_type = Node.ValueType.LValue,
                };
                const identifier_expr_node = self.append_and_get(identifier_expr_value);

                const one_lit_value = Node
                {
                    .value = Node.Value {
                        .int_lit = IntegerLiteral {
                            .value = 1,
                            .bit_count = 32,
                            .signed = false,
                        }
                    },
                    .parent = self.current_block,
                    .value_type = Node.ValueType.RValue,
                };
                const one_lit_node = self.append_and_get(one_lit_value);

                const postfix_increment_value = Node
                {
                    .value = Node.Value {
                        .binary_expr = BinaryExpression {
                            .left = identifier_expr_node,
                            .right =  one_lit_node,
                            .id =  BinaryExpression.ID.Plus,
                            .parenthesis = false,
                        },
                        },
                    .parent = self.current_block,
                    .value_type = Node.ValueType.RValue,
                };

                const postfix_increment_node = self.append_and_get(postfix_increment_value);

                const postfix_assignment_value = Node
                {
                    .value = Node.Value {
                        .binary_expr = BinaryExpression {
                            .left = identifier_expr_node,
                            .right =  postfix_increment_node,
                            .id =  BinaryExpression.ID.Assignment,
                            .parenthesis = false,
                        },
                        },
                    .parent = self.current_block,
                    .value_type = Node.ValueType.RValue,
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

    fn parse_if(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer, parent_node: *Node) ?*Node
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
            };
            const else_block_node = self.append_and_get(else_block_value);
            self.block(allocator, consumer, else_block_node, true);
            branch_node.value.branch_expr.else_block = else_block_node;
        }

        return branch_node;
    }

    fn parse_break(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer, parent_node: *Node) ?*Node
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
                    .origin = undefined,
                }
            },
            .parent = parent_node,
            .value_type = Node.ValueType.RValue,
        };

        const result = self.append_and_get(break_value);
        return result;
    }

    fn statement(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer, parent_node: 
        *Node) ?*Node
    {
        const token = consumer.peek();
        var statement_node: ?*Node = null;

        switch (token.value)
        {
            Token.ID.keyword =>
            {
                const keyword = token.value.keyword;
                switch (keyword)
                {
                    KeywordID.@"return" =>
                    {
                        statement_node = self.parse_return(allocator, consumer, parent_node);
                    },
                    KeywordID.@"for" =>
                    {
                        statement_node = self.parse_for(allocator, consumer, parent_node);
                    },
                    KeywordID.@"if" =>
                    {
                        statement_node = self.parse_if(allocator, consumer, parent_node);
                    },
                    KeywordID.@"break" =>
                    {
                        statement_node = self.parse_break(allocator, consumer, parent_node);
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
                                .var_value =  undefined,
                                .var_scope =  parent_node,
                                .backend_ref =  0,
                                .is_function_arg =  false,
                            },
                        },
                        .parent = parent_node,
                        .value_type = Node.ValueType.LValue,
                    };

                    var var_decl_node = self.append_and_get(var_decl_node_value);
                    if (consumer.expect_and_consume_operator(Operator.Assignment) == null)
                    {
                        var_decl_node.value.var_decl.var_type = self.parse_type(allocator, consumer, parent_node);

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

                    if (consumer.expect_sign(';') == null)
                    {
                        var_decl_node.value.var_decl.var_value = self.parse_expression(allocator, consumer, parent_node);
                    }
                    else
                    {
                        // no initialization
                        panic("not implemented: var declaration without initialization\n", .{});
                    }

                    statement_node = var_decl_node;
                }
                else
                {
                    self.compiler.log("Rectifying and parsing identifier expression...\n", .{});
                    consumer.next_index -= 1;
                    statement_node = self.parse_expression(allocator, consumer, parent_node);
                }
            },
            Token.ID.operator =>
            {
                const operator = token.value.operator;
                switch (operator)
                {
                    Operator.Dereference =>
                    {
                        statement_node = self.parse_expression(allocator, consumer, parent_node);
                    },
                    else => panic("ni: {}\n", .{token.value}),
                }
            },
            else =>
            {
                panic("ni: {}\n", .{token.value});
            },
        }

        if (statement_node != null and statement_node.?.value != Node.Value.branch_expr and statement_node.?.value != Node.Value.loop_expr)
        {
            //print("node type: {}\n", .{statement_node.?.value});
            if (consumer.expect_and_consume_sign(';') == null)
            {
                self.compiler.report_error("Expected semicolon at the end of the statement\n", .{});
            }
        }

        return statement_node;
    }

    fn block(self: *Parser, allocator: *Allocator, consumer: *TokenConsumer, block_node: *Node, allow_no_braces: bool) void
    {
        self.current_block = block_node;

        const has_braces = consumer.expect_and_consume_sign('{') != null;
        const expected_end = '}';

        if (!has_braces and !allow_no_braces)
        {
            self.compiler.report_error("Expected braces in block\n", .{});
        }

        if (has_braces)
        {
            var next_token = consumer.tokens[consumer.next_index];
            var statements_left_to_parse = consumer.expect_sign(expected_end) == null;

            while (statements_left_to_parse)
            {
                if (self.statement(allocator, consumer, block_node)) |statement_node|
                {
                    self.compiler.log("New statement: {}\n", .{statement_node});
                    block_node.value.block_expr.statements.append(statement_node) catch |err| {
                        panic("Failed to allocate memory for statement", .{});
                    };
                    next_token = consumer.tokens[consumer.next_index];
                    statements_left_to_parse = consumer.expect_sign(expected_end) == null;
                }
                else
                {
                    self.compiler.report_error("Error parsing statement\n", .{});
                }
            }

            const end = consumer.expect_and_consume_sign(expected_end);
            if (end == null)
            {
                self.compiler.report_error("Expected end sign at the end of the block", .{});
            }
        }
        else
        {
            if (self.statement(allocator, consumer, block_node)) |statement_node|
            {
                block_node.value.block_expr.statements.append(statement_node) catch |err| {
                    panic("Failed to allocate memory for statement", .{});
                };
            }
            else
            {
                self.compiler.report_error("Error parsing statement\n", .{});
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
        };

        var function_node = self.append_and_get(function_node_value);
        self.current_function = function_node;

        var function_type = TypeIdentifier
        {
            .value = TypeIdentifier.Value
            {
                .function = TypeIdentifier.Function {
                    // @TODO: make this pointer-stable
                    .arg_types = NodeRefBuffer.init(allocator),
                    .return_type = null,
                }
            }
        };

        var next_token = consumer.tokens[consumer.next_index];
        var arg_types = NodeRefBuffer.init(allocator);
        var args_left_to_parse = !(next_token.value == Token.ID.operator and next_token.value.operator == Operator.RightParenthesis);
        {
            self.parsing_function_args = true;

            while (args_left_to_parse)
            {
                const arg_node = self.parse_expression(allocator, consumer, function_node);
                if (arg_node.value != Node.ID.var_decl)
                {
                    // @TODO: improve error message
                    self.compiler.report_error("Error parsing argument\n", .{});
                }

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
                        self.compiler.report_error("Expected comma after function argument", .{});
                    }
                }
            }

            self.parsing_function_args = false;
        }

        if (consumer.expect_and_consume_operator(Operator.RightParenthesis) == null)
        {
            self.compiler.report_error("Expected end of argument list\n", .{});
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
        };

        function_node.value.function_decl.type = self.append_and_get(function_type_node_value);

        var block_node_value = Node
        {
            .parent = function_node,
            .value_type = Node.ValueType.LValue,
            .value = Node.Value
            {
                .block_expr = BlockExpression
                {
                    .statements = NodeRefBuffer.init(allocator),
                    .id = BlockExpression.ID.Function,
                },
                },
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
            // @TODO: handle this properly
            panic("not implemented\n", .{});
        }
        const name = name_token.?.value.identifier;

        //print("Name found: {s}\n", .{function_name.?.value.identifier});

        if (consumer.next_index + 2 >= consumer.tokens.len)
        {
            // @TODO: handle this properly
            panic("not implemented\n", .{});
        }

        const constant = consumer.expect_and_consume_operator(Operator.Constant);
        if (constant == null)
        {
            panic("not implemented: top level declaration with type in the middle specified\n", .{});
        }

        if (consumer.expect_and_consume_operator(Operator.LeftParenthesis) != null)
        {
            const function_result = self.parse_function_declaration(allocator, consumer) catch |err| {
                self.compiler.report_error("Couldn't parse the function\n", .{});
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
                self.compiler.report_error("Couldn't parse the function\n", .{});
            }
        }
        else if (consumer.expect_keyword(KeywordID.@"struct") != null)
        {
            _ = self.parse_type(allocator, consumer, null);
            //var struct_type = consumer.get_type_consuming_tokens(allocator, null);
            //struct_type.value.type_identifier.value.structure.name = name;
        }
        else
        {
            panic("Couldn't parse top level declaration\n", .{});
        }
    }
};

pub const ParserResult = struct
{
    function_declarations: []*Node,

    fn print_tree(self: *const ParserResult, compiler: *Compiler) void
    {
        print("Printing AST:\n\n", .{});
        for (self.function_declarations) |function|
        {
            compiler.log("{}", .{function});
        }
    }
};

pub fn parse(allocator: *Allocator, compiler: *Compiler, lexer_result: LexerResult) ParserResult
{
    // print("Parser\n", .{});
    const token_count = lexer_result.tokens.len;
    assert(token_count > 0);

    var token_consumer = TokenConsumer
    {
        .tokens = lexer_result.tokens,
        .next_index = 0,
        .compiler = compiler,
    };

    var parser = Parser
    {
        .nb = NodeBuffer.init(allocator) catch |err| {
            panic("Couldn't allocate the bucket node buffer\n", .{});
        },
        .current_function = undefined,
        .current_block = undefined,
        .compiler = compiler,
        .function_declarations = NodeRefBuffer.init(allocator),
        .parsing_function_args = false,
    };

    while (token_consumer.next_index < token_count)
    {
        parser.tld(allocator, &token_consumer) catch |err| {
            panic("Couldn't parse TLD\n", .{});
        };
    }

    const result = ParserResult
    {
        .function_declarations = parser.function_declarations.items,
    };

    result.print_tree(compiler);

    return result;
}
