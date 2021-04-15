// Parser
const std = @import("std");
const assert = std.debug.assert;
const print = std.debug.print;
const panic = std.debug.panic;
const Allocator = std.mem.Allocator;
const Internal = @import("compiler.zig");
const Type = Internal.Type;
const Compiler = Internal.Compiler;
const KeywordID = Internal.KeywordID;
const Lexer = @import("lexer.zig");
const Token = Lexer.Token;
const LexerResult = Lexer.LexerResult;
const NodeRefBuffer = std.ArrayList(*Node);

const IntegerLiteral = struct {
    value: u64,
    bit_count: u16,
    signed: bool,
    // padding
};

const UnaryExpression = struct {
    node_ref: *Node,
    id: ID,
    location: Location,

    const ID = enum {
        AddressOf,
        PointerDereference,
    };
    const Location = enum {
        Prefix,
        Postfix,
    };
};

const BinaryExpression = struct {
    left: *Node,
    right: *Node,
    id: ID,
    parenthesis: bool,

    const ID = enum {
        Plus,
        Minus,
        Multiplication,
        VariableDeclaration,
        Assignment,
        Subscript,
        Compare_Equal,
        Compare_NotEqual,
        Compare_LessThan,
        Compare_GreaterThan,
        Compare_LessThanOrEqual,
        Compare_GreaterThanOrEqual,
    };
};

const ReturnExpression = struct {
    expression: ?*Node,
};

// @Info: Variable expression must reference a variable declaration (in which function arguments are included)
const VariableExpression = struct {
    declaration: *Node,
};

const BlockExpression = struct {
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

const VariableDeclaration = struct {
    name: []const u8,
    var_type: *Type,
    var_value: *Node,
    var_scope: *Node,
    backend_ref: *void,
    is_function_arg: bool,
};

const BranchExpression = struct {
    condition: *Node,
    if_block: *Node,
    else_block: *Node,
    exit_block_ref: *void,
};

const LoopExpression = struct {
    prefix: *Node,
    body: *Node,
    postfix: *Node,
    exit_block_ref: *void,
    continue_block_ref: *void,
};

// @TODO: improve this one
const BreakExpression = struct {
    target: *Node,
    origin: *Node,
};

const FunctionDeclaration = struct {
    blocks: NodeRefBuffer,
    arguments: NodeRefBuffer,
    variables: NodeRefBuffer,
    name: []const u8,
    type: ?*Type,
};

const InvokeExpression = struct {
    arguments: NodeRefBuffer,
    expression: *Node,
};

const SubscriptExpression = struct {
    expression: *Node,
    index: *Node,
};

const ArrayLiteral = struct {
    elements: NodeRefBuffer,
    type_expression: *Type,
};

pub const Node = struct {
    value: Value,
    parent: ?*Node,
    value_type: ValueType,

    pub const Value = union(ID) {
        var_decl: VariableDeclaration,
        function_decl: FunctionDeclaration,
        int_lit: IntegerLiteral,
        array_lit: ArrayLiteral,
        unary_expr: UnaryExpression,
        binary_expr: BinaryExpression,
        return_expr: ReturnExpression,
        var_expr: VariableExpression,
        invoke_expr: InvokeExpression,
        block_expr: BlockExpression,
        branch_expr: BranchExpression,
        loop_expr: LoopExpression,
        break_expr: BreakExpression,
        subscript_expr: SubscriptExpression,
    };

    pub const ID = enum {
        var_decl,
        function_decl,
        int_lit,
        array_lit,
        unary_expr,
        binary_expr,
        return_expr,
        var_expr,
        invoke_expr,
        block_expr,
        branch_expr,
        loop_expr,
        break_expr,
        subscript_expr,
    };

    pub const ValueType = enum {
        RValue,
        LValue,
    };
};

const TokenConsumer = struct
{
    tokens: []const Token,
    next_index: usize,

    fn peek(self: *TokenConsumer) Token {
        const token = self.tokens[self.next_index];
        return token;
    }

    fn expect_and_consume(self: *TokenConsumer, id: Token.ID) ?Token {
        const token = self.tokens[self.next_index];
        if (token.value == id) {
            self.next_index += 1;
            return token;
        }

        return null;
    }
    fn expect_sign(self: *TokenConsumer, sign: u8) ?Token {
        const token = self.tokens[self.next_index];
        if (token.value == Token.ID.sign and token.value.sign == sign) {
            return token;
        }

        return null;
    }

    fn expect_and_consume_sign(self: *TokenConsumer, sign: u8) ?Token {
        const token = self.expect_sign(sign);
        if (token != null) {
            self.next_index += 1;
        }

        return token;
    }

    fn get_type_consuming_tokens(self: *TokenConsumer, types: *std.ArrayList(Type)) *Type {
        const t = self.tokens[self.next_index];
        self.next_index += 1;

        switch (t.value)
        {
            Token.ID.type => {
                var result = t.value.type;
                return result;
            },
            Token.ID.sign => {
                const sign = t.value.sign;
                switch (sign)
                {
                    '&' => {
                        const pointer_type = self.get_type_consuming_tokens(types);
                        const result = Type.get_pointer_type(pointer_type, types);
                        return result;
                    },
                    '[' => {
                        const in_brackets_token = self.expect_and_consume(Token.ID.int_lit);
                        var array_length: u64 = 0;
                        if (in_brackets_token != null)
                        {
                            array_length = in_brackets_token.?.value.int_lit;
                        }
                        else
                        {
                            panic("Not implemented\n", .{});
                        }
                        const right_bracket = self.expect_and_consume_sign(']');
                        if (right_bracket == null)
                        {
                            panic("Expected ] after array index\n", .{});
                        }

                        const array_elem_type = self.get_type_consuming_tokens(types);

                        const array_type = Type.get_array_type(array_elem_type, array_length, types);
                        return array_type;
                    },
                    else => {}
                }
            },
            else => {}
        }

        panic("Couldn't find type for token {}", .{t});
    }

};

const Parser = struct
{
    nb: std.ArrayList(Node),
    current_function: ?*Node,
    current_block: ?*Node,

    function_declarations: NodeRefBuffer,

    compiler: *Compiler,

    fn append_and_get_ref(self: *Parser, node: Node) *Node {
        self.nb.append(node) catch |err| {
            panic("Couldn't allocate memory for node", .{});
        };
        const result = &self.nb.items[self.nb.items.len - 1];
        return result;
    }

    fn primary_expression(self: *Parser, consumer: *TokenConsumer, types: *std.ArrayList(Type), parent_node: ?*Node) ?*Node {
        const token = consumer.peek();
        switch (token.value) {
            Token.ID.int_lit => {
                consumer.next_index += 1;
                const int_value = token.value.int_lit;
                print("Found int literal: {}\n", .{int_value});
                const int_lit_node = Node{
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

                return self.append_and_get_ref(int_lit_node);
            },
            Token.ID.sign => {
                panic("Not implemented: {c}\n", .{token.value.sign});
            },
            else => {
                panic("Not implemented: {}\n", .{token.value});
            },
        }
    }

    fn expression(self: *Parser, consumer: *TokenConsumer, types: *std.ArrayList(Type), parent_node: ?*Node) ?*Node {
        const left_expr_result = self.primary_expression(consumer, types, parent_node);
        if (left_expr_result == null) {
            return null;
        }

        var left = left_expr_result.?;
        if (left.value == Node.Value.var_decl) {
            panic("Variable declaration not implemented\n", .{});
        } else {
            return self.right_expression(consumer, types, parent_node, &left);
        }
    }

    fn right_expression(self: *Parser, consumer: *TokenConsumer, types: *std.ArrayList(Type), parent_node: ?*Node, left_ptr: **Node) ?*Node {
        while (true) {
            var token = consumer.peek();
            var maybe_binop: ?BinaryExpression.ID = undefined;
            switch (token.value) {
                Token.ID.sign => {
                    const sign = token.value.sign;
                    switch (sign) {
                        ':' => {
                            consumer.next_index += 1;
                            if (consumer.expect_and_consume_sign(':') != null) {
                                if (consumer.expect_sign('(') != null) {
                                    panic("Unreachable. Unexpected function here\n", .{});
                                } else {
                                    maybe_binop = null;
                                }
                            } else {
                                maybe_binop = BinaryExpression.ID.VariableDeclaration;
                            }
                        },
                        '=' => {
                            if (consumer.expect_and_consume_sign('=') != null) {
                                maybe_binop = BinaryExpression.ID.Compare_Equal;
                            } else {
                                maybe_binop = BinaryExpression.ID.Assignment;
                            }
                        },
                        '+' => {
                            consumer.next_index += 1;
                            maybe_binop = BinaryExpression.ID.Plus;
                        },
                        '-' => {
                            consumer.next_index += 1;
                            maybe_binop = BinaryExpression.ID.Minus;
                        },
                        '<' => {
                            consumer.next_index += 1;
                            if (consumer.expect_and_consume_sign('=') != null) {
                                maybe_binop = BinaryExpression.ID.Compare_LessThanOrEqual;
                            } else {
                                maybe_binop = BinaryExpression.ID.Compare_LessThan;
                            }
                        },
                        '>' => {
                            consumer.next_index += 1;
                            if (consumer.expect_and_consume_sign('=') != null) {
                                maybe_binop = BinaryExpression.ID.Compare_GreaterThanOrEqual;
                            } else {
                                maybe_binop = BinaryExpression.ID.Compare_GreaterThan;
                            }
                        },
                        '*' => {
                            consumer.next_index += 1;
                            maybe_binop = BinaryExpression.ID.Multiplication;
                        },
                        '[' => {
                            consumer.next_index += 1;
                            maybe_binop = BinaryExpression.ID.Subscript;
                        },
                        '/' => {
                            consumer.next_index += 1;
                            panic("/ to binary expression sign not implemented\n", .{});
                        },
                        else => {
                            maybe_binop = null;
                        },
                    }
                },
                else => {
                    maybe_binop = null;
                },
            }

            if (maybe_binop == null) {
                break;
            }

            const binop = maybe_binop.?;
            assert(binop != BinaryExpression.ID.VariableDeclaration);
            var left_expr = left_ptr.*;

            if (binop == BinaryExpression.ID.Subscript) {
                panic("Array subscript not implemented\n", .{});
            } else {
                panic("Binary expression not implemented\n", .{});
            }
        }

        return left_ptr.*;
    }

    fn parse_return(self: *Parser, consumer: *TokenConsumer, types: *std.ArrayList(Type), parent_node: ?*Node) ?*Node {
        consumer.next_index += 1;
        var return_node_value = Node{
            .value = Node.Value{
                .return_expr = ReturnExpression{
                    .expression = null,
                },
            },
            .value_type = Node.ValueType.LValue,
            .parent = parent_node,
        };

        var return_node = self.append_and_get_ref(return_node_value);
        const ret_expr = self.expression(consumer, types, return_node);
        if (ret_expr != null) {
            return_node.value.return_expr.expression = ret_expr.?;
        } else {
            self.compiler.report_error("Failed to parse return expression\n", .{});
            std.os.exit(1);
        }

        return return_node;
    }

    fn statement(self: *Parser, consumer: *TokenConsumer, types: *std.ArrayList(Type), parent_node: ?*Node) ?*Node {
        const token = consumer.peek();
        var statement_node: ?*Node = null;

        switch (token.value) {
            Token.ID.keyword => {
                const keyword = token.value.keyword;
                switch (keyword) {
                    KeywordID.@"return" => {
                        statement_node = self.parse_return(consumer, types, parent_node);
                    },
                    else => {
                        panic("Keyword unhandled: {}\n", .{keyword});
                    },
                }
            },
            else => {
                panic("Case: {}\n", .{token.value});
            },
        }

        if (statement_node != null and statement_node.?.value != Node.Value.branch_expr and statement_node.?.value != Node.Value.loop_expr) {
            if (consumer.expect_and_consume_sign(';') == null) {
                self.compiler.report_error("Expected semicolon at the end of the statement\n", .{});
                std.os.exit(1);
            }
        }

        return statement_node;
    }

    fn block(self: *Parser, consumer: *TokenConsumer, types: *std.ArrayList(Type), block_node: *Node, allow_no_braces: bool) void {
        self.current_block = block_node;

        const has_braces = consumer.expect_and_consume_sign('{') != null;
        const expected_end = '}';

        if (!has_braces and !allow_no_braces) {
            self.compiler.report_error("Expected braces in block\n", .{});
            std.os.exit(1);
        }

        if (has_braces) {
            var next_token = consumer.tokens[consumer.next_index];
            var statements_left_to_parse = consumer.expect_sign(expected_end) == null;

            while (statements_left_to_parse) {
                const statement_result = self.statement(consumer, types, block_node);
                if (statement_result == null) {
                    self.compiler.report_error("Error parsing statement\n", .{});
                    std.os.exit(1);
                }
                const statement_node = statement_result.?;
                block_node.value.block_expr.statements.append(statement_node) catch |err| {
                    panic("Failed to allocate memory for statement", .{});
                };
                next_token = consumer.tokens[consumer.next_index];
                statements_left_to_parse = consumer.expect_sign(expected_end) == null;
            }

            const end = consumer.expect_and_consume_sign(expected_end);
            if (end == null) {
                self.compiler.report_error("Expected end sign at the end of the block", .{});
                std.os.exit(1);
            }
        } else {
            const statement_result = self.statement(consumer, types, block_node);
            if (statement_result == null) {
                self.compiler.report_error("Error parsing statement\n", .{});
                std.os.exit(1);
            }
            const statement_node = statement_result.?;
            block_node.value.block_expr.statements.append(statement_node) catch |err| {
                panic("Failed to allocate memory for statement", .{});
            };
        }
    }

    fn function(self: *Parser, consumer: *TokenConsumer, types: *std.ArrayList(Type)) !?*Node
    {
        var allocator = std.heap.page_allocator;

        const function_name = consumer.expect_and_consume(Token.ID.symbol);
        if (function_name == null) {
            return null;
        }

        print("Name found: {s}\n", .{function_name.?.value.symbol});

        if (consumer.next_index + 2 >= consumer.tokens.len) {
            return null;
        }

        const first_token = consumer.tokens[consumer.next_index];
        const second_token = consumer.tokens[consumer.next_index + 1];

        if (!(first_token.value == Token.ID.sign and first_token.value.sign == ':' and second_token.value == Token.ID.sign and second_token.value.sign == ':')) {
            return null;
        }

        // @Info: Now this is a constant
        consumer.next_index += 2;

        if (consumer.expect_and_consume_sign('(') == null) {
            self.compiler.report_error("Couldn't parse function. Missing (\n", .{});
            std.os.exit(1);
        }

        var function_node_value = Node{
            .value = Node.Value{
                .function_decl = FunctionDeclaration{
                    .name = function_name.?.value.symbol,
                    .arguments = std.ArrayList(*Node).init(allocator),
                    .blocks = std.ArrayList(*Node).init(allocator),
                    .variables = std.ArrayList(*Node).init(allocator),
                    .type = null,
                },
            },
            .parent = null,
            .value_type = Node.ValueType.LValue,
        };

        var function_node = self.append_and_get_ref(function_node_value);
        self.current_function = function_node;

        var function_type =  Type.Function {
            .arg_types = std.ArrayList(*Type).init(allocator),
            .ret_type = undefined,
        };

        var next_token = consumer.tokens[consumer.next_index];
        var args_left_to_parse = !(next_token.value == Token.ID.sign and next_token.value.sign == ')');

        while (args_left_to_parse)
        {
            const arg_node_result = self.expression(consumer, types, function_node);

            if (arg_node_result == null)
            {
                self.compiler.report_error("Error parsing argument\n", .{});
                return null;
            }

            var arg_node = arg_node_result.?;
            if (arg_node.value != Node.ID.var_decl)
            {
                // @TODO: improve error message
                self.compiler.report_error("Error parsing argument\n", .{});
                return null;
            }

            arg_node.value.var_decl.is_function_arg = true;
            const arg_type = arg_node.value.var_decl.var_type;
            try function_type.arg_types.append(arg_type);
            try function_node.value.function_decl.arguments.append(arg_node);

            next_token = consumer.tokens[consumer.next_index];
            args_left_to_parse = !(next_token.value == Token.ID.sign and next_token.value.sign == ')');
            if (args_left_to_parse)
            {
                const comma = consumer.expect_and_consume_sign(',');
                if (comma == null)
                {
                    // print error
                    self.compiler.report_error("Expected comma after function argument", .{});
                    return null;
                }
            }
        }

        if (consumer.expect_and_consume_sign(')') == null)
        {
            self.compiler.report_error("Expected end of argument list\n", .{});
            return null;
        }

        if (consumer.expect_and_consume_sign('-') != null)
        {
            if (consumer.expect_and_consume_sign('>') == null)
            {
                self.compiler.report_error("Expected > after - in the function declaration return part\n", .{});
                return null;
            }

            const ret_type = consumer.get_type_consuming_tokens(types);
            function_type.ret_type = ret_type;
        }
        else
        {
            const void_type = Type.get_void_type(types);
            if (void_type != null)
            {
                function_type.ret_type = void_type.?;
            }
            else
            {
                panic("Couldn't find void type", .{});
            }
        }

        function_node.value.function_decl.type = Type.get_function_type(types, function_type);

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

        var block_node = self.append_and_get_ref(block_node_value);
        self.current_function.?.value.function_decl.blocks.append(block_node) catch |err| {
            panic("Failed to allocate memory for block node reference\n", .{});
        };

        self.block(consumer, types, block_node, false);

        return function_node;
    }
};

pub const ParserResult = struct
{
    node_buffer: []Node,
    function_declarations: []*Node,
};

pub fn parse(allocator: *Allocator, compiler: *Compiler, lexer_result: LexerResult, types: *std.ArrayList(Type)) ParserResult
{
    const token_count = lexer_result.tokens.len;
    assert(token_count > 0);

    var token_consumer = TokenConsumer{
        .tokens = lexer_result.tokens,
        .next_index = 0,
    };

    var parser = Parser{
        .nb = std.ArrayList(Node).init(allocator),
        .current_function = null,
        .current_block = null,
        .compiler = compiler,
        .function_declarations = NodeRefBuffer.init(allocator),
    };

    while (token_consumer.next_index < token_count)
    {
        var function_node = parser.function(&token_consumer, types) catch |err| {
            parser.compiler.report_error("Couldn't parse the function\n", .{});
            std.os.exit(0);
        };

        if (function_node != null) {
            parser.function_declarations.append(function_node.?) catch |err| {
                panic("Failed to allocate node reference for top-level declaration\n", .{});
            };
        } else {
            parser.compiler.report_error("Couldn't parse the function\n", .{});
            std.os.exit(1);
        }
    }

    const result = ParserResult {
        .node_buffer = parser.nb.items,
        .function_declarations = parser.function_declarations.items,
    };

    return result;
}
