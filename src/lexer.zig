const std = @import("std");

const ArrayList = std.ArrayList;
const print = std.debug.print;
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const Internal = @import("compiler.zig");
const TypeBuffer = Internal.TypeBuffer;
const KeywordID = Internal.KeywordID;
const Type = Internal.Type;
const Compiler = Internal.Compiler;
const Log = Compiler.LogLevel;
const Operator = Internal.Operator;

pub const Token = struct
{
    value: Value,
    start: u64,
    end: u64,
    line: u32,
    column: u32,

    pub const ID = enum(u8)
    {
        int_lit,
        float_lit,
        char_lit,
        str_lit,
        identifier,
        keyword,
        sign,
        operator,
    };

    const Value = union(ID)
    {
        int_lit: u64,
        float_lit: f64,
        char_lit: u8,
        str_lit: []const u8,
        identifier: []const u8,
        keyword: KeywordID,
        sign: u8,
        operator: Operator,

        pub fn format(self: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void
        {
            switch (self)
            {
                Token.Value.identifier =>
                {
                    try std.fmt.format(writer, "Value {c} .identifier = \"{s}\" {c}", .{'{', self.identifier, '}'});
                },
                Token.Value.str_lit =>
                {
                    try std.fmt.format(writer, "Value {c} .str_lit = \"{s}\" {c}", .{'{', self.str_lit, '}'});
                },
                Token.Value.int_lit =>
                {
                    try std.fmt.format(writer, "Value {c} .int_lit = {} {c}", .{'{', self.int_lit, '}'});
                },
                Token.Value.float_lit =>
                {
                    try std.fmt.format(writer, "Value {c} .float_lit = {} {c}", .{'{', self.float_lit, '}'});
                },
                Token.Value.char_lit =>
                {
                    try std.fmt.format(writer, "Value {c} .char_lit = '{c}' {c}", .{'{', self.char_lit, '}'});
                },
                Token.Value.keyword =>
                {
                    try std.fmt.format(writer, "Value {c} .keyword = {} {c}", .{'{', self.keyword, '}'});
                },
                Token.Value.sign =>
                {
                    if (self.sign == ' ')
                    {
                        panic("Space is not allowed\n", .{});
                    }
                    try std.fmt.format(writer, "Value {c} .sign = {c} {c}", .{'{', self.sign, '}'});
                },
                Token.Value.operator =>
                {
                    const operator = self.operator;
                    switch (operator)
                    {
                        Operator.LeftParenthesis => try writer.writeAll("Operator ("),
                        Operator.RightParenthesis => try writer.writeAll("Operator )"),
                        Operator.Constant => try writer.writeAll("Operator ::"),
                        Operator.Arrow => try writer.writeAll("Operator ->"),
                        Operator.Declaration => try writer.writeAll("Operator :"),
                        Operator.Assignment => try writer.writeAll("Operator ="),
                        Operator.Equal => try writer.writeAll("Operator =="),
                        Operator.Plus => try writer.writeAll("Operator +"),
                        Operator.Minus => try writer.writeAll("Operator -"),
                        Operator.Multiplication => try writer.writeAll("Operator *"),
                        Operator.LessThan => try writer.writeAll("Operator <"),
                        Operator.GreaterThan => try writer.writeAll("Operator >"),
                        Operator.AddressOf => try writer.writeAll("Operator &"),
                        Operator.Dereference => try writer.writeAll("Operator @"),
                        Operator.LeftBracket => try writer.writeAll("Operator ["),
                        Operator.RightBracket => try writer.writeAll("Operator ]"),
                        Operator.Dot => try writer.writeAll("Operator ."),
                        else => panic("not implemented: {}\n", .{operator}),
                    }
                },
            }
        }
    };
};

const Tokenizer = struct
{
    tokens: ArrayList(Token),
    compiler: *Compiler,

    fn new_token(self: *Tokenizer, value: Token.Value, start: u64, end: u64, line: u32, column: u32) void
    {
        var token = Token{
            .value = value,
            .start = start,
            .end = end,
            .line = line,
            .column = column,
        };
        self.compiler.log(Log.debug, "Added new token: {}\n", .{token});
        self.tokens.append(token) catch |err| {
            panic("Failed to allocate a new token\n", .{});
        };
    }
    fn match_name(self: Tokenizer, name: []const u8) Token.Value
    {
        if (std.meta.stringToEnum(KeywordID, name)) |keyword|
        {
            // print("Keyword: {}\n", .{keyword.?});
            const result = Token.Value{
                .keyword = keyword,
            };
            return result;
        }

        return Token.Value{ .identifier = name };
    }
};

pub const LexerResult = struct
{
    tokens: []Token,
    line_count: u32,
};

pub fn lexical_analyze(allocator: *Allocator, compiler: *Compiler, src_file: [] const u8) LexerResult
{
    compiler.current_module = Compiler.Module.lexer;
    compiler.log(Compiler.LogLevel.debug, "\n==============\nLEXER\n==============\n\n", .{});

    var tokenizer = Tokenizer
    {
        .tokens = ArrayList(Token).init(allocator),
        .compiler = compiler,
    };

    var current_line_start: u64 = 0;
    var line_count: u32 = 0;

    var i: u64 = 0;

    while (i < src_file.len) : (i += 1)
    {
        const c = src_file[i];
        var start: u64 = i;
        var end: u64 = i;

        if (c == '/' and src_file[i + 1] == '/')
        {
            var ch = c;
            while (ch != '\n')
            {
                i += 1;
                ch = src_file[i];
            }
            i -= 1;
            continue;
        }

        const column = @intCast(u32, start - current_line_start);

        switch (c)
        {
            'a'...'z', 'A'...'Z', '_' =>
            {
                var ch = c;
                while (std.ascii.isAlpha(ch) or std.ascii.isDigit(ch) or ch == '_') {
                    i += 1;
                    ch = src_file[i];
                }
                end = i;
                i -= 1;

                const len = end - start;
                const identifier_slice = src_file[start..end];
                //print("Symbol found: {}. Length: {}\n", .{ symbol_slice, len });

                assert(identifier_slice.len != 0);
                const token_type = tokenizer.match_name(identifier_slice);

                tokenizer.new_token(token_type, start, end, line_count, column);
            },

            '0'...'9' =>
            {
                var int_ch = c;
                while (std.ascii.isDigit(int_ch)) {
                    i += 1;
                    int_ch = src_file[i];
                }
                end = i;
                i -= 1;
                const number_str = src_file[start..end];
                const value = std.fmt.parseUnsigned(u64, number_str, 10) catch |err| {
                    panic("Couldn't parse number\n", .{});
                };
                // print("Could parse a number: {}\n", .{value});
                const int_lit = Token.Value{
                    .int_lit = value,
                };
                tokenizer.new_token(int_lit, start, end, line_count, column);
            },

            '\"' =>
            {
                while (true) {
                    i += 1;
                    if (src_file[i] == '\"') {
                        break;
                    }
                }
                i += 1;
                end = i;
                const len = end - start;
                const str_lit = src_file[start..end];
                // print("String literal found: {}. Length: {}\n", .{ str_lit, len });
                const str_lit_type = Token.Value{
                    .str_lit = str_lit,
                };
                tokenizer.new_token(str_lit_type, start, end, line_count, column);
            },

            '\'' =>
            {
                const char_lit = src_file[i + 1];
                end = i + 2;
                i += 2;

                const char_lit_type = Token.Value{
                    .char_lit = char_lit,
                };
                tokenizer.new_token(char_lit_type, start, end, line_count, column);
            },
            ';', '{', '}', ',' =>
            {
                // print("Default sign token: {c}\n", .{c});
                const sign = Token.Value
                {
                    .sign = c,
                };
                end = i + 1;
                tokenizer.new_token(sign, start, end, line_count, column);
            },
            ':' =>
            {
                if (src_file[i + 1] == ':')
                {
                    const operator = Token.Value
                    {
                        .operator = Operator.Constant,
                    };
                    end = i + 2;
                    i += 1;
                    tokenizer.new_token(operator, start, end, line_count, column);
                }
                else
                {
                    const operator = Token.Value
                    {
                        .operator = Operator.Declaration,
                    };
                    end = i + 1;
                    tokenizer.new_token(operator, start, end, line_count, column);
                }
            },
            '(' =>
            {
                const operator = Token.Value
                {
                    .operator = Operator.LeftParenthesis,
                };
                end = i + 1;
                tokenizer.new_token(operator, start, end, line_count, column);
            },
            ')' =>
            {
                const operator = Token.Value
                {
                    .operator = Operator.RightParenthesis,
                };
                end = i + 1;
                tokenizer.new_token(operator, start, end, line_count, column);
            },
            '+' =>
            {
                const next_ch = src_file[i + 1];
                if (next_ch == '=')
                {
                    const operator = Token.Value
                    {
                        .operator = Operator.PlusAssignment,
                    };
                    end = i + 2;
                    i += 1;
                    tokenizer.new_token(operator, start, end, line_count, column);
                }
                else
                {
                    const operator = Token.Value
                    {
                        .operator = Operator.Plus,
                    };
                    end = i + 1;
                    tokenizer.new_token(operator, start, end, line_count, column);
                }
            },
            '-' =>
            {
                const next_ch = src_file[i + 1];
                if (next_ch == '=')
                {
                    const operator = Token.Value
                    {
                        .operator = Operator.MinusAssignment,
                    };
                    end = i + 2;
                    i += 1;
                    tokenizer.new_token(operator, start, end, line_count, column);
                }
                else if (next_ch == '>')
                {
                    const operator = Token.Value
                    {
                        .operator = Operator.Arrow,
                    };
                    end = i + 2;
                    i += 1;
                    tokenizer.new_token(operator, start, end, line_count, column);
                }
                else
                {
                    const operator = Token.Value
                    {
                        .operator = Operator.Minus,
                    };
                    end = i + 1;
                    tokenizer.new_token(operator, start, end, line_count, column);
                }
            },
            '*' =>
            {
                const next_ch = src_file[i + 1];
                if (next_ch == '=')
                {
                    const operator = Token.Value
                    {
                        .operator = Operator.MultiplicationAssignment,
                    };
                    end = i + 2;
                    i += 1;
                    tokenizer.new_token(operator, start, end, line_count, column);
                }
                else
                {
                    const operator = Token.Value
                    {
                        .operator = Operator.Multiplication,
                    };
                    end = i + 1;
                    tokenizer.new_token(operator, start, end, line_count, column);
                }
            },
            '=' =>
            {
                const next_ch = src_file[i + 1];

                if (next_ch == '=')
                {
                    const operator = Token.Value
                    {
                        .operator = Operator.Equal,
                    };
                    end = i + 2;
                    i += 1;
                    tokenizer.new_token(operator, start, end, line_count, column);
                }
                else
                {
                    const operator = Token.Value
                    {
                        .operator = Operator.Assignment,
                    };
                    end = i + 1;
                    tokenizer.new_token(operator, start, end, line_count, column);
                }
            },
            '>' =>
            {
                const next_ch = src_file[i + 1];

                if (next_ch == '=')
                {
                    const operator = Token.Value
                    {
                        .operator = Operator.GreaterOrEqualThan,
                    };
                    end = i + 2;
                    i += 1;
                    tokenizer.new_token(operator, start, end, line_count, column);
                }
                else if (next_ch == '>')
                {
                    const next_to_next_ch = src_file[i + 2];
                    if (next_to_next_ch == '=')
                    {
                        const operator = Token.Value
                        {
                            .operator = Operator.RightShiftAssignment,
                        };
                        end = i + 3;
                        i += 2;
                        tokenizer.new_token(operator, start, end, line_count, column);
                    }
                    else
                    {
                        const operator = Token.Value
                        {
                            .operator = Operator.RightShift,
                        };
                        end = i + 2;
                        i += 1;
                        tokenizer.new_token(operator, start, end, line_count, column);
                    }
                }
                else
                {
                    const operator = Token.Value
                    {
                        .operator = Operator.GreaterThan,
                    };
                    end = i + 1;
                    tokenizer.new_token(operator, start, end, line_count, column);
                }
            },
            '<' =>
            {
                const next_ch = src_file[i + 1];

                if (next_ch == '=')
                {
                    const operator = Token.Value
                    {
                        .operator = Operator.LessOrEqualThan,
                    };
                    end = i + 2;
                    i += 1;
                    tokenizer.new_token(operator, start, end, line_count, column);
                }
                else if (next_ch == '<')
                {
                    const next_to_next_ch = src_file[i + 2];
                    if (next_to_next_ch == '=')
                    {
                        const operator = Token.Value
                        {
                            .operator = Operator.LeftShiftAssignment,
                        };
                        end = i + 3;
                        i += 2;
                        tokenizer.new_token(operator, start, end, line_count, column);
                    }
                    else
                    {
                        const operator = Token.Value
                        {
                            .operator = Operator.LeftShift,
                        };
                        end = i + 2;
                        i += 1;
                        tokenizer.new_token(operator, start, end, line_count, column);
                    }
                }
                else
                {
                    const operator = Token.Value
                    {
                        .operator = Operator.LessThan,
                    };
                    end = i + 1;
                    tokenizer.new_token(operator, start, end, line_count, column);
                }
            },
            '&' =>
            {
                const operator = Token.Value
                {
                    .operator = Operator.AddressOf,
                };
                end = i + 1;
                tokenizer.new_token(operator, start, end, line_count, column);
            },
            '@' =>
            {
                const operator = Token.Value
                {
                    .operator = Operator.Dereference,
                };
                end = i + 1;
                tokenizer.new_token(operator, start, end, line_count, column);
            },
            '[' =>
            {
                const operator = Token.Value
                {
                    .operator = Operator.LeftBracket,
                };
                end = i + 1;
                tokenizer.new_token(operator, start, end, line_count, column);
            },
            ']' =>
            {
                const operator = Token.Value
                {
                    .operator = Operator.RightBracket,
                };
                end = i + 1;
                tokenizer.new_token(operator, start, end, line_count, column);
            },
            '.' =>
            {
                const operator = Token.Value
                {
                    .operator = Operator.Dot,
                };
                end = i + 1;
                tokenizer.new_token(operator, start, end, line_count, column);
            },
            // Ignore spaces, tabs and return characters
            '\t', ' ', '\r' => {},

            '\n' =>
            {
                line_count += 1;
                current_line_start = i + 1;
            },
            else =>
            {
                panic("not implemented: {c}\n", .{c});
            },
        }
    }

    line_count += 1;

    const result = LexerResult
    {
        .tokens = tokenizer.tokens.items,
        .line_count = line_count,
    };

    return result;
}
