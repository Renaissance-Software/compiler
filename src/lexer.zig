const std = @import("std");

const ArrayList = std.ArrayList;
const print = std.debug.print;
const panic = std.debug.panic;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const Compiler = @import("compiler.zig");
pub fn log(comptime format: []const u8, arguments: anytype) void
{
    Compiler.log(.lexer, format, arguments);
}

pub const Keyword = struct
{
    value: ID,
    start: u56,
    line: u32,
    column: u32,

    pub const ID = enum
    {
        @"break",
        @"continue",
        @"else",
        @"extern",
        @"for",
        @"if",
        @"noreturn",
        @"return",
        @"struct",
        @"switch",
        @"unreachable",
        @"void",
        @"while",
    };
};

pub const LexerResult = struct
{
    tokens: []Token,
    int_literals: []IntLiteral,
    char_literals: []CharLiteral,
    string_literals: []StringLiteral,
    identifiers: []Identifier,
    keywords: []Keyword,
    signs: []Sign,
    operators: []Operator,
    line_count: u32,
};

pub const Token = enum(u8)
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

const Tokenizer = struct
{
    tokens: ArrayList(Token),
    int_literals: ArrayList(IntLiteral),
    char_literals: ArrayList(CharLiteral),
    string_literals: ArrayList(StringLiteral),
    identifiers: ArrayList(Identifier),
    keywords: ArrayList(Keyword),
    signs: ArrayList(Sign),
    operators: ArrayList(Operator),
};

pub const Identifier = struct
{
    value: []const u8,
    start: u64,
    line: u32,
    column: u32,

    comptime
    {
        if (@sizeOf(Identifier) != @sizeOf(u64) * 4) @compileError("Incorrect size of identifier in lexer\n");
    }
};

pub const IntLiteral = struct
{
    value: u64,
    start: u64,
    end: u64,
    line: u32,
    column: u32,
};

pub const CharLiteral = packed struct
{
    start: u56,
    value: u8,
    line: u32,
    column: u32,

    comptime
    {
        assert(@sizeOf(CharLiteral) == 2 * @sizeOf(u64));
    }
};

pub const Sign = CharLiteral;

pub const Operator = packed struct
{
    start: u56,
    value: ID,
    line: u32,
    column: u32,

    pub const ID = enum(u8)
    {
        Declaration,
        LeftParenthesis,
        RightParenthesis,
        LeftBracket,
        RightBracket,
        Dot,
        Plus,
        Minus,
        AddressOf,
        Dereference,
        Multiplication,
        Division,
        Modulus,
        LeftShift,
        RightShift,
        LessThan,
        LessOrEqualThan,
        GreaterThan,
        GreaterOrEqualThan,
        Equal,
        NotEqual,
        BitwiseAnd,
        BitwiseOr,
        BitwiseXOR,
        Assignment,
        PlusAssignment,
        MinusAssignment,
        MultiplicationAssignment,
        DivisionAssignment,
        ModulusAssignment,
        RightShiftAssignment,
        LeftShiftAssignment,
        BitwiseAndAssignment,
        BitwiseOrAssignment,
        BitwiseXORAssignment,
        Arrow,
        Constant,
        CompilerIntrinsic,
    };

    comptime
    {
        assert(@sizeOf(Operator) == @sizeOf(u64) * 2);
    }
};

pub const StringLiteral = Identifier;


pub fn lexical_analyze(allocator: *Allocator, src_file: [] const u8) LexerResult
{
    log("\n==============\nLEXER\n==============\n\n", .{});

    var tokenizer = Tokenizer
    {
        .tokens = ArrayList(Token).init(allocator),
        .int_literals = ArrayList(IntLiteral).init(allocator),
        .char_literals = ArrayList(CharLiteral).init(allocator),
        .string_literals = ArrayList(StringLiteral).init(allocator),
        .identifiers = ArrayList(Identifier).init(allocator),
        .keywords = ArrayList(Keyword).init(allocator),
        .signs = ArrayList(Sign).init(allocator),
        .operators = ArrayList(Operator).init(allocator),
    };

    var current_line_start: u64 = 0;
    var line_count: u32 = 0;

    var i: u64 = 0;

    while (i < src_file.len) : (i += 1)
    {
        const c = src_file[i];

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

        var start: u64 = i;
        var end: u64 = i;
        const column = @intCast(u32, start - current_line_start);

        switch (c)
        {
            'a'...'z', 'A'...'Z', '_' =>
            {
                var ch = c;
                while (std.ascii.isAlpha(ch) or std.ascii.isDigit(ch) or ch == '_')
                {
                    i += 1;
                    ch = src_file[i];
                }
                end = i;
                i -= 1;

                const name_slice = src_file[start..end];
                assert(name_slice.len != 0);

                if (std.meta.stringToEnum(Keyword.ID, name_slice)) |keyword|
                {
                    tokenizer.keywords.append(.{
                        .value = keyword,
                        .start = @intCast(u56, start),
                        .line = line_count,
                        .column = column,
                    }) catch unreachable;
                    tokenizer.tokens.append(.keyword) catch unreachable;
                }
                else
                {
                    tokenizer.identifiers.append(.{
                        .value = name_slice,
                        .start = start,
                        .line = line_count,
                        .column = column,
                    }) catch unreachable;
                    tokenizer.tokens.append(.identifier) catch unreachable;
                }
            },

            '0'...'9' =>
            {
                var int_ch = c;
                while (std.ascii.isDigit(int_ch))
                {
                    i += 1;
                    int_ch = src_file[i];
                }

                end = i;
                i -= 1;

                const number_str = src_file[start..end];
                const value = std.fmt.parseUnsigned(u64, number_str, 10) catch {
                    panic("Couldn't parse number\n", .{});
                };
                tokenizer.int_literals.append(.{
                    .value = value,
                    .start = start,
                    .end = end,
                    .line = line_count,
                    .column = column,
                }) catch unreachable;
                tokenizer.tokens.append(.int_lit) catch unreachable;
            },

            // @Info: in tokens we are not taking into the account the double quotes
            '\"' =>
            {
                while (true)
                {
                    i += 1;
                    if (src_file[i] == '\"')
                    {
                        break;
                    }
                }

                end = i;
                start += 1;
                const str_lit = src_file[start..end];
                tokenizer.string_literals.append(.{
                    .value = str_lit,
                    .start = start,
                    .line = line_count,
                    .column = column,
                }) catch unreachable;
                tokenizer.tokens.append(.str_lit) catch unreachable;
            },

            '\'' =>
            {
                const char_lit = src_file[i + 1];
                tokenizer.char_literals.append(.
                    {
                        .value = char_lit,
                        .start = @intCast(u56, start),
                        .line = line_count,
                        .column = column,
                    }) catch unreachable;
                tokenizer.tokens.append(.char_lit) catch unreachable;
            },
            ';', '{', '}', ',' =>
            {
                tokenizer.signs.append(.
                    {
                        .value = c,
                        .start = @intCast(u56, start),
                        .line = line_count,
                        .column = column,
                    }) catch unreachable;
                tokenizer.tokens.append(.sign) catch unreachable;
            },
            ':' =>
            {
                tokenizer.tokens.append(.operator) catch unreachable;

                if (src_file[i + 1] == ':')
                {
                    i += 1;

                    tokenizer.operators.append(.{
                        .value = Operator.ID.Constant,
                        .start = @intCast(u56, start),
                        .line = line_count,
                        .column = column,
                    }) catch unreachable;
                }
                else
                {
                    tokenizer.operators.append(.{
                        .value = Operator.ID.Declaration,
                        .start = @intCast(u56, start),
                        .line = line_count,
                        .column = column,
                    }) catch unreachable;
                }
            },
            '(' =>
            {
                tokenizer.operators.append(.{
                    .value = Operator.ID.LeftParenthesis,
                    .start = @intCast(u56, start),
                    .line = line_count,
                    .column = column,
                }) catch unreachable;
                tokenizer.tokens.append(.operator) catch unreachable;
            },
            ')' =>
            {
                tokenizer.operators.append(.{
                    .value = Operator.ID.RightParenthesis,
                    .start = @intCast(u56, start),
                    .line = line_count,
                    .column = column,
                }) catch unreachable;
                tokenizer.tokens.append(.operator) catch unreachable;
            },
            '+' =>
            {
                tokenizer.tokens.append(.operator) catch unreachable;
                const next_ch = src_file[i + 1];
                if (next_ch == '=')
                {
                    i += 1;

                    tokenizer.operators.append(.{
                        .value = Operator.ID.PlusAssignment,
                        .start = @intCast(u56, start),
                        .line = line_count,
                        .column = column,
                    }) catch unreachable;
                }
                else
                {
                    tokenizer.operators.append(.{
                        .value = Operator.ID.Plus,
                        .start = @intCast(u56, start),
                        .line = line_count,
                        .column = column,
                    }) catch unreachable;
                }
            },
            '-' =>
            {
                tokenizer.tokens.append(.operator) catch unreachable;

                const next_ch = src_file[i + 1];
                if (next_ch == '=')
                {
                    i += 1;

                    tokenizer.operators.append(.{
                        .value = Operator.ID.MinusAssignment,
                        .start = @intCast(u56, start),
                        .line = line_count,
                        .column = column,
                    }) catch unreachable;
                }
                else if (next_ch == '>')
                {
                    i += 1;

                    tokenizer.operators.append(.{
                        .value = Operator.ID.Arrow,
                        .start = @intCast(u56, start),
                        .line = line_count,
                        .column = column,
                    }) catch unreachable;
                }
                else
                {
                    tokenizer.operators.append(.{
                        .value = Operator.ID.Minus,
                        .start = @intCast(u56, start),
                        .line = line_count,
                        .column = column,
                    }) catch unreachable;
                }
            },
            '*' =>
            {
                tokenizer.tokens.append(.operator) catch unreachable;

                const next_ch = src_file[i + 1];
                if (next_ch == '=')
                {
                    i += 1;

                    tokenizer.operators.append(.{
                        .value = Operator.ID.MultiplicationAssignment,
                        .start = @intCast(u56, start),
                        .line = line_count,
                        .column = column,
                    }) catch unreachable;
                }
                else
                {
                    tokenizer.operators.append(.{
                        .value = Operator.ID.Multiplication,
                        .start = @intCast(u56, start),
                        .line = line_count,
                        .column = column,
                    }) catch unreachable;
                }
            },
            '=' =>
            {
                tokenizer.tokens.append(.operator) catch unreachable;

                const next_ch = src_file[i + 1];

                if (next_ch == '=')
                {
                    i += 1;

                    tokenizer.operators.append(.{
                        .value = Operator.ID.Equal,
                        .start = @intCast(u56, start),
                        .line = line_count,
                        .column = column,
                    }) catch unreachable;
                }
                else
                {
                    tokenizer.operators.append(.{
                        .value = Operator.ID.Assignment,
                        .start = @intCast(u56, start),
                        .line = line_count,
                        .column = column,
                    }) catch unreachable;
                }
            },
            '>' =>
            {
                tokenizer.tokens.append(.operator) catch unreachable;

                const next_ch = src_file[i + 1];

                if (next_ch == '=')
                {
                    i += 1;

                    tokenizer.operators.append(.{
                        .value = Operator.ID.GreaterOrEqualThan,
                        .start = @intCast(u56, start),
                        .line = line_count,
                        .column = column,
                    }) catch unreachable;
                }
                else if (next_ch == '>')
                {
                    const next_to_next_ch = src_file[i + 2];
                    if (next_to_next_ch == '=')
                    {
                        i += 2;

                        tokenizer.operators.append(.{
                            .value = Operator.ID.RightShiftAssignment,
                            .start = @intCast(u56, start),
                            .line = line_count,
                            .column = column,
                        }) catch unreachable;
                    }
                    else
                    {
                        i += 1;

                        tokenizer.operators.append(.{
                            .value = Operator.ID.RightShift,
                            .start = @intCast(u56, start),
                            .line = line_count,
                            .column = column,
                        }) catch unreachable;
                    }
                }
                else
                {
                    tokenizer.operators.append(.{
                        .value = Operator.ID.GreaterThan,
                        .start = @intCast(u56, start),
                        .line = line_count,
                        .column = column,
                    }) catch unreachable;
                }
            },
            '<' =>
            {
                tokenizer.tokens.append(.operator) catch unreachable;

                const next_ch = src_file[i + 1];

                if (next_ch == '=')
                {
                    i += 1;

                    tokenizer.operators.append(.{
                        .value = Operator.ID.LessOrEqualThan,
                        .start = @intCast(u56, start),
                        .line = line_count,
                        .column = column,
                    }) catch unreachable;
                }
                else if (next_ch == '<')
                {
                    const next_to_next_ch = src_file[i + 2];
                    if (next_to_next_ch == '=')
                    {
                        i += 2;

                        tokenizer.operators.append(.{
                            .value = Operator.ID.LeftShiftAssignment,
                            .start = @intCast(u56, start),
                            .line = line_count,
                            .column = column,
                        }) catch unreachable;
                    }
                    else
                    {
                        i += 1;

                        tokenizer.operators.append(.{
                            .value = Operator.ID.LeftShift,
                            .start = @intCast(u56, start),
                            .line = line_count,
                            .column = column,
                        }) catch unreachable;
                    }
                }
                else
                {
                    tokenizer.operators.append(.{
                        .value = Operator.ID.LessThan,
                        .start = @intCast(u56, start),
                        .line = line_count,
                        .column = column,
                    }) catch unreachable;
                }
            },
            '&' =>
            {
                tokenizer.operators.append(.{
                    .value = Operator.ID.AddressOf,
                    .start = @intCast(u56, start),
                    .line = line_count,
                    .column = column,
                }) catch unreachable;
                tokenizer.tokens.append(.operator) catch unreachable;
            },
            '@' =>
            {
                tokenizer.operators.append(.{
                    .value = Operator.ID.Dereference,
                    .start = @intCast(u56, start),
                    .line = line_count,
                    .column = column,
                }) catch unreachable;
                tokenizer.tokens.append(.operator) catch unreachable;
            },
            '[' =>
            {
                tokenizer.operators.append(.{
                    .value = Operator.ID.LeftBracket,
                    .start = @intCast(u56, start),
                    .line = line_count,
                    .column = column,
                }) catch unreachable;
                tokenizer.tokens.append(.operator) catch unreachable;
            },
            ']' =>
            {
                tokenizer.operators.append(.{
                    .value = Operator.ID.RightBracket,
                    .start = @intCast(u56, start),
                    .line = line_count,
                    .column = column,
                }) catch unreachable;
                tokenizer.tokens.append(.operator) catch unreachable;
            },
            '.' =>
            {
                tokenizer.operators.append(.{
                    .value = Operator.ID.Dot,
                    .start = @intCast(u56, start),
                    .line = line_count,
                    .column = column,
                }) catch unreachable;
                tokenizer.tokens.append(.operator) catch unreachable;
            },
            '#' =>
            {
                tokenizer.operators.append(.{
                    .value = Operator.ID.CompilerIntrinsic,
                    .start = @intCast(u56, start),
                    .line = line_count,
                    .column = column,
                }) catch unreachable;
                tokenizer.tokens.append(.operator) catch unreachable;
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
        .int_literals = tokenizer.int_literals.items,
        .char_literals = tokenizer.char_literals.items,
        .string_literals = tokenizer.string_literals.items,
        .identifiers = tokenizer.identifiers.items,
        .keywords = tokenizer.keywords.items,
        .signs = tokenizer.signs.items,
        .operators = tokenizer.operators.items,
        .line_count = line_count,
    };

    return result;
}
