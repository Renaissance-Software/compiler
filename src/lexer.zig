const std = @import("std");
const ArrayList = std.ArrayList;
const print = std.debug.print;
const panic = std.debug.panic;
const Allocator = std.mem.Allocator;
const Internal = @import("compiler.zig");
const TypeBuffer = Internal.TypeBuffer;
const KeywordID = Internal.KeywordID;
const Type = Internal.Type;

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
        symbol,
        keyword,
        type,
        sign,
        //intrinsic,
    };

    const Value = union(ID)
    {
        int_lit: u64,
        float_lit: f64,
        char_lit: u8,
        str_lit: []const u8,
        symbol: []const u8,
        keyword: KeywordID,
        type: *Type, // @ TODO: change this
        sign: u8,
        //intrinsic: u64, // @ TODO: change this
    };
};

const Tokenizer = struct
{
    tokens: ArrayList(Token),

    fn new_token(self: *Tokenizer, value: Token.Value, start: u64, end: u64, line: u32, column: u32) void
    {
        var token = Token{
            .value = value,
            .start = start,
            .end = end,
            .line = line,
            .column = column,
        };
        print("Added new token: {}\n", .{token});
        self.tokens.append(token) catch |err| {
            panic("Failed to allocate a new token\n", .{});
        };
    }
    fn match_name(self: Tokenizer, name: []const u8, types: *TypeBuffer) Token.Value
    {
        if (std.meta.stringToEnum(KeywordID, name)) |keyword|
        {
            // print("Keyword: {}\n", .{keyword.?});
            const result = Token.Value{
                .keyword = keyword,
            };
            return result;
        }
        else
        {
            // print("Keyword not found\n", .{});
        }

        if (Type.get_type_by_name(types, name)) |type_decl|
        {
            return Token.Value{ .type = type_decl };
        }

        return Token.Value{ .symbol = name };
    }
};

pub const LexerResult = struct
{
    tokens: []Token,
    line_count: u32,
};

pub fn lexical_analyze(allocator: *Allocator, src_file: [] const u8, types: *TypeBuffer) LexerResult
{
    var tokenizer = Tokenizer{ .tokens = ArrayList(Token).init(allocator) };

    var current_line_start: u64 = 0;
    var line_count: u32 = 0;

    var i: u64 = 0;
    while (i < src_file.len) : (i += 1)
    {
        const c = src_file[i];
        var start: u64 = i;
        var end: u64 = i;

        switch (c) {
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
                const symbol_slice = src_file[start..end];
                //print("Symbol found: {}. Length: {}\n", .{ symbol_slice, len });
                const column = @intCast(u32, start - current_line_start);

                const token_type = tokenizer.match_name(symbol_slice, types);

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
                const column = @intCast(u32, start - current_line_start);
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
                const column = @intCast(u32, start - current_line_start);
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

                // print("Char literal found: {c}\n", .{char_lit});
                const column = @intCast(u32, start - current_line_start);
                const char_lit_type = Token.Value{
                    .char_lit = char_lit,
                };
                tokenizer.new_token(char_lit_type, start, end, line_count, column);
            },

            ' ' => {},

            '\n' =>
            {
                line_count += 1;
                current_line_start = i + 1;
            },

            else =>
            {
                // print("Default sign token: {c}\n", .{c});
                const column = @intCast(u32, start - current_line_start);
                const sign = Token.Value{
                    .sign = c,
                };
                end = i + 1;
                tokenizer.new_token(sign, start, end, line_count, column);
            },
        }
    }

    line_count += 1;

    const result = LexerResult {
        .tokens = tokenizer.tokens.items,
        .line_count = line_count,
    };

    return result;
}
