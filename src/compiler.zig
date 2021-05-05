const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const panic = std.debug.panic;
const print = std.debug.print;
const _BucketArrayModule = @import("bucket_array.zig");
const BucketArrayList = _BucketArrayModule.BucketArrayList;

pub const KeywordID = enum
{
    @"if",
    @"else",
    @"for",
    @"while",
    @"break",
    @"continue",
    @"return",
    @"struct",
};

pub const TypeBuffer = BucketArrayList(Type, 64);
pub const TypeRefBuffer = ArrayList(*Type);
pub const Type = struct
{
    value: Value,

    pub const Value = union(ID)
    {
        void_type,
        integer: Integer,
        function: Function,
        pointer: Pointer,
        array: Array,
        structure: Struct,
    };

    pub const ID = enum
    {
        void_type,
        integer,
        function,
        pointer,
        array,
        structure,
    };

    pub const Integer = struct
    {
        bits: u16,
        signed: bool,
    };

    pub const Function = struct
    {
        arg_types: TypeRefBuffer,
        ret_type: *Type,
    };

    pub const Pointer = struct
    {
        type: *Type,
    };

    pub const Array = struct
    {
        type: *Type,
        count: u64,
    };

    pub const Struct = struct
    {
        fields: []Field,
        name: []const u8,

        pub const Field = struct 
        {
            name: []const u8,
            type: *Type,
            parent: *Type,
            index: u64,

            pub fn format(self: Field, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void
            {
                try std.fmt.format(writer, "{s}", .{self.name});
            }
        };
    };

    pub fn find_type_by_name(types: *TypeBuffer, name: []const u8) ?*Type
    {
        for (types.list.items) |type_bucket|
        {
            var index : u64 = 0;
            while (index < type_bucket.len) : (index += 1)
            {
                const type_decl = &type_bucket.items[index];
                if (type_decl.value == Type.ID.void_type)
                {
                    return type_decl;
                }
            }
        }
    }

    pub fn get_void_type(types: *TypeBuffer) *Type
    {
        for (types.list.items) |type_bucket|
        {
            var index : u64 = 0;
            while (index < type_bucket.len) : (index += 1)
            {
                const type_decl = &type_bucket.items[index];
                if (type_decl.value == Type.ID.void_type)
                {
                    return type_decl;
                }
            }
        }

        panic("Void type is not registered\n", .{});
    }

    pub fn get_integer_type(bits: u16, signed: bool, types: *TypeBuffer) *Type
    {
        for (types.list.items) |type_bucket|
        {
            var index : u64 = 0;
            while (index < type_bucket.len) : (index += 1)
            {
                const type_decl = &type_bucket.items[index];
                if (type_decl.value == Type.ID.integer and type_decl.value.integer.bits == bits and type_decl.value.integer.signed == signed)
                {
                    return type_decl;
                }
            }
        }

        panic("Integer type with {} bits and signedness {} is not registered\n", .{bits, signed});
    }

    pub fn get_pointer_type(p_type: *Type, types: *TypeBuffer) *Type
    {
        for (types.list.items) |type_bucket|
        {
            var index : u64 = 0;
            while (index < type_bucket.len) : (index += 1)
            {
                const type_decl = &type_bucket.items[index];
                if (type_decl.value == Type.ID.pointer and type_decl.value.pointer.type == p_type)
                {
                    return type_decl;
                }
            }
        }

        const new_type = Type
        {
            .value = Type.Value
            {
                .pointer = Type.Pointer
                {
                    .type = p_type,
                },
            },
        };

        const result = types.append(new_type) catch |err| {
            panic("Failing to allocate a new type\n", .{});
        };

        return result;
    }

    pub fn get_array_type(arr_type: *Type, count: usize, types: *TypeBuffer) *Type
    {
        for (types.list.items) |type_bucket|
        {
            var index : u64 = 0;
            while (index < type_bucket.len) : (index += 1)
            {
                const type_decl = &type_bucket.items[index];
                if (type_decl.value == Type.ID.array and type_decl == arr_type and type_decl.value.array.count == count)
                {
                    return type_decl;
                }
            }
        }

        const new_type = Type
        {
            .value = Type.Value
            {
                .array = Type.Array
                {
                    .type = arr_type,
                    .count = count,
                },
            },
        };

        const result = types.append(new_type) catch |err| {
            panic("Failing to allocate a new type\n", .{});
        };

        return result;
    }

    pub fn create_struct_type(types: *TypeBuffer, fields: []Type.Struct.Field, name: []const u8) *Type
    {
        const new_struct = Type
        {
            .value = Type.Value {
                .structure = Struct{
                    .fields = fields,
                    .name = name,
                },
            },
        };

        const result = types.append(new_struct) catch |err| {
            panic("Failed to allocate memory for new struct type\n", .{});
        };

        return result;
    }


    pub fn get_function_type(types: *TypeBuffer, function_type: Function) *Type
    {
        for (types.list.items) |type_bucket|
        {
            var index : u64 = 0;
            while (index < type_bucket.len) : (index += 1)
            {
                const type_decl = &type_bucket.items[index];
                if (type_decl.value == Type.ID.function and type_decl.value.function.ret_type == function_type.ret_type and type_decl.value.function.arg_types.items.len == function_type.arg_types.items.len)
                {
                    var i: usize = 0;
                    while (i < function_type.arg_types.items.len)
                    {
                        if (function_type.arg_types.items[i] == type_decl.value.function.arg_types.items[i])
                        {
                            // @Info: this is the function type to be returned
                            return type_decl;
                        }
                    }
                }
            }
        }


        const fn_type = Type{
            .value = Type.Value {
                .function = function_type,
            },
        };

        const result = types.append(fn_type) catch |err| {
            panic("Failed to allocate function type", .{});
        };

        //print("Function type: {}", .{result.value.function.ret_type});
        return result;
    }

    // @Info: Loopkup by name
    // @TODO: improve speed
    pub fn get_type_by_name(types: *TypeBuffer, name: []const u8) ?*Type
    {
        if (std.mem.eql(u8, name, "void"))
        {
            return Type.get_void_type(types);
        }
        else if (std.mem.eql(u8, name, "s8"))
        {
            return Type.get_integer_type(8, true, types);
        }
        else if (std.mem.eql(u8, name, "s16"))
        {
            return Type.get_integer_type(16, true, types);
        }
        else if (std.mem.eql(u8, name, "s32"))
        {
            return Type.get_integer_type(32, true, types);
        }
        else if (std.mem.eql(u8, name, "s64"))
        {
            return Type.get_integer_type(64, true, types);
        }
        else if (std.mem.eql(u8, name, "u8"))
        {
            return Type.get_integer_type(8, false, types);
        }
        else if (std.mem.eql(u8, name, "u16"))
        {
            return Type.get_integer_type(16, false, types);
        }
        else if (std.mem.eql(u8, name, "u32"))
        {
            return Type.get_integer_type(32, false, types);
        }
        else if (std.mem.eql(u8, name, "u64"))
        {
            return Type.get_integer_type(64, false, types);
        }

        for (types.list.items) |type_bucket|
        {
            var index : u64 = 0;
            while (index < type_bucket.len) : (index += 1)
            {
                const type_decl = &type_bucket.items[index];
                switch (type_decl.value)
                {
                    Type.ID.structure =>
                    {
                        if (std.mem.eql(u8, type_decl.value.structure.name, name))
                        {
                            return type_decl;
                        }
                    },
                    else => { },
                }
            }
        }

        return null;
    }

    pub fn init(allocator: *Allocator) TypeBuffer
    {
        var types = TypeBuffer.init(allocator) catch |err| {
            panic("Failed to allocate type buffer\n", .{});
        };
        const int_bits = [_]u8{ 8, 16, 32, 64 };
        const names = [8][]const u8{ "u8", "s8", "u16", "s16", "u32", "s32", "u64", "s64" };
        var bit_index: u64 = 0;

        while (bit_index < int_bits.len) : (bit_index += 1)
        {
            const int_type = Type.Integer{
                .bits = int_bits[bit_index],
                .signed = false,
            };
            const t_type = Type.Value{
                .integer = int_type,
            };
            var integer_type = Type{
                .value = t_type,
            };
            _ = types.append(integer_type) catch |err| {
                panic("Error allocating memory for primitive type\n", .{});
            };

            integer_type.value.integer.signed = true;
            _ = types.append(integer_type) catch |err| {
                panic("Error allocating memory for primitive type\n", .{});
            };
        }

        const void_type = Type
        {
            .value = Type.ID.void_type,
        };
        _ = types.append(void_type) catch |err| {
            panic("Error allocating memory for void type\n", .{});
        };

        return types;
    }

    pub fn format(self: *const Type, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void
    {
        switch (self.value)
        {
            Type.ID.integer =>
            {
                if (self.value.integer.signed)
                {
                    try std.fmt.format(writer, "s{}", .{self.value.integer.bits});
                }
                else
                {
                    try std.fmt.format(writer, "u{}", .{self.value.integer.bits});
                }
            },
            Type.ID.array =>
            {
                try std.fmt.format(writer, "{c}{}{c}{}", .{'[', self.value.array.count, ']', self.value.array.type});
            },
            Type.ID.pointer =>
            {
                try std.fmt.format(writer, "&{}", .{self.value.pointer.type});
            },
            Type.ID.structure => try std.fmt.format(writer, "{s}", .{self.value.structure.name}),
            else => panic("Not implemented: {}\n", .{self.value}),
        }
    }
};

pub const Operator = enum
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
};

pub const Compiler = struct
{
    log_level: LogLevel,
    module_log: u8,
    current_module: Module,

    pub fn report_error(self: *Compiler, comptime fmt: []const u8, args: anytype) noreturn 
    {
        self.always_log(fmt, args);
        if (true)
        {
            panic("panicked here\n", .{});
        }
        else
        {
            std.os.exit(1);
        }
    }

    pub fn always_log(self: *Compiler, comptime format: []const u8, args: anytype) void
    {
        print(format, args);
    }

    pub fn should_log(self: *Compiler, log_level: Compiler.LogLevel) bool
    {
        const log_level_is_enough = @enumToInt(log_level) >= @enumToInt(self.log_level);
        const should_log_module = self.module_log & (@as(u32, 1) << @intCast(u5, @enumToInt(self.current_module))) != 0;
        const result = log_level_is_enough and should_log_module;

        return result;
    }

    pub fn log(self: *Compiler, level: Compiler.LogLevel, comptime format: []const u8, args: anytype) void
    {
        if (self.should_log(level))
        {
            self.always_log(format, args);
        }
    }

    pub fn get_log_module(log_general: bool, log_lexer: bool, log_parser: bool, log_semantics: bool, log_bytecode: bool) u8
    {
        const log_general_int: u8 = @as(u8, @boolToInt(log_general)) << @enumToInt(Module.general);
        const log_lexer_int: u8 = @as(u8, @boolToInt(log_lexer)) << @enumToInt(Module.lexer);
        const log_parser_int: u8 = @as(u8, @boolToInt(log_parser)) << @enumToInt(Module.parser);
        const log_semantics_int: u8 = @as(u8, @boolToInt(log_semantics)) << @enumToInt(Module.semantics);
        const log_bytecode_int: u8 = @as(u8, @boolToInt(log_bytecode)) << @enumToInt(Module.bytecode);

        const module_log: u8 = log_lexer_int | log_parser_int | log_semantics_int | log_bytecode_int;
        return module_log;
    }

    pub const LogLevel = enum
    {
        debug,
        info,
        critical,
    };

    pub const Module = enum
    {
        general,
        lexer,
        parser,
        semantics,
        bytecode,
    };
};
