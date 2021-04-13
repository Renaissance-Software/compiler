const std = @import("std");
const Allocator = std.mem.Allocator;
const panic = std.debug.panic;
const print = std.debug.print;

pub const KeywordID = enum {
    @"if",
    @"else",
    @"for",
    @"while",
    @"break",
    @"continue",
    @"return",
};

const TypeRefBuffer = std.ArrayList(*Type);
pub const Type = struct {
    value: Value,
    name: []const u8,

    pub const Value = union(ID) {
        void,
        integer: Integer,
        function: Function,
        pointer: Pointer,
        array: Array,
    };

    pub const ID = enum {
        void,
        integer,
        function,
        pointer,
        array,
    };

    pub const Integer = struct {
        bits: u16,
        signed: bool,
    };

    pub const Function = struct {
        arg_types: TypeRefBuffer,
        ret_type: *Type,
    };

    pub const Pointer = struct {
        p_type: *Type,
    };

    pub const Array = struct {
        type: *Type,
        count: u64,
    };

    pub fn get_void_type(types: *std.ArrayList(Type)) ?*Type {
        for (types.items) |*t| {
            if (t.value == Type.ID.void) {
                return t;
            }
        }

        return null;
    }

    pub fn get_function_type(types: *std.ArrayList(Type), function_type: Function) *Type {
        for (types.items) |*t| {
            if (t.value == Type.ID.function and t.value.function.ret_type == function_type.ret_type and t.value.function.arg_types.items.len == function_type.arg_types.items.len) {
                var i: usize = 0;
                while (i < function_type.arg_types.items.len) {
                    if (function_type.arg_types.items[i] == t.value.function.arg_types.items[i]) {
                        return t;
                    }
                }
            }
        }

        const type_value = Type.Value{
            .function = function_type,
        };

        const fn_type = Type{
            .value = type_value,
            .name = undefined,
        };

        types.append(fn_type) catch |err| {
            panic("Failed to allocate function type", .{});
        };

        const result = &types.items[types.items.len - 1];
        return result;
    }

    pub fn init(allocator: *Allocator) std.ArrayList(Type) {
        var types = std.ArrayList(Type).init(allocator);
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
                .name = names[bit_index * 2],
            };
            types.append(integer_type) catch |err| {
                panic("Error allocating memory for primitive type\n", .{});
            };

            integer_type.value.integer.signed = true;
            integer_type.name = names[bit_index * 2 + 1];
            types.append(integer_type) catch |err| {
                panic("Error allocating memory for primitive type\n", .{});
            };
        }

        return types;
    }
};

pub const Compiler = struct {
    errors_reported: bool,
    pub fn report_error(self: *Compiler, comptime fmt: []const u8, args: anytype) void {
        self.errors_reported = true;
        print(fmt, args);
    }
};
