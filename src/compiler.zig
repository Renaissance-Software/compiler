const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const panic = std.debug.panic;
const print = std.debug.print;
const _BucketArrayModule = @import("bucket_array.zig");
const BucketArrayList = _BucketArrayModule.BucketArrayList;

pub const should_log = true;

pub const KeywordID = enum
{
    @"if",
    @"else",
    @"for",
    @"while",
    @"break",
    @"continue",
    @"return",
};

pub const TypeBuffer = BucketArrayList(Type, 64);
pub const TypeRefBuffer = ArrayList(*Type);
pub const Type = struct
{
    value: Value,
    name: []const u8,

    pub const Value = union(ID) {
        void_type,
        integer: Integer,
        function: Function,
        pointer: Pointer,
        array: Array,
    };

    pub const ID = enum {
        void_type,
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

    pub fn get_void_type(types: *TypeBuffer) ?*Type
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
                if (type_decl.value == Type.ID.pointer and type_decl.value.pointer.p_type == p_type)
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
                    .p_type = p_type,
                },
            },
            .name = undefined,
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
            .name = undefined,
        };

        const result = types.append(new_type) catch |err| {
            panic("Failing to allocate a new type\n", .{});
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
            .name = undefined,
        };

        const result = types.append(fn_type) catch |err| {
            panic("Failed to allocate function type", .{});
        };

        //print("Function type: {}", .{result.value.function.ret_type});
        return result;
    }

    pub fn get_type_by_name(types: *TypeBuffer, name: []const u8) ?*Type
    {
        for (types.list.items) |type_bucket|
        {
            var index : u64 = 0;
            while (index < type_bucket.len) : (index += 1)
            {
                const type_decl = &type_bucket.items[index];
                if (std.mem.eql(u8, type_decl.name, name))
                {
                    return type_decl;
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
                .name = names[bit_index * 2],
            };
            _ = types.append(integer_type) catch |err| {
                panic("Error allocating memory for primitive type\n", .{});
            };

            integer_type.value.integer.signed = true;
            integer_type.name = names[bit_index * 2 + 1];
            _ = types.append(integer_type) catch |err| {
                panic("Error allocating memory for primitive type\n", .{});
            };
        }

        const void_type = Type
        {
            .value = Type.ID.void_type,
            .name = "void",
        };
        _ = types.append(void_type) catch |err| {
            panic("Error allocating memory for void type\n", .{});
        };

        return types;
    }
};

pub const Compiler = struct
{
    errors_reported: bool,
    pub fn report_error(self: *Compiler, comptime fmt: []const u8, args: anytype) void {
        self.errors_reported = true;
        print(fmt, args);
    }

    pub fn log(self: *Compiler, comptime format: []const u8, args: anytype) void
    {
        if (should_log)
        {
            print(format, args);
        }
    }
};
