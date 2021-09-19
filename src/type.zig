const std = @import("std");
const assert = std.debug.assert;
const ArrayList = std.ArrayList;
usingnamespace @import("entity.zig");

const Type = @This();
value: u64,

pub const unresolved_type = Type { .value = 0 };
const ID = enum(IntType)
{
    unresolved,
    builtin,
    integer,
    pointer,
    structure,
    array,
    slice,
    function,

    const position = @bitSizeOf(Type) - @bitSizeOf(ID);

    const IntType = u4;
};

const Resolution = struct
{
    const position = Type.ID.position - @bitSizeOf(u1);
};

const Module = struct
{
    const position = @bitSizeOf(Type) / 2;
    const bit_count = Resolution.position - Module.position;
    const mask = std.math.maxInt(std.meta.Int(.unsigned, bit_count));

    comptime 
    {
        assert(bit_count > @bitSizeOf(u16));
    }
};

pub const Integer = struct
{
    const Signedness = enum(u1)
    {
        unsigned,
        signed,

        const position = Resolution.position - @bitSizeOf(Signedness);
    };

    pub fn new(bit_count: u16, signedness: Signedness) Type
    {
        return .{ .value = (@as(u64, @enumToInt(Type.ID.integer)) << Type.ID.position) | (@as(u64, @enumToInt(signedness)) << Signedness.position) | bit_count };
    }
};

pub const Function = struct
{
    argument_types: []Type,
    return_type: Type,
    attributes: u64,

    pub const Attribute = enum(u64)
    {
        @"noreturn",
        @"extern",

        comptime
        {
            assert(std.enums.values(Attribute).len <= @bitSizeOf(u64));
        }
    };

    pub fn new(index: u64, module: u64) Type
    {
        return .{ .value = (@as(u64, @enumToInt(Type.ID.function)) << Type.ID.position) | (module << Module.position) | index };
    }
    
    pub fn append(function_types: *ArrayList(Type.Function), function_type: Type.Function, module_index: u64) Type
    {
        const index = function_types.items.len;
        function_types.append(function_type) catch unreachable;
        return new(index, module_index);
    }
};

pub const Array = struct
{
    type: Type,
    length_expression: u64, // @INFO: @TODO: Length should be resolved at compile-time
};

pub const Struct = struct
{
    types: []Type,
    names: [][]const u8,
    name: []const u8,
    alignment: u64,
};

pub const Pointer = struct
{
    type: Type,
};

pub const Slice = struct
{
    type: Type,
};

pub const Builtin = struct
{
    const Identity = enum
    {
        void_type,
        noreturn_type,
    };

    pub const void_type = Type { .value = (@as(u64, @enumToInt(Type.ID.builtin)) << Type.ID.position) | @enumToInt(Builtin.Identity.void_type) };
    pub const noreturn_type = Type { .value = (@as(u64, @enumToInt(Type.ID.builtin)) << Type.ID.position) | @enumToInt(Builtin.Identity.noreturn_type) };
};

pub fn new_unresolved_type(index: u64, module: u64) Type
{
    return .{ .value = (@as(u64, @enumToInt(Type.ID.unresolved)) << Type.ID.position) | (module << Module.position) | index };
}

// @TODO: (this is hardcoded) use more than u32, less?
pub fn get_index(self: Type) u32
{
    return @truncate(u32, self.value);
}

pub fn set_new_index(self: *Type, new_index: u64) void
{
    self.value = (self.value & 0xffffffff00000000) | new_index;
}

pub fn mark_as_resolved(self: *Type) void
{
    self.value |= 1 << Type.Resolution.position;
}

pub fn is_resolved(self: Type) bool
{
    return (self.value & (1 << Type.Resolution.position)) >> Type.Resolution.position != 0;
}

pub fn get_ID(self: Type) ID
{
    return @intToEnum(ID, (self.value & (std.math.maxInt(ID.IntType) << ID.position)) >> ID.position);
}

pub fn get_module_index(self: Type) u64
{
    return (self.value & (Module.mask << Module.position)) >> Module.position;
}
