const std = @import("std");
const assert = std.debug.assert;

pub const Entity = packed struct
{
    value: Type,
    
    const Type = u64;

    const Self = @This();
    comptime
    {
        assert(@sizeOf(Self) == @sizeOf(u64));
    }

    const resolved_position = @bitSizeOf(Self) - 1;

    pub const Level = enum(IntType)
    {
        builtin,
        global,
        module,
        scope,

        const position = resolved_position - @bitSizeOf(Level);
        const IntType = u2;
    };

    pub const BuiltinID = enum
    {
        void_type,
        noreturn_type,
        os,
    };

    pub const GlobalID = enum
    {
        modules,
    };

    pub const ModuleID = enum
    {
        internal_functions,
        external_functions,
        imported_modules,
    };

    pub const ScopeID = enum
    {
        statements,
        variable_declarations,
        identifier_expressions,
        invoke_expressions,
        field_access_expressions,
        integer_literals,
    };

    pub fn new(base_index: u64, comptime array_id: anytype) Self
    {
        const level = comptime switch(@TypeOf(array_id))
        {
            BuiltinID => Level.builtin,
            GlobalID => Level.global,
            ModuleID => Level.module,
            ScopeID => Level.scope,
            else => unreachable,
        };

        const result = Self
        {
            .value = (@intCast(Self.Type, base_index) >> 32) | (@intCast(Self.Type, @enumToInt(level)) << Level.position) | @enumToInt(array_id),
        };

        const type_match = (@TypeOf(array_id) == ModuleID);
        if (type_match)
        {
            if (array_id == Self.ModuleID.unresolved_types)
            {
                print("[#] New identifier: {} {} {}\n{}\n", .{base_index, level, array_id, result});
            }
        }

        return result;
    }

    pub fn get_index(self: Self) u32
    {
        return @truncate(u32, self.value);
    }

    // @TODO: make this compile time and fast
    pub fn from_builtin_id(comptime id: BuiltinID) Self
    {
        return comptime Self
        {
            .index = @intCast(u32, id_integer),
            .features = (@enumToInt(Level.builtin) << Level.position) | @as(u32, @enumToInt(id)),
        };
    }

    pub fn get_void_type() TypeID
    {
        return Self.new(0, Self.BuiltinID.void_type);
    }

    pub fn get_noreturn_type() TypeID
    {
        return Self.new(0, Self.BuiltinID.noreturn_type);
    }

    pub fn get_builtin_os() Self
    {
        return Self.new(0, Self.BuiltinID.os);
    }

    pub fn get_level(self: Self) Level
    {
        return @intToEnum(Level, (self.features & (std.math.maxInt(Level.IntType) << Level.position)) >> Level.position);
    }

    pub fn get_array_index(self: Self) ArrayIDEnumType
    {
        return @truncate(ArrayIDEnumType, self.features);
    }

    pub fn is_resolved(self: Self) callconv(.Inline) bool
    {
        return ((self.features & @as(u32, 0x80000000)) >> 31) != 0;
    }
};
