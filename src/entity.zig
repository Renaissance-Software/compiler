const std = @import("std");
const assert = std.debug.assert;
const print = std.debug.print;
const panic = std.debug.panic;

pub const Entity = packed struct
{
    value: Type,
    
    const Type = u64;

    const Self = @This();
    comptime
    {
        assert(@sizeOf(Self) == @sizeOf(Type));
    }

    pub const Level = enum(IntType)
    {
        builtin,
        global,
        module,
        scope,

        const position = @bitSizeOf(Type) - @bitSizeOf(Level);
        const IntType = u2;
    };

    const array_id_position = Level.position - @bitSizeOf(ScopeID); 
    const ArrayIDEnumType = std.meta.Int(.unsigned, Level.position - array_id_position);
    const array_index_position = @bitSizeOf(u32);
    const ArrayIndexEnumType = std.meta.Int(.unsigned, array_id_position - array_index_position);

    pub const BuiltinID = enum
    {
        os,
    };

    pub const GlobalID = enum
    {
        modules,
        resolved_internal_functions,
        resolved_external_functions,
        resolved_invoke_expressions,
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
        return_expressions,
        assignments,
        argument,
        comparisons,
        compound_assignments,
        loops,
        branches,
        break_expressions,
        arithmetic_expressions,
    };

    const LevelToArrayIDMap = blk:
    {
        const levels = std.enums.values(Level);
        var array_ids: [levels.len]type = undefined;
        inline for (levels) |level, i|
        {
            array_ids[i] = switch(level)
            {
                .builtin => BuiltinID,
                .global => GlobalID,
                .module => ModuleID,
                .scope => ScopeID,
            };
        }

        break :blk array_ids;
    };

    pub fn new(base_index: u64, comptime array_id: anytype, array_index: u64) Self
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
            .value = (@intCast(Self.Type, @enumToInt(level)) << Level.position) | (@intCast(Self.Type, @enumToInt(array_id)) << array_id_position) | (array_index << array_index_position) | base_index,
        };

        return result;
    }

    pub fn get_index(self: Self) u32
    {
        return @truncate(u32, self.value);
    }

    // @TODO: make this compile time and fast
    //pub fn from_builtin_id(comptime id: BuiltinID) Self
    //{
        //return comptime Self
        //{
            //.index = @intCast(u32, id_integer),
            //.features = (@enumToInt(Level.builtin) << Level.position) | @as(u32, @enumToInt(id)),
        //};
    //}

    pub fn get_builtin_os() Self
    {
        return Self.new(0, Self.BuiltinID.os, 0);
    }

    pub fn get_level(self: Self) Level
    {
        return @intToEnum(Level, @intCast(Level.IntType, (self.value & (std.math.maxInt(Level.IntType) << Level.position)) >> Level.position));
    }

    pub fn get_array_id(self: Self, comptime level: Level) LevelToArrayIDMap[@enumToInt(level)]
    {
        // this means they are resolved
        if (level != .scope and self.get_level() != .global)
        {
            assert(level == self.get_level());
        }
        return @intToEnum(LevelToArrayIDMap[@enumToInt(level)], @intCast(std.meta.Int(.unsigned, @bitSizeOf(LevelToArrayIDMap[@enumToInt(level)])), (self.value & (std.math.maxInt(ArrayIDEnumType) << array_id_position)) >> array_id_position));
    }

    pub fn get_array_index(self: Self) u32
    {
        return @intCast(u32, (self.value & (std.math.maxInt(ArrayIndexEnumType) << array_index_position)) >> array_index_position);
    }

    pub fn is_resolved(self: Self) callconv(.Inline) bool
    {
        return ((self.features & @as(u32, 0x80000000)) >> 31) != 0;
    }

    pub fn set_index(self: *Self, index: u32) void
    {
        self.value = (self.value & 0xffffffff00000000) | index;
    }
};
