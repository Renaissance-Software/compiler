const std = @import("std");
const Allocator = std.mem.Allocator;
const panic = std.debug.panic;
const print = std.debug.print;
const ArrayList = std.ArrayList;

const IR = @import("bytecode.zig");
const x86_64 = @import("codegen/x86_64/codegen.zig");
const arm64 = @import("codegen/arm64/codegen.zig");

pub fn encode(allocator: *Allocator, module: *IR.Module, target: std.Target) void
{
    const arch = target.cpu.arch;
    switch (arch)
    {
        .x86_64 => x86_64.encode(allocator, module, target),
        .aarch64 => arm64.encode(allocator, module, target),
        else => panic("Architecture {} is not supported\n", .{arch}),
    }
}

pub const DataBuffer = ArrayList(u8);
pub const CodeBuffer = ArrayList(u8);
pub const ConstantDataList = ArrayList(ConstantData);

pub const ConstantData = struct
{
    value: *IR.Value,
    offset: u64,
};

// @TODO: standardize this to a most abstract struct
pub const Import = struct
{
    pub const Symbol = struct
    {
        name: []const u8,
        name_RVA: u32,
        offset_in_data: u32,
    };

    pub const Library = struct
    {
        symbols: ArrayList(Import.Symbol),
        name: []const u8,
        name_RVA: u32,
        RVA: u32,
        image_thunk_RVA: u32,
    };
};

pub fn write_executable(name: []const u8, content: []const u8) void
{
    print("Creating executable: {s}\n", .{name});
    const file = std.fs.cwd().createFile(name, comptime if (std.builtin.target.os.tag == .windows) .{} else .{ .mode = 0o777}) catch |err| panic("Error creating file {s}: {}\n", .{name, err});
    defer file.close();

    file.writeAll(content) catch |file_wr_err| {
        panic("Error writting bytes to a file: {}\n", .{file_wr_err});
    };
}

pub const Section = struct
{
    buffer: DataBuffer,
    name: []const u8,
    base_RVA: u32,
    permissions: u8,

    pub const Permission = enum(u8)
    {
        read = 1,
        write = 2,
        execute = 4,
    };

    pub const Directory = extern struct
    {
        RVA: u32,
        size: u32,
    };
};
