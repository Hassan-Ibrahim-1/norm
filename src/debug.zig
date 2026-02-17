const std = @import("std");

const compiler = @import("compiler.zig");
const Chunk = compiler.Chunk;
const OpCode = compiler.OpCode;
const Io = std.Io;

pub fn disassembleChunk(
    w: *Io.Writer,
    chunk: *const Chunk,
    name: []const u8,
) void {
    w.print("== {s} ==\n", .{name}) catch unreachable;
    var offset: usize = 0;
    while (offset < chunk.code.items.len) {
        offset = disassembleInstruction(w, chunk, offset) catch unreachable;
    }
}

pub fn disassembleInstruction(
    w: *Io.Writer,
    chunk: *const Chunk,
    offset: usize,
) Io.Writer.Error!usize {
    var buf: [4]u8 = undefined;
    _ = fmt.printInt(
        &buf,
        offset,
        10,
        .lower,
        .{ .width = 4, .fill = '0' },
    );
    try w.print("{s} ", .{buf});

    if (offset > 0 and
        chunk.lines.items[offset] == chunk.lines.items[offset - 1])
    {
        try w.print("   | ", .{});
    } else {
        _ = fmt.printInt(
            &buf,
            chunk.lines.items[offset],
            10,
            .lower,
            .{ .width = 4, .fill = ' ' },
        );
        try w.print("{s} ", .{buf});
    }

    const instruction: OpCode = @enumFromInt(chunk.code.items[offset]);

    return switch (instruction) {
        .op_add,
        .op_subtract,
        .op_multiply,
        .op_divide,
        .op_negate,
        .op_return,
        => simpleInstruction(w, instruction, offset),

        .op_constant => constantInstruction(w, instruction, chunk, offset),
        .op_constant_long => longConstantInstruction(w, instruction, chunk, offset),
    };
}

fn simpleInstruction(
    w: *Io.Writer,
    instruction: OpCode,
    offset: usize,
) Io.Writer.Error!usize {
    try w.print("{t}\n", .{instruction});
    return offset + 1;
}

fn constantInstruction(
    w: *Io.Writer,
    instruction: OpCode,
    chunk: *const Chunk,
    offset: usize,
) Io.Writer.Error!usize {
    const constant = chunk.code.items[offset + 1];
    try w.print(
        "{s:<16} {d:>4} '{f}'\n",
        .{
            @tagName(instruction),
            constant,
            chunk.constants.items[constant],
        },
    );
    return offset + 2;
}

const mem = std.mem;

fn longConstantInstruction(
    w: *Io.Writer,
    instruction: OpCode,
    chunk: *const Chunk,
    offset: usize,
) Io.Writer.Error!usize {
    const constant = mem.readInt(u24, chunk.code.items[offset + 1 .. offset + 4].ptr[0..3], .little);
    try w.print(
        "{s:<16} {d:>4} '{f}'\n",
        .{
            @tagName(instruction),
            constant,
            chunk.constants.items[constant],
        },
    );
    return offset + 4;
}

pub fn dbgw(w: *Io.Writer, prefix: []const u8, value: anytype) @TypeOf(value) {
    w.print("{s} = {f}\n", .{
        prefix,
        std.json.fmt(value, .{ .whitespace = .indent_4 }),
    }) catch unreachable;
    return value;
}

pub fn dbg(prefix: []const u8, value: anytype) @TypeOf(value) {
    std.debug.print("{s} = {f}\n", .{
        prefix,
        std.json.fmt(value, .{ .whitespace = .indent_4 }),
    });
    return value;
}

const trait = @import("trait.zig");
const fmt = std.fmt;

pub fn opCodeToBytes(comptime ops: anytype) [ops.len]u8 {
    var bytes: [ops.len]u8 = undefined;
    inline for (ops, 0..) |op, i| {
        const T = @TypeOf(op);
        if (trait.isEnum(OpCode, op)) {
            bytes[i] = @as(OpCode, op).byte();
        } else if (trait.isInt(T) and op < std.math.maxInt(u8)) {
            bytes[i] = @intCast(op);
        } else @compileError(
            fmt.comptimePrint(
                "ops can only contain OpCode or u8 types, found: {}",
                .{@TypeOf(T)},
            ),
        );
    }
    return bytes;
}

const ers = @import("errors.zig");
pub fn reportErrors(errors: anytype, file_name: []const u8, source: []const u8) void {
    const stderr = std.debug.lockStderrWriter(&.{});
    for (errors) |diag| {
        ers.reportError(stderr, &diag.promote(file_name, source)) catch unreachable;
    }
}
