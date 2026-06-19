const std = @import("std");

const compiler = @import("compiler.zig");
const Ast = @import("Ast.zig");
const Chunk = compiler.Chunk;
const OpCode = compiler.OpCode;
const Io = std.Io;
const mem = std.mem;
const fmt = std.fmt;

pub fn disassembleChunk(
    w: *Io.Writer,
    chunk: *const Chunk,
    name: []const u8,
    source: []const u8,
) void {
    w.print("== {s} ==\n", .{name}) catch unreachable;
    var offset: usize = 0;
    var prev_line: u32 = 0;
    while (offset < chunk.code.items.len) {
        if (offset < chunk.lines.items.len) {
            const line = chunk.lines.items[offset];
            if (line != prev_line and line > 0) {
                const line_str = getLine(source, line);
                const line_str_trim = mem.trim(u8, line_str, " ");
                w.print("// \"{s}\"\n", .{line_str_trim}) catch unreachable;
            }
            prev_line = line;
        }
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

    if (offset > 0 and offset < chunk.lines.items.len and
        chunk.lines.items[offset] == chunk.lines.items[offset - 1])
    {
        try w.print("   | ", .{});
    } else if (offset < chunk.lines.items.len) {
        _ = fmt.printInt(
            &buf,
            chunk.lines.items[offset],
            10,
            .lower,
            .{ .width = 4, .fill = ' ' },
        );
        try w.print("{s} ", .{buf});
    } else {
        try w.print("    ", .{});
    }

    const instruction: OpCode = @enumFromInt(chunk.code.items[offset]);

    return switch (instruction) {
        .op_constant => constantInstruction(w, instruction, chunk, offset),
        .op_constant_long => longConstantInstruction(w, instruction, chunk, offset),

        .op_add_int,
        .op_subtract_int,
        .op_multiply_int,
        .op_add_float,
        .op_subtract_float,
        .op_multiply_float,
        .op_divide_float,
        .op_concat,
        .op_true,
        .op_false,
        .op_negate_int,
        .op_negate_float,
        .op_equal_int,
        .op_not_equal_int,
        .op_greater_int,
        .op_greater_equal_int,
        .op_less_int,
        .op_less_equal_int,
        .op_equal_float,
        .op_not_equal_float,
        .op_greater_float,
        .op_greater_equal_float,
        .op_less_float,
        .op_less_equal_float,
        .op_equal_string,
        .op_not_equal_string,
        .op_equal_bool,
        .op_not_equal_bool,
        .op_and,
        .op_or,
        .op_not,
        .op_nil,
        .op_cast_to_int,
        .op_cast_to_float,
        .op_temp_print,
        .op_pop,
        .op_return,
        => simpleInstruction(w, instruction, offset),

        .op_store,
        .op_load,
        => shortInstruction(w, instruction, chunk, offset),

        .op_jump,
        .op_jump_if_false,
        => jumpInstruction(w, instruction, chunk, offset),

        .op_loop,
        => loopInstruction(w, instruction, chunk, offset),

        .op_pop_n => popNInstructions(w, instruction, chunk, offset),
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
        "{t:<16} {d:>4} '{f}'\n",
        .{
            instruction,
            constant,
            chunk.constants.items[constant],
        },
    );
    return offset + 2;
}

fn longConstantInstruction(
    w: *Io.Writer,
    instruction: OpCode,
    chunk: *const Chunk,
    offset: usize,
) Io.Writer.Error!usize {
    const constant = mem.readInt(u24, chunk.code.items[offset + 1 .. offset + 4].ptr[0..3], .little);
    try w.print(
        "{t:<16} {d:>4} '{f}'\n",
        .{
            instruction,
            constant,
            chunk.constants.items[constant],
        },
    );
    return offset + 4;
}

fn shortInstruction(
    w: *Io.Writer,
    instruction: OpCode,
    chunk: *const Chunk,
    offset: usize,
) Io.Writer.Error!usize {
    const slot = mem.readInt(u16, chunk.code.items[offset + 1 .. offset + 3].ptr[0..2], .little);
    try w.print(
        "{t:<16} {d:>4}\n",
        .{
            instruction,
            slot,
        },
    );
    return offset + 3;
}

fn jumpInstruction(
    w: *Io.Writer,
    instruction: OpCode,
    chunk: *const Chunk,
    offset: usize,
) Io.Writer.Error!usize {
    const jump_offset = mem.readInt(u16, chunk.code.items[offset + 1 .. offset + 3].ptr[0..2], .little);
    std.debug.print("\n", .{});
    const instruction_index = offset + jump_offset + 3;
    const jump_instruction: OpCode = @enumFromInt(chunk.code.items[instruction_index]);
    const line = chunk.lines.items[instruction_index];
    try w.print(
        "{t:<16} {d:>4} -> {d} '{t}' on line {d}\n",
        .{
            instruction,
            jump_offset,
            instruction_index,
            jump_instruction,
            line,
        },
    );
    return offset + 3;
}

fn loopInstruction(
    w: *Io.Writer,
    instruction: OpCode,
    chunk: *const Chunk,
    offset: usize,
) Io.Writer.Error!usize {
    const jump_offset = mem.readInt(u16, chunk.code.items[offset + 1 .. offset + 3].ptr[0..2], .little);
    std.debug.print("\n", .{});
    dbg("jump_offset", jump_offset);
    dbg("offset", offset);
    const instruction_index = offset - jump_offset + 3;
    const jump_instruction: OpCode = @enumFromInt(chunk.code.items[instruction_index]);
    const line = chunk.lines.items[instruction_index];
    try w.print(
        "{t:<16} {d:>4} -> {d} '{t}' on line {d}\n",
        .{
            instruction,
            jump_offset,
            instruction_index,
            jump_instruction,
            line,
        },
    );
    return offset + 3;
}

fn popNInstructions(
    w: *Io.Writer,
    instruction: OpCode,
    chunk: *const Chunk,
    offset: usize,
) Io.Writer.Error!usize {
    const n = mem.readInt(u16, chunk.code.items[offset + 1 .. offset + 3].ptr[0..2], .little);
    try w.print("{t:<16} {d:>4}\n", .{ instruction, n });
    return offset + 3;
}

pub fn dbgw(w: *Io.Writer, prefix: []const u8, value: anytype) @TypeOf(value) {
    w.print("{s} = {f}\n", .{
        prefix,
        std.json.fmt(value, .{ .whitespace = .indent_4 }),
    }) catch unreachable;
    return value;
}

pub fn dbg(prefix: []const u8, value: anytype) void {
    std.debug.print("{s} = {f}\n", .{
        prefix,
        std.json.fmt(value, .{ .whitespace = .indent_4 }),
    });
}

pub fn dbgr(prefix: []const u8, value: anytype) @TypeOf(value) {
    std.debug.print("{s} = {f}\n", .{
        prefix,
        std.json.fmt(value, .{ .whitespace = .indent_4 }),
    });
    return value;
}

const trait = @import("trait.zig");

pub fn opCodeToBytes(ops: anytype) [ops.len]u8 {
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
    var buffer: [64]u8 = undefined;
    const stderr = std.debug.lockStderr(&buffer);
    defer std.debug.unlockStderr();
    const w = &stderr.file_writer.interface;
    for (errors) |diag| {
        ers.reportError(w, &diag.promote(file_name, source)) catch unreachable;
    }
}

const Allocator = mem.Allocator;

pub fn printStmts(gpa: Allocator, stmts: anytype) []u8 {
    var aw: Io.Writer.Allocating = .init(gpa);

    for (stmts, 0..) |stmt, i| {
        if (i == stmts.len - 1) {
            aw.writer.print("{f}", .{stmt}) catch @panic("whoops");
        } else {
            aw.writer.print("{f}\n", .{stmt}) catch @panic("whoops");
        }
    }

    return aw.toOwnedSlice() catch oom();
}

fn oom() noreturn {
    @panic("oom");
}

fn getLine(str: []const u8, line: u32) []const u8 {
    var iter = mem.splitScalar(u8, str, '\n');
    var i: usize = 0;
    while (iter.next()) |line_str| : (i += 1) {
        if (i == line - 1) return line_str;
    }
    return "";
}
