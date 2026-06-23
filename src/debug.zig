const std = @import("std");

const compiler = @import("compiler.zig");
const Ast = @import("Ast.zig");
const Chunk = compiler.Chunk;
const OpCode = compiler.OpCode;
const Value = @import("value.zig").Value;
const Io = std.Io;
const mem = std.mem;
const meta = std.meta;
const fmt = std.fmt;

const native_endian = @import("builtin").cpu.arch.endian();

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
    const instruction_index = offset - (jump_offset - 3);
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

pub fn parseChunk(
    gpa: Allocator,
    bytes: []const u8,
    constants: []const Value,
    lines: []const u32,
) Chunk {
    var chunk = Chunk.init(gpa);
    errdefer chunk.deinit();

    if (lines.len != bytes.len) {
        chunk.lines.appendNTimes(gpa, 0, bytes.len) catch oom();
    } else {
        chunk.lines.appendSlice(gpa, lines) catch oom();
    }

    chunk.code.appendSlice(gpa, bytes) catch oom();

    chunk.constants.ensureTotalCapacityPrecise(gpa, constants.len) catch oom();
    for (constants) |constant| {
        if (constant == .string) {
            const s = constant.string.data;
            const string = if (chunk.strings.getKey(s)) |existing| existing else string: {
                const arena = chunk.string_arena.allocator();
                const duped = arena.dupe(u8, s) catch oom();
                chunk.strings.put(arena, duped, {}) catch oom();
                break :string duped;
            };
            chunk.constants.appendAssumeCapacity(.{ .string = .ref(string) });
        } else {
            chunk.constants.appendAssumeCapacity(constant);
        }
    }

    return chunk;
}

/// We ignore line numbers here, if you care about them you have to test them separately.
pub fn expectEqualChunks(w: *Io.Writer, expected: Chunk, actual: Chunk, source: []const u8) !void {
    const testing = std.testing;

    errdefer {
        disassembleChunk(w, &expected, "expected chunk", source);
        w.writeByte('\n') catch {};
        disassembleChunk(w, &actual, "actual chunk", source);
        w.writeByte('\n') catch {};
    }

    if (expected.code.items.len != actual.code.items.len) {
        w.print(
            "Unequal code lengths, expected={}, actual={}\n",
            .{ expected.code.items.len, actual.code.items.len },
        ) catch {};
        return error.TestExpectedEqual;
    }

    var instruction_index: usize = 0;
    while (instruction_index < expected.code.items.len) {
        const expected_instruction: OpCode = @enumFromInt(expected.code.items[instruction_index]);
        const actual_instruction: OpCode = @enumFromInt(actual.code.items[instruction_index]);

        errdefer {
            printWithLineNumbers(w, source) catch {};
            w.print("First difference occurs on line {}\n", .{actual.lines.items[instruction_index]}) catch {};
            w.print("==== expected ====\n", .{}) catch {};
            _ = disassembleInstruction(w, &expected, instruction_index) catch {};
            w.print("\n==== actual ====\n", .{}) catch {};
            _ = disassembleInstruction(w, &actual, instruction_index) catch {};
            w.writeByte('\n') catch {};
        }

        if (expected_instruction != actual_instruction) {
            return error.TestExpectedEqual;
        }

        const arity = expected_instruction.arity();
        if (arity > 0) {
            const start = instruction_index + 1;
            const end = instruction_index + 1 + arity;
            const expected_argument = expected.code.items[start..end];
            const actual_argument = actual.code.items[start..end];

            try testing.expectEqualSlices(u8, expected_argument, actual_argument);
        }

        instruction_index += arity + 1;
    }

    if (expected.constants.items.len != actual.constants.items.len) {
        w.print(
            "Unequal constants length, expected={}, actual={}",
            .{ expected.constants.items.len, actual.constants.items.len },
        ) catch {};
        return error.TestExpectedEqual;
    }

    for (0..expected.constants.items.len) |constant_index| {
        const expected_constant = expected.constants.items[constant_index];
        const actual_constant = actual.constants.items[constant_index];

        errdefer {
            w.print(
                "Unequal constants\n=== expected ===\n{t}: {f}\n\n=== actual ===\n{t}: {f}\n",
                .{ expected_constant, expected_constant, actual_constant, actual_constant },
            ) catch {};
        }

        try testing.expectEqual(expected_constant, actual_constant);
    }
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

pub fn printWithLineNumbers(w: *Io.Writer, str: []const u8) Io.Writer.Error!void {
    var lines = mem.splitScalar(u8, str, '\n');
    var line_number: usize = 1;
    while (lines.next()) |line| : (line_number += 1) {
        try w.print("{d} {s}\n", .{ line_number, line });
    }
}
