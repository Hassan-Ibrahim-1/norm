const std = @import("std");
const Ast = @import("parser.zig").Ast;
const Token = @import("Lexer.zig").Token;
const mem = std.mem;
const Allocator = mem.Allocator;
const Value = @import("value.zig").Value;

pub const OpCode = enum(u8) {
    // Next byte is constant index.
    //
    // VM: Pop a constant off the stack.
    op_constant,
    // Next three little endian bytes represent constant index
    //
    // VM: Pop a constant off the stack
    op_constant_long,

    // VM: Pop two constants off the stack and then push one
    op_add,
    op_subtract,
    op_multiply,
    op_divide,

    // VM: Push a constant off the stack and then push one
    op_negate,

    // Temporary: Signals end of execution
    op_return,

    pub fn byte(op: OpCode) u8 {
        return @intFromEnum(op);
    }
};

pub const Chunk = struct {
    code: std.ArrayList(u8),
    lines: std.ArrayList(u32),
    constants: std.ArrayList(Value),

    pub const empty: Chunk = .{
        .code = .empty,
        .lines = .empty,
        .constants = .empty,
    };

    pub fn write(c: *Chunk, gpa: Allocator, b: u8, line: u32) void {
        c.code.append(gpa, b) catch unreachable;
        c.lines.append(gpa, line) catch unreachable;
    }

    pub fn writeOp(c: *Chunk, gpa: Allocator, op: OpCode, line: u32) void {
        c.write(gpa, op.byte(), line);
    }

    pub fn writeConstant(c: *Chunk, gpa: Allocator, value: Value, line: u32) void {
        const i = c.addConstant(gpa, value);
        if (i < std.math.maxInt(u8)) {
            c.writeOp(gpa, .op_constant, line);
            c.write(gpa, @intCast(i), line);
        } else {
            c.writeOp(gpa, .op_constant_long, line);
            const arr = c.code.addManyAsArray(gpa, 3) catch unreachable;
            mem.writeInt(u24, arr, @intCast(i), .little);
            // c.write(gpa, @intCast(i & 0xff), line);
            // c.write(gpa, @intCast((i >> 8) & 0xff), line);
            // c.write(gpa, @intCast((i >> 16) & 0xff), line);
        }
    }

    fn addConstant(c: *Chunk, gpa: Allocator, value: Value) usize {
        c.constants.append(gpa, value) catch unreachable;
        return c.constants.items.len - 1;
    }

    pub fn deinit(c: *Chunk, gpa: Allocator) void {
        c.code.deinit(gpa);
        c.lines.deinit(gpa);
        c.constants.deinit(gpa);
        c.* = undefined;
    }
};

pub const Compiler = struct {
    gpa: Allocator,
    compiling_chunk: *Chunk,

    pub fn init(gpa: Allocator, chunk: *Chunk) Compiler {
        return .{
            .gpa = gpa,
            .compiling_chunk = chunk,
        };
    }

    fn expression(c: *Compiler, expr: *Ast.Expr) void {
        switch (expr.*) {
            .binary => |*b| c.binary(b),
            .unary => |*u| c.unary(u),
            .grouping => |*g| c.grouping(g),
            .literal => |*l| c.literal(l),
        }
    }

    fn binary(c: *Compiler, b: *Ast.Binary) void {
        c.expression(b.left);
        c.expression(b.right);

        const line = b.operator.line;
        switch (b.operator.type) {
            .plus => c.emitOpCode(.op_add, line),
            .minus => c.emitOpCode(.op_subtract, line),
            .star => c.emitOpCode(.op_multiply, line),
            .slash => c.emitOpCode(.op_divide, line),
            else => unreachable,
        }
    }

    fn unary(c: *Compiler, u: *Ast.Unary) void {
        c.expression(u.expr);
        const line = u.operator.line;
        switch (u.operator.type) {
            .minus => c.emitOpCode(.op_negate, line),
            else => unreachable,
        }
    }

    fn grouping(c: *Compiler, g: *Ast.Grouping) void {
        c.expression(g.expr);
    }

    fn literal(c: *Compiler, l: *Ast.Literal) void {
        c.compiling_chunk.writeConstant(
            c.gpa,
            litValToVal(l.value),
            l.token.line,
        );
    }

    fn litValToVal(l: Ast.Literal.Value) Value {
        return switch (l) {
            .float => .{ .float = l.float },
            .integer => .{ .integer = l.integer },
            else => unreachable,
        };
    }

    fn emitConstant(c: *Compiler, value: Value, line: u32) void {
        c.compiling_chunk.writeConstant(c.gpa, value, line);
    }

    fn emitOpCode(c: *Compiler, op: OpCode, line: u32) void {
        c.emitByte(op.byte(), line);
    }

    fn emitOpCodes(c: *Compiler, op1: OpCode, op2: OpCode, line: u32) void {
        c.emitOpCode(op1, line);
        c.emitOpCode(op2, line);
    }

    fn emitByte(c: *Compiler, b: u8, line: u32) void {
        c.compiling_chunk.write(c.gpa, b, line);
    }

    fn emitBytes(c: *Compiler, a: u8, b: u8) void {
        c.emitByte(a);
        c.emitByte(b);
    }
};

pub fn compile(gpa: Allocator, ast: *Ast) Chunk {
    var chunk: Chunk = .empty;
    if (ast.errors.len > 0) return chunk;
    var c: Compiler = .init(gpa, &chunk);

    const expr = ast.expr;
    c.expression(expr);

    c.emitOpCode(.op_return, 0);

    return chunk;
}

const testing = std.testing;
const debug = @import("debug.zig");

test "basic chunk ops" {
    const gpa = std.testing.allocator;
    var chunk: Chunk = .empty;
    defer chunk.deinit(gpa);

    chunk.writeConstant(gpa, .{ .integer = 10 }, 1);
    chunk.writeConstant(gpa, .{ .float = 3.14 }, 1);
    chunk.writeOp(gpa, .op_return, 2);

    const expected_code = [_]u8{
        OpCode.op_constant.byte(), 0,
        OpCode.op_constant.byte(), 1,
        OpCode.op_return.byte(),
    };
    const expected_lines = [_]u32{ 1, 1, 1, 1, 2 };
    const expected_constants = [_]Value{ .{ .integer = 10 }, .{ .float = 3.14 } };

    try testing.expectEqualSlices(u8, &expected_code, chunk.code.items);
    try testing.expectEqualSlices(u32, &expected_lines, chunk.lines.items);
    try testing.expectEqualSlices(Value, &expected_constants, chunk.constants.items);
}

test "chunk long instruction" {}

const Lexer = @import("Lexer.zig");
const parser = @import("parser.zig");

fn testCompile(gpa: Allocator, source: []const u8) !Chunk {
    var l = Lexer.init(source);
    var ast = parser.parse(gpa, &l);
    defer ast.arena.deinit();
    return compile(gpa, &ast);
}

const CompilerTestCase = struct {
    source: []const u8,
    expected_code: []const u8,
    expected_lines: []const u32,
    expected_constants: []const Value,
};

test "number literals" {
    const gpa = std.testing.allocator;
    const tests: []const CompilerTestCase = &.{
        .{
            .source = "2",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_return }),
            .expected_lines = &.{ 1, 1, 0 },
            .expected_constants = &.{.{ .integer = 2 }},
        },
        .{
            .source = "3.0",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_return }),
            .expected_lines = &.{ 1, 1, 0 },
            .expected_constants = &.{.{ .float = 3.0 }},
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source = \"{s}\"\n", .{t.source});
        var chunk = try testCompile(gpa, t.source);
        defer chunk.deinit(gpa);

        try testing.expectEqualSlices(u8, t.expected_code, chunk.code.items);
        try testing.expectEqualSlices(u32, t.expected_lines, chunk.lines.items);
        try testing.expectEqualSlices(Value, t.expected_constants, chunk.constants.items);
    }
}

test "binary expression" {
    const gpa = std.testing.allocator;
    const tests: []const CompilerTestCase = &.{
        .{
            .source = "2 + 3",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_add, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .integer = 3 } },
        },
        .{
            .source = "2.0 + 3.0",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_add, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .float = 2 }, .{ .float = 3 } },
        },
        .{
            .source = "2 + 3.0",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_add, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .float = 3 } },
        },
        .{
            .source = "2 * 3.0",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_multiply, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .float = 3 } },
        },
        .{
            .source = "2 / 3.0",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_divide, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .float = 3 } },
        },
        .{
            .source = "2 - 3.0",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_subtract, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .float = 3 } },
        },
        .{
            .source = "(2 + 3.0) - 4.0 / 2.0",
            .expected_code = &debug.opCodeToBytes(&.{
                .op_constant,
                0,
                .op_constant,
                1,
                .op_add,
                .op_constant,
                2,
                .op_constant,
                3,
                .op_divide,
                .op_subtract,
                .op_return,
            }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .float = 3 }, .{ .float = 4.0 }, .{ .float = 2.0 } },
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source = \"{s}\"\n", .{t.source});
        var chunk = try testCompile(gpa, t.source);
        defer chunk.deinit(gpa);

        try testing.expectEqualSlices(u8, t.expected_code, chunk.code.items);
        try testing.expectEqualSlices(u32, t.expected_lines, chunk.lines.items);
        try testing.expectEqualSlices(Value, t.expected_constants, chunk.constants.items);
    }
}
