const std = @import("std");
const sema = @import("sema.zig");
const Nir = sema.Nir;
const NormType = sema.NormType;
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

    // Boolean values
    //
    // VM: Push `true` or `false` onto the stack
    op_true,
    op_false,

    // Negate an integer of a float
    //
    // VM: Pop a constant off the stack and then push negated
    op_negate,

    // Comparison operators
    //
    // VM: Pop two constants off the stack, compare them, and then push a boolean value
    op_equal,
    op_not_equal,
    op_greater,
    op_greater_equal,
    op_less,
    op_less_equal,

    // Binary logical operators
    //
    // VM: Pop two constants off the stack and then push a boolean value
    op_and,
    op_or,

    // Unary logical operator
    //
    // VM: Pop off one constant and then push a boolean value
    op_not,

    // Nil value
    //
    // VM: Push nil on the stack
    op_nil,

    // Casting operators
    //
    // VM: Pop a constant off the stack and convert it to target type
    op_cast_to_int,
    op_cast_to_float,

    // Temporary: Signals end of execution
    op_return,

    pub fn byte(op: OpCode) u8 {
        return @intFromEnum(op);
    }
};

fn oom() noreturn {
    @panic("oom");
}

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
        c.code.append(gpa, b) catch oom();
        c.lines.append(gpa, line) catch oom();
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
            const arr = c.code.addManyAsArray(gpa, 3) catch oom();
            mem.writeInt(u24, arr, @intCast(i), .little);
        }
    }

    fn addConstant(c: *Chunk, gpa: Allocator, value: Value) usize {
        c.constants.append(gpa, value) catch oom();
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

    fn expression(c: *Compiler, expr: *Nir.Expr) void {
        switch (expr.kind) {
            .binary => |*b| c.binary(b),
            .unary => |*u| c.unary(u),
            .cast => |*ca| c.cast(ca),
            .grouping => |*g| c.grouping(g),
            .literal => |*l| c.literal(l),
        }
    }

    fn binary(c: *Compiler, b: *Nir.Binary) void {
        c.expression(b.left);
        c.expression(b.right);

        const line = b.operator.line;
        const op_code: OpCode = switch (b.operator.type) {
            .plus => .op_add,
            .minus => .op_subtract,
            .star => .op_multiply,
            .slash => .op_divide,
            .equal_equal => .op_equal,
            .bang_equal => .op_not_equal,
            .greater => .op_greater,
            .greater_equal => .op_greater_equal,
            .less => .op_less,
            .less_equal => .op_less_equal,
            .kw_and => .op_and,
            .kw_or => .op_or,
            else => unreachable,
        };
        c.emitOpCode(op_code, line);
    }

    fn unary(c: *Compiler, u: *Nir.Unary) void {
        c.expression(u.expr);
        const line = u.operator.line;
        switch (u.operator.type) {
            .minus => c.emitOpCode(.op_negate, line),
            .bang => c.emitOpCode(.op_not, line),
            else => unreachable,
        }
    }

    fn cast(c: *Compiler, cst: *Nir.Cast) void {
        c.expression(cst.expr);
        const line = cst.target.line;
        const op_code: OpCode = switch (cst.target.type) {
            .kw_int => .op_cast_to_int,
            .kw_float => .op_cast_to_float,
            else => unreachable,
        };
        c.emitOpCode(op_code, line);
    }

    fn grouping(c: *Compiler, g: *Nir.Grouping) void {
        c.expression(g.expr);
    }

    fn literal(c: *Compiler, l: *Nir.Literal) void {
        switch (l.value) {
            .float => |f| c.compiling_chunk.writeConstant(
                c.gpa,
                .{ .float = f },
                l.token.line,
            ),
            .integer => |i| c.compiling_chunk.writeConstant(
                c.gpa,
                .{ .integer = i },
                l.token.line,
            ),
            .boolean => |b| if (b)
                c.emitOpCode(.op_true, l.token.line)
            else
                c.emitOpCode(.op_false, l.token.line),
            .nil => c.emitOpCode(.op_nil, l.token.line),
            .string => @panic("todo"),
        }
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

pub fn compile(gpa: Allocator, nir: *Nir) Chunk {
    if (nir.errors.len > 0) return .empty;

    var chunk: Chunk = .empty;
    var c: Compiler = .init(gpa, &chunk);

    const expr = nir.expr;
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

// TODO:
test "chunk long instruction" {}

const Lexer = @import("Lexer.zig");
const parser = @import("parser.zig");
const ers = @import("errors.zig");
const Io = std.Io;

fn testCompile(gpa: Allocator, source: []const u8) !Chunk {
    var l = Lexer.init(source);

    var ast = parser.parse(gpa, &l);
    defer ast.arena.deinit();

    if (ast.errors.len > 0) {
        debug.reportErrors(ast.errors, "test_runner", source);
        return error.ParserFailed;
    }

    var nir = sema.analyze(gpa, &ast);
    defer nir.arena.deinit();

    if (nir.errors.len > 0) {
        debug.reportErrors(nir.errors, "test_runner", source);
        return error.SemaFailed;
    }

    return compile(gpa, &nir);
}

const CompilerTestCase = struct {
    source: []const u8,
    expected_code: []const u8,
    expected_lines: []const u32,
    expected_constants: []const Value,
};

test "literals" {
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
        .{
            .source = "true",
            .expected_code = &debug.opCodeToBytes(&.{ .op_true, .op_return }),
            .expected_lines = &.{ 1, 0 },
            .expected_constants = &.{},
        },
        .{
            .source = "false",
            .expected_code = &debug.opCodeToBytes(&.{ .op_false, .op_return }),
            .expected_lines = &.{ 1, 0 },
            .expected_constants = &.{},
        },
        // .{
        //     .source = "nil",
        //     .expected_code = &debug.opCodeToBytes(&.{ .op_nil, .op_return }),
        //     .expected_lines = &.{ 1, 0 },
        //     .expected_constants = &.{},
        // },
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

test "arithmetic expressions" {
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
            .source = "(2.0 + 3.0) - 4.0 / 2.0",
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
            .expected_constants = &.{ .{ .float = 2.0 }, .{ .float = 3 }, .{ .float = 4.0 }, .{ .float = 2.0 } },
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

test "auto cast arithmetic expressions" {
    const gpa = testing.allocator;
    const tests: []const CompilerTestCase = &.{
        .{
            .source = "2 + 3.0",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_cast_to_float, .op_constant, 1, .op_add, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .float = 3 } },
        },
        .{
            .source = "2 * 3.0",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_cast_to_float, .op_constant, 1, .op_multiply, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .float = 3 } },
        },
        .{
            .source = "2.0 / 3",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_cast_to_float, .op_divide, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .float = 2 }, .{ .integer = 3 } },
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

test "comparison" {
    const gpa = testing.allocator;
    const tests: []const CompilerTestCase = &.{
        // TODO: use something other than numbers
        .{
            .source = "2 < 3",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_less, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .integer = 3 } },
        },
        .{
            .source = "2 <= 3",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_less_equal, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .integer = 3 } },
        },
        .{
            .source = "2 > 3",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_greater, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .integer = 3 } },
        },
        .{
            .source = "2 >= 3",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_greater_equal, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .integer = 3 } },
        },
        .{
            .source = "2 == 3",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_equal, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .integer = 3 } },
        },
        .{
            .source = "2 != 3",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_not_equal, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .integer = 3 } },
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

test "logical" {
    const gpa = testing.allocator;
    const tests: []const CompilerTestCase = &.{
        .{
            .source = "true and true",
            .expected_code = &debug.opCodeToBytes(&.{ .op_true, .op_true, .op_and, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 0 },
            .expected_constants = &.{},
        },
        .{
            .source = "true or false",
            .expected_code = &debug.opCodeToBytes(&.{ .op_true, .op_false, .op_or, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 0 },
            .expected_constants = &.{},
        },
        .{
            .source = "!false",
            .expected_code = &debug.opCodeToBytes(&.{ .op_false, .op_not, .op_return }),
            .expected_lines = &.{ 1, 1, 0 },
            .expected_constants = &.{},
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

test "casting" {
    const gpa = testing.allocator;
    const tests: []const CompilerTestCase = &.{
        .{
            .source = "float(2)",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_cast_to_float, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 0 },
            .expected_constants = &.{.{ .integer = 2 }},
        },
        .{
            .source = "int(2.0)",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_cast_to_int, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 0 },
            .expected_constants = &.{.{ .float = 2.0 }},
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
