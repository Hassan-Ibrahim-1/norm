const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const testing = std.testing;
const Io = std.Io;
const builtin = @import("builtin");

const debug = @import("debug.zig");
const dbg = debug.dbg;
const ers = @import("errors.zig");
const Lexer = @import("Lexer.zig");
const Nir = @import("Nir.zig");
const NormType = Nir.NormType;
const parser = @import("parser.zig");
const sema = @import("sema.zig");
const Token = @import("Lexer.zig").Token;
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
    op_add_int,
    op_subtract_int,
    op_multiply_int,

    op_add_float,
    op_subtract_float,
    op_multiply_float,
    op_divide_float,

    // Concatenate two strings
    //
    // VM: Pop two strings off the stack and push a concatenated string
    op_concat,

    // Boolean values
    //
    // VM: Push `true` or `false` onto the stack
    op_true,
    op_false,

    // Negate an integer of a float
    //
    // VM: Pop a constant off the stack and then push negated
    op_negate_int,

    op_negate_float,

    // Comparison operators
    //
    // VM: Pop two constants off the stack, compare them, and then push a boolean value
    op_equal_int,
    op_not_equal_int,
    op_greater_int,
    op_greater_equal_int,
    op_less_int,
    op_less_equal_int,

    op_equal_float,
    op_not_equal_float,
    op_greater_float,
    op_greater_equal_float,
    op_less_float,
    op_less_equal_float,

    op_equal_string,
    op_not_equal_string,

    op_equal_bool,
    op_not_equal_bool,

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

    // Store a value into a variable
    // op_store <stack_slot: u16>
    //
    // VM: Pop a value off the stack and store it in the given <stack_slot>
    op_store,

    // Load the value of a variable
    // op_load <stack_slot: u16>
    //
    // VM: Push a the value stored at <stack_slot> onto the top of the stack
    op_load,

    // Temporary opcode to print a value
    //
    // VM: Pop a value off the stack and print it
    op_temp_print,

    // VM: Pop a value off the stack and discard it
    op_pop,

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
    gpa: Allocator,
    code: std.ArrayList(u8),
    lines: std.ArrayList(u32),
    constants: std.ArrayList(Value),

    string_arena: std.heap.ArenaAllocator,
    strings: std.StringHashMapUnmanaged(void),

    pub fn init(gpa: Allocator) Chunk {
        return .{
            .gpa = gpa,
            .string_arena = .init(gpa),
            .code = .empty,
            .lines = .empty,
            .constants = .empty,
            .strings = .empty,
        };
    }

    pub fn write(c: *Chunk, b: u8, line: u32) void {
        c.code.append(c.gpa, b) catch oom();
        c.lines.append(c.gpa, line) catch oom();
    }

    pub fn writeShort(c: *Chunk, b: u16, line: u32) void {
        const arr = c.code.addManyAsArray(c.gpa, 2) catch oom();
        mem.writeInt(u16, arr, b, .little);
        c.lines.append(c.gpa, line) catch oom();
    }

    pub fn writeOp(c: *Chunk, op: OpCode, line: u32) void {
        c.write(op.byte(), line);
    }

    pub fn writeConstant(c: *Chunk, value: Value, line: u32) void {
        const i = c.addConstant(value);
        if (i < std.math.maxInt(u8)) {
            c.writeOp(.op_constant, line);
            c.write(@intCast(i), line);
        } else {
            c.writeOp(.op_constant_long, line);
            const arr = c.code.addManyAsArray(c.gpa, 3) catch oom();
            mem.writeInt(u24, arr, @intCast(i), .little);
        }
    }

    pub fn writeString(c: *Chunk, s: []const u8, line: u32) void {
        const string = if (c.strings.getKey(s)) |existing| existing else string: {
            const arena = c.string_arena.allocator();
            const duped = arena.dupe(u8, s) catch oom();
            c.strings.put(arena, duped, {}) catch oom();
            break :string duped;
        };
        c.writeConstant(.{ .string = .ref(string) }, line);
    }

    fn addConstant(c: *Chunk, value: Value) usize {
        c.constants.append(c.gpa, value) catch oom();
        return c.constants.items.len - 1;
    }

    pub fn deinit(c: *Chunk) void {
        c.code.deinit(c.gpa);
        c.lines.deinit(c.gpa);
        c.constants.deinit(c.gpa);
        c.string_arena.deinit();
        c.* = undefined;
    }
};

const ResolvedSymbol = struct {
    ident: Token,
    sym: Nir.Symbol,
};

pub const Compiler = struct {
    gpa: Allocator,
    scratch: Allocator,

    compiling_chunk: *Chunk,
    sym_table: *Nir.SymbolTable,
    stmts: []Nir.Stmt,
    current_stmt: usize,
    global_locations: std.ArrayList([]const u8),

    fn init(gpa: Allocator, scratch: Allocator, nir: *Nir, chunk: *Chunk) Compiler {
        return .{
            .gpa = gpa,
            .scratch = scratch,
            .compiling_chunk = chunk,
            .sym_table = &nir.sym_table,
            .stmts = nir.stmts,
            .current_stmt = 0,
            .global_locations = .empty,
        };
    }

    fn deinit(c: *Compiler) void {
        c.global_locations.deinit(c.scratch);
    }

    fn compile(c: *Compiler) void {
        for (c.stmts, 0..) |stmt, i| {
            c.current_stmt = i;
            c.statement(stmt);
        }
    }

    fn statement(c: *Compiler, stmt: Nir.Stmt) void {
        switch (stmt) {
            .expression => |expr_stmt| {
                c.expression(expr_stmt.expr);
                if (!builtin.is_test) {
                    c.emitOpCode(.op_pop, expr_stmt.expr.token().line);
                }
            },
            .var_decl => |vd| {
                const sym = c.sym_table.find(vd.ident.lexeme);
                if (sym.scope.level == .top) {
                    c.globalDecl(vd);
                }
            },
            .print => |p| {
                c.expression(p.expr);
                c.emitOpCode(.op_temp_print, p.print.line);
            },
            .var_assign => @panic("todo"),
        }
    }

    fn globalDecl(c: *Compiler, vd: Nir.Stmt.VarDecl) void {
        c.expression(vd.value);

        const location = c.global_locations.items.len;
        c.global_locations.append(c.scratch, vd.ident.lexeme) catch oom();

        c.emitStore(@intCast(location), vd.ident.line);
    }

    fn globalIdent(c: *Compiler, ident: Token) void {
        const location = c.resolveGlobalIdentLoc(ident);
        c.emitLoad(location, ident.line);
    }

    fn resolveGlobalIdentLoc(c: *Compiler, ident: Token) u16 {
        for (c.global_locations.items, 0..) |global_ident, i| {
            if (mem.eql(u8, global_ident, ident.lexeme)) {
                return @intCast(i);
            }
        }
        unreachable;
    }

    fn identifier(c: *Compiler, i: *Nir.Expr.Identifier) void {
        // TODO: proper scopes
        c.globalIdent(i.ident);
    }

    fn expression(c: *Compiler, expr: *Nir.Expr) void {
        switch (expr.kind) {
            .binary => |*b| c.binary(b),
            .unary => |*u| c.unary(u),
            .cast => |*ca| c.cast(ca, expr.type),
            .grouping => |*g| c.grouping(g),
            .literal => |*l| c.literal(l),
            .identifier => |*i| c.identifier(i),
        }
    }

    fn binary(c: *Compiler, b: *Nir.Expr.Binary) void {
        c.expression(b.left);
        c.expression(b.right);

        const line = b.operator.line;
        const operand_type = b.left.type;
        const op_code: OpCode = switch (b.operator.type) {
            .plus => switch (operand_type) {
                .n_string => .op_concat,
                .n_float => .op_add_float,
                .n_int => .op_add_int,
                else => unreachable,
            },
            .minus => switch (operand_type) {
                .n_float => .op_subtract_float,
                .n_int => .op_subtract_int,
                else => unreachable,
            },
            .star => switch (operand_type) {
                .n_float => .op_multiply_float,
                .n_int => .op_multiply_int,
                else => unreachable,
            },
            .slash => .op_divide_float,
            .equal_equal => switch (operand_type) {
                .n_string => .op_equal_string,
                .n_float => .op_equal_float,
                .n_int => .op_equal_int,
                .n_bool => .op_equal_bool,
                else => unreachable,
            },
            .bang_equal => switch (operand_type) {
                .n_string => .op_not_equal_string,
                .n_float => .op_not_equal_float,
                .n_int => .op_not_equal_int,
                .n_bool => .op_not_equal_bool,
                else => unreachable,
            },
            .greater => switch (operand_type) {
                .n_float => .op_greater_float,
                .n_int => .op_greater_int,
                else => unreachable,
            },
            .greater_equal => switch (operand_type) {
                .n_float => .op_greater_equal_float,
                .n_int => .op_greater_equal_int,
                else => unreachable,
            },
            .less => switch (operand_type) {
                .n_float => .op_less_float,
                .n_int => .op_less_int,
                else => unreachable,
            },
            .less_equal => switch (operand_type) {
                .n_float => .op_less_equal_float,
                .n_int => .op_less_equal_int,
                else => unreachable,
            },
            .kw_and => .op_and,
            .kw_or => .op_or,
            else => unreachable,
        };
        c.emitOpCode(op_code, line);
    }

    fn unary(c: *Compiler, u: *Nir.Expr.Unary) void {
        c.expression(u.expr);
        const line = u.operator.line;
        const ty = u.expr.type;
        switch (u.operator.type) {
            .minus => {
                const op_code: OpCode = switch (ty) {
                    .n_float => .op_negate_float,
                    .n_int => .op_negate_int,
                    else => unreachable,
                };
                c.emitOpCode(op_code, line);
            },
            .bang => c.emitOpCode(.op_not, line),
            else => unreachable,
        }
    }

    fn cast(c: *Compiler, cst: *Nir.Expr.Cast, target: NormType) void {
        c.expression(cst.expr);
        const line = cst.token.line;
        const op_code: OpCode = switch (target) {
            .n_int => .op_cast_to_int,
            .n_float => .op_cast_to_float,
            else => unreachable,
        };
        c.emitOpCode(op_code, line);
    }

    fn grouping(c: *Compiler, g: *Nir.Expr.Grouping) void {
        c.expression(g.expr);
    }

    fn literal(c: *Compiler, l: *Nir.Expr.Literal) void {
        switch (l.value) {
            .float => |f| c.compiling_chunk.writeConstant(.{ .float = f }, l.token.line),
            .integer => |i| c.compiling_chunk.writeConstant(.{ .integer = i }, l.token.line),
            .boolean => |b| if (b)
                c.emitOpCode(.op_true, l.token.line)
            else
                c.emitOpCode(.op_false, l.token.line),
            .string => |s| c.compiling_chunk.writeString(s, l.token.line),
            .nil => c.emitOpCode(.op_nil, l.token.line),
        }
    }

    fn emitConstant(c: *Compiler, value: Value, line: u32) void {
        c.compiling_chunk.writeConstant(value, line);
    }

    fn emitOpCode(c: *Compiler, op: OpCode, line: u32) void {
        c.emitByte(op.byte(), line);
    }

    fn emitOpCodes(c: *Compiler, op1: OpCode, op2: OpCode, line: u32) void {
        c.emitOpCode(op1, line);
        c.emitOpCode(op2, line);
    }

    fn emitStore(c: *Compiler, location: u16, line: u32) void {
        c.emitOpCode(.op_store, line);
        c.emitShort(location, line);
    }

    fn emitLoad(c: *Compiler, location: u16, line: u32) void {
        c.emitOpCode(.op_load, line);
        c.emitShort(location, line);
    }

    fn emitByte(c: *Compiler, b: u8, line: u32) void {
        c.compiling_chunk.write(b, line);
    }

    fn emitShort(c: *Compiler, b: u16, line: u32) void {
        c.compiling_chunk.writeShort(b, line);
    }

    fn emitBytes(c: *Compiler, a: u8, b: u8) void {
        c.emitByte(a);
        c.emitByte(b);
    }
};

pub fn compile(gpa: Allocator, nir: *Nir) Chunk {
    if (nir.errors.len > 0) return .init(gpa);

    var chunk: Chunk = .init(gpa);
    var c: Compiler = .init(gpa, gpa, nir, &chunk);
    defer c.deinit();

    c.compile();

    c.emitOpCode(.op_return, 0);

    return chunk;
}

test "basic chunk ops" {
    const gpa = testing.allocator;
    var chunk: Chunk = .init(gpa);
    defer chunk.deinit();

    chunk.writeConstant(.{ .integer = 10 }, 1);
    chunk.writeConstant(.{ .float = 3.14 }, 1);
    chunk.writeOp(.op_return, 2);

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

fn testCompile(gpa: Allocator, source: []const u8) !Chunk {
    var l = Lexer.init(source);
    const tokens = l.scanTokens(gpa);
    defer {
        gpa.free(tokens.tokens);
        gpa.free(tokens.errors);
    }
    if (tokens.errors.len > 0) {
        dbg("tokens.errors", tokens.errors);
        return error.LexerError;
    }

    var ast = parser.parse(gpa, tokens.tokens, true);
    defer ast.arena.deinit();
    if (ast.errors.len > 0) {
        dbg("ast.errors", ast.errors);
        return error.ParserError;
    }

    var nir = sema.analyze(gpa, &ast);
    defer nir.deinit();

    if (nir.errors.len > 0) {
        debug.reportErrors(nir.errors, "test_runner", source);
        return error.SemaFailed;
    }

    return compile(gpa, &nir);
}

const TestCase = struct {
    source: []const u8,
    expected_code: []const u8,
    expected_lines: []const u32,
    expected_constants: []const Value,
};

const TestCaseMinimal = struct {
    source: []const u8,
    expected_code: []const u8,
    expected_constants: []const Value,
};

test "literals" {
    const gpa = testing.allocator;
    const tests: []const TestCase = &.{
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
        defer chunk.deinit();

        try testing.expectEqualSlices(u8, t.expected_code, chunk.code.items);
        try testing.expectEqualSlices(u32, t.expected_lines, chunk.lines.items);
        try testing.expectEqualSlices(Value, t.expected_constants, chunk.constants.items);
    }
}

test "arithmetic expressions" {
    const gpa = testing.allocator;
    const tests: []const TestCase = &.{
        .{
            .source = "2 + 3",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_add_int, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .integer = 3 } },
        },
        .{
            .source = "2.0 + 3.0",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_add_float, .op_return }),
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
                .op_add_float,
                .op_constant,
                2,
                .op_constant,
                3,
                .op_divide_float,
                .op_subtract_float,
                .op_return,
            }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .float = 2.0 }, .{ .float = 3 }, .{ .float = 4.0 }, .{ .float = 2.0 } },
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source = \"{s}\"\n", .{t.source});
        var chunk = try testCompile(gpa, t.source);
        defer chunk.deinit();

        try testing.expectEqualSlices(u8, t.expected_code, chunk.code.items);
        try testing.expectEqualSlices(u32, t.expected_lines, chunk.lines.items);
        try testing.expectEqualSlices(Value, t.expected_constants, chunk.constants.items);
    }
}

test "auto cast arithmetic expressions" {
    const gpa = testing.allocator;
    const tests: []const TestCase = &.{
        .{
            .source = "2 + 3.0",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_cast_to_float, .op_constant, 1, .op_add_float, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .float = 3 } },
        },
        .{
            .source = "2 * 3.0",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_cast_to_float, .op_constant, 1, .op_multiply_float, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .float = 3 } },
        },
        .{
            .source = "2.0 / 3",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_cast_to_float, .op_divide_float, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .float = 2 }, .{ .integer = 3 } },
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source = \"{s}\"\n", .{t.source});
        var chunk = try testCompile(gpa, t.source);
        defer chunk.deinit();

        try testing.expectEqualSlices(u8, t.expected_code, chunk.code.items);
        try testing.expectEqualSlices(u32, t.expected_lines, chunk.lines.items);
        try testing.expectEqualSlices(Value, t.expected_constants, chunk.constants.items);
    }
}

test "comparison" {
    const gpa = testing.allocator;
    const tests: []const TestCase = &.{
        // TODO: use something other than numbers
        .{
            .source = "2 < 3",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_less_int, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .integer = 3 } },
        },
        .{
            .source = "2 <= 3",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_less_equal_int, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .integer = 3 } },
        },
        .{
            .source = "2 > 3",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_greater_int, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .integer = 3 } },
        },
        .{
            .source = "2 >= 3",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_greater_equal_int, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .integer = 3 } },
        },
        .{
            .source = "2 == 3",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_equal_int, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .integer = 3 } },
        },
        .{
            .source = "2 != 3",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_not_equal_int, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .integer = 3 } },
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source = \"{s}\"\n", .{t.source});
        var chunk = try testCompile(gpa, t.source);
        defer chunk.deinit();

        try testing.expectEqualSlices(u8, t.expected_code, chunk.code.items);
        try testing.expectEqualSlices(u32, t.expected_lines, chunk.lines.items);
        try testing.expectEqualSlices(Value, t.expected_constants, chunk.constants.items);
    }
}

test "logical" {
    const gpa = testing.allocator;
    const tests: []const TestCase = &.{
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
        defer chunk.deinit();

        try testing.expectEqualSlices(u8, t.expected_code, chunk.code.items);
        try testing.expectEqualSlices(u32, t.expected_lines, chunk.lines.items);
        try testing.expectEqualSlices(Value, t.expected_constants, chunk.constants.items);
    }
}

test "casting" {
    const gpa = testing.allocator;
    const tests: []const TestCase = &.{
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
        defer chunk.deinit();

        try testing.expectEqualSlices(u8, t.expected_code, chunk.code.items);
        try testing.expectEqualSlices(u32, t.expected_lines, chunk.lines.items);
        try testing.expectEqualSlices(Value, t.expected_constants, chunk.constants.items);
    }
}

test "string concatenation" {
    const gpa = testing.allocator;
    const tests: []const TestCase = &.{
        .{
            .source = "\"Hello, \" + \"World\"",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_concat, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .string = .ref("Hello, ") }, .{ .string = .ref("World") } },
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source = \"{s}\"\n", .{t.source});
        var chunk = try testCompile(gpa, t.source);
        defer chunk.deinit();

        try testing.expectEqualSlices(u8, t.expected_code, chunk.code.items);
        try testing.expectEqualSlices(u32, t.expected_lines, chunk.lines.items);
        try testing.expectEqualDeep(t.expected_constants, chunk.constants.items);
    }
}

test "global variables - simple op_store" {
    const gpa = testing.allocator;
    const tests: []const TestCase = &.{
        .{
            .source = "x := 10;",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_store, 0, 0, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 0 },
            .expected_constants = &.{.{ .integer = 10 }},
        },
        .{
            .source = "x := 10; y := false;",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_store, 0, 0, .op_false, .op_store, 1, 0, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{.{ .integer = 10 }},
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source = \"{s}\"\n", .{t.source});
        var chunk = try testCompile(gpa, t.source);
        defer chunk.deinit();

        try testing.expectEqualSlices(u8, t.expected_code, chunk.code.items);
        try testing.expectEqualSlices(u32, t.expected_lines, chunk.lines.items);
        try testing.expectEqualDeep(t.expected_constants, chunk.constants.items);
    }
}

test "global variables - op_load" {
    const gpa = testing.allocator;
    // zig fmt: off
    const tests: []const TestCaseMinimal = &.{
        .{
            .source = "x := 10; x;",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_store, 0, 0, .op_load, 0, 0, .op_return }),
            .expected_constants = &.{.{ .integer = 10 }},
        },
        .{
            .source = "x := 10; y := x;",
            .expected_code = &debug.opCodeToBytes(&.{
                .op_constant, 0,
                .op_store, 0, 0,
                .op_load, 0, 0,
                .op_store, 1, 0,
                .op_return,
            }),
            .expected_constants = &.{.{ .integer = 10 }},
        },
        .{
            .source = "x := 10; y := x; x + y",
            .expected_code = &debug.opCodeToBytes(&.{
                .op_constant, 0,
                .op_store, 0, 0,
                .op_load, 0, 0,
                .op_store, 1, 0,
                .op_load, 0, 0,
                .op_load, 1, 0,
                .op_add_int,
                .op_return,
            }),
            .expected_constants = &.{.{ .integer = 10 }},
        },
        .{
            .source =
            \\hello := "Hello";
            \\world := "World";
            \\hello
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                .op_constant, 0,
                .op_store, 0, 0,
                .op_constant, 1,
                .op_store, 1, 0,
                .op_load, 0, 0,
                .op_return,
            }),
            .expected_constants = &.{ .{ .string = .ref("Hello") }, .{ .string = .ref("World") } }
        }
    };
    // zig fmt: on

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source = \"{s}\"\n", .{t.source});
        var chunk = try testCompile(gpa, t.source);
        defer chunk.deinit();

        try testing.expectEqualSlices(u8, t.expected_code, chunk.code.items);
        try testing.expectEqualDeep(t.expected_constants, chunk.constants.items);
    }
}

test "temporary print opcode" {
    const gpa = testing.allocator;
    const tests: []const TestCaseMinimal = &.{
        .{
            .source = "print(10);",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_temp_print, .op_return }),
            .expected_constants = &.{.{ .integer = 10 }},
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source = \"{s}\"\n", .{t.source});
        var chunk = try testCompile(gpa, t.source);
        defer chunk.deinit();

        try testing.expectEqualSlices(u8, t.expected_code, chunk.code.items);
        try testing.expectEqualDeep(t.expected_constants, chunk.constants.items);
    }
}
