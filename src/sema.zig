const std = @import("std");
const mem = std.mem;
const Io = std.Io;
const Allocator = mem.Allocator;

const Ast = @import("parser.zig").Ast;
const debug = @import("debug.zig");
const ers = @import("errors.zig");
const Token = @import("Lexer.zig").Token;

pub const NormType = enum {
    n_int,
    n_float,
    n_bool,

    n_invalid,

    pub fn format(nt: NormType, w: *Io.Writer) Io.Writer.Error!void {
        try w.print("{s}", .{@tagName(nt)[2..]});
    }

    pub const number_types: []const NormType = &.{ .n_int, .n_float };
};

pub const Diagnostics = struct {
    line: u32,
    hints: []const []const u8 = &.{},
    notes: []const []const u8 = &.{},
    error_msg: []const u8,

    pub fn promote(
        d: *const Diagnostics,
        file_name: []const u8,
        source: []const u8,
    ) ers.Diagnostics {
        return .{
            .error_msg = d.error_msg,
            .line = d.line,
            .notes = d.notes,
            .hints = d.hints,
            .file_name = file_name,
            .source = source,
        };
    }
};

/// Norm intermediate representation
///
/// Typed AST
pub const Nir = struct {
    pub const Binary = struct {
        left: *Expr,
        operator: Token,
        right: *Expr,

        pub fn format(expr: *const Binary, w: *Io.Writer) Io.Writer.Error!void {
            try w.print("({f} {s} {f})", .{ expr.left, expr.operator.lexeme, expr.right });
        }
    };

    pub const Unary = struct {
        operator: Token,
        expr: *Expr,

        pub fn format(expr: *const Unary, w: *Io.Writer) Io.Writer.Error!void {
            try w.print("({s}{f})", .{ expr.operator.lexeme, expr.expr });
        }
    };

    pub const Cast = struct {
        target: Token,
        expr: *Expr,

        pub fn format(expr: *const Cast, w: *Io.Writer) Io.Writer.Error!void {
            const target = switch (expr.target.type) {
                .kw_float => "float",
                .kw_int => "int",
                else => unreachable,
            };
            try w.print("{s}({f})", .{ target, expr.expr });
        }
    };

    pub const Literal = struct {
        pub const Value = union(enum) {
            integer: i32,
            float: f64,
            string: []const u8,
            boolean: bool,
            nil: void,

            pub fn format(value: *const Value, w: *Io.Writer) Io.Writer.Error!void {
                try switch (value.*) {
                    .integer => |i| w.print("{}", .{i}),
                    .float => |i| w.print("{d:.3}", .{i}),
                    .string => |i| w.print("{s}", .{i}),
                    .boolean => |i| w.print("{}", .{i}),
                    .nil => w.print("nil", .{}),
                };
            }

            fn fromAst(ast_val: Ast.Literal.Value) Value {
                return switch (ast_val) {
                    .integer => |x| .{ .integer = x },
                    .float => |x| .{ .float = x },
                    .string => |x| .{ .string = x },
                    .boolean => |x| .{ .boolean = x },
                    .nil => .nil,
                };
            }
        };

        value: Value,
        token: Token,

        pub fn format(expr: *const Literal, w: *Io.Writer) Io.Writer.Error!void {
            try w.print("{f}", .{expr.value});
        }
    };

    pub const Grouping = struct {
        paren: Token,
        expr: *Expr,

        pub fn format(expr: *const Grouping, w: *Io.Writer) Io.Writer.Error!void {
            try w.print("({f})", .{expr.expr});
        }
    };

    pub const Expr = struct {
        type: NormType,
        kind: union(enum) {
            binary: Binary,
            unary: Unary,
            cast: Cast,
            grouping: Grouping,
            literal: Literal,
        },

        pub const invalid: Expr = .{
            .type = .n_invalid,
            .kind = undefined,
        };

        pub fn token(e: *const Expr) Token {
            return switch (e.kind) {
                .binary => |*b| b.operator,
                .unary => |*u| u.operator,
                .cast => |*c| c.target,
                .literal => |*l| l.token,
                .grouping => |*g| g.paren,
            };
        }

        pub fn format(expr: *const Expr, w: *Io.Writer) Io.Writer.Error!void {
            // _ = debug.dbgw(w, "", expr);
            try switch (expr.kind) {
                .literal => |l| w.print("{f}", .{l}),
                inline else => |b| w.print("{f}:{f}", .{ b, expr.type }),
            };
        }
    };

    arena: std.heap.ArenaAllocator,
    expr: *Expr,
    errors: []Diagnostics,
};

fn makeGrouping(arena: Allocator, grping: *Nir.Expr, paren: Token, ty: NormType) *Nir.Expr {
    const e = makeExpr(arena);
    e.* = .{ .kind = .{ .grouping = .{ .paren = paren, .expr = grping } }, .type = ty };
    return e;
}

fn makeUnary(arena: Allocator, expr: *Nir.Expr, op: Token, ty: NormType) *Nir.Expr {
    const e = makeExpr(arena);
    e.* = .{
        .kind = .{ .unary = .{ .expr = expr, .operator = op } },
        .type = ty,
    };
    return e;
}

fn makeCast(arena: Allocator, expr: *Nir.Expr, target: Token, ty: NormType) *Nir.Expr {
    const e = makeExpr(arena);
    e.* = .{
        .kind = .{ .cast = .{ .expr = expr, .target = target } },
        .type = ty,
    };
    return e;
}

fn makeBinary(arena: Allocator, left: *Nir.Expr, op: Token, right: *Nir.Expr, ty: NormType) *Nir.Expr {
    const e = makeExpr(arena);
    e.* = .{
        .kind = .{ .binary = .{ .left = left, .operator = op, .right = right } },
        .type = ty,
    };
    return e;
}

fn makeLiteral(arena: Allocator, value: Nir.Literal.Value, token: Token, ty: NormType) *Nir.Expr {
    const e = makeExpr(arena);
    e.* = .{
        .kind = .{ .literal = .{ .token = token, .value = value } },
        .type = ty,
    };
    return e;
}

fn makeInvalid(arena: Allocator) *Nir.Expr {
    const expr = makeExpr(arena);
    expr.* = .invalid;
    return expr;
}

fn makeExpr(arena: Allocator) *Nir.Expr {
    return arena.create(Nir.Expr) catch oom();
}

const Sema = struct {
    arena: std.heap.ArenaAllocator,
    errors: std.ArrayList(Diagnostics),
    // TODO: reset on statement boundary
    panic_mode: bool,

    fn init(gpa: Allocator) Sema {
        return .{
            .arena = .init(gpa),
            .errors = .empty,
            .panic_mode = false,
        };
    }

    fn expression(s: *Sema, expr: *Ast.Expr) *Nir.Expr {
        return switch (expr.*) {
            .binary => |*b| s.binary(b),
            .unary => |*u| s.unary(u),
            .cast => |*c| s.cast(c),
            .grouping => |*g| s.grouping(g),
            .literal => |*l| s.literal(l),
        };
    }

    fn binary(s: *Sema, b: *Ast.Binary) *Nir.Expr {
        const left = s.expression(b.left);
        const right = s.expression(b.right);
        const arena = s.arena.allocator();

        switch (b.operator.type) {
            .plus, .minus, .star, .slash => {
                // TODO: casting
                if (left.type != right.type) {
                    s.reportError(left, "{f} {s} {f} is not a valid operation", .{
                        left.type,
                        b.operator.lexeme,
                        right.type,
                    });
                    return makeInvalid(arena);
                }
                if (!s.expectTypes(
                    left,
                    NormType.number_types,
                    "{f} {s} {f} is not a valid operation",
                    .{ left.type, b.operator.lexeme, right.type },
                )) {
                    return makeInvalid(arena);
                }

                return makeBinary(arena, left, b.operator, right, left.type);
            },
            .equal_equal, .bang_equal => {
                if (left.type != right.type) {
                    s.reportError(left, "Cannot compare {f} and {f}.", .{ left.type, right.type });
                    return makeInvalid(arena);
                }
                return makeBinary(arena, left, b.operator, right, .n_bool);
            },
            .greater, .greater_equal, .less, .less_equal => {
                if (left.type != right.type or !checkTypes(left, NormType.number_types)) {
                    s.reportError(left, "Cannot compare {f} and {f}.", .{ left.type, right.type });
                    return makeInvalid(arena);
                }
                return makeBinary(arena, left, b.operator, right, .n_bool);
            },
            .kw_and, .kw_or => {
                if (left.type != right.type or left.type != .n_bool) {
                    s.reportError(left, "Cannot compare {f} and {f}.", .{ left.type, right.type });
                    return makeInvalid(arena);
                }
                return makeBinary(arena, left, b.operator, right, .n_bool);
            },
            else => unreachable,
        }
    }

    fn unary(s: *Sema, u: *Ast.Unary) *Nir.Expr {
        const arena = s.arena.allocator();
        const expr = s.expression(u.expr);
        switch (u.operator.type) {
            .minus => {
                if (!s.expectTypes(expr, NormType.number_types, "Cannot negate {t}", .{expr.type}))
                    return makeInvalid(arena);
                return makeUnary(arena, expr, u.operator, expr.type);
            },
            .bang => {
                if (!s.expectType(expr, .n_bool, "! only supports bools", .{}))
                    return makeInvalid(arena);
                return makeUnary(arena, expr, u.operator, expr.type);
            },
            else => unreachable,
        }
    }

    fn cast(s: *Sema, c: *Ast.Cast) *Nir.Expr {
        const arena = s.arena.allocator();

        const expr = s.expression(c.expr);
        if (expr.type == .n_invalid) return makeInvalid(arena);

        const target_type: NormType = switch (c.target.type) {
            .kw_float => if (!s.expectType(expr, .n_int, "Cannot cast {f} to float, expected int", .{expr.type}))
                return makeInvalid(arena)
            else
                .n_float,
            .kw_int => if (!s.expectType(expr, .n_float, "Cannot cast {f} to int, expected float", .{expr.type}))
                return makeInvalid(arena)
            else
                .n_int,
            else => unreachable,
        };

        return makeCast(arena, expr, c.target, target_type);
    }

    fn grouping(s: *Sema, g: *Ast.Grouping) *Nir.Expr {
        const expr = s.expression(g.expr);
        if (expr.type == .n_invalid) return makeInvalid(s.arena.allocator());
        return makeGrouping(s.arena.allocator(), expr, g.paren, expr.type);
    }

    fn literal(s: *Sema, l: *Ast.Literal) *Nir.Expr {
        const ty: NormType = switch (l.value) {
            .float => .n_float,
            .integer => .n_int,
            .boolean => .n_bool,
            .nil, .string => @panic("todo"),
        };
        return makeLiteral(s.arena.allocator(), .fromAst(l.value), l.token, ty);
    }

    fn expectType(
        s: *Sema,
        expr: *Nir.Expr,
        ty: NormType,
        comptime error_msg: []const u8,
        args: anytype,
    ) bool {
        if (expr.type == ty) return true;
        s.reportError(expr, error_msg, args);
        return false;
    }

    fn expectTypes(
        s: *Sema,
        expr: *Nir.Expr,
        comptime types: []const NormType,
        comptime error_msg: []const u8,
        args: anytype,
    ) bool {
        if (checkTypes(expr, types)) return true;
        s.reportError(expr, error_msg, args);
        return false;
    }

    fn checkTypes(expr: *Nir.Expr, comptime types: []const NormType) bool {
        for (types) |ty|
            if (expr.type == ty) return true;
        return false;
    }

    fn diag(s: *Sema, expr: *Nir.Expr, comptime error_msg: []const u8, args: anytype) Diagnostics {
        return .{
            .error_msg = std.fmt.allocPrint(s.arena.allocator(), error_msg, args) catch oom(),
            .line = expr.token().line,
        };
    }

    fn reportError(s: *Sema, expr: *Nir.Expr, comptime error_msg: []const u8, args: anytype) void {
        if (s.panic_mode) return;
        s.errors.append(s.arena.allocator(), s.diag(expr, error_msg, args)) catch oom();
        s.panic_mode = true;
    }
};

fn oom() noreturn {
    @panic("oom");
}

pub fn analyze(gpa: Allocator, ast: *Ast) Nir {
    if (ast.errors.len > 0) return undefined;

    var sema = Sema.init(gpa);
    const nir_expr = sema.expression(ast.expr);
    const errors = sema.errors.toOwnedSlice(sema.arena.allocator()) catch oom();
    return .{
        .arena = sema.arena,
        .errors = errors,
        .expr = nir_expr,
    };
}

const Lexer = @import("Lexer.zig");
const parser = @import("parser.zig");
const dbg = @import("debug.zig").dbg;

fn testAnalyze(gpa: Allocator, source: []const u8) ![]const u8 {
    var l = Lexer.init(source);

    var ast = parser.parse(gpa, &l);
    defer ast.arena.deinit();
    if (ast.errors.len > 0) {
        debug.reportErrors(ast.errors, "test_runner", source);
        return error.ParserFailed;
    }

    var nir = analyze(gpa, &ast);
    defer nir.arena.deinit();
    if (nir.errors.len > 0) {
        debug.reportErrors(nir.errors, "test_runner", source);
        return error.SemaFailed;
    }

    var aw = Io.Writer.Allocating.init(gpa);
    try nir.expr.format(&aw.writer);
    return aw.toOwnedSlice();
}

fn testAnalyzeFailure(gpa: Allocator, source: []const u8) !Nir {
    var l = Lexer.init(source);

    var ast = parser.parse(gpa, &l);
    defer ast.arena.deinit();
    if (ast.errors.len > 0) {
        debug.reportErrors(ast.errors, "test_runner", source);
        return error.ParserFailed;
    }

    const nir = analyze(gpa, &ast);
    if (nir.errors.len == 0) {
        std.debug.print("sema passed with output=\"{f}\"\n", .{nir.expr});
        return error.SemaPassed;
    }
    return nir;
}

const testing = std.testing;

test "arithmetic failure" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        error_msg: []const u8,
    } = &.{
        .{ .source = "1 + true", .error_msg = "int + bool is not a valid operation" },
        .{ .source = "1.0 + true", .error_msg = "float + bool is not a valid operation" },
        .{ .source = "true + 1.0", .error_msg = "bool + float is not a valid operation" },
        .{ .source = "false + 2", .error_msg = "bool + int is not a valid operation" },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"\n", .{t.source});

        const nir = try testAnalyzeFailure(gpa, t.source);
        defer nir.arena.deinit();

        try testing.expect(nir.errors.len == 1);
        try testing.expectEqualStrings(t.error_msg, nir.errors[0].error_msg);
    }
}

test "arithmetic" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{ .source = "2 + 2", .expected = "(2 + 2):int" },
        .{ .source = "-2", .expected = "(-2):int" },
        .{ .source = "-2", .expected = "(-2):int" },
        .{ .source = "2 * 3 + 4", .expected = "((2 * 3):int + 4):int" },
        .{ .source = "9.0 / 3.0 * 32.0", .expected = "((9.000 / 3.000):float * 32.000):float" },
    };

    for (tests) |t| {
        const actual = try testAnalyze(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}

test "comparison" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{ .source = "2 > 1", .expected = "(2 > 1):bool" },
        .{ .source = "3 <= 2", .expected = "(3 <= 2):bool" },
        .{ .source = "1 == 1", .expected = "(1 == 1):bool" },
        .{ .source = "1 != 2", .expected = "(1 != 2):bool" },
        .{ .source = "true == true", .expected = "(true == true):bool" },
        .{ .source = "true != false", .expected = "(true != false):bool" },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"", .{t.source});

        const actual = try testAnalyze(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}

test "comparison failure" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        error_msg: []const u8,
    } = &.{
        .{ .source = "true > false", .error_msg = "Cannot compare bool and bool." },
        .{ .source = "true == 1", .error_msg = "Cannot compare bool and int." },
        .{ .source = "true != 2", .error_msg = "Cannot compare bool and int." },
        .{ .source = "2 < false", .error_msg = "Cannot compare int and bool." },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"\n", .{t.source});

        const nir = try testAnalyzeFailure(gpa, t.source);
        defer nir.arena.deinit();

        try testing.expect(nir.errors.len == 1);
        try testing.expectEqualStrings(t.error_msg, nir.errors[0].error_msg);
    }
}

test "logical" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{ .source = "true and true", .expected = "(true and true):bool" },
        .{ .source = "true or false", .expected = "(true or false):bool" },
        .{ .source = "!true", .expected = "(!true):bool" },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"", .{t.source});

        const actual = try testAnalyze(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}

test "logical failure" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        error_msg: []const u8,
    } = &.{
        .{ .source = "true and 1", .error_msg = "Cannot compare bool and int." },
        .{ .source = "1 or true", .error_msg = "Cannot compare int and bool." },
        .{ .source = "1 and 1", .error_msg = "Cannot compare int and int." },
        .{ .source = "!1", .error_msg = "! only supports bools" },
        .{ .source = "!1.0", .error_msg = "! only supports bools" },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"\n", .{t.source});

        const nir = try testAnalyzeFailure(gpa, t.source);
        defer nir.arena.deinit();

        try testing.expect(nir.errors.len == 1);
        try testing.expectEqualStrings(t.error_msg, nir.errors[0].error_msg);
    }
}
