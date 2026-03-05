const std = @import("std");
const mem = std.mem;
const Io = std.Io;
const Allocator = mem.Allocator;

const Ast = @import("parser.zig").Ast;
const debug = @import("debug.zig");
const ers = @import("errors.zig");
const Token = @import("Lexer.zig").Token;

const Nir = @import("Nir.zig");
const NormType = Nir.NormType;

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

fn makeCast(arena: Allocator, expr: *Nir.Expr, token: Token, target: NormType) *Nir.Expr {
    const e = makeExpr(arena);
    e.* = .{
        .kind = .{ .cast = .{ .expr = expr, .token = token } },
        .type = target,
    };
    return e;
}

fn fakeTargetToken(target: NormType, line: u32) Token {
    return switch (target) {
        .n_int => .{ .type = .kw_int, .lexeme = "int", .line = line },
        .n_float => .{ .type = .kw_float, .lexeme = "float", .line = line },
        else => unreachable,
    };
}

fn makeAnonCast(arena: Allocator, expr: *Nir.Expr, target: NormType) *Nir.Expr {
    const expr_token = expr.token();
    const target_token = fakeTargetToken(target, expr_token.line);
    return makeCast(arena, expr, target_token, target);
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
    errors: std.ArrayList(Nir.Diagnostics),
    // TODO: reset on statement boundary
    panic_mode: bool,
    invalid: *Nir.Expr,

    const TypeMask = u64;

    fn mask(ty: NormType) TypeMask {
        return @as(u64, 1) << ty.int();
    }

    const cast_map = init: {
        var m = [_]TypeMask{0} ** @typeInfo(NormType).@"enum".fields.len;
        m[NormType.n_int.int()] = mask(.n_float);
        m[NormType.n_float.int()] = mask(.n_int);
        break :init m;
    };

    fn canCast(ty: NormType, target: NormType) bool {
        return (cast_map[ty.int()] & mask(target)) != 0;
    }

    fn commonType(l: NormType, r: NormType) NormType {
        if (l == r) return l;
        if (l.isNumeric() or r.isNumeric()) {
            return .n_float;
        }
        @panic("hmm");
    }

    fn init(gpa: Allocator) Sema {
        var arena = std.heap.ArenaAllocator.init(gpa);
        const invalid = makeInvalid(arena.allocator());
        return .{
            .invalid = invalid,
            .arena = arena,
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

    fn tryCast(s: *Sema, expr: *Nir.Expr, target: NormType) ?*Nir.Expr {
        if (expr.type == target) return expr;
        const arena = s.arena.allocator();
        if (canCast(expr.type, target)) {
            return makeAnonCast(arena, expr, target);
        }
        return null;
    }

    fn binary(s: *Sema, b: *Ast.Binary) *Nir.Expr {
        const left = s.expression(b.left);
        const right = s.expression(b.right);
        const arena = s.arena.allocator();

        switch (b.operator.type) {
            .plus => {
                if (left.type.isNumeric() and right.type.isNumeric()) {
                    const common_type = commonType(left.type, right.type);
                    const cast_left = s.tryCast(left, common_type) orelse return s.castError(left, common_type);
                    const cast_right = s.tryCast(right, common_type) orelse return s.castError(right, common_type);
                    return makeBinary(arena, cast_left, b.operator, cast_right, common_type);
                }
                if (left.type.isString() and right.type.isString()) {
                    return makeBinary(arena, left, b.operator, right, .n_string);
                }
                return s.invalidBinaryOp(left, b.operator, right);
            },
            .minus, .star => {
                if (left.type.isNumeric() and right.type.isNumeric()) {
                    const common_type = commonType(left.type, right.type);
                    const cast_left = s.tryCast(left, common_type) orelse return s.castError(left, common_type);
                    const cast_right = s.tryCast(right, common_type) orelse return s.castError(right, common_type);
                    return makeBinary(arena, cast_left, b.operator, cast_right, common_type);
                }
                return s.invalidBinaryOp(left, b.operator, right);
            },

            .slash => {
                if (left.type.isNumeric() and right.type.isNumeric()) {
                    const cast_left = s.tryCast(left, .n_float) orelse return s.invalid;
                    const cast_right = s.tryCast(right, .n_float) orelse return s.invalid;
                    return makeBinary(arena, cast_left, b.operator, cast_right, .n_float);
                }
                return s.invalidBinaryOp(left, b.operator, right);
            },

            .equal_equal, .bang_equal => {
                if (left.type.isComparable() and right.type.isComparable()) {
                    const common_type = commonType(left.type, right.type);
                    const cast_left = s.tryCast(left, common_type) orelse s.compareError(left, right);
                    const cast_right = s.tryCast(right, common_type) orelse s.compareError(left, right);
                    return makeBinary(arena, cast_left, b.operator, cast_right, .n_bool);
                }
                return s.compareError(left, right);
            },
            .greater, .greater_equal, .less, .less_equal => {
                if (left.type.isOrderable() and right.type.isOrderable()) {
                    const common_type = commonType(left.type, right.type);
                    const cast_left = s.tryCast(left, common_type) orelse s.compareError(left, right);
                    const cast_right = s.tryCast(right, common_type) orelse s.compareError(left, right);
                    return makeBinary(arena, cast_left, b.operator, cast_right, .n_bool);
                }
                return s.compareError(left, right);
            },
            .kw_and, .kw_or => {
                if (left.type != .n_bool or right.type != .n_bool) {
                    return s.compareError(left, right);
                }
                return makeBinary(arena, left, b.operator, right, .n_bool);
            },
            else => unreachable,
        }
    }

    fn castError(s: *Sema, expr: *Nir.Expr, target: NormType) *Nir.Expr {
        return s.reportErrorInv(expr, "Cannot cast {f} to {f}", .{ expr.type, target });
    }

    fn compareError(s: *Sema, left: *Nir.Expr, right: *Nir.Expr) *Nir.Expr {
        return s.reportErrorInv(left, "Cannot compare {f} and {f}.", .{ left.type, right.type });
    }

    fn invalidBinaryOp(s: *Sema, left: *Nir.Expr, op: Token, right: *Nir.Expr) *Nir.Expr {
        return s.reportErrorInv(left, "{f} {s} {f} is not a valid operation", .{
            left.type,
            op.lexeme,
            right.type,
        });
    }

    fn unary(s: *Sema, u: *Ast.Unary) *Nir.Expr {
        const arena = s.arena.allocator();
        const expr = s.expression(u.expr);
        switch (u.operator.type) {
            .minus => {
                if (expr.type.isNumeric()) {
                    return makeUnary(arena, expr, u.operator, expr.type);
                }
                return s.reportErrorInv(expr, "Cannot negate {f}", .{expr.type});
            },
            .bang => {
                if (expr.type.isBool()) {
                    return makeUnary(arena, expr, u.operator, expr.type);
                }
                return s.reportErrorInv(expr, "! only supports bools, got {f}", .{expr.type});
            },
            else => unreachable,
        }
    }

    fn cast(s: *Sema, c: *Ast.Cast) *Nir.Expr {
        const arena = s.arena.allocator();

        const expr = s.expression(c.expr);
        if (expr.type == .n_invalid) return s.invalid;

        const target_type: NormType = switch (c.token.type) {
            .kw_float => if (!s.expectType(expr, .n_int, "Cannot cast {f} to float, expected int", .{expr.type}))
                return s.invalid
            else
                .n_float,
            .kw_int => if (!s.expectType(expr, .n_float, "Cannot cast {f} to int, expected float", .{expr.type}))
                return s.invalid
            else
                .n_int,
            else => unreachable,
        };

        return makeCast(arena, expr, c.token, target_type);
    }

    fn grouping(s: *Sema, g: *Ast.Grouping) *Nir.Expr {
        const expr = s.expression(g.expr);
        if (expr.type == .n_invalid) return s.invalid;
        return makeGrouping(s.arena.allocator(), expr, g.paren, expr.type);
    }

    fn literal(s: *Sema, l: *Ast.Literal) *Nir.Expr {
        const ty: NormType = switch (l.value) {
            .float => .n_float,
            .integer => .n_int,
            .boolean => .n_bool,
            .string => .n_string,
            .nil => @panic("todo"),
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

    fn expectTypesEx(
        s: *Sema,
        left: *Nir.Expr,
        right: *Nir.Expr,
        comptime types: []const NormType,
        comptime error_msg: []const u8,
        args: anytype,
    ) bool {
        if (checkTypes(left, types) and checkTypes(right, types)) return true;
        s.reportError(left, error_msg, args);
        return false;
    }

    fn checkTypes(expr: *Nir.Expr, comptime types: []const NormType) bool {
        for (types) |ty|
            if (expr.type == ty) return true;
        return false;
    }

    fn diag(s: *Sema, expr: *Nir.Expr, comptime error_msg: []const u8, args: anytype) Nir.Diagnostics {
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

    fn reportErrorInv(s: *Sema, expr: *Nir.Expr, comptime error_msg: []const u8, args: anytype) *Nir.Expr {
        s.reportError(expr, error_msg, args);
        return s.invalid;
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
    errdefer nir.arena.deinit();
    if (nir.errors.len == 0) {
        std.debug.print("sema passed with output=\"{f}\"\n", .{nir.expr});
        return error.SemaPassed;
    }
    return nir;
}

const testing = std.testing;

test "arithmetic" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{ .source = "2 + 2", .expected = "(2 + 2):int" },
        .{ .source = "2 / 2", .expected = "(float(2) / float(2)):float" },
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

test "string concat" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{ .source = "\"Hello, \" + \"World\"", .expected = "(\"Hello, \" + \"World\"):string" },
    };

    for (tests) |t| {
        const actual = try testAnalyze(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}

test "string concat failure" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        error_msg: []const u8,
    } = &.{
        .{ .source = "\"h\" + true", .error_msg = "string + bool is not a valid operation" },
        .{ .source = "2.0 + \"h\" ", .error_msg = "float + string is not a valid operation" },
        .{ .source = "3 + \"h\" ", .error_msg = "int + string is not a valid operation" },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"\n", .{t.source});

        const nir = try testAnalyzeFailure(gpa, t.source);
        defer nir.arena.deinit();

        try testing.expect(nir.errors.len == 1);
        try testing.expectEqualStrings(t.error_msg, nir.errors[0].error_msg);
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
        .{ .source = "\"Hey\" == \"Hey\"", .expected = "(\"Hey\" == \"Hey\"):bool" },
        .{ .source = "\"Hey\" != \"Hey\"", .expected = "(\"Hey\" != \"Hey\"):bool" },
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
        .{ .source = "\"Hey\" == 2", .error_msg = "Cannot compare string and int." },
        .{ .source = "2 == \"Hey\"", .error_msg = "Cannot compare int and string." },
        .{ .source = "\"Hey\" != 2", .error_msg = "Cannot compare string and int." },
        .{ .source = "2 != \"Hey\"", .error_msg = "Cannot compare int and string." },
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
        .{ .source = "!1", .error_msg = "! only supports bools, got int" },
        .{ .source = "!1.0", .error_msg = "! only supports bools, got float" },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"\n", .{t.source});

        const nir = try testAnalyzeFailure(gpa, t.source);
        defer nir.arena.deinit();

        try testing.expect(nir.errors.len == 1);
        try testing.expectEqualStrings(t.error_msg, nir.errors[0].error_msg);
    }
}

test "casting" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{ .source = "float(2)", .expected = "float(2)" },
        .{ .source = "int(2.0)", .expected = "int(2.000)" },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"", .{t.source});

        const actual = try testAnalyze(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}

test "auto casting" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{ .source = "2 + 3.0", .expected = "(float(2) + 3.000):float" },
        .{ .source = "3.0 * 4", .expected = "(3.000 * float(4)):float" },
        .{ .source = "2 - 3.0", .expected = "(float(2) - 3.000):float" },
        .{ .source = "3.0 - 4", .expected = "(3.000 - float(4)):float" },
        .{ .source = "2 / 3.0", .expected = "(float(2) / 3.000):float" },
        .{ .source = "3.0 / 4", .expected = "(3.000 / float(4)):float" },
        .{ .source = "2 == 3.0", .expected = "(float(2) == 3.000):bool" },
        .{ .source = "3.0 == 4", .expected = "(3.000 == float(4)):bool" },
        .{ .source = "2 != 3.0", .expected = "(float(2) != 3.000):bool" },
        .{ .source = "3.0 != 4", .expected = "(3.000 != float(4)):bool" },
        .{ .source = "2 > 3.0", .expected = "(float(2) > 3.000):bool" },
        .{ .source = "3.0 > 4", .expected = "(3.000 > float(4)):bool" },
        .{ .source = "2 >= 3.0", .expected = "(float(2) >= 3.000):bool" },
        .{ .source = "3.0 >= 4", .expected = "(3.000 >= float(4)):bool" },
        .{ .source = "2 < 3.0", .expected = "(float(2) < 3.000):bool" },
        .{ .source = "3.0 < 4", .expected = "(3.000 < float(4)):bool" },
        .{ .source = "2 <= 3.0", .expected = "(float(2) <= 3.000):bool" },
        .{ .source = "3.0 <= 4", .expected = "(3.000 <= float(4)):bool" },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"", .{t.source});

        const actual = try testAnalyze(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}
