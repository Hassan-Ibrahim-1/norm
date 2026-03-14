const std = @import("std");
const mem = std.mem;
const Io = std.Io;
const Allocator = mem.Allocator;

const Ast = @import("Ast.zig");
const debug = @import("debug.zig");
const ers = @import("errors.zig");
const Token = @import("Lexer.zig").Token;

const Nir = @import("Nir.zig");
const NormType = Nir.NormType;

fn makeIdentifier(arena: Allocator, ident: Token, scope: *Nir.Scope, ty: NormType) *Nir.Expr {
    const e = makeExpr(arena);
    e.* = .{ .kind = .{ .identifier = .{ .ident = ident, .scope = scope } }, .type = ty };
    return e;
}

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

fn makeLiteral(arena: Allocator, value: Nir.Expr.Literal.Value, token: Token, ty: NormType) *Nir.Expr {
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
    arena: Allocator,
    errors: std.ArrayList(Nir.Diagnostics),
    // TODO: reset on statement boundary
    panic_mode: bool,
    invalid_expr: *Nir.Expr,
    sym_table: Nir.SymbolTable,

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

    fn init(gpa: Allocator, arena: Allocator) Sema {
        return .{
            .invalid_expr = makeInvalid(arena),
            .sym_table = .init(gpa),
            .arena = arena,
            .errors = .empty,
            .panic_mode = false,
        };
    }

    fn analyzeTypeExpr(s: *Sema, expr: *Ast.Expr) NormType {
        // TODO: support struct/enum expressions
        if (expr.* != .identifier) {
            return s.expectedTypeExprErr(expr);
        }

        return switch (expr.identifier.ident.type) {
            .kw_int => .n_int,
            .kw_float => .n_float,
            .kw_string => .n_string,
            .kw_bool => .n_bool,

            .identifier => @panic("todo"),
            else => unreachable,
        };
    }

    fn analyzeGlobalSymbols(s: *Sema, stmts: []Ast.Stmt) void {
        for (stmts) |stmt| {
            switch (stmt) {
                .var_decl => |vd| {
                    const var_type =
                        if (vd.type_expr) |type_expr|
                            s.analyzeTypeExpr(type_expr)
                        else
                            .n_invalid;
                    s.sym_table.registerGlobal(vd.ident.lexeme, var_type);
                },
                else => {},
            }
        }
    }

    fn analyze(s: *Sema, stmts: []Ast.Stmt) []Nir.Stmt {
        s.analyzeGlobalSymbols(stmts);

        var nir_stmts: std.ArrayList(Nir.Stmt) = .empty;

        for (stmts) |stmt| {
            const nir_stmt = s.statement(stmt);
            nir_stmts.append(s.arena, nir_stmt) catch oom();
        }

        return nir_stmts.items;
    }

    fn statement(s: *Sema, stmt: Ast.Stmt) Nir.Stmt {
        switch (stmt) {
            .expression => |expr_stmt| {
                const nir_expr = s.expression(expr_stmt.expr);
                return .{ .expression = .{ .expr = nir_expr } };
            },
            .var_decl => |vd| {
                const sym = s.sym_table.find(vd.ident.lexeme);
                const value =
                    if (vd.value) |v|
                        s.expectTypeAutoCast(s.expression(v), sym.type)
                    else
                        @panic("todo: zero values");
                if (value == null) return .invalid;
                if (sym.type == .n_invalid) sym.type = value.?.type;

                return .{ .var_decl = .{ .ident = vd.ident, .value = value.?, .type = sym.type } };
            },
            .print => |p| {
                const nir_expr = s.expression(p.expr);
                return .{ .print = .{ .print = p.print, .expr = nir_expr } };
            },
            .block => |block| {
                var nir_stmts: std.ArrayList(Nir.Stmt) = .empty;
                for (block.stmts) |b_stmt| {
                    const nir_stmt = s.statement(b_stmt);
                    nir_stmts.append(s.arena, nir_stmt) catch oom();
                }
                return .{ .block = .{ .token = block.token, .stmts = nir_stmts.items } };
            },
            .var_assign => @panic("todo"),
        }
    }

    fn expectedTypeExprErr(s: *Sema, expr: *Ast.Expr) NormType {
        s.reportErrorAst(
            expr,
            "Expected type identifier found {f}. TODO: support struct/enum expressions",
            .{expr},
        );
        return .n_invalid;
    }

    fn expression(s: *Sema, expr: *Ast.Expr) *Nir.Expr {
        return switch (expr.*) {
            .binary => |*b| s.binary(b),
            .unary => |*u| s.unary(u),
            .cast => |*c| s.cast(c),
            .grouping => |*g| s.grouping(g),
            .identifier => |*i| s.identifier(i),
            .literal => |*l| s.literal(l),
        };
    }

    fn tryCast(s: *Sema, expr: *Nir.Expr, target: NormType) ?*Nir.Expr {
        if (expr.type == target) return expr;
        const arena = s.arena;
        if (canCast(expr.type, target)) {
            return makeAnonCast(arena, expr, target);
        }
        return null;
    }

    fn expectTypeAutoCast(s: *Sema, expr: *Nir.Expr, target: NormType) ?*Nir.Expr {
        if (expr.type == target or target == .n_invalid) return expr;
        const casted = s.tryCast(expr, target) orelse {
            s.reportError(expr, "Expected {f} got {f}", .{ target, expr.type });
            return null;
        };
        return casted;
    }

    fn binary(s: *Sema, b: *Ast.Expr.Binary) *Nir.Expr {
        const left = s.expression(b.left);
        const right = s.expression(b.right);
        const arena = s.arena;

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
                    const cast_left = s.tryCast(left, .n_float) orelse return s.invalid_expr;
                    const cast_right = s.tryCast(right, .n_float) orelse return s.invalid_expr;
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

    fn unary(s: *Sema, u: *Ast.Expr.Unary) *Nir.Expr {
        const arena = s.arena;
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

    fn cast(s: *Sema, c: *Ast.Expr.Cast) *Nir.Expr {
        const arena = s.arena;

        const expr = s.expression(c.expr);
        if (expr.type == .n_invalid) return s.invalid_expr;

        const target_type: NormType = switch (c.token.type) {
            .kw_float => if (!s.expectType(expr, .n_int, "Cannot cast {f} to float, expected int", .{expr.type}))
                return s.invalid_expr
            else
                .n_float,
            .kw_int => if (!s.expectType(expr, .n_float, "Cannot cast {f} to int, expected float", .{expr.type}))
                return s.invalid_expr
            else
                .n_int,
            else => unreachable,
        };

        return makeCast(arena, expr, c.token, target_type);
    }

    fn grouping(s: *Sema, g: *Ast.Expr.Grouping) *Nir.Expr {
        const expr = s.expression(g.expr);
        if (expr.type == .n_invalid) return s.invalid_expr;
        return makeGrouping(s.arena, expr, g.paren, expr.type);
    }

    fn identifier(s: *Sema, i: *Ast.Expr.Identifier) *Nir.Expr {
        const sym = s.sym_table.tryFind(i.ident.lexeme) orelse return s.undefinedVariable(i.ident);
        return makeIdentifier(s.arena, i.ident, sym.scope, sym.type);
    }

    fn undefinedVariable(s: *Sema, name: Token) *Nir.Expr {
        s.reportErrorLine(name.line, "Undefined variable {s}", .{name.lexeme});
        return s.invalid_expr;
    }

    fn literal(s: *Sema, l: *Ast.Expr.Literal) *Nir.Expr {
        const ty: NormType = switch (l.value) {
            .float => .n_float,
            .integer => .n_int,
            .boolean => .n_bool,
            .string => .n_string,
            .nil => @panic("todo"),
        };
        return makeLiteral(s.arena, .fromAst(l.value), l.token, ty);
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

    fn diag(s: *Sema, line: u32, comptime error_msg: []const u8, args: anytype) Nir.Diagnostics {
        return .{
            .error_msg = std.fmt.allocPrint(s.arena, error_msg, args) catch oom(),
            .line = line,
        };
    }

    fn reportError(s: *Sema, expr: *Nir.Expr, comptime error_msg: []const u8, args: anytype) void {
        if (s.panic_mode) return;
        s.errors.append(s.arena, s.diag(expr.token().line, error_msg, args)) catch oom();
        s.panic_mode = true;
    }

    fn reportErrorAst(s: *Sema, expr: *Ast.Expr, comptime error_msg: []const u8, args: anytype) void {
        if (s.panic_mode) return;
        s.errors.append(s.arena, s.diag(expr.token().line, error_msg, args)) catch oom();
        s.panic_mode = true;
    }

    fn reportErrorLine(s: *Sema, line: u32, comptime error_msg: []const u8, args: anytype) void {
        if (s.panic_mode) return;
        s.errors.append(s.arena, s.diag(line, error_msg, args)) catch oom();
        s.panic_mode = true;
    }

    fn reportErrorInv(s: *Sema, expr: *Nir.Expr, comptime error_msg: []const u8, args: anytype) *Nir.Expr {
        s.reportError(expr, error_msg, args);
        return s.invalid_expr;
    }
};

fn oom() noreturn {
    @panic("oom");
}

pub fn analyze(gpa: Allocator, ast: *Ast) Nir {
    if (ast.errors.len > 0) return undefined;

    var arena: std.heap.ArenaAllocator = .init(gpa);

    var sema = Sema.init(gpa, arena.allocator());

    const stmts = sema.analyze(ast.stmts);

    return .{
        .arena = arena,
        .errors = sema.errors.items,
        .stmts = stmts,
        .sym_table = sema.sym_table,
    };
}

const Lexer = @import("Lexer.zig");
const parser = @import("parser.zig");
const dbg = @import("debug.zig").dbg;

fn testAnalyzeExpr(gpa: Allocator, source: []const u8) ![]const u8 {
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

    var nir = analyze(gpa, &ast);
    defer nir.deinit();
    if (nir.errors.len > 0) {
        debug.reportErrors(nir.errors, "test_runner", source);
        return error.SemaFailed;
    }

    var aw = Io.Writer.Allocating.init(gpa);
    try nir.stmts[0].expression.expr.format(&aw.writer);
    return aw.toOwnedSlice();
}

fn testAnalyzeExprFailure(gpa: Allocator, source: []const u8) !Nir {
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

    var nir = analyze(gpa, &ast);
    errdefer nir.deinit();
    if (nir.errors.len == 0) {
        std.debug.print("expected an error, sema passed with output=\"{f}\"\n", .{nir.stmts[0].expression.expr});
        return error.SemaPassed;
    }
    return nir;
}

fn testAnalyze(gpa: Allocator, source: []const u8) ![]const u8 {
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

    var nir = analyze(gpa, &ast);
    defer nir.deinit();
    if (nir.errors.len > 0) {
        debug.reportErrors(nir.errors, "test_runner", source);
        return error.SemaFailed;
    }

    return debug.printStmts(gpa, nir.stmts);
}

fn testAnalyzeFailure(gpa: Allocator, source: []const u8) !Nir {
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

    var nir = analyze(gpa, &ast);
    errdefer nir.deinit();
    if (nir.errors.len == 0) {
        const stmts = debug.printStmts(gpa, nir.stmts);
        defer gpa.free(stmts);
        std.debug.print("expected an error, sema passed with output=\"{s}\"\n", .{stmts});
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
        const actual = try testAnalyzeExpr(gpa, t.source);
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

        var nir = try testAnalyzeExprFailure(gpa, t.source);
        defer nir.deinit();

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
        .{
            .source = "\"Hello, \" + \"World\"",
            .expected = "(\"Hello, \" + \"World\"):string",
        },
    };

    for (tests) |t| {
        const actual = try testAnalyzeExpr(gpa, t.source);
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

        var nir = try testAnalyzeExprFailure(gpa, t.source);
        defer nir.deinit();

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

        const actual = try testAnalyzeExpr(gpa, t.source);
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

        var nir = try testAnalyzeExprFailure(gpa, t.source);
        defer nir.deinit();

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

        const actual = try testAnalyzeExpr(gpa, t.source);
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

        var nir = try testAnalyzeExprFailure(gpa, t.source);
        defer nir.deinit();

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

        const actual = try testAnalyzeExpr(gpa, t.source);
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

        const actual = try testAnalyzeExpr(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}

test "full variable declarations - simple values" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{
            .source = "x: int = 10;",
            .expected = "x: int = 10;",
        },
        .{
            .source = "x: float = 10.0;",
            .expected = "x: float = 10.000;",
        },
        .{
            .source = "x: bool = true;",
            .expected = "x: bool = true;",
        },
        .{
            .source = "x: string = \"hey\";",
            .expected = "x: string = \"hey\";",
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"", .{t.source});

        const actual = try testAnalyze(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}

test "full variable declarations - complex values" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{
            .source = "x: int = 10 + 2;",
            .expected = "x: int = (10 + 2):int;",
        },
        .{
            .source = "x: float = 10;",
            .expected = "x: float = float(10);",
        },
        .{
            .source = "x: int = 10 + 2.0;",
            .expected = "x: int = int((float(10) + 2.000):float);",
        },
        .{
            .source = "x: bool = 2 > 1;",
            .expected = "x: bool = (2 > 1):bool;",
        },
        .{
            .source = "x: string = \"Hello, \" + \"World\";",
            .expected = "x: string = (\"Hello, \" + \"World\"):string;",
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"", .{t.source});

        const actual = try testAnalyze(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}

test "tracking variables" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{
            .source = "x: int = 10; x;",
            .expected = "x: int = 10;\nx:int;",
        },
        .{
            .source = "x: int = 10; x + 10;",
            .expected = "x: int = 10;\n(x:int + 10):int;",
        },
        .{
            .source = "x: int = 10; y: int = x; y + 1;",
            .expected = "x: int = 10;\ny: int = x:int;\n(y:int + 1):int;",
        },
        .{
            .source = "x: int = 10; y: int = x; z: int = y * x;",
            .expected = "x: int = 10;\ny: int = x:int;\nz: int = (y:int * x:int):int;",
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"", .{t.source});

        const actual = try testAnalyze(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}

test "undefined variable" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        error_msg: []const u8,
    } = &.{
        .{
            .source = "x;",
            .error_msg = "Undefined variable x",
        },
        .{
            .source = "x: int = 10; x; y",
            .error_msg = "Undefined variable y",
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"\n", .{t.source});

        var nir = try testAnalyzeFailure(gpa, t.source);
        defer nir.deinit();

        try testing.expect(nir.errors.len == 1);
        try testing.expectEqualStrings(t.error_msg, nir.errors[0].error_msg);
    }
}

test "variable declaration - type inference" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{
            .source = "x := 10;",
            .expected = "x: int = 10;",
        },
        .{
            .source = "x := 10.0;",
            .expected = "x: float = 10.000;",
        },
        .{
            .source = "x := true;",
            .expected = "x: bool = true;",
        },
        .{
            .source = "x := \"hey\";",
            .expected = "x: string = \"hey\";",
        },
        .{
            .source = "x := 10 + 2;",
            .expected = "x: int = (10 + 2):int;",
        },
        .{
            .source = "x := float(10);",
            .expected = "x: float = float(10);",
        },
        .{
            .source = "x := int(10 + 2.0);",
            .expected = "x: int = int((float(10) + 2.000):float);",
        },
        .{
            .source = "x := 2 > 1;",
            .expected = "x: bool = (2 > 1):bool;",
        },
        .{
            .source = "x := \"Hello, \" + \"World\";",
            .expected = "x: string = (\"Hello, \" + \"World\"):string;",
        },
        .{
            .source = "x := 10 + 2; y := x * 3;",
            .expected = "x: int = (10 + 2):int;\ny: int = (x:int * 3):int;",
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"", .{t.source});

        const actual = try testAnalyze(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}

test "block statements" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{
            .source = "{}",
            .expected =
            \\{
            \\}
            ,
        },
        .{
            .source =
            \\{
            \\    2 + 1;
            \\}
            ,
            .expected =
            \\{
            \\    (2 + 1):int;
            \\}
            ,
        },
        .{
            .source =
            \\{
            \\    2 + 1;
            \\    2 * 1;
            \\}
            ,
            .expected =
            \\{
            \\    (2 + 1):int;
            \\    (2 * 1):int;
            \\}
            ,
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"", .{t.source});

        const actual = try testAnalyze(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}

test "temporary print stmt" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{
            .source = "print(10);",
            .expected = "print(10);",
        },
        .{
            .source = "x := 10; print(x);",
            .expected = "x: int = 10;\nprint(x:int);",
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"", .{t.source});

        const actual = try testAnalyze(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}
