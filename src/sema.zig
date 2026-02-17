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
            try w.print("{f}", .{expr.expr});
        }
    };

    pub const Expr = struct {
        type: NormType,
        kind: union(enum) {
            binary: Binary,
            unary: Unary,
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
                .literal => |*l| l.token,
                .grouping => |*g| g.paren,
            };
        }

        pub fn format(expr: *const Expr, w: *Io.Writer) Io.Writer.Error!void {
            // _ = debug.dbgw(w, "", expr);
            try switch (expr.kind) {
                // TODO: can probably shorten this with comptime
                .binary => |b| w.print("{f}:{t}", .{ b, expr.type }),
                .unary => |b| w.print("{f}:{t}", .{ b, expr.type }),
                .grouping => |b| w.print("{f}:{t}", .{ b, expr.type }),
                .literal => |b| w.print("{f}:{t}", .{ b, expr.type }),
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
                    s.reportError(left, "{t} {s} {t} is not a valid operation", .{
                        left.type,
                        b.operator.lexeme,
                        right.type,
                    });
                    return makeInvalid(arena);
                }
                if (!s.expectTypes(
                    left,
                    NormType.number_types,
                    "{t} {s} {t} is not a valid operation",
                    .{ left.type, b.operator.lexeme, right.type },
                )) {
                    return makeInvalid(arena);
                }

                return makeBinary(arena, left, b.operator, right, .n_int);
            },
            .equal_equal, .bang_equal => {
                if (left.type != right.type) {
                    s.reportError(left, "Cannot compare {t} and {t}.", .{ left.type, right.type });
                    return makeInvalid(arena);
                }
                return makeBinary(arena, left, b.operator, right, .n_bool);
            },
            .greater, .greater_equal, .less, .less_equal => {
                if (left.type != right.type or !checkTypes(left, NormType.number_types)) {
                    s.reportError(left, "Cannot compare {t} and {t}.", .{ left.type, right.type });
                    return makeInvalid(arena);
                }
                return makeBinary(arena, left, b.operator, right, .n_bool);
            },
            .kw_and, .kw_or => {
                if (left.type != right.type or left.type != .n_bool) {
                    s.reportError(left, "Cannot compare {t} and {t}.", .{ left.type, right.type });
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
