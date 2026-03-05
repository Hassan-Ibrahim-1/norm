const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const Io = std.Io;

const ers = @import("errors.zig");
const Lexer = @import("Lexer.zig");
const Token = Lexer.Token;

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

pub const Stmt = union(enum) {
    pub const Expression = struct {
        expr: *Expr,
    };

    pub const VarDecl = struct {
        ident: Token,
        // At least one of the below in non-null
        type_expr: ?*Expr,
        value: ?*Expr,
    };

    pub const VarAssign = struct {
        ident: Token,
        expr: *Expr,
    };

    expression: Expression,
    var_decl: VarDecl,
    var_assign: VarAssign,
};

pub const Expr = union(enum) {
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
        token: Token,
        expr: *Expr,

        pub fn format(expr: *const Cast, w: *Io.Writer) Io.Writer.Error!void {
            const target = switch (expr.token.type) {
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

    binary: Binary,
    unary: Unary,
    cast: Cast,
    grouping: Grouping,
    literal: Literal,

    pub fn format(expr: *const Expr, w: *Io.Writer) Io.Writer.Error!void {
        try switch (expr.*) {
            inline else => |b| w.print("{f}", .{b}),
        };
    }
};

arena: std.heap.ArenaAllocator,
stmts: []Stmt,
errors: []Diagnostics,
