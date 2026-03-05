//! Norm intermediate representation
//!
//! Typed AST for a file

const std = @import("std");
const mem = std.mem;
const Io = std.Io;

const Allocator = mem.Allocator;
const Ast = @import("Ast.zig");
const debug = @import("debug.zig");
const ers = @import("errors.zig");
const Token = @import("Lexer.zig").Token;

const trait = @import("trait.zig");

pub const Scope = struct {
    level: Level,
    // If `level` is .top this field is garbage
    parent: *Scope,

    pub const Level = enum {
        top,
        local,
    };
};

pub const Symbol = struct {
    name: Token,
    type: NormType,
    scope: *Scope,
};

pub const SymbolTable = struct {
    pub const SymMap = std.StringHashMapUnmanaged(Symbol);

    pub const Locals = struct {
        parent: *Locals,
        locals: SymMap,
    };

    top: SymMap,
    locals: std.ArrayList(Locals),
};

pub const NormType = enum {
    n_int,
    n_float,
    n_bool,
    n_string,

    n_invalid,

    pub fn format(nt: NormType, w: *Io.Writer) Io.Writer.Error!void {
        try w.print("{s}", .{@tagName(nt)[2..]});
    }

    pub fn isNumeric(ty: NormType) bool {
        return ty == .n_float or ty == .n_int;
    }

    pub fn isInt(ty: NormType) bool {
        return ty == .n_int;
    }

    pub fn isFloat(ty: NormType) bool {
        return ty == .n_float;
    }

    pub fn isBool(ty: NormType) bool {
        return ty == .n_bool;
    }

    pub fn isString(ty: NormType) bool {
        return ty == .n_string;
    }

    pub fn isComparable(ty: NormType) bool {
        return switch (ty) {
            .n_int, .n_float, .n_bool, .n_string => true,
            else => false,
        };
    }

    pub fn isOrderable(ty: NormType) bool {
        return switch (ty) {
            .n_int, .n_float => true,
            else => false,
        };
    }

    pub fn int(ty: NormType) trait.SmallestEnumBackingType(NormType) {
        return @intFromEnum(ty);
    }
};

pub const Expr = struct {
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

            pub fn fromAst(ast_val: Ast.Expr.Literal.Value) Value {
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
            try switch (expr.value) {
                .string => |s| w.print("\"{s}\"", .{s}),
                else => w.print("{f}", .{expr.value}),
            };
        }
    };

    pub const Grouping = struct {
        paren: Token,
        expr: *Expr,

        pub fn format(expr: *const Grouping, w: *Io.Writer) Io.Writer.Error!void {
            try w.print("({f})", .{expr.expr});
        }
    };

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
            .cast => |*c| c.token,
            .literal => |*l| l.token,
            .grouping => |*g| g.paren,
        };
    }

    pub fn format(expr: *const Expr, w: *Io.Writer) Io.Writer.Error!void {
        if (expr.type == .n_invalid) return w.print("invalid - an error was not reported", .{});
        try switch (expr.kind) {
            inline .cast, .literal => |l| w.print("{f}", .{l}),
            inline else => |b| w.print("{f}:{f}", .{ b, expr.type }),
        };
    }
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

arena: std.heap.ArenaAllocator,
expr: *Expr,
sym_table: SymbolTable,
errors: []Diagnostics,
