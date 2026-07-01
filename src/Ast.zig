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
    pub const invalid: Stmt = undefined;

    pub const Expression = struct {
        expr: *Expr,

        pub fn format(e: *const Stmt.Expression, w: *Io.Writer) Io.Writer.Error!void {
            try w.print("{f};", .{e.expr});
        }
    };

    pub const VarDecl = struct {
        ident: Token,
        // At least one of the below in non-null
        type_expr: ?*Expr,
        value: ?*Expr,
        mutable: bool,

        pub fn format(vd: *const Stmt.VarDecl, w: *Io.Writer) Io.Writer.Error!void {
            const mut = if (vd.mutable) "mut " else "";
            if (vd.type_expr != null and vd.value != null) {
                try w.print("{s}{s}: {f} = {f};", .{ mut, vd.ident.lexeme, vd.type_expr.?, vd.value.? });
            } else if (vd.type_expr != null) {
                try w.print("{s}{s}: {f};", .{ mut, vd.ident.lexeme, vd.type_expr.? });
            } else {
                try w.print("{s}{s} := {f};", .{ mut, vd.ident.lexeme, vd.value.? });
            }
        }
    };

    pub const VarAssign = struct {
        ident: Token,
        value: *Expr,

        pub fn format(va: *const Stmt.VarAssign, w: *Io.Writer) Io.Writer.Error!void {
            try w.print("{s} = {f};", .{ va.ident.lexeme, va.value });
        }
    };

    pub const Block = struct {
        token: Token, // {
        end_token: Token, // }
        stmts: []Stmt,

        pub fn format(b: *const Stmt.Block, w: *Io.Writer) Io.Writer.Error!void {
            try w.writeAll("{\n");
            for (b.stmts) |stmt| {
                try w.print("    {f}\n", .{stmt});
            }
            try w.writeAll("}");
        }
    };

    pub const If = struct {
        pub const ElseIf = struct {
            token: Token, // if
            condition: *Expr,
            then_block: Block,
        };

        token: Token, // if
        condition: *Expr,
        then_block: Block,
        else_if_blocks: []ElseIf,
        else_block: ?Block,

        pub fn format(i: *const If, w: *Io.Writer) Io.Writer.Error!void {
            try w.print("if {f} {f}", .{ i.condition, i.then_block });
            for (i.else_if_blocks) |else_if| {
                try w.print(" else if {f} {f}", .{ else_if.condition, else_if.then_block });
            }
            if (i.else_block) |else_block| {
                try w.print(" else {f}", .{else_block});
            }
        }
    };

    pub const Break = struct {
        token: Token,

        pub fn format(_: *const Break, w: *Io.Writer) Io.Writer.Error!void {
            try w.writeAll("break;");
        }
    };

    pub const Continue = struct {
        token: Token,

        pub fn format(_: *const Continue, w: *Io.Writer) Io.Writer.Error!void {
            try w.writeAll("continue;");
        }
    };

    pub const For = struct {
        pub const InitializerStmt = union(enum) {
            var_decl: VarDecl,
            var_assign: VarAssign,
            expr: Expression,

            pub fn format(i: *const InitializerStmt, w: *Io.Writer) Io.Writer.Error!void {
                switch (i.*) {
                    inline else => |s| try s.format(w),
                }
            }
        };

        pub const IncrementStmt = union(enum) {
            var_assign: VarAssign,
            expr: Expression,

            pub fn format(i: *const IncrementStmt, w: *Io.Writer) Io.Writer.Error!void {
                switch (i.*) {
                    inline else => |s| try s.format(w),
                }
            }
        };

        token: Token, // for

        // initializer and increment are guaranteed to not both be null
        initializer: ?InitializerStmt,
        condition: *Expr,
        increment: ?IncrementStmt,

        block: Block,

        pub fn format(f: *const For, w: *Io.Writer) Io.Writer.Error!void {
            try w.print("for ", .{});
            if (f.initializer) |init| {
                try w.print("{f} ", .{init});
            }
            try w.print("{f}; ", .{f.condition});
            if (f.increment) |incr| {
                try w.print("{f} ", .{incr});
            }
            try w.print("{f}", .{f.block});
        }
    };

    pub const ConditionFor = struct {
        token: Token, // for
        condition: *Expr,
        block: Block,

        pub fn format(f: *const ConditionFor, w: *Io.Writer) Io.Writer.Error!void {
            try w.print("for {f} {f}", .{ f.condition, f.block });
        }
    };

    pub const InfiniteFor = struct {
        token: Token, // for
        block: Block,

        pub fn format(f: *const InfiniteFor, w: *Io.Writer) Io.Writer.Error!void {
            try w.print("for {f}", .{f.block});
        }
    };

    pub const Return = struct {
        token: Token, // return
        expr: ?*Expr,

        pub fn format(r: *const Return, w: *Io.Writer) Io.Writer.Error!void {
            if (r.expr) |expr| {
                try w.print("return {f};", .{expr});
            } else {
                try w.writeAll("return;");
            }
        }
    };

    pub const Print = struct {
        token: Token,
        expr: *Expr,

        pub fn format(p: *const Stmt.Print, w: *Io.Writer) Io.Writer.Error!void {
            try w.print("print({f});", .{p.expr});
        }
    };

    expression: Expression,
    var_decl: VarDecl,
    var_assign: VarAssign,
    block: Block,
    if_stmt: If,
    for_stmt: For,
    condition_for: ConditionFor,
    infinite_for: InfiniteFor,
    break_stmt: Break,
    continue_stmt: Continue,
    return_stmt: Return,
    print: Print,

    pub fn token(s: *const Stmt) Token {
        return switch (s.*) {
            .expression => |e| e.expr.token(),
            .var_decl => |vd| vd.ident,
            .var_assign => |va| va.ident,
            .block => |b| b.token,
            .if_stmt => |i| i.token,
            .for_stmt => |f| f.token,
            .condition_for => |f| f.token,
            .infinite_for => |f| f.token,
            .break_stmt => |b| b.token,
            .continue_stmt => |c| c.token,
            .return_stmt => |r| r.token,
            .print => |p| p.token,
        };
    }

    pub fn format(stmt: Stmt, w: *Io.Writer) Io.Writer.Error!void {
        switch (stmt) {
            inline else => |s| try w.print("{f}", .{s}),
        }
    }
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
                    .string => |i| w.print("\"{s}\"", .{i}),
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

    pub const Identifier = struct {
        ident: Token,

        pub fn format(ident: *const Identifier, w: *Io.Writer) Io.Writer.Error!void {
            try w.print("{s}", .{ident.ident.lexeme});
        }
    };

    pub const Function = struct {
        token: Token, // fn
        parameters: []Parameter,
        return_type: ?*Expr,
        body: Stmt.Block,

        pub const Parameter = struct {
            name: Token,
            type: *Expr,
        };

        pub fn format(f: *const Function, w: *Io.Writer) Io.Writer.Error!void {
            try w.print("fn (", .{});
            for (f.parameters, 0..) |param, i| {
                if (i == f.parameters.len - 1) {
                    try w.print("{s}: {f}", .{ param.name.lexeme, param.type });
                } else {
                    try w.print("{s}: {f}, ", .{ param.name.lexeme, param.type });
                }
            }

            try w.print(") ", .{});

            if (f.return_type) |return_type| {
                try w.print("{f} ", .{return_type});
            }

            try w.print("{f}", .{f.body});
        }
    };

    pub const Call = struct {
        token: Token, //left_paren
        callee: *Expr,
        args: []*Expr,

        pub fn format(c: *const Call, w: *Io.Writer) Io.Writer.Error!void {
            try w.print("{f}(", .{c.callee});
            for (c.args, 0..) |arg, i| {
                if (i == c.args.len - 1) {
                    try w.print("{f}", .{arg});
                } else {
                    try w.print("{f}, ", .{arg});
                }
            }
            try w.print(")", .{});
        }
    };

    binary: Binary,
    unary: Unary,
    cast: Cast,
    grouping: Grouping,
    literal: Literal,
    identifier: Identifier,
    function: Function,
    call: Call,

    pub fn token(e: *const Expr) Token {
        return switch (e.*) {
            .binary => |b| b.operator,
            .unary => |b| b.operator,
            .cast => |b| b.token,
            .grouping => |b| b.paren,
            .literal => |b| b.token,
            .identifier => |b| b.ident,
            .function => |b| b.token,
            .call => |b| b.token,
        };
    }

    pub fn format(expr: *const Expr, w: *Io.Writer) Io.Writer.Error!void {
        try switch (expr.*) {
            inline else => |b| w.print("{f}", .{b}),
        };
    }
};

arena: std.heap.ArenaAllocator,
stmts: []Stmt,
errors: []Diagnostics,
