const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;

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
    };

    pub const VarDecl = struct {
        ident: Token,
        // At least one of the below in non-null
        type_expr: ?*Expr,
        value: ?*Expr,
        mutable: bool,
    };

    pub const VarAssign = struct {
        ident: Token,
        value: *Expr,
    };

    pub const Block = struct {
        token: Token, // {
        end_token: Token, // }
        stmts: []Stmt,
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
    };

    pub const Break = struct {
        token: Token,
    };

    pub const Continue = struct {
        token: Token,
    };

    pub const For = struct {
        pub const InitializerStmt = union(enum) {
            var_decl: VarDecl,
            var_assign: VarAssign,
            expr: Expression,
        };

        pub const IncrementStmt = union(enum) {
            var_assign: VarAssign,
            expr: Expression,
        };

        token: Token, // for

        // initializer and increment are guaranteed to not both be null
        initializer: ?InitializerStmt,
        condition: *Expr,
        increment: ?IncrementStmt,

        block: Block,
    };

    pub const ConditionFor = struct {
        token: Token, // for
        condition: *Expr,
        block: Block,
    };

    pub const InfiniteFor = struct {
        token: Token, // for
        block: Block,
    };

    pub const Return = struct {
        token: Token, // return
        expr: ?*Expr,
    };

    pub const Print = struct {
        token: Token,
        expr: *Expr,
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
};

pub const Expr = union(enum) {
    pub const Binary = struct {
        left: *Expr,
        operator: Token,
        right: *Expr,
    };

    pub const Unary = struct {
        operator: Token,
        expr: *Expr,
    };

    pub const Cast = struct {
        token: Token,
        expr: *Expr,
    };

    pub const Literal = struct {
        pub const Value = union(enum) {
            integer: i32,
            float: f64,
            string: []const u8,
            boolean: bool,
            nil: void,
        };

        value: Value,
        token: Token,
    };

    pub const Grouping = struct {
        paren: Token,
        expr: *Expr,
    };

    pub const Identifier = struct {
        ident: Token,
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
    };

    pub const Call = struct {
        token: Token, //left_paren
        callee: *Expr,
        args: []*Expr,
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
};

arena: std.heap.ArenaAllocator,
stmts: []Stmt,
errors: []Diagnostics,
