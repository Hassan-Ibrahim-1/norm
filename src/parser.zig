const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const Io = std.Io;
const ers = @import("errors.zig");
const testing = std.testing;

const debug = @import("debug.zig");
const Lexer = @import("Lexer.zig");
const Token = Lexer.Token;

pub const Ast = struct {
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

    pub const Expr = union(enum) {
        binary: Binary,
        unary: Unary,
        grouping: Grouping,
        literal: Literal,

        pub fn format(expr: *const Expr, w: *Io.Writer) Io.Writer.Error!void {
            // _ = debug.dbgw(w, "", expr);
            try switch (expr.*) {
                .binary => |b| w.print("{f}", .{b}),
                .unary => |b| w.print("{f}", .{b}),
                .grouping => |b| w.print("{f}", .{b}),
                .literal => |b| w.print("{f}", .{b}),
            };
        }
    };

    arena: std.heap.ArenaAllocator,
    expr: *Expr,
    errors: []Diagnostics,
};

pub const Diagnostics = struct {
    line: u32,
    hints: []const []const u8 = &.{},
    notes: []const []const u8 = &.{},
    error_msg: []const u8,
};

const Parser = struct {
    arena: std.heap.ArenaAllocator,
    lexer: *Lexer,
    current: Token,
    previous: Token,
    // TODO: reset on statement boundary
    panic_mode: bool,
    errors: std.ArrayList(Diagnostics),

    const Precedence = enum {
        lowest,
        _or, // or
        _and, // and
        equality, // == !=
        comparison, // < > <= >=
        term, // + -
        factor, // * /
        unary, // ! -
        call, // . ()

        fn int(p: Precedence) u32 {
            return @intFromEnum(p);
        }

        fn next(p: Precedence) Precedence {
            return @enumFromInt(p.int() + 1);
        }
    };

    const ParseRule = struct {
        prefix: ?*const fn (p: *Parser) *Ast.Expr,
        infix: ?*const fn (p: *Parser, left: *Ast.Expr) *Ast.Expr,
        precedence: Precedence,
    };

    const parse_rules = [_]ParseRule{
        .{ .prefix = grouping, .infix = null, .precedence = .lowest }, // left_paren
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // right_paren
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // left_brace
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // right_brace
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // comma
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // dot
        .{ .prefix = unary, .infix = binary, .precedence = .term }, // minus
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // minus_equal
        .{ .prefix = null, .infix = binary, .precedence = .term }, // plus
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // plus_equal
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // semicolon
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // colon
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // colon_equal
        .{ .prefix = null, .infix = binary, .precedence = .factor }, // slash
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // slash_equal
        .{ .prefix = null, .infix = binary, .precedence = .factor }, // star
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // star_equal
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // question
        .{ .prefix = unary, .infix = null, .precedence = .unary }, // bang
        .{ .prefix = null, .infix = binary, .precedence = .comparison }, // bang_equal
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // equal
        .{ .prefix = null, .infix = binary, .precedence = .comparison }, // equal_equal
        .{ .prefix = null, .infix = binary, .precedence = .comparison }, // greater
        .{ .prefix = null, .infix = binary, .precedence = .comparison }, // greater_equal
        .{ .prefix = null, .infix = binary, .precedence = .comparison }, // less
        .{ .prefix = null, .infix = binary, .precedence = .comparison }, // less_equal
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // equal_greater
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // identifier
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // string
        .{ .prefix = float, .infix = null, .precedence = .lowest }, // float
        .{ .prefix = int, .infix = null, .precedence = .lowest }, // int
        .{ .prefix = null, .infix = binary, .precedence = ._and }, // _and
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // _struct
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // _else
        .{ .prefix = boolean, .infix = null, .precedence = .lowest }, // _false
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // _for
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // _fn
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // _if
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // _try
        .{ .prefix = nil, .infix = null, .precedence = .lowest }, // nil
        .{ .prefix = null, .infix = binary, .precedence = ._or }, // _or
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // _return
        .{ .prefix = boolean, .infix = null, .precedence = .lowest }, // _true
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // mut
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // import
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // _switch
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // _enum
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // range
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // _error
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // eof,
    };

    fn getRule(tt: Token.Type) *const ParseRule {
        return &parse_rules[@intFromEnum(tt)];
    }

    pub fn init(gpa: Allocator, l: *Lexer) Parser {
        var parser: Parser = .{
            .arena = .init(gpa),
            .lexer = l,
            .current = undefined,
            .previous = undefined,
            .panic_mode = false,
            .errors = .empty,
        };
        parser.next();
        parser.next();
        return parser;
    }

    fn expression(p: *Parser, precedence: Precedence) *Ast.Expr {
        const rule = getRule(p.previous.type);
        const prefix = rule.prefix orelse {
            p.reportError(.{
                .error_msg = "expected expression.",
                .line = p.previous.line,
            });
            return undefined;
        };
        var expr = prefix(p);

        while (!p.check(.semicolon) and precedence.int() < p.peekPrecedence().int()) {
            const infix_rule = getRule(p.current.type);
            const infix = infix_rule.infix orelse return expr;
            p.next();
            expr = infix(p, expr);
        }
        return expr;
    }

    fn peekPrecedence(p: *Parser) Precedence {
        return getRule(p.current.type).precedence;
    }

    fn check(p: *Parser, ty: Token.Type) bool {
        return p.current.type == ty;
    }

    fn grouping(p: *Parser) *Ast.Expr {
        const paren = p.previous;
        p.next();
        const expr = p.expression(.lowest);
        p.consume(.right_paren, "Expect ')' after expression.");
        return makeGrouping(p.arena.allocator(), expr, paren);
    }

    fn unary(p: *Parser) *Ast.Expr {
        const operator = p.previous;
        p.next();
        const expr = p.expression(.unary);
        return makeUnary(p.arena.allocator(), expr, operator);
    }

    fn float(p: *Parser) *Ast.Expr {
        const value = std.fmt.parseFloat(f32, p.previous.lexeme) catch unreachable;
        return makeLiteral(p.arena.allocator(), .{ .float = value }, p.previous);
    }

    fn int(p: *Parser) *Ast.Expr {
        // think more on this since you can negate integers in the language,
        // what happens if you negate the largest integer?
        // same goes for the `float` function
        const value = std.fmt.parseInt(i32, p.previous.lexeme, 10) catch unreachable;
        return makeLiteral(p.arena.allocator(), .{ .integer = value }, p.previous);
    }

    fn boolean(p: *Parser) *Ast.Expr {
        const value = switch (p.previous.type) {
            .kw_true => true,
            .kw_false => false,
            else => unreachable,
        };
        return makeLiteral(p.arena.allocator(), .{ .boolean = value }, p.previous);
    }

    fn nil(p: *Parser) *Ast.Expr {
        return makeLiteral(p.arena.allocator(), .nil, p.previous);
    }

    fn binary(p: *Parser, left: *Ast.Expr) *Ast.Expr {
        const operator = p.previous;
        const prec = getRule(operator.type).precedence;
        p.next();
        const right = p.expression(prec);
        return makeBinary(p.arena.allocator(), left, operator, right);
    }

    fn next(p: *Parser) void {
        p.previous = p.current;

        while (true) {
            p.current = p.lexer.scanToken();
            if (p.current.type != ._error) break;
            p.reportError(.{
                .error_msg = p.current.lexeme,
                .line = p.current.line,
            });
        }
    }

    fn consume(p: *Parser, expected: Token.Type, msg: []const u8) void {
        if (p.current.type != expected) {
            p.reportError(.{
                .error_msg = msg,
                .line = p.current.line,
            });
            return;
        }
        p.next();
    }

    fn reportError(p: *Parser, diag: Diagnostics) void {
        if (p.panic_mode) return;
        p.errors.append(p.arena.allocator(), diag) catch unreachable;
        p.panic_mode = true;
    }
};

/// `arena.deinit` must be called even if expr is null
pub fn parse(gpa: Allocator, l: *Lexer) Ast {
    var p = Parser.init(gpa, l);
    const expr = p.expression(.lowest);
    const errors = p.errors.toOwnedSlice(p.arena.allocator()) catch unreachable;
    return .{
        .arena = p.arena,
        .errors = errors,
        .expr = expr,
    };
}

fn makeGrouping(arena: Allocator, grping: *Ast.Expr, paren: Token) *Ast.Expr {
    const e = makeExpr(arena);
    e.* = .{ .grouping = .{ .paren = paren, .expr = grping } };
    return e;
}

fn makeUnary(arena: Allocator, expr: *Ast.Expr, operator: Token) *Ast.Expr {
    const e = makeExpr(arena);
    e.* = .{ .unary = .{ .expr = expr, .operator = operator } };
    return e;
}

fn makeBinary(arena: Allocator, left: *Ast.Expr, operator: Token, right: *Ast.Expr) *Ast.Expr {
    const e = makeExpr(arena);
    e.* = .{ .binary = .{ .left = left, .operator = operator, .right = right } };
    return e;
}

fn makeLiteral(arena: Allocator, value: Ast.Literal.Value, token: Token) *Ast.Expr {
    const e = makeExpr(arena);
    e.* = .{ .literal = .{ .token = token, .value = value } };
    return e;
}

fn makeExpr(arena: Allocator) *Ast.Expr {
    return arena.create(Ast.Expr) catch unreachable;
}

fn testParse(gpa: Allocator, source: []const u8) ![]const u8 {
    var l = Lexer.init(source);
    var ast = parse(gpa, &l);
    defer ast.arena.deinit();
    if (ast.errors.len > 0) {
        _ = debug.dbg("ast.errors", ast.errors);
        return error.ParserError;
    }

    const expr = ast.expr;
    var aw = Io.Writer.Allocating.init(gpa);
    try expr.format(&aw.writer);

    return aw.toOwnedSlice();
}

test "arithmetic expressions" {
    const gpa = std.testing.allocator;

    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{
            .source = "2",
            .expected = "2",
        },

        .{
            .source = "-67",
            .expected = "(-67)",
        },
        .{
            .source = "-1+2",
            .expected = "((-1) + 2)",
        },
        .{
            .source = "-(1+2)",
            .expected = "(-(1 + 2))",
        },
        .{
            .source = "(1+2) * 3",
            .expected = "((1 + 2) * 3)",
        },
        .{
            .source = "(1+2) * -3",
            .expected = "((1 + 2) * (-3))",
        },
        .{
            .source = "(1+2) / -3",
            .expected = "((1 + 2) / (-3))",
        },
        .{
            .source = "1+2+3+4",
            .expected = "(((1 + 2) + 3) + 4)",
        },
        .{
            .source = "1*2*3*4",
            .expected = "(((1 * 2) * 3) * 4)",
        },
        .{
            .source = "1+2*3",
            .expected = "(1 + (2 * 3))",
        },
        .{
            .source = "1*2+3",
            .expected = "((1 * 2) + 3)",
        },
        .{
            .source = "1-2-3",
            .expected = "((1 - 2) - 3)",
        },
        .{
            .source = "1-2+3",
            .expected = "((1 - 2) + 3)",
        },
        .{
            .source = "1/2/3",
            .expected = "((1 / 2) / 3)",
        },
        .{
            .source = "1/2*3",
            .expected = "((1 / 2) * 3)",
        },
        .{
            .source = "2*3+4*5",
            .expected = "((2 * 3) + (4 * 5))",
        },
        .{
            .source = "2+3*4+5",
            .expected = "((2 + (3 * 4)) + 5)",
        },
        .{
            .source = "-1*-2",
            .expected = "((-1) * (-2))",
        },
        .{
            .source = "--1",
            .expected = "(-(-1))",
        },
        .{
            .source = "1+2*3-4/5",
            .expected = "((1 + (2 * 3)) - (4 / 5))",
        },
        .{
            .source = "(1+2)*(3+4)",
            .expected = "((1 + 2) * (3 + 4))",
        },
        .{
            .source = "((1+2))",
            .expected = "(1 + 2)",
        },
        .{
            .source = "1.5+2.5",
            .expected = "(1.500 + 2.500)",
        },
        .{
            .source = "2.5*3.0",
            .expected = "(2.500 * 3.000)",
        },
    };

    for (tests) |t| {
        const parsed = try testParse(gpa, t.source);
        defer gpa.free(parsed);
        try testing.expectEqualStrings(t.expected, parsed);
    }
}

test "literals" {
    const gpa = std.testing.allocator;

    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{ .source = "1", .expected = "1" },
        .{ .source = "2.0", .expected = "2.000" },
        .{ .source = "true", .expected = "true" },
        .{ .source = "false", .expected = "false" },
        .{ .source = "nil", .expected = "nil" },
    };

    for (tests) |t| {
        const parsed = try testParse(gpa, t.source);
        defer gpa.free(parsed);
        try testing.expectEqualStrings(t.expected, parsed);
    }
}

test "comparison" {
    const gpa = std.testing.allocator;

    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{ .source = "1 > 2", .expected = "(1 > 2)" },
        .{ .source = "1 != 2", .expected = "(1 != 2)" },
        // TODO:
    };

    for (tests) |t| {
        const parsed = try testParse(gpa, t.source);
        defer gpa.free(parsed);
        try testing.expectEqualStrings(t.expected, parsed);
    }
}

test "logical" {
    const gpa = std.testing.allocator;

    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{ .source = "true or false", .expected = "(true or false)" },
        .{ .source = "true and true", .expected = "(true and true)" },
        .{ .source = "!false", .expected = "(!false)" },
        .{ .source = "!true", .expected = "(!true)" },
        // TODO:
    };

    for (tests) |t| {
        const parsed = try testParse(gpa, t.source);
        defer gpa.free(parsed);
        try testing.expectEqualStrings(t.expected, parsed);
    }
}
