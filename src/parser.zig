const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const Allocator = mem.Allocator;
const Io = std.Io;
const testing = std.testing;

const debug = @import("debug.zig");
const dbg = debug.dbg;
const ers = @import("errors.zig");
const Lexer = @import("Lexer.zig");
const Token = Lexer.Token;
const Ast = @import("Ast.zig");

const Parser = struct {
    arena: Allocator,
    scratch: Allocator,
    tokens: []Token,

    current_index: usize,

    previous: Token,
    current: Token,
    next: Token,

    // TODO: reset on statement boundary
    panic_mode: bool,
    errors: std.ArrayList(Ast.Diagnostics),

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
        .{ .prefix = identifier, .infix = null, .precedence = .lowest }, // identifier
        .{ .prefix = string, .infix = null, .precedence = .lowest }, // string
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
        .{ .prefix = cast, .infix = null, .precedence = .lowest }, // kw_int
        .{ .prefix = cast, .infix = null, .precedence = .lowest }, // kw_float
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // kw_bool
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // _error
        .{ .prefix = null, .infix = null, .precedence = .lowest }, // eof,
    };

    fn getRule(tt: Token.Type) *const ParseRule {
        return &parse_rules[@intFromEnum(tt)];
    }

    fn init(arena: Allocator, scratch: Allocator, tokens: []Token) Parser {
        var parser: Parser = .{
            .arena = arena,
            .scratch = scratch,
            .tokens = tokens,
            .current_index = 0,
            .previous = tokens[0],
            .current = tokens[0],
            .next = tokens[0],
            .panic_mode = false,
            .errors = .empty,
        };
        parser.advance();
        return parser;
    }

    fn statement(p: *Parser) Ast.Stmt {
        if (p.check(.identifier)) {
            if (p.checkNextEither(.colon, .colon_equal)) {
                p.advance();
                return p.varDecl();
            }
            // p.reportError(.{ .error_msg = "idk man", .line = p.previous.line });
        }
        return p.expressionStmt();
    }

    fn expressionStmt(p: *Parser) Ast.Stmt {
        const expr = p.expression(.lowest);
        p.consumeSemicolon();

        return .{
            .expression = .{ .expr = expr },
        };
    }

    fn varDecl(p: *Parser) Ast.Stmt {
        const ident = p.previous;

        var type_expr: ?*Ast.Expr = null;
        if (p.match(.colon)) {
            type_expr = p.expression(.lowest);
            if (!p.match(.equal)) {
                p.consumeSemicolon();
                return .{
                    .var_decl = .{
                        .ident = ident,
                        .type_expr = type_expr,
                        .value = null,
                    },
                };
            }
        } else {
            p.consume(.colon_equal, "unreachable");
        }

        const value = p.expression(.lowest);
        p.consumeSemicolon();

        return .{
            .var_decl = .{
                .ident = ident,
                .type_expr = type_expr,
                .value = value,
            },
        };
    }

    fn consumeSemicolon(p: *Parser) void {
        if (builtin.is_test) {
            _ = p.match(.semicolon);
            return;
        }
        p.consume(.semicolon, "Expect ';' after statement");
    }

    fn expression(p: *Parser, precedence: Precedence) *Ast.Expr {
        p.advance();

        const rule = getRule(p.previous.type);
        const prefix = rule.prefix orelse {
            p.reportError(.{
                .error_msg = "expected expression.",
                .line = p.previous.line,
            });
            // TODO: fix this, should not return undefined.
            // maybe just some error/zero value for expression.
            return undefined;
        };
        var expr = prefix(p);

        while (!p.check(.semicolon) and precedence.int() < p.peekPrecedence().int()) {
            const infix_rule = getRule(p.current.type);
            const infix = infix_rule.infix orelse return expr;
            p.advance();
            expr = infix(p, expr);
        }
        return expr;
    }

    fn peekPrecedence(p: *Parser) Precedence {
        return getRule(p.current.type).precedence;
    }

    fn grouping(p: *Parser) *Ast.Expr {
        const paren = p.previous;
        const expr = p.expression(.lowest);
        p.consume(.right_paren, "Expect ')' after expression.");
        return makeGrouping(p.arena, expr, paren);
    }

    fn unary(p: *Parser) *Ast.Expr {
        const operator = p.previous;
        const expr = p.expression(.unary);
        return makeUnary(p.arena, expr, operator);
    }

    fn float(p: *Parser) *Ast.Expr {
        const value = std.fmt.parseFloat(f32, p.previous.lexeme) catch unreachable;
        return makeLiteral(p.arena, .{ .float = value }, p.previous);
    }

    fn int(p: *Parser) *Ast.Expr {
        // think more on this since you can negate integers in the language,
        // what happens if you negate the largest integer?
        // same goes for the `float` function
        const value = std.fmt.parseInt(i32, p.previous.lexeme, 10) catch unreachable;
        return makeLiteral(p.arena, .{ .integer = value }, p.previous);
    }

    fn cast(p: *Parser) *Ast.Expr {
        const cast_target = p.previous;
        p.consume(.left_paren, "Expect '(' before cast expression.");
        const expr = p.expression(.lowest);
        p.consume(.right_paren, "Expect ')' after cast expression.");

        return makeCast(p.arena, expr, cast_target);
    }

    fn boolean(p: *Parser) *Ast.Expr {
        const value = switch (p.previous.type) {
            .kw_true => true,
            .kw_false => false,
            else => unreachable,
        };
        return makeLiteral(p.arena, .{ .boolean = value }, p.previous);
    }

    fn nil(p: *Parser) *Ast.Expr {
        return makeLiteral(p.arena, .nil, p.previous);
    }

    fn string(p: *Parser) *Ast.Expr {
        // remove the quotes around the string
        const strval = p.previous.lexeme[1 .. p.previous.lexeme.len - 1];
        return makeLiteral(p.arena, .{ .string = strval }, p.previous);
    }

    fn binary(p: *Parser, left: *Ast.Expr) *Ast.Expr {
        const operator = p.previous;
        const prec = getRule(operator.type).precedence;
        const right = p.expression(prec);
        return makeBinary(p.arena, left, operator, right);
    }

    fn identifier(p: *Parser) *Ast.Expr {
        return makeIdentifier(p.arena, p.previous);
    }

    fn advance(p: *Parser) void {
        if (p.current_index < p.tokens.len - 1) p.current_index += 1;

        p.previous = p.current;
        p.current = p.next;
        p.next = p.tokens[p.current_index];
    }

    fn consume(p: *Parser, expected: Token.Type, msg: []const u8) void {
        if (p.match(expected)) return;

        p.reportError(.{
            .error_msg = msg,
            .line = p.current.line,
        });
    }

    fn check(p: *Parser, ty: Token.Type) bool {
        return p.current.type == ty;
    }

    fn checkEither(p: *Parser, a: Token.Type, b: Token.Type) bool {
        return p.current.type == a or p.current.type == b;
    }

    fn checkNext(p: *Parser, ty: Token.Type) bool {
        return p.next.type == ty;
    }

    fn checkNextEither(p: *Parser, a: Token.Type, b: Token.Type) bool {
        return p.next.type == a or p.next.type == b;
    }

    fn match(p: *Parser, ty: Token.Type) bool {
        if (!p.check(ty)) return false;
        p.advance();
        return true;
    }

    fn matchEither(p: *Parser, a: Token.Type, b: Token.Type) bool {
        return p.match(a) or p.match(b);
    }

    fn reportError(p: *Parser, diag: Ast.Diagnostics) void {
        if (p.panic_mode) return;
        p.errors.append(p.arena, diag) catch oom();
        p.panic_mode = true;
    }
};

/// `arena.deinit` must be called even if expr is null
pub fn parse(gpa: Allocator, tokens: []Token) Ast {
    var arena: std.heap.ArenaAllocator = .init(gpa);

    var p = Parser.init(arena.allocator(), gpa, tokens);

    var stmts: std.ArrayList(Ast.Stmt) = .empty;

    while (true) {
        const stmt = p.statement();
        stmts.append(arena.allocator(), stmt) catch oom();

        // dbg("parser", .{
        //     .previous = p.previous,
        //     .current = p.current,
        //     .next = p.next,
        //     .errors = p.errors.items,
        //     .stmts = stmts,
        // });

        if (p.current.type == .eof) break;
    }

    return .{
        .arena = arena,
        .errors = p.errors.items,
        .stmts = stmts.items,
    };
}

fn makeGrouping(arena: Allocator, grping: *Ast.Expr, paren: Token) *Ast.Expr {
    const e = makeExpr(arena);
    e.* = .{ .grouping = .{ .paren = paren, .expr = grping } };
    return e;
}

fn makeIdentifier(arena: Allocator, ident: Token) *Ast.Expr {
    const e = makeExpr(arena);
    e.* = .{ .identifier = .{ .ident = ident } };
    return e;
}

fn makeUnary(arena: Allocator, expr: *Ast.Expr, operator: Token) *Ast.Expr {
    const e = makeExpr(arena);
    e.* = .{ .unary = .{ .expr = expr, .operator = operator } };
    return e;
}

fn makeCast(arena: Allocator, expr: *Ast.Expr, token: Token) *Ast.Expr {
    const e = makeExpr(arena);
    e.* = .{ .cast = .{ .expr = expr, .token = token } };
    return e;
}

fn makeBinary(arena: Allocator, left: *Ast.Expr, operator: Token, right: *Ast.Expr) *Ast.Expr {
    const e = makeExpr(arena);
    e.* = .{ .binary = .{ .left = left, .operator = operator, .right = right } };
    return e;
}

fn makeLiteral(arena: Allocator, value: Ast.Expr.Literal.Value, token: Token) *Ast.Expr {
    const e = makeExpr(arena);
    e.* = .{ .literal = .{ .token = token, .value = value } };
    return e;
}

fn makeExpr(arena: Allocator) *Ast.Expr {
    return arena.create(Ast.Expr) catch oom();
}

fn oom() noreturn {
    @panic("oom");
}

fn testParse(gpa: Allocator, source: []const u8) ![]const u8 {
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

    var ast = parse(gpa, tokens.tokens);
    defer ast.arena.deinit();
    if (ast.errors.len > 0) {
        dbg("ast.errors", ast.errors);
        return error.ParserError;
    }

    if (ast.stmts.len > 1) {
        dbg("ast.stmts", ast.stmts);
        return error.TooManyStatements;
    }

    const expr = ast.stmts[0].expression.expr;
    var aw = Io.Writer.Allocating.init(gpa);
    try expr.format(&aw.writer);

    return aw.toOwnedSlice();
}

fn testParseStmts(gpa: Allocator, source: []const u8) ![]const u8 {
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

    var ast = parse(gpa, tokens.tokens);
    defer ast.arena.deinit();
    if (ast.errors.len > 0) {
        dbg("ast.errors", ast.errors);
        return error.ParserError;
    }

    return debug.printAstStmts(gpa, ast.stmts);
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
        errdefer dbg("test case", t);

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
        .{ .source = "\"hey\"", .expected = "hey" },
    };

    for (tests) |t| {
        errdefer dbg("test case", t);

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
        .{ .source = "1 <= 2", .expected = "(1 <= 2)" },
        .{ .source = "true == (2 < 1)", .expected = "(true == (2 < 1))" },
    };

    for (tests) |t| {
        errdefer dbg("test case", t);

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
        .{ .source = "!false or !true", .expected = "((!false) or (!true))" },
        .{ .source = "!false and !true", .expected = "((!false) and (!true))" },
        .{ .source = "2 < 1 and 2 > 1", .expected = "((2 < 1) and (2 > 1))" },
        .{ .source = "true or 2 > 1", .expected = "(true or (2 > 1))" },
    };

    for (tests) |t| {
        errdefer dbg("test case", t);

        const parsed = try testParse(gpa, t.source);
        defer gpa.free(parsed);
        try testing.expectEqualStrings(t.expected, parsed);
    }
}

test "casting" {
    const gpa = testing.allocator;

    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{ .source = "float(2)", .expected = "float(2)" },
        .{ .source = "int(2)", .expected = "int(2)" },
    };

    for (tests) |t| {
        errdefer dbg("test case", t);

        const parsed = try testParse(gpa, t.source);
        defer gpa.free(parsed);
        try testing.expectEqualStrings(t.expected, parsed);
    }
}

test "identifier" {
    const gpa = testing.allocator;

    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{ .source = "x", .expected = "x" },
        .{ .source = "Map", .expected = "Map" },
    };

    for (tests) |t| {
        errdefer dbg("test case", t);

        const parsed = try testParse(gpa, t.source);
        defer gpa.free(parsed);
        try testing.expectEqualStrings(t.expected, parsed);
    }
}

test "expression statement" {
    const gpa = testing.allocator;

    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{
            .source = "2;",
            .expected = "2;",
        },
        .{
            .source = "2;3;",
            .expected = "2;\n3;",
        },
        .{
            .source = "2 + 3 < 4;",
            .expected = "((2 + 3) < 4);",
        },
    };

    for (tests) |t| {
        errdefer dbg("test case", t);

        const parsed = try testParseStmts(gpa, t.source);
        defer gpa.free(parsed);
        try testing.expectEqualStrings(t.expected, parsed);
    }
}

test "variable declaration" {
    const gpa = testing.allocator;

    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{
            .source = "x := 10;",
            .expected = "x := 10;",
        },
        .{
            .source = "x: Number = 10;",
            .expected = "x: Number = 10;",
        },
        .{
            .source = "x: Number;",
            .expected = "x: Number;",
        },
    };

    for (tests) |t| {
        errdefer dbg("test case", t);

        const parsed = try testParseStmts(gpa, t.source);
        defer gpa.free(parsed);
        try testing.expectEqualStrings(t.expected, parsed);
    }
}
