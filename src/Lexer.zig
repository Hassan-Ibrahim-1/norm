const std = @import("std");
const Allocator = std.mem.Allocator;

const Lexer = @This();

source: []const u8,
start: usize,
current: usize,
line: u32,

pub const Token = struct {
    pub const Type = enum {
        left_paren,
        right_paren,
        left_brace,
        right_brace,
        comma,
        dot,
        minus,
        minus_equal,
        plus,
        plus_equal,
        semicolon,
        colon,
        colon_equal,
        slash,
        slash_equal,
        star,
        star_equal,
        question,
        bang,
        bang_equal,
        equal,
        equal_equal,
        greater,
        greater_equal,
        less,
        less_equal,
        equal_greater,

        // literals.
        identifier,
        string,
        number_float,
        number_int,

        // keywords.
        kw_and,
        kw_struct,
        kw_else,
        kw_false,
        kw_for,
        kw_fn,
        kw_if,
        kw_try,
        kw_nil,
        kw_or,
        kw_return,
        kw_true,
        kw_mut,
        kw_import,
        kw_switch,
        kw_enum,
        kw_range,

        _error,
        eof,
    };

    lexeme: []const u8,
    line: u32,
    type: Type,

    pub fn format(t: Token, w: *std.Io.Writer) std.Io.Writer.Error!void {
        try w.print(
            ".{{ .lexeme = \"{s}\", .line = {}, .type = {t}}}",
            .{ t.lexeme, t.line, t.type },
        );
    }
};

pub fn init(source: []const u8) Lexer {
    return .{
        .source = source,
        .start = 0,
        .current = 0,
        .line = 1,
    };
}

fn tokenOptionalEqual(l: *Lexer, c: u8) Token {
    const table: []const struct {
        char: u8,
        single: Token.Type,
        equal: Token.Type,
    } = &.{
        .{ .char = '-', .single = .minus, .equal = .minus_equal },
        .{ .char = '+', .single = .plus, .equal = .plus_equal },
        .{ .char = ':', .single = .colon, .equal = .colon_equal },
        .{ .char = '/', .single = .slash, .equal = .slash_equal },
        .{ .char = '*', .single = .star, .equal = .star_equal },
        .{ .char = '!', .single = .bang, .equal = .bang_equal },
        .{ .char = '>', .single = .greater, .equal = .greater_equal },
        .{ .char = '<', .single = .less, .equal = .less_equal },
    };

    for (table) |entry| {
        if (entry.char == c) {
            if (l.match('=')) {
                return l.createToken(entry.equal);
            }
            return l.createToken(entry.single);
        }
    }

    unreachable;
}

pub fn scanToken(l: *Lexer) Token {
    l.skipWhitespace();
    l.start = l.current;

    if (l.isAtEnd()) return l.createToken(.eof);

    const c = l.next();

    switch (c) {
        '(' => return l.createToken(.left_paren),
        ')' => return l.createToken(.right_paren),
        '{' => return l.createToken(.left_brace),
        '}' => return l.createToken(.right_brace),
        ';' => return l.createToken(.semicolon),
        ',' => return l.createToken(.comma),
        '.' => return l.createToken(.dot),

        '-', '+', ':', '/', '*', '!', '>', '<' => return l.tokenOptionalEqual(c),

        '=' => {
            if (l.match('=')) {
                return l.createToken(.equal_equal);
            } else if (l.match('>')) {
                return l.createToken(.equal_greater);
            }
            return l.createToken(.equal);
        },

        '"' => return l.string(),

        else => {
            if (isDigit(c)) {
                return l.number();
            }
            if (isAlpha(c)) {
                return l.identifier();
            }
            return l.errorToken("Unexpected character.");
        },
    }
}

fn scanTokens(l: *Lexer, gpa: Allocator) Allocator.Error![]Token {
    var tokens: std.ArrayList(Token) = .empty;
    errdefer tokens.deinit(gpa);

    while (true) {
        const token = l.scanToken();
        try tokens.append(gpa, token);
        if (token.type == .eof) {
            return tokens.toOwnedSlice(gpa);
        }
    }
}

fn string(l: *Lexer) Token {
    while (!l.isAtEnd() and l.peek() != '"') {
        if (l.peek() == '\n') l.line += 1;
        _ = l.next();
    }

    if (l.isAtEnd()) return l.errorToken("Unterminated string");

    // consume the '"'
    _ = l.next();
    return l.createToken(.string);
}

fn number(l: *Lexer) Token {
    while (!l.isAtEnd() and isDigit(l.peek())) _ = l.next();

    if (!l.isAtEnd() and l.peek() == '.' and isDigit(l.peekNext())) {
        _ = l.next();

        while (!l.isAtEnd() and isDigit(l.peek())) _ = l.next();
        return l.createToken(.number_float);
    }
    return l.createToken(.number_int);
}

fn identifier(l: *Lexer) Token {
    while (!l.isAtEnd() and (isAlpha(l.peek()) or isDigit(l.peek()))) {
        _ = l.next();
    }
    return l.createToken(l.identifierType());
}

fn identifierType(l: *Lexer) Token.Type {
    return switch (l.source[l.start]) {
        'a' => l.checkKeyword(1, "nd", .kw_and),
        'c' => l.checkKeyword(1, "truct", .kw_struct),
        'n' => l.checkKeyword(1, "il", .kw_nil),
        'o' => l.checkKeyword(1, "r", .kw_or),
        'm' => l.checkKeyword(1, "ut", .kw_mut),
        's' => l.checkKeyword(1, "witch", .kw_switch),

        'e' => if (l.current > l.start)
            switch (l.source[l.start + 1]) {
                'l' => l.checkKeyword(2, "se", .kw_else),
                'n' => l.checkKeyword(2, "um", .kw_enum),
                else => .identifier,
            }
        else
            .identifier,

        'r' => if (l.current > l.start)
            switch (l.source[l.start + 1]) {
                'e' => l.checkKeyword(2, "turn", .kw_return),
                'a' => l.checkKeyword(2, "nge", .kw_range),
                else => .identifier,
            }
        else
            .identifier,

        'i' => if (l.current > l.start)
            switch (l.source[l.start + 1]) {
                'f' => .kw_if,
                'm' => l.checkKeyword(2, "port", .kw_import),
                else => .identifier,
            }
        else
            .identifier,

        'f' => if (l.current > l.start)
            switch (l.source[l.start + 1]) {
                'a' => l.checkKeyword(2, "lse", .kw_false),
                'o' => l.checkKeyword(2, "r", .kw_for),
                'n' => .kw_fn,
                else => .identifier,
            }
        else
            .identifier,

        't' => if (l.current > l.start and l.source[l.start + 1] == 'r')
            switch (l.source[l.start + 2]) {
                'u' => l.checkKeyword(3, "e", .kw_true),
                'y' => .kw_try,
                else => .identifier,
            }
        else
            .identifier,

        else => .identifier,
    };
}

fn checkKeyword(
    l: *Lexer,
    start: usize,
    rest: []const u8,
    typ: Token.Type,
) Token.Type {
    const current = l.source[start + l.start .. l.current];
    if (std.mem.eql(u8, rest, current)) {
        return typ;
    }
    return .identifier;
}

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isAlpha(c: u8) bool {
    return (c >= 'a' and c <= 'z') or
        (c >= 'A' and c <= 'Z') or
        c == '_';
}

fn errorToken(l: *Lexer, msg: []const u8) Token {
    return .{
        .type = ._error,
        .lexeme = msg,
        .line = l.line,
    };
}

fn match(l: *Lexer, c: u8) bool {
    if (!l.isAtEnd() and l.peek() == c) {
        _ = l.next();
        return true;
    }
    return false;
}

fn next(l: *Lexer) u8 {
    defer l.current += 1;
    return l.source[l.current];
}

fn skipWhitespace(l: *Lexer) void {
    while (!l.isAtEnd()) {
        switch (l.peek()) {
            ' ', '\t', '\r' => _ = l.next(),
            '\n' => {
                l.line += 1;
                _ = l.next();
            },
            '/' => {
                // ignore comments
                if (l.peekNext() == '/') {
                    while (!l.isAtEnd() and l.peek() != '\n') {
                        _ = l.next();
                    }
                } else return;
            },

            else => return,
        }
    }
}

fn peek(l: *Lexer) u8 {
    return l.source[l.current];
}

fn peekNext(l: *Lexer) u8 {
    return l.source[l.current + 1];
}

fn isAtEnd(l: *Lexer) bool {
    return l.current == l.source.len;
}

fn createToken(l: *Lexer, typ: Token.Type) Token {
    return .{
        .type = typ,
        .line = l.line,
        .lexeme = l.source[l.start..l.current],
    };
}

const testing = std.testing;

fn testTokenLine(lexeme: []const u8, typ: Token.Type, line: u32) Token {
    return Token{
        .lexeme = lexeme,
        .type = typ,
        .line = line,
    };
}

fn testToken(lexeme: []const u8, typ: Token.Type) Token {
    return Token{
        .lexeme = lexeme,
        .type = typ,
        .line = 1,
    };
}

fn expectTokenEqual(expected: Token, actual: Token) !void {
    errdefer std.debug.print(
        "Unequal tokens:\n=====================\nExpected = {f}\n=====================\nGot = {f}\n\n",
        .{ expected, actual },
    );

    if (expected.line != actual.line) return error.TestExpectedEqual;
    if (expected.type != actual.type) return error.TestExpectedEqual;
    if (!std.mem.eql(u8, expected.lexeme, actual.lexeme)) return error.TestExpectedEqual;
}

fn expectTokensEqual(expected: []const Token, actual: []const Token) !void {
    if (expected.len != actual.len) {
        std.debug.print(
            "Unequal lengths, expected={}, got={}\n",
            .{ expected.len, actual.len },
        );
        dumpTokens("expected", expected);
        std.debug.print("========================\n", .{});
        dumpTokens("actual", actual);
        return error.TestExpectedEqual;
    }
    for (expected, actual) |e, a| {
        try expectTokenEqual(e, a);
    }
}

fn dumpTokens(prefix: []const u8, tokens: []const Token) void {
    std.debug.print("{s} = .{{\n", .{prefix});

    for (0.., tokens) |i, t| {
        std.debug.print("    [{}] = {f},\n", .{ i, t });
    }

    std.debug.print("}}\n", .{});
}

fn testScanTokens(gpa: Allocator, source: []const u8) Allocator.Error![]Token {
    var l = Lexer.init(source);
    return l.scanTokens(gpa);
}

test "simple" {
    const tests: []const struct {
        source: []const u8,
        expected: []const Token,
    } = &.{
        .{
            .source = "+ += - -= * *= / /= : := ! != > >= < <= = == =>",
            .expected = &.{
                testToken("+", .plus),
                testToken("+=", .plus_equal),
                testToken("-", .minus),
                testToken("-=", .minus_equal),
                testToken("*", .star),
                testToken("*=", .star_equal),
                testToken("/", .slash),
                testToken("/=", .slash_equal),
                testToken(":", .colon),
                testToken(":=", .colon_equal),
                testToken("!", .bang),
                testToken("!=", .bang_equal),
                testToken(">", .greater),
                testToken(">=", .greater_equal),
                testToken("<", .less),
                testToken("<=", .less_equal),
                testToken("=", .equal),
                testToken("==", .equal_equal),
                testToken("=>", .equal_greater),
                testToken("", .eof),
            },
        },
        .{
            .source = "2.2 + 1.0 + 2",
            .expected = &.{
                testToken("2.2", .number_float),
                testToken("+", .plus),
                testToken("1.0", .number_float),
                testToken("+", .plus),
                testToken("2", .number_int),
                testToken("", .eof),
            },
        },
    };

    const gpa = std.testing.allocator;

    for (tests) |t| {
        const tokens = try testScanTokens(gpa, t.source);
        defer gpa.free(tokens);

        try expectTokensEqual(t.expected, tokens);
    }
}

test "more tokens" {
    const tests: []const struct {
        source: []const u8,
        expected: []const Token,
    } = &.{
        .{
            .source = "2 + 2",
            .expected = &.{
                testToken("2", .number_int),
                testToken("+", .plus),
                testToken("2", .number_int),
                testToken("", .eof),
            },
        },

        // Parentheses and multiplication
        .{
            .source = "(1 * 3)",
            .expected = &.{
                testToken("(", .left_paren),
                testToken("1", .number_int),
                testToken("*", .star),
                testToken("3", .number_int),
                testToken(")", .right_paren),
                testToken("", .eof),
            },
        },

        // Variable declaration
        .{
            .source = "x := 10",
            .expected = &.{
                testToken("x", .identifier),
                testToken(":=", .colon_equal),
                testToken("10", .number_int),
                testToken("", .eof),
            },
        },

        // Comparison
        .{
            .source = "a >= b",
            .expected = &.{
                testToken("a", .identifier),
                testToken(">=", .greater_equal),
                testToken("b", .identifier),
                testToken("", .eof),
            },
        },

        // Logical operators
        .{
            .source = "true and false",
            .expected = &.{
                testToken("true", .kw_true),
                testToken("and", .kw_and),
                testToken("false", .kw_false),
                testToken("", .eof),
            },
        },

        .{
            .source = "for i < 5",
            .expected = &.{
                testToken("for", .kw_for),
                testToken("i", .identifier),
                testToken("<", .less),
                testToken("5", .number_int),
                testToken("", .eof),
            },
        },

        // String literal
        .{
            .source = "\"hello\"",
            .expected = &.{
                testToken("\"hello\"", .string),
                testToken("", .eof),
            },
        },

        // Function call
        .{
            .source = "some_func(x)",
            .expected = &.{
                testToken("some_func", .identifier),
                testToken("(", .left_paren),
                testToken("x", .identifier),
                testToken(")", .right_paren),
                testToken("", .eof),
            },
        },
        .{
            .source = "for i := range 10 { io.println(i); }",
            .expected = &.{
                testToken("for", .kw_for),
                testToken("i", .identifier),
                testToken(":=", .colon_equal),
                testToken("range", .kw_range),
                testToken("10", .number_int),
                testToken("{", .left_brace),
                testToken("io", .identifier),
                testToken(".", .dot),
                testToken("println", .identifier),
                testToken("(", .left_paren),
                testToken("i", .identifier),
                testToken(")", .right_paren),
                testToken(";", .semicolon),
                testToken("}", .right_brace),
                testToken("", .eof),
            },
        },
    };

    const gpa = std.testing.allocator;

    for (tests) |t| {
        errdefer std.debug.print("failed test with source = \"{s}\"\n\n", .{t.source});

        const tokens = try testScanTokens(gpa, t.source);
        defer gpa.free(tokens);

        try expectTokensEqual(t.expected, tokens);
    }
}

test "comments" {
    const tests: []const struct {
        source: []const u8,
        expected: []const Token,
    } = &.{
        .{
            .source = "// This should not count at all\n2 + 2 // Neither should this\n//Or this\n\n\n",
            .expected = &.{
                testTokenLine("2", .number_int, 2),
                testTokenLine("+", .plus, 2),
                testTokenLine("2", .number_int, 2),
                testTokenLine("", .eof, 6),
            },
        },
    };

    const gpa = std.testing.allocator;

    for (tests) |t| {
        errdefer std.debug.print("failed test with source = \"{s}\"\n\n", .{t.source});

        const tokens = try testScanTokens(gpa, t.source);
        defer gpa.free(tokens);

        try expectTokensEqual(t.expected, tokens);
    }
}
