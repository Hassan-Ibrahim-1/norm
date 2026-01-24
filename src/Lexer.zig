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
        number,

        // keywords.
        _and,
        _struct,
        _else,
        _false,
        _for,
        _fn,
        _if,
        _try,
        nil,
        _or,
        _return,
        _true,
        mut,
        import,
        _switch,
        _enum,
        range,

        _error,
        eof,
    };

    lexeme: []const u8,
    line: u32,
    type: Type,

    pub fn format(self: Token, w: *std.Io.Writer) std.Io.Writer.Error!void {
        try w.print(
            ".{{ .lexeme = \"{s}\", .line = {}, .type = {t}}}",
            .{ self.lexeme, self.line, self.type },
        );
    }
};

pub const Error = error{
    UnrecognizedCharacter,
};

pub fn init(source: []const u8) Lexer {
    return .{
        .source = source,
        .start = 0,
        .current = 0,
        .line = 0,
    };
}

fn tokenOptionalEqual(self: *Lexer, c: u8) Token {
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
            if (self.match('=')) {
                return self.createToken(entry.equal);
            }
            return self.createToken(entry.single);
        }
    }

    unreachable;
}

fn scanToken(self: *Lexer) Error!Token {
    self.skipWhitespace();
    self.start = self.current;

    if (self.isAtEnd()) return self.createToken(.eof);

    const c = self.next();

    switch (c) {
        '(' => return self.createToken(.left_paren),
        ')' => return self.createToken(.right_paren),
        '{' => return self.createToken(.left_brace),
        '}' => return self.createToken(.right_brace),
        ';' => return self.createToken(.semicolon),
        ',' => return self.createToken(.comma),
        '.' => return self.createToken(.dot),

        '-', '+', ':', '/', '*', '!', '>', '<' => return self.tokenOptionalEqual(c),

        '=' => {
            if (self.match('=')) {
                return self.createToken(.equal_equal);
            } else if (self.match('>')) {
                return self.createToken(.equal_greater);
            }
            return self.createToken(.equal);
        },

        '"' => return self.string(),

        else => {
            if (isDigit(c)) {
                return self.number();
            }
            if (isAlpha(c)) {
                return self.identifier();
            }
            std.debug.print("char: {}", .{c});
            return self.errorToken("Unexpected character.");
        },
    }
}

fn scanTokens(
    self: *Lexer,
    gpa: Allocator,
) (Allocator.Error || Error)![]const Token {
    var tokens: std.ArrayList(Token) = .empty;
    errdefer tokens.deinit(gpa);

    while (true) {
        const token = try self.scanToken();
        try tokens.append(gpa, token);
        if (token.type == .eof) {
            return tokens.toOwnedSlice(gpa);
        }
    }
}

fn string(self: *Lexer) Token {
    while (!self.isAtEnd() and self.peek() != '"') {
        if (self.peek() == '\n') self.line += 1;
        _ = self.next();
    }

    if (self.isAtEnd()) return self.errorToken("Unterminated string");

    // consume the '"'
    _ = self.next();
    return self.createToken(.string);
}

fn number(self: *Lexer) Token {
    while (!self.isAtEnd() and isDigit(self.peek())) _ = self.next();

    if (!self.isAtEnd() and self.peek() == '.' and isDigit(self.peekNext())) {
        _ = self.next();

        while (isDigit(self.peek())) _ = self.next();
    }

    return self.createToken(.number);
}

fn identifier(self: *Lexer) Token {
    while (!self.isAtEnd() and (isAlpha(self.peek()) or isDigit(self.peek()))) {
        _ = self.next();
    }
    return self.createToken(self.identifierType());
}

fn identifierType(self: *Lexer) Token.Type {
    return switch (self.source[self.start]) {
        'a' => self.checkKeyword(1, "nd", ._and),
        'c' => self.checkKeyword(1, "truct", ._struct),
        'n' => self.checkKeyword(1, "il", .nil),
        'o' => self.checkKeyword(1, "r", ._or),
        'm' => self.checkKeyword(1, "ut", .mut),
        's' => self.checkKeyword(1, "witch", ._switch),

        'e' => if (self.current > self.start)
            switch (self.source[self.start + 1]) {
                'l' => self.checkKeyword(2, "se", ._else),
                'n' => self.checkKeyword(2, "um", ._enum),
                else => .identifier,
            }
        else
            .identifier,

        'r' => if (self.current > self.start)
            switch (self.source[self.start + 1]) {
                'e' => self.checkKeyword(2, "turn", ._return),
                'a' => self.checkKeyword(2, "nge", .range),
                else => .identifier,
            }
        else
            .identifier,

        'i' => if (self.current > self.start)
            switch (self.source[self.start + 1]) {
                'f' => ._if,
                'm' => self.checkKeyword(2, "port", .import),
                else => .identifier,
            }
        else
            .identifier,

        'f' => if (self.current > self.start)
            switch (self.source[self.start + 1]) {
                'a' => self.checkKeyword(2, "lse", ._false),
                'o' => self.checkKeyword(2, "r", ._for),
                'n' => ._fn,
                else => .identifier,
            }
        else
            .identifier,

        't' => if (self.current > self.start and self.source[self.start + 1] == 'r')
            switch (self.source[self.start + 2]) {
                'u' => self.checkKeyword(3, "e", ._true),
                'y' => ._try,
                else => .identifier,
            }
        else
            .identifier,

        else => .identifier,
    };
}

fn checkKeyword(
    self: *Lexer,
    start: usize,
    rest: []const u8,
    typ: Token.Type,
) Token.Type {
    const current = self.source[start + self.start .. self.current];
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

fn errorToken(self: *Lexer, msg: []const u8) Token {
    return Token{
        .type = ._error,
        .lexeme = msg,
        .line = self.line,
    };
}

fn match(self: *Lexer, c: u8) bool {
    if (!self.isAtEnd() and self.peek() == c) {
        _ = self.next();
        return true;
    }
    return false;
}

fn next(self: *Lexer) u8 {
    defer self.current += 1;
    return self.source[self.current];
}

fn skipWhitespace(self: *Lexer) void {
    while (!self.isAtEnd()) {
        switch (self.peek()) {
            ' ', '\t', '\r' => _ = self.next(),
            '\n' => {
                self.line += 1;
                _ = self.next();
            },
            '/' => {
                if (self.peekNext() == '/') {
                    while (self.peek() != '\n' and !self.isAtEnd()) {
                        _ = self.next();
                    }
                } else return;
            },

            else => return,
        }
    }
}

fn peek(self: *Lexer) u8 {
    return self.source[self.current];
}

fn peekNext(self: *Lexer) u8 {
    return self.source[self.current + 1];
}

fn isAtEnd(self: *Lexer) bool {
    return self.current == self.source.len;
}

fn createToken(self: *Lexer, typ: Token.Type) Token {
    return .{
        .type = typ,
        .line = self.line,
        .lexeme = self.source[self.start..self.current],
    };
}

const testing = std.testing;

fn testToken(lexeme: []const u8, typ: Token.Type) Token {
    return Token{
        .lexeme = lexeme,
        .type = typ,
        .line = 0,
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

fn testScanTokens(gpa: Allocator, source: []const u8) (Allocator.Error || Error)![]const Token {
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
                testToken("2", .number),
                testToken("+", .plus),
                testToken("2", .number),
                testToken("", .eof),
            },
        },

        // Parentheses and multiplication
        .{
            .source = "(1 * 3)",
            .expected = &.{
                testToken("(", .left_paren),
                testToken("1", .number),
                testToken("*", .star),
                testToken("3", .number),
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
                testToken("10", .number),
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
                testToken("true", ._true),
                testToken("and", ._and),
                testToken("false", ._false),
                testToken("", .eof),
            },
        },

        .{
            .source = "for i < 5",
            .expected = &.{
                testToken("for", ._for),
                testToken("i", .identifier),
                testToken("<", .less),
                testToken("5", .number),
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
                testToken("for", ._for),
                testToken("i", .identifier),
                testToken(":=", .colon_equal),
                testToken("range", .range),
                testToken("10", .number),
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
