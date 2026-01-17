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

pub fn scanToken(self: *Lexer) Error!Token {
    self.skipWhitespace();

    if (self.isAtEnd()) return self.createToken(.eof);

    self.start = self.current;
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

        else => @panic("todo"),
    }
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

fn testScanTokens(
    gpa: Allocator,
    source: []const u8,
) (Allocator.Error || Error)![]const Token {
    var l = Lexer.init(source);
    var tokens: std.ArrayList(Token) = .empty;
    errdefer tokens.deinit(gpa);

    while (true) {
        const token = try l.scanToken();
        if (token.type == .eof) {
            return tokens.toOwnedSlice(gpa);
        }
        try tokens.append(gpa, token);
    }
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
    std.debug.print("{s}: {any}\n", .{ prefix, tokens });
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
