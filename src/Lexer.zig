const std = @import("std");
const Allocator = std.mem.Allocator;

const Lexer = @This();

source: []const u8,
start: usize,
end: usize,
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
};

pub const Error = error{
    UnrecognizedCharacter,
};

pub fn init(source: []const u8) Lexer {
    return .{
        .source = source,
        .start = 0,
        .end = 0,
        .line = 0,
    };
}

pub fn scanToken(self: *Lexer) Error!Token {
    _ = self; // autofix
    return eofToken(0);
}

fn eofToken(line: u32) Token {
    return .{
        .type = .eof,
        .line = line,
        .lexeme = "",
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

test "simple" {
    const tests: []const struct {
        source: []const u8,
        expected: []const Token,
    } = &.{
        .{
            .source = "+",
            .expected = &.{
                testToken("+", .plus),
            },
        },
    };

    const gpa = std.testing.allocator;

    for (tests) |t| {
        const tokens = try testScanTokens(gpa, t.source);
        defer gpa.free(tokens);

        try testing.expectEqualSlices(Token, t.expected, tokens);
    }
}
