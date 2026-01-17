const std = @import("std");

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
    };
}

pub fn scanToken() Error!Token {}
