//! Some helpful comptime functions to determine a type's capabilities

const std = @import("std");

pub inline fn isInt(T: type) bool {
    return switch (@typeInfo(T)) {
        .int, .comptime_int => true,
        else => false,
    };
}

const fmt = std.fmt;

pub inline fn isFloat(T: type) bool {
    return switch (@typeInfo(T)) {
        .float, .comptime_float => true,
        else => false,
    };
}

pub inline fn isEnumLiteral(x: type) bool {
    return @typeInfo(x) == .enum_literal;
}

const meta = std.meta;

pub inline fn isEnum(T: type, t: anytype) bool {
    if (@TypeOf(t) == T) return true;
    if (!isEnumLiteral(@TypeOf(t))) return false;

    const ti = @typeInfo(T).@"enum";
    inline for (ti.fields) |field| {
        if (comptime std.mem.eql(u8, field.name, @tagName(t))) {
            return true;
        }
    }
    return false;
}
