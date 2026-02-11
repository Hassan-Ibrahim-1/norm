const std = @import("std");
const trait = @import("trait.zig");

pub fn @"f32"(x: anytype) f32 {
    return switch (@typeInfo(@TypeOf(x))) {
        .int, .comptime_int => @floatFromInt(x),
        .float, .comptime_float => @floatCast(x),
        else => @compileError("Expected a float or integer"),
    };
}

pub fn as(T: type, x: anytype) T {
    const V = @TypeOf(x);
    if (trait.isInt(T)) {
        if (trait.isInt(V)) {
            return @intCast(x);
        }
        if (trait.isFloat(V)) {
            return @intFromFloat(x);
        }
    } else if (trait.isFloat(T)) {
        if (trait.isInt(V)) {
            return @floatFromInt(x);
        }
        if (trait.isFloat(V)) {
            return @floatCast(x);
        }
    }
    castError(T, V);
}

fn castError(Target: type, From: type) noreturn {
    @compileError(
        std.fmt.comptimePrint(
            "Cannot cast {s} to {s}",
            .{ @typeName(From), @typeName(Target) },
        ),
    );
}
