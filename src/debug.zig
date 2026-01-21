const std = @import("std");

pub fn dbg(prefix: []const u8, value: anytype) @TypeOf(value) {
    std.debug.print("{s} = {f}\n", .{
        prefix,
        std.json.fmt(value, .{ .whitespace = .indent_4 }),
    });
    return value;
}
