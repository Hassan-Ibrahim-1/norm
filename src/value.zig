const std = @import("std");
const mem = std.mem;
const Allocator = std.mem.Allocator;
const Io = std.Io;

pub const Value = union(enum) {
    pub const String = struct {
        data: []const u8,

        pub fn ref(s: []const u8) String {
            return .{ .data = s };
        }
    };

    integer: i32,
    float: f64,
    boolean: bool,
    nil: void,
    string: String,

    pub fn format(v: *const Value, w: *Io.Writer) Io.Writer.Error!void {
        try switch (v.*) {
            .string => |s| w.print("{s}", .{s.data}),
            .integer => |i| w.print("{}", .{i}),
            .float => |f| w.print("{d:.3}", .{f}),
            .boolean => |b| w.print("{}", .{b}),
            .nil => w.print("nil", .{}),
        };
    }
};
