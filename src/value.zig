const std = @import("std");
const Io = std.Io;

pub const Value = union(enum) {
    integer: i32,
    float: f64,

    pub fn format(v: *const Value, w: *Io.Writer) Io.Writer.Error!void {
        try w.print("{any}", .{v});
    }
};
