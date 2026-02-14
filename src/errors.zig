const std = @import("std");
const mem = std.mem;
const Io = std.Io;

pub const Diagnostics = struct {
    file_name: []const u8,
    source: []const u8,
    line: u32,
    hints: []const []const u8 = &.{},
    notes: []const []const u8 = &.{},
    error_msg: []const u8,
};

pub fn reportError(w: *Io.Writer, diagnostics: *const Diagnostics) Io.Writer.Error!void {
    const lines = getLines(diagnostics.source, diagnostics.line, 1);

    try w.print("{s}:{}: error: {s}\n", .{
        diagnostics.file_name,
        diagnostics.line,
        diagnostics.error_msg,
    });

    const indent = "    ";
    var line_iter = mem.splitScalar(u8, lines, '\n');
    while (line_iter.next()) |line| {
        try w.print("{s}{s}\n", .{ indent, line });
    }
    try w.writeByte('\n');

    for (diagnostics.hints) |hint| {
        try w.print("hint: {s}\n", .{hint});
    }
    for (diagnostics.notes) |note| {
        try w.print("note: {s}\n", .{note});
    }
}

fn getLines(str: []const u8, line: u32, surrounding: u32) []const u8 {
    const line_index = line - 1;
    const start_line = if (surrounding > line_index) 0 else line_index - surrounding;
    const end_line = line_index + surrounding;

    var iter = mem.splitScalar(u8, str, '\n');

    var start_index: usize = 0;
    var end_index: usize = str.len;

    var i: usize = 0;
    var previous_index: usize = 0;
    while (iter.next() != null) : ({
        i += 1;
        previous_index = iter.index orelse 0;
    }) {
        if (i == start_line) start_index = previous_index;
        if (i == end_line) {
            end_index = iter.index orelse str.len;
            break;
        }
    }

    return str[start_index..end_index];
}

const testing = std.testing;

test getLines {
    const tests: []const struct {
        source: []const u8,
        line: u32,
        surrounding: u32,
        expected: []const u8,
    } = &.{
        .{
            .source = "",
            .line = 1,
            .surrounding = 0,
            .expected = "",
        },
        .{
            .source = "",
            .line = 1,
            .surrounding = 3,
            .expected = "",
        },
        .{
            .source =
            \\start
            \\mid
            \\last
            ,
            .line = 1,
            .surrounding = 0,
            .expected = "start\n",
        },
        .{
            .source =
            \\start
            \\mid
            \\last
            ,
            .line = 2,
            .surrounding = 0,
            .expected = "mid\n",
        },
        .{
            .source =
            \\start
            \\mid
            \\last
            ,
            .line = 3,
            .surrounding = 0,
            .expected = "last",
        },
        .{
            .source =
            \\start
            \\mid
            \\last
            ,
            .line = 2,
            .surrounding = 1,
            .expected =
            \\start
            \\mid
            \\last
            ,
        },
        .{
            .source =
            \\start
            \\mid
            \\last
            ,
            .line = 1,
            .surrounding = 1,
            .expected =
            \\start
            \\mid
            \\
            ,
        },
        .{
            .source =
            \\start
            \\mid
            \\last
            ,
            .line = 3,
            .surrounding = 1,
            .expected =
            \\mid
            \\last
            ,
        },
        .{
            .source =
            \\start
            \\line2
            \\mid
            \\line4
            \\last
            ,
            .line = 3,
            .surrounding = 3,
            .expected =
            \\start
            \\line2
            \\mid
            \\line4
            \\last
            ,
        },
        .{
            .source =
            \\start
            \\line2
            \\mid
            \\line4
            \\last
            ,
            .line = 1,
            .surrounding = 10,
            .expected =
            \\start
            \\line2
            \\mid
            \\line4
            \\last
            ,
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("test case failed with source=\n'{s}'\n", .{t.source});
        const actual = getLines(t.source, t.line, t.surrounding);
        try testing.expectEqualStrings(t.expected, actual);
    }
}
