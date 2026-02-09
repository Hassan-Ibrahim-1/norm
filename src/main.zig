const std = @import("std");
const Io = std.Io;
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const assert = std.debug.assert;
const Compiler = compiler.Compiler;
const Lexer = @import("Lexer.zig");
const parser = @import("parser.zig");
const compiler = @import("compiler.zig");
const debug = @import("debug.zig");

pub fn main() !void {
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    defer assert(gpa.deinit() == .ok);
    const alloc = gpa.allocator();

    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

    const stdin = std.fs.File.stdin();
    _ = stdin; // autofix

    var stdout_buf: [1024]u8 = undefined;
    var stdout_w = std.fs.File.stdout().writer(&stdout_buf);
    const stdout = &stdout_w.interface;

    var stderr_buf: [1024]u8 = undefined;
    var stderr_w = std.fs.File.stderr().writer(&stderr_buf);
    const stderr = &stderr_w.interface;
    _ = stderr; // autofix

    var c = Compiler.init(alloc);
    var chunk = c.compile(undefined);
    defer chunk.deinit(alloc);

    debug.disassembleChunk(stdout, &chunk, "test chunk");

    try stdout.flush();

    // if (args.len == 2)
    //     try runFile(alloc, args[1], stdout, stderr)
    // else
    //     try repl(alloc, stdout, stderr, stdin);
}

fn runFile(gpa: Allocator, path: []const u8, stdout: *Io.Writer, stderr: *Io.Writer) !void {
    _ = stderr; // autofix
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const source = try file.readToEndAlloc(gpa, 1_000_000);
    defer gpa.free(source);

    try stdout.flush();
}

fn repl(gpa: Allocator, stdout: *Io.Writer, stderr: *Io.Writer, stdin: std.fs.File) !void {
    while (true) {
        var line_buf: [1000]u8 = undefined;
        _ = try stdout.write("> ");
        try stdout.flush();

        const len = try stdin.read(&line_buf);
        if (len == 0) {
            _ = try stdout.write("\n");
            break;
        }
        const line = line_buf[0..len];

        var lexer = Lexer.init(line);

        const ast = parser.parse(gpa, &lexer);
        defer ast.arena.deinit();

        if (ast.expr) |expr| {
            try stdout.print("{f}\n", .{expr});
        } else {
            try stderr.print("parse error\n", .{});
            try stderr.flush();
        }
    }
}

test {
    _ = Lexer;
    _ = parser.parse;
    _ = compiler.Compiler;
    std.testing.refAllDeclsRecursive(@This());
}
