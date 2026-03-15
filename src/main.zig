const std = @import("std");
const Io = std.Io;
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const assert = std.debug.assert;
const Compiler = compiler.Compiler;
const Vm = @import("vm.zig").Vm;
const Lexer = @import("Lexer.zig");
const ers = @import("errors.zig");
const sema = @import("sema.zig");
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

    var stdout_buf: [1024]u8 = undefined;
    var stdout_w = std.fs.File.stdout().writer(&stdout_buf);
    const stdout = &stdout_w.interface;

    var stderr_buf: [1024]u8 = undefined;
    var stderr_w = std.fs.File.stderr().writer(&stderr_buf);
    const stderr = &stderr_w.interface;

    defer {
        stderr.flush() catch unreachable;
        stdout.flush() catch unreachable;
    }

    if (args.len > 2) {
        try stderr.print("usage: norm [file]\n", .{});
        try stderr.flush();
    } else if (args.len == 2) {
        try runFile(alloc, args[1], stdout, stderr);
    } else {
        try repl(alloc, stdout, stderr, stdin);
    }
}

fn runFile(gpa: Allocator, path: []const u8, stdout: *Io.Writer, stderr: *Io.Writer) !void {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const source = try file.readToEndAlloc(gpa, 1_000_000);
    defer gpa.free(source);

    var lexer = Lexer.init(source);
    var tokens = lexer.scanTokens(gpa);
    defer tokens.deinit(gpa);

    if (tokens.errors.len > 0) {
        for (tokens.errors) |diag| {
            try stderr.print("{s}\n", .{diag.error_msg});
        }
        return;
    }

    var ast = parser.parse(gpa, tokens.tokens, false);
    defer ast.arena.deinit();
    if (ast.errors.len > 0) {
        debug.reportErrors(ast.errors, path, source);
        return;
    }

    var nir = sema.analyze(gpa, &ast);
    defer nir.deinit();
    if (nir.errors.len > 0) {
        debug.reportErrors(nir.errors, path, source);
        return;
    }

    const stmts = debug.printStmts(gpa, nir.stmts);
    defer gpa.free(stmts);
    try stderr.print("{s}\n\n", .{stmts});

    var chunk = compiler.compile(gpa, &nir);
    defer chunk.deinit();

    var vm = Vm.init(gpa, stdout, stderr);
    defer vm.deinit();

    _ = try vm.interpret(&chunk);
}

fn repl(gpa: Allocator, stdout: *Io.Writer, stderr: *Io.Writer, stdin: std.fs.File) !void {
    while (true) {
        var line_buf: [1000]u8 = undefined;
        _ = try stdout.writeAll("> ");
        try stdout.flush();
        defer {
            stderr.flush() catch unreachable;
            stdout.flush() catch unreachable;
        }

        const len = try stdin.read(&line_buf);
        if (len == 0) {
            try stdout.writeAll("\n");
            break;
        }
        const line = line_buf[0..len];

        var lexer = Lexer.init(line);
        var tokens = lexer.scanTokens(gpa);
        defer tokens.deinit(gpa);
        if (tokens.errors.len > 0) {
            for (tokens.errors) |diag| {
                try stderr.print("{s}\n", .{diag.error_msg});
            }
            continue;
        }

        var ast = parser.parse(gpa, tokens.tokens, true);
        defer ast.arena.deinit();
        if (ast.errors.len > 0) {
            for (ast.errors) |diag| {
                try stderr.print("{s}\n", .{diag.error_msg});
            }
            continue;
        }

        var nir = sema.analyze(gpa, &ast);
        defer nir.deinit();
        if (nir.errors.len > 0) {
            for (nir.errors) |diag| {
                try stderr.print("{s}\n", .{diag.error_msg});
            }
            continue;
        }
        const stmts = debug.printStmts(gpa, nir.stmts);
        defer gpa.free(stmts);
        try stderr.print("{s}\n", .{stmts});

        var chunk = compiler.compile(gpa, &nir);
        defer chunk.deinit();

        var vm = Vm.init(gpa, stdout, stderr);
        defer vm.deinit();

        const value = try vm.interpret(&chunk);
        try stdout.print("{f}\n", .{value});
    }
}

test {
    _ = Lexer;
    _ = parser.parse;
    _ = compiler.Compiler;
    std.testing.refAllDeclsRecursive(@This());
}
