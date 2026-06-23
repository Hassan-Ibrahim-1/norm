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

pub fn main(init: std.process.Init) !void {
    const io = init.io;

    var gpa: std.heap.DebugAllocator(.{}) = .init;
    defer assert(gpa.deinit() == .ok);
    const alloc = gpa.allocator();

    const args = try init.minimal.args.toSlice(init.arena.allocator());

    const stdin = Io.File.stdin();
    _ = stdin; // autofix

    var stdout_buf: [128]u8 = undefined;
    var stdout_w = Io.File.stdout().writer(io, &stdout_buf);
    const stdout = &stdout_w.interface;

    var stderr_buf: [128]u8 = undefined;
    var stderr_w = Io.File.stderr().writer(io, &stderr_buf);
    const stderr = &stderr_w.interface;

    defer {
        stderr.flush() catch unreachable;
        stdout.flush() catch unreachable;
    }

    var dump_mode = false;
    var file_idx: usize = 1;

    if (args.len > 1 and std.mem.eql(u8, args[1], "-S")) {
        dump_mode = true;
        file_idx = 2;
    }

    if (dump_mode) {
        if (file_idx >= args.len) {
            try stderr.print("usage: norm -S [file]\n", .{});
            try stderr.flush();
        } else {
            try dumpChunk(io, alloc, args[file_idx], stderr);
        }
    } else if (args.len == 2) {
        try runFile(io, alloc, args[1], stdout, stderr);
    } else {
        try stderr.print("usage: norm [file]\n", .{});
        try stderr.flush();
    }
}

fn dumpChunk(io: Io, gpa: Allocator, path: []const u8, stderr: *Io.Writer) !void {
    const file = Io.Dir.cwd().openFile(io, path, .{}) catch |err| switch (err) {
        error.FileNotFound => return stderr.print("{s}: file not found\n", .{path}),
        else => return err,
    };
    defer file.close(io);

    var file_reader = file.reader(io, &.{});
    const source = try file_reader.interface.allocRemaining(gpa, .unlimited);
    defer gpa.free(source);

    try debug.printWithLineNumbers(stderr, source);
    try stderr.writeByte('\n');

    var lexer = Lexer.init(source);
    var tokens = lexer.scanTokens(gpa);
    defer tokens.deinit(gpa);

    if (tokens.errors.len > 0) {
        for (tokens.errors) |diag| {
            try stderr.print("{s}\n", .{diag.error_msg});
        }
        return;
    }

    var ast = parser.parse(gpa, tokens.tokens);
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

    var chunk = compiler.compile(gpa, &nir);
    defer chunk.deinit();

    debug.disassembleChunk(stderr, &chunk, "main", source);
}

fn runFile(io: Io, gpa: Allocator, path: []const u8, stdout: *Io.Writer, stderr: *Io.Writer) !void {
    const file = Io.Dir.cwd().openFile(io, path, .{}) catch |err| switch (err) {
        error.FileNotFound => return stderr.print("{s}: file not found\n", .{path}),
        else => return err,
    };
    defer file.close(io);

    var file_reader = file.reader(io, &.{});
    const source = try file_reader.interface.allocRemaining(gpa, .unlimited);
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

    var ast = parser.parse(gpa, tokens.tokens);
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
    // try stderr.print("{s}\n\n", .{stmts});

    var chunk = compiler.compile(gpa, &nir);
    defer chunk.deinit();

    try stderr.print("constants length: {}\n", .{chunk.constants.items.len});

    var vm = Vm.init(gpa, stdout, stderr);
    defer vm.deinit();

    _ = try vm.interpret(&chunk);
}

test {
    std.testing.refAllDecls(@This());
}
