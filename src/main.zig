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
const mem = std.mem;

const Args = struct {
    pub const Action = enum {
        run,
        codegen,
        analyze,
        parse,
    };

    file: []const u8,
    action: Action,
};

fn parseArgs(args: []const []const u8, stderr: *Io.Writer) !Args {
    if (args.len == 1) {
        try stderr.writeAll("usage: norm <run|codegen|analyze|parse> [file]\n");
        return error.BadArgs;
    }
    if (args.len == 2) {
        return .{
            .file = args[1],
            .action = .run,
        };
    }
    if (args.len > 3) {
        try stderr.writeAll("usage: norm <run|codegen|analyze|parse> [file]\n");
        return error.BadArgs;
    }

    const option = args[1];
    const file = args[2];
    var action: Args.Action = .run;
    if (mem.eql(u8, option, "run")) {
        action = .run;
    } else if (mem.eql(u8, option, "codegen")) {
        action = .codegen;
    } else if (mem.eql(u8, option, "analyze")) {
        action = .analyze;
    } else if (mem.eql(u8, option, "parse")) {
        action = .parse;
    } else {
        try stderr.writeAll("usage: norm <run|codegen|analyze|parse> [file]\n");
        return error.BadArgs;
    }
    return .{
        .file = file,
        .action = action,
    };
}

pub fn main(init: std.process.Init) !void {
    const io = init.io;

    var gpa: std.heap.DebugAllocator(.{}) = .init;
    defer assert(gpa.deinit() == .ok);
    const alloc = gpa.allocator();

    const stdin = Io.File.stdin();
    _ = stdin; // autofix

    var stdout_buf: [128]u8 = undefined;
    var stdout_w = Io.File.stdout().writer(io, &stdout_buf);
    const stdout = &stdout_w.interface;

    var stderr_buf: [128]u8 = undefined;
    var stderr_w = Io.File.stderr().writer(io, &stderr_buf);
    const stderr = &stderr_w.interface;

    const arg_slice = try init.minimal.args.toSlice(init.arena.allocator());
    const args = parseArgs(arg_slice, stderr) catch |err| switch (err) {
        error.BadArgs => return,
        else => return err,
    };

    defer {
        stderr.flush() catch unreachable;
        stdout.flush() catch unreachable;
    }

    try runFile(io, alloc, args.file, stdout, stderr, args.action);
}

fn runFile(io: Io, gpa: Allocator, path: []const u8, stdout: *Io.Writer, stderr: *Io.Writer, action: Args.Action) !void {
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
    if (action == .parse) {
        const stmts = debug.printStmts(gpa, ast.stmts);
        try stderr.print("{s}\n", .{stmts});
        return;
    }

    var nir = sema.analyze(gpa, &ast);
    defer nir.deinit();
    if (nir.errors.len > 0) {
        debug.reportErrors(nir.errors, path, source);
        return;
    }
    if (action == .analyze) {
        const stmts = debug.printStmts(gpa, nir.stmts);
        try stderr.print("{s}\n", .{stmts});
        return;
    }

    var chunk = compiler.compile(gpa, &nir);
    defer chunk.deinit();
    if (action == .codegen) {
        debug.disassembleChunk(stderr, &chunk, "main", source);
        return;
    }

    var vm = Vm.init(gpa, stdout, stderr);
    defer vm.deinit();

    _ = try vm.interpret(&chunk);
}

test {
    std.testing.refAllDecls(@This());
}
