const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const Io = std.Io;
const testing = std.testing;

const opts = @import("opts");

const compiler = @import("compiler.zig");
const Chunk = compiler.Chunk;
const OpCode = compiler.OpCode;
const debug = @import("debug.zig");
const Lexer = @import("Lexer.zig");
const parser = @import("parser.zig");
const sema = @import("sema.zig");
const Token = @import("Lexer.zig").Token;
const Value = @import("value.zig").Value;

const stack_max = 1024;

pub const Vm = struct {
    gpa: Allocator,
    chunk: *Chunk,
    ip: [*]u8,
    stack: [stack_max]Value,
    stack_top: [*]Value,

    strings: std.StringHashMapUnmanaged(void),

    stdout: *Io.Writer,
    stderr: *Io.Writer,

    pub const Error = error{
        RuntimeError,
    };

    pub fn init(gpa: Allocator, stdout: *Io.Writer, stderr: *Io.Writer) Vm {
        return .{
            .gpa = gpa,
            .chunk = undefined,
            .ip = undefined,
            .stdout = stdout,
            .stderr = stderr,
            .stack = undefined,
            .stack_top = undefined,
            .strings = .empty,
        };
    }

    pub fn deinit(vm: *Vm) void {
        vm.* = undefined;
    }

    pub fn interpret(vm: *Vm, chunk: *Chunk) Error!Value {
        vm.chunk = chunk;
        vm.ip = chunk.code.items.ptr;
        vm.resetStack();
        return vm.run();
    }

    fn run(vm: *Vm) Error!Value {
        while (true) {
            const instruction: OpCode = @enumFromInt(vm.readByte());
            if (comptime opts.debug_trace) {
                _ = debug.disassembleInstruction(
                    vm.stderr,
                    vm.chunk,
                    vm.ip - vm.chunk.code.items.ptr - 1,
                ) catch unreachable;

                std.debug.print("          ", .{});
                const stack_len = vm.stack_top - &vm.stack;
                for (0..stack_len) |i| {
                    std.debug.print("[ {f} ]", .{vm.stack[i]});
                }
                std.debug.print("\n", .{});
            }

            switch (instruction) {
                .op_constant => {
                    vm.push(vm.readConstant());
                },

                .op_constant_long => {
                    vm.push(vm.readLongConstant());
                },

                .op_add_int => {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(.{ .integer = a.integer + b.integer });
                },

                .op_add_float => {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(.{ .float = a.float + b.float });
                },

                .op_subtract_int => {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(.{ .integer = a.integer - b.integer });
                },

                .op_subtract_float => {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(.{ .float = a.float - b.float });
                },

                .op_multiply_int => {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(.{ .integer = a.integer * b.integer });
                },

                .op_multiply_float => {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(.{ .float = a.float * b.float });
                },

                .op_divide_float => {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(.{ .float = a.float / b.float });
                },

                .op_concat => {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(vm.stringConcat(a.string, b.string));
                },

                .op_negate_int => {
                    const a = vm.pop();
                    vm.push(.{ .integer = -a.integer });
                },

                .op_negate_float => {
                    const a = vm.pop();
                    vm.push(.{ .float = -a.float });
                },

                .op_true => vm.push(.{ .boolean = true }),
                .op_false => vm.push(.{ .boolean = false }),
                .op_nil => vm.push(.nil),

                .op_equal_int => {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(.{ .boolean = a.integer == b.integer });
                },
                .op_not_equal_int => {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(.{ .boolean = a.integer != b.integer });
                },
                .op_greater_int => {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(.{ .boolean = a.integer > b.integer });
                },
                .op_greater_equal_int => {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(.{ .boolean = a.integer >= b.integer });
                },
                .op_less_int => {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(.{ .boolean = a.integer < b.integer });
                },
                .op_less_equal_int => {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(.{ .boolean = a.integer <= b.integer });
                },

                .op_equal_float => {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(.{ .boolean = a.float == b.float });
                },
                .op_not_equal_float => {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(.{ .boolean = a.float != b.float });
                },
                .op_greater_float => {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(.{ .boolean = a.float > b.float });
                },
                .op_greater_equal_float => {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(.{ .boolean = a.float >= b.float });
                },
                .op_less_float => {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(.{ .boolean = a.float < b.float });
                },
                .op_less_equal_float => {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(.{ .boolean = a.float <= b.float });
                },

                .op_equal_string => {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(.{ .boolean = stringEqual(a.string, b.string) });
                },
                .op_not_equal_string => {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(.{ .boolean = !stringEqual(a.string, b.string) });
                },

                .op_equal_bool => {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(.{ .boolean = a.boolean == b.boolean });
                },
                .op_not_equal_bool => {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(.{ .boolean = a.boolean != b.boolean });
                },

                .op_and => {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(.{ .boolean = a.boolean and b.boolean });
                },
                .op_or => {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(.{ .boolean = a.boolean or b.boolean });
                },
                .op_not => {
                    const a = vm.pop();
                    vm.push(.{ .boolean = !a.boolean });
                },

                .op_cast_to_int => {
                    const a = vm.pop();
                    vm.push(.{ .integer = @intFromFloat(a.float) });
                },
                .op_cast_to_float => {
                    const a = vm.pop();
                    vm.push(.{ .float = @floatFromInt(a.integer) });
                },

                .op_store => {
                    const value = vm.peek();
                    const stack_slot = vm.readShort();
                    vm.stack[stack_slot] = value;
                },

                .op_load => {
                    const stack_slot = vm.readShort();
                    vm.push(vm.stack[stack_slot]);
                },

                .op_pop => {
                    _ = vm.pop();
                },

                .op_return => {
                    return vm.pop();
                },
            }
        }
    }

    fn readByte(vm: *Vm) u8 {
        defer vm.ip += 1;
        return vm.ip[0];
    }

    fn readShort(vm: *Vm) u16 {
        const short_bytes = vm.readNBytes(2);
        return mem.readInt(u16, &short_bytes, .little);
    }

    fn readNBytes(vm: *Vm, comptime n: usize) [n]u8 {
        defer vm.ip += n;
        var ret: [n]u8 = undefined;
        inline for (0..n) |i| {
            ret[i] = vm.ip[i];
        }
        return ret;
    }

    fn readConstant(vm: *Vm) Value {
        return vm.chunk.constants.items[vm.readByte()];
    }

    fn readLongConstant(vm: *Vm) Value {
        const index_bytes = vm.readNBytes(3);
        const constant_index = mem.readInt(u24, &index_bytes, .little);
        return vm.chunk.constants.items[constant_index];
    }

    fn resetStack(vm: *Vm) void {
        vm.stack_top = &vm.stack;
    }

    fn push(vm: *Vm, value: Value) void {
        vm.stack_top[0] = value;
        vm.stack_top += 1;
    }

    fn peek(vm: *Vm) Value {
        return (vm.stack_top - 1)[0];
    }

    fn pop(vm: *Vm) Value {
        vm.stack_top -= 1;
        return vm.stack_top[0];
    }

    fn stringConcat(vm: *Vm, a: Value.String, b: Value.String) Value {
        const arena = vm.chunk.string_arena.allocator();
        const data = mem.concat(arena, u8, &.{ a.data, b.data }) catch oom();
        return .{ .string = .ref(data) };
    }

    fn stringEqual(a: Value.String, b: Value.String) bool {
        if (a.data.ptr == b.data.ptr) return true;
        return mem.eql(u8, a.data, b.data);
    }
};

fn oom() noreturn {
    @panic("oom");
}

const cast = @import("cast.zig");

const dbg = debug.dbg;

fn testRun(
    gpa: Allocator,
    source: []const u8,
    stdout: *Io.Writer,
    stderr: *Io.Writer,
) !Value {
    var l = Lexer.init(source);
    const tokens = l.scanTokens(gpa);
    defer {
        gpa.free(tokens.tokens);
        gpa.free(tokens.errors);
    }
    if (tokens.errors.len > 0) {
        dbg("tokens.errors", tokens.errors);
        return error.LexerError;
    }

    var ast = parser.parse(gpa, tokens.tokens, true);
    defer ast.arena.deinit();
    if (ast.errors.len > 0) {
        dbg("ast.errors", ast.errors);
        return error.ParserError;
    }

    var nir = sema.analyze(gpa, &ast);
    defer nir.deinit();
    if (nir.errors.len > 0) {
        debug.reportErrors(nir.errors, "test_runner", source);
        return error.SemaFailed;
    }

    var chunk = compiler.compile(gpa, &nir);
    defer chunk.deinit();

    var vm = Vm.init(gpa, stdout, stderr);
    defer vm.deinit();

    return vm.interpret(&chunk);
}

const NoFreeResult = struct {
    chunk: Chunk,
    value: Value,
};

fn testRunNoFree(
    gpa: Allocator,
    source: []const u8,
    stdout: *Io.Writer,
    stderr: *Io.Writer,
) !NoFreeResult {
    var l = Lexer.init(source);
    const tokens = l.scanTokens(gpa);
    defer {
        gpa.free(tokens.tokens);
        gpa.free(tokens.errors);
    }
    if (tokens.errors.len > 0) {
        dbg("tokens.errors", tokens.errors);
        return error.LexerError;
    }

    var ast = parser.parse(gpa, tokens.tokens, true);
    defer ast.arena.deinit();
    if (ast.errors.len > 0) {
        dbg("ast.errors", ast.errors);
        return error.ParserError;
    }

    var nir = sema.analyze(gpa, &ast);
    defer nir.deinit();
    if (nir.errors.len > 0) {
        debug.reportErrors(nir.errors, "test_runner", source);
        return error.SemaFailed;
    }

    var chunk = compiler.compile(gpa, &nir);

    var vm = Vm.init(gpa, stdout, stderr);
    defer vm.deinit();

    return .{ .value = try vm.interpret(&chunk), .chunk = chunk };
}

test "literals" {
    const gpa = testing.allocator;
    var discarding: Io.Writer.Discarding = .init(&.{});
    const w = &discarding.writer;

    const tests: []const struct {
        source: []const u8,
        expected: Value,
    } = &.{
        .{ .source = "2", .expected = .{ .integer = 2 } },
        .{ .source = "2.0", .expected = .{ .float = 2.0 } },
        .{ .source = "true", .expected = .{ .boolean = true } },
        .{ .source = "false", .expected = .{ .boolean = false } },
        // .{ .source = "nil", .expected = .nil },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test with source=\"{s}\"\n", .{t.source});
        const value = try testRun(gpa, t.source, w, w);
        try testing.expectEqual(t.expected, value);
    }
}

const as = cast.as;

test "arithmetic" {
    const gpa = testing.allocator;
    var discarding: Io.Writer.Discarding = .init(&.{});
    const w = &discarding.writer;

    const tests: []const struct {
        source: []const u8,
        expected: Value,
    } = &.{
        .{ .source = "2", .expected = .{ .integer = 2 } },
        .{ .source = "2 + 3", .expected = .{ .integer = 5 } },
        .{ .source = "2 - 3", .expected = .{ .integer = -1 } },
        .{ .source = "2 * 3", .expected = .{ .integer = 6 } },
        .{ .source = "2 / 3", .expected = .{ .float = 2.0 / 3.0 } },
        .{ .source = "(2 + 1) / 3", .expected = .{ .float = 1.0 } },
        .{ .source = "-2", .expected = .{ .integer = -2 } },
        .{ .source = "-(2 * 3)", .expected = .{ .integer = -6 } },
        .{ .source = "1.0 + 2.0", .expected = .{ .float = 3.0 } },
        .{ .source = "1.0 - 2.0", .expected = .{ .float = -1.0 } },
        .{ .source = "1 + 2.0", .expected = .{ .float = 3.0 } },
        .{ .source = "3.0 - 1", .expected = .{ .float = 2.0 } },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test with source=\"{s}\"\n", .{t.source});
        const value = try testRun(gpa, t.source, w, w);
        try testing.expectEqual(t.expected, value);
    }
}

test "comparison" {
    const gpa = testing.allocator;
    var discarding: Io.Writer.Discarding = .init(&.{});
    const w = &discarding.writer;

    const tests: []const struct {
        source: []const u8,
        expected: Value,
    } = &.{
        .{ .source = "true == true", .expected = .{ .boolean = true } },
        .{ .source = "false == true", .expected = .{ .boolean = false } },
        .{ .source = "false != true", .expected = .{ .boolean = true } },
        // .{ .source = "nil == nil", .expected = .{ .boolean = true } },
        // .{ .source = "nil != nil", .expected = .{ .boolean = false } },
        .{ .source = "1 == 1", .expected = .{ .boolean = true } },
        .{ .source = "1 != 1", .expected = .{ .boolean = false } },
        .{ .source = "1 == 2", .expected = .{ .boolean = false } },
        .{ .source = "1 != 2", .expected = .{ .boolean = true } },
        .{ .source = "1 < 2", .expected = .{ .boolean = true } },
        .{ .source = "1 <= 2", .expected = .{ .boolean = true } },
        .{ .source = "1 > 2", .expected = .{ .boolean = false } },
        .{ .source = "1 >= 2", .expected = .{ .boolean = false } },
        .{ .source = "2 > 1", .expected = .{ .boolean = true } },
        .{ .source = "2 >= 1", .expected = .{ .boolean = true } },
        .{ .source = "2 < 1", .expected = .{ .boolean = false } },
        .{ .source = "2 <= 1", .expected = .{ .boolean = false } },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test with source=\"{s}\"\n", .{t.source});
        const value = try testRun(gpa, t.source, w, w);
        try testing.expectEqual(t.expected, value);
    }
}

test "logical" {
    const gpa = testing.allocator;
    var discarding: Io.Writer.Discarding = .init(&.{});
    const w = &discarding.writer;

    const tests: []const struct {
        source: []const u8,
        expected: Value,
    } = &.{
        .{ .source = "true and true", .expected = .{ .boolean = true } },
        .{ .source = "true and false", .expected = .{ .boolean = false } },
        .{ .source = "true or false", .expected = .{ .boolean = true } },
        .{ .source = "false or false", .expected = .{ .boolean = false } },
        .{ .source = "!true", .expected = .{ .boolean = false } },
        .{ .source = "!false", .expected = .{ .boolean = true } },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test with source=\"{s}\"\n", .{t.source});
        const value = try testRun(gpa, t.source, w, w);
        try testing.expectEqual(t.expected, value);
    }
}

test "casting" {
    const gpa = testing.allocator;
    var discarding: Io.Writer.Discarding = .init(&.{});
    const w = &discarding.writer;

    const tests: []const struct {
        source: []const u8,
        expected: Value,
    } = &.{
        .{ .source = "float(2)", .expected = .{ .float = 2.0 } },
        .{ .source = "int(2.0)", .expected = .{ .integer = 2 } },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test with source=\"{s}\"\n", .{t.source});
        const value = try testRun(gpa, t.source, w, w);
        try testing.expectEqual(t.expected, value);
    }
}

test "string concatenation" {
    const gpa = testing.allocator;
    var discarding: Io.Writer.Discarding = .init(&.{});
    const w = &discarding.writer;

    const tests: []const struct {
        source: []const u8,
        expected: Value,
    } = &.{
        .{ .source = "\"Hello, \" + \"World\"", .expected = .{ .string = .ref("Hello, World") } },
        .{ .source = "\"Hello, \" + \"World\" + \"!\"", .expected = .{ .string = .ref("Hello, World!") } },
        .{ .source = "\"Hello\" + \", \" + \"World\" + \"!\"", .expected = .{ .string = .ref("Hello, World!") } },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test with source=\"{s}\"\n", .{t.source});
        var result = try testRunNoFree(gpa, t.source, w, w);
        defer result.chunk.deinit();
        try testing.expectEqualDeep(t.expected, result.value);
    }
}

test "string comparisons" {
    const gpa = testing.allocator;
    var discarding: Io.Writer.Discarding = .init(&.{});
    const w = &discarding.writer;

    const tests: []const struct {
        source: []const u8,
        expected: Value,
    } = &.{
        .{ .source = "\"2\" == \"2\"", .expected = .{ .boolean = true } },
        .{ .source = "\"2\" != \"2\"", .expected = .{ .boolean = false } },
        .{ .source = "\"Hey\" == \"Hey\"", .expected = .{ .boolean = true } },
        .{ .source = "\"ey\" == \"Hey\"", .expected = .{ .boolean = false } },
        .{ .source = "\"ey\" != \"Hey\"", .expected = .{ .boolean = true } },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test with source=\"{s}\"\n", .{t.source});
        const value = try testRun(gpa, t.source, w, w);
        try testing.expectEqual(t.expected, value);
    }
}

test "variables - simple store and load" {
    const gpa = testing.allocator;
    var discarding: Io.Writer.Discarding = .init(&.{});
    const w = &discarding.writer;

    const tests: []const struct {
        source: []const u8,
        expected: Value,
    } = &.{
        .{
            .source = "x := 10; x",
            .expected = .{ .integer = 10 },
        },
        .{
            .source = "x := 5; x + x",
            .expected = .{ .integer = 10 },
        },
        .{
            .source = "x := 10; y := x; y + 10",
            .expected = .{ .integer = 20 },
        },
        .{
            .source = "x := 10; y := x * 3 + 1; y + 10 == 41",
            .expected = .{ .boolean = true },
        },
        .{
            .source = "x :=\"Hello\";x",
            .expected = .{ .string = .ref("Hello") },
        },
        .{
            .source =
            \\hello := "Hello";
            \\world := "World";
            \\world
            ,
            .expected = .{ .string = .ref("World") },
        },
        .{
            .source =
            \\hello := "Hello";
            \\world := "World";
            \\hello
            ,
            .expected = .{ .string = .ref("Hello") },
        },
        .{
            .source =
            \\hello := "Hello";
            \\world := "World";
            \\hello + world
            ,
            .expected = .{ .string = .ref("HelloWorld") },
        },
        .{
            .source =
            \\hello := "Hello";
            \\world := "World";
            \\hello + ", " + world + "!"
            ,
            .expected = .{ .string = .ref("Hello, World!") },
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test with source=\"{s}\"\n", .{t.source});
        var result = try testRunNoFree(gpa, t.source, w, w);
        defer result.chunk.deinit();
        try testing.expectEqualDeep(t.expected, result.value);
    }
}

test "variables - irrelevance of declaration order" {
    const gpa = testing.allocator;
    var discarding: Io.Writer.Discarding = .init(&.{});
    const w = &discarding.writer;

    const tests: []const struct {
        source: []const u8,
        expected: Value,
    } = &.{.{
        .source = "x := y; y := 1; x",
        .expected = .{ .integer = 1 },
    }};

    for (tests) |t| {
        errdefer std.debug.print("failed test with source=\"{s}\"\n", .{t.source});
        var result = try testRunNoFree(gpa, t.source, w, w);
        defer result.chunk.deinit();
        try testing.expectEqualDeep(t.expected, result.value);
    }
}
