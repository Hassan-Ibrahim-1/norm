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

                .op_add => {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(valueAdd(a, b));
                },

                .op_subtract => {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(valueSubtract(a, b));
                },

                .op_multiply => {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(valueMultiply(a, b));
                },
                .op_divide => {
                    const b = vm.pop();
                    const a = vm.pop();
                    vm.push(valueDivide(a, b));
                },

                .op_negate => {
                    const a = vm.pop();
                    const negated: Value = switch (a) {
                        .float => |f| .{ .float = -f },
                        .integer => |i| .{ .integer = -i },
                        else => unreachable,
                    };
                    vm.push(negated);
                },

                .op_true => vm.push(.{ .boolean = true }),
                .op_false => vm.push(.{ .boolean = false }),
                .op_nil => vm.push(.nil),

                .op_equal => {
                    const b = vm.pop();
                    const a = vm.pop();
                    const value = switch (a) {
                        .integer => a.integer == b.integer,
                        .float => a.float == b.float,
                        .boolean => a.boolean == b.boolean,
                        .nil => true,
                    };
                    vm.push(.{ .boolean = value });
                },
                .op_not_equal => {
                    const b = vm.pop();
                    const a = vm.pop();
                    const value = switch (a) {
                        .integer => a.integer != b.integer,
                        .float => a.float != b.float,
                        .boolean => a.boolean != b.boolean,
                        .nil => false,
                    };
                    vm.push(.{ .boolean = value });
                },
                .op_greater => {
                    const b = vm.pop();
                    const a = vm.pop();
                    const value = switch (a) {
                        .integer => a.integer > b.integer,
                        .float => a.float > b.float,
                        .boolean, .nil => unreachable,
                    };
                    vm.push(.{ .boolean = value });
                },
                .op_greater_equal => {
                    const b = vm.pop();
                    const a = vm.pop();
                    const value = switch (a) {
                        .integer => a.integer >= b.integer,
                        .float => a.float >= b.float,
                        .boolean, .nil => unreachable,
                    };
                    vm.push(.{ .boolean = value });
                },
                .op_less => {
                    const b = vm.pop();
                    const a = vm.pop();
                    const value = switch (a) {
                        .integer => a.integer < b.integer,
                        .float => a.float < b.float,
                        .boolean, .nil => unreachable,
                    };
                    vm.push(.{ .boolean = value });
                },
                .op_less_equal => {
                    const b = vm.pop();
                    const a = vm.pop();
                    const value = switch (a) {
                        .integer => a.integer <= b.integer,
                        .float => a.float <= b.float,
                        .boolean, .nil => unreachable,
                    };
                    vm.push(.{ .boolean = value });
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

    fn pop(vm: *Vm) Value {
        vm.stack_top -= 1;
        return vm.stack_top[0];
    }
};

const cast = @import("cast.zig");

fn valueAdd(a: Value, b: Value) Value {
    return switch (a) {
        .float => |af| if (b == .integer)
            .{ .float = af + cast.as(f64, b.integer) }
        else
            .{ .float = af + b.float },

        .integer => |ai| if (b == .integer)
            .{ .integer = ai + b.integer }
        else
            .{ .float = cast.as(f64, ai) + b.float },
        else => unreachable,
    };
}

fn valueSubtract(a: Value, b: Value) Value {
    return switch (a) {
        .float => |af| if (b == .integer)
            .{ .float = af - cast.as(f64, b.integer) }
        else
            .{ .float = af - b.float },

        .integer => |ai| if (b == .integer)
            .{ .integer = ai - b.integer }
        else
            .{ .float = cast.as(f64, ai) - b.float },
        else => unreachable,
    };
}

fn valueMultiply(a: Value, b: Value) Value {
    return switch (a) {
        .float => |af| if (b == .integer)
            .{ .float = af * cast.as(f64, b.integer) }
        else
            .{ .float = af * b.float },

        .integer => |ai| if (b == .integer)
            .{ .integer = ai * b.integer }
        else
            .{ .float = cast.as(f64, ai) * b.float },
        else => unreachable,
    };
}

fn valueDivide(a: Value, b: Value) Value {
    return switch (a) {
        .float => |af| if (b == .integer)
            .{ .float = af / cast.as(f64, b.integer) }
        else
            .{ .float = af / b.float },

        .integer => |ai| if (b == .integer)
            .{ .float = cast.as(f64, ai) / cast.as(f64, b.integer) }
        else
            .{ .float = cast.as(f64, ai) / b.float },
        else => unreachable,
    };
}

fn testRun(
    gpa: Allocator,
    source: []const u8,
    stdout: *Io.Writer,
    stderr: *Io.Writer,
) !Value {
    var l = Lexer.init(source);

    var ast = parser.parse(gpa, &l);
    defer ast.arena.deinit();

    if (ast.errors.len > 0) {}

    var nir = sema.analyze(gpa, &ast);
    defer nir.arena.deinit();

    var chunk = compiler.compile(gpa, &nir);
    defer chunk.deinit(gpa);

    var vm = Vm.init(gpa, stdout, stderr);
    defer vm.deinit();

    return vm.interpret(&chunk);
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
