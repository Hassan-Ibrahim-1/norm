const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const testing = std.testing;
const Io = std.Io;
const builtin = @import("builtin");
const assert = std.debug.assert;

const debug = @import("debug.zig");
const dbg = debug.dbg;
const ers = @import("errors.zig");
const as = @import("cast.zig").as;
const Lexer = @import("Lexer.zig");
const Nir = @import("Nir.zig");
const NormType = Nir.NormType;
const parser = @import("parser.zig");
const sema = @import("sema.zig");
const Token = @import("Lexer.zig").Token;
const Value = @import("value.zig").Value;

pub const OpCode = enum(u8) {
    // Next byte is constant index.
    //
    // VM: Pop a constant off the stack.
    op_constant,
    // Next three little endian bytes represent constant index
    //
    // VM: Pop a constant off the stack
    op_constant_long,

    // VM: Pop two constants off the stack and then push one

    op_add_int,
    op_subtract_int,
    op_multiply_int,

    op_add_float,
    op_subtract_float,
    op_multiply_float,
    op_divide_float,

    // Concatenate two strings
    //
    // VM: Pop two strings off the stack and push a concatenated string
    op_concat,

    // Boolean values
    //
    // VM: Push `true` or `false` onto the stack
    op_true,
    op_false,

    // Negate an integer of a float
    //
    // VM: Pop a constant off the stack and then push negated
    op_negate_int,

    op_negate_float,

    // Comparison operators
    //
    // VM: Pop two constants off the stack, compare them, and then push a boolean value
    op_equal_int,
    op_not_equal_int,
    op_greater_int,
    op_greater_equal_int,
    op_less_int,
    op_less_equal_int,

    op_equal_float,
    op_not_equal_float,
    op_greater_float,
    op_greater_equal_float,
    op_less_float,
    op_less_equal_float,

    op_equal_string,
    op_not_equal_string,

    op_equal_bool,
    op_not_equal_bool,

    // Binary logical operators
    //
    // VM: Pop two constants off the stack and then push a boolean value
    op_and,
    op_or,

    // Unary logical operator
    //
    // VM: Pop off one constant and then push a boolean value
    op_not,

    // Nil value
    //
    // VM: Push nil on the stack
    op_nil,

    // Casting operators
    //
    // VM: Pop a constant off the stack and convert it to target type
    op_cast_to_int,
    op_cast_to_float,

    // Store a value into a variable
    // op_store <stack_slot: u16>
    //
    // VM: Pop a value off the stack and store it in the given <stack_slot>
    op_store,

    // Load the value of a variable
    // op_load <stack_slot: u16>
    //
    // VM: Push a the value stored at <stack_slot> onto the top of the stack
    op_load,

    // Temporary opcode to print a value
    //
    // VM: Pop a value off the stack and print it
    op_temp_print,

    // VM: Pop a value off the stack and discard it
    op_pop,

    // op_pop_n <n: u16>
    //
    // VM: Pop n values off the stack and discard then
    op_pop_n,

    // op_jump <offset: u16>
    //
    // Move forward by `offset` instructions
    op_jump,

    // op_jump_forward_if_false <offset: u16>
    //
    // Look at the top of the stack and if the value is false then
    // move forward by `offset` instructions
    op_jump_if_false,

    // op_loop <offset: u16>
    //
    // Move back by `offset` instructions
    op_loop,

    // Temporary: Signals end of execution
    op_return,

    pub fn byte(op: OpCode) u8 {
        return @intFromEnum(op);
    }
};

fn oom() noreturn {
    @panic("oom");
}

pub const Chunk = struct {
    gpa: Allocator,
    code: std.ArrayList(u8),
    lines: std.ArrayList(u32),
    constants: std.ArrayList(Value),

    string_arena: std.heap.ArenaAllocator,
    strings: std.StringHashMapUnmanaged(void),

    pub fn init(gpa: Allocator) Chunk {
        return .{
            .gpa = gpa,
            .string_arena = .init(gpa),
            .code = .empty,
            .lines = .empty,
            .constants = .empty,
            .strings = .empty,
        };
    }

    pub fn write(c: *Chunk, b: u8, line: u32) void {
        c.code.append(c.gpa, b) catch oom();
        c.lines.append(c.gpa, line) catch oom();
    }

    pub fn writeShort(c: *Chunk, b: u16, line: u32) void {
        const arr = c.code.addManyAsArray(c.gpa, 2) catch oom();
        mem.writeInt(u16, arr, b, .little);
        c.lines.append(c.gpa, line) catch oom();
        c.lines.append(c.gpa, line) catch oom();
    }

    pub fn writeOp(c: *Chunk, op: OpCode, line: u32) void {
        c.write(op.byte(), line);
    }

    pub fn writeConstant(c: *Chunk, value: Value, line: u32) void {
        const i = c.addConstant(value);
        if (i < std.math.maxInt(u8)) {
            c.writeOp(.op_constant, line);
            c.write(@intCast(i), line);
        } else {
            c.writeOp(.op_constant_long, line);
            c.lines.append(c.gpa, line) catch oom();
            c.lines.append(c.gpa, line) catch oom();
            const arr = c.code.addManyAsArray(c.gpa, 3) catch oom();
            mem.writeInt(u24, arr, @intCast(i), .little);
        }
    }

    pub fn writeString(c: *Chunk, s: []const u8, line: u32) void {
        const string = if (c.strings.getKey(s)) |existing| existing else string: {
            const arena = c.string_arena.allocator();
            const duped = arena.dupe(u8, s) catch oom();
            c.strings.put(arena, duped, {}) catch oom();
            break :string duped;
        };
        c.writeConstant(.{ .string = .ref(string) }, line);
    }

    fn addConstant(c: *Chunk, value: Value) usize {
        c.constants.append(c.gpa, value) catch oom();
        return c.constants.items.len - 1;
    }

    pub fn deinit(c: *Chunk) void {
        c.code.deinit(c.gpa);
        c.lines.deinit(c.gpa);
        c.constants.deinit(c.gpa);
        c.string_arena.deinit();
        c.* = undefined;
    }
};

pub const Compiler = struct {
    gpa: Allocator,
    scratch: Allocator,

    compiling_chunk: *Chunk,
    sym_table: *Nir.SymbolTable,
    stmts: []Nir.Stmt,

    current_scope: *Nir.Scope,

    // (scope, list of jump indexes to patch)
    unpatched_jump_stmts: std.AutoHashMapUnmanaged(*Nir.Scope, std.ArrayList(usize)),

    fn init(gpa: Allocator, scratch: Allocator, nir: *Nir, chunk: *Chunk) Compiler {
        return .{
            .gpa = gpa,
            .scratch = scratch,
            .current_scope = nir.sym_table.top_scope,
            .compiling_chunk = chunk,
            .unpatched_jump_stmts = .empty,
            .sym_table = &nir.sym_table,
            .stmts = nir.stmts,
        };
    }

    fn deinit(c: *Compiler) void {
        c.unpatched_jump_stmts.deinit(c.scratch);
        c.* = undefined;
    }

    fn compile(c: *Compiler) void {
        for (c.stmts) |stmt| {
            c.statement(stmt);
        }
    }

    fn statement(c: *Compiler, stmt: Nir.Stmt) void {
        switch (stmt) {
            .expression => |expr_stmt| {
                c.expression(expr_stmt.expr);
                if (!builtin.is_test) {
                    c.emitOpCode(.op_pop, expr_stmt.expr.token().line);
                }
            },
            .var_decl => |vd| c.expression(vd.value),
            .print => |p| {
                c.expression(p.expr);
                c.emitOpCode(.op_temp_print, p.print.line);
            },
            .block => |block| {
                c.beginScope(block.scope);
                defer c.endScope(block.end_token.line);
                for (block.stmts) |block_stmt| {
                    c.statement(block_stmt);
                }
            },
            .var_assign => |va| {
                const sym = c.findSym(va.ident.lexeme);
                c.expression(va.value);
                c.emitStore(@intCast(sym.stack_slot), va.ident.line);
                // We need this here because op_store does not pop values off the stack.
                c.emitOpCode(.op_pop, va.ident.line);
            },
            .if_stmt => |if_stmt| {
                const has_branches = if_stmt.else_block != null or if_stmt.else_if_blocks.len != 0;

                c.expression(if_stmt.condition);
                const if_false_jump = c.emitJump(.op_jump_if_false, if_stmt.token.line);

                c.statement(.{ .block = if_stmt.then_block });
                const if_true_jump =
                    if (has_branches) c.emitJump(.op_jump, if_stmt.then_block.end_token.line) else 0;

                c.patchJump(if_false_jump);

                const else_if_jumps =
                    c.scratch.alloc(usize, if_stmt.else_if_blocks.len) catch oom();
                defer c.scratch.free(else_if_jumps);

                for (if_stmt.else_if_blocks, 0..) |else_if, i| {
                    c.expression(else_if.condition);
                    const else_if_false_jump = c.emitJump(.op_jump_if_false, else_if.token.line);

                    c.statement(.{ .block = else_if.then_block });

                    const last_branch = if_stmt.else_block == null and i == if_stmt.else_if_blocks.len - 1;
                    else_if_jumps[i] = if (last_branch) 0 else c.emitJump(.op_jump, else_if.then_block.end_token.line);

                    c.patchJump(else_if_false_jump);
                }

                if (if_stmt.else_block) |else_block| {
                    c.statement(.{ .block = else_block });
                }

                if (has_branches) {
                    c.patchJump(if_true_jump);
                }

                for (else_if_jumps) |jump| {
                    if (jump == 0) continue;
                    c.patchJump(jump);
                }
            },
            .for_stmt => |for_stmt| {
                c.beginScope(for_stmt.scope);
                defer c.endScope(for_stmt.block.token.line);

                if (for_stmt.initializer) |initializer| {
                    const nir_init: Nir.Stmt = switch (initializer) {
                        .var_assign => |va| .{ .var_assign = va },
                        .var_decl => |vd| .{ .var_decl = vd },
                        .expr => |expr| .{ .expression = expr },
                    };
                    c.statement(nir_init);
                }
                const loop_index = c.compiling_chunk.code.items.len;
                c.expression(for_stmt.condition);
                const jump_index = c.emitJump(.op_jump_if_false, for_stmt.token.line);

                c.statement(.{ .block = for_stmt.block });

                if (for_stmt.increment) |increment| {
                    const nir_increment: Nir.Stmt = switch (increment) {
                        .var_assign => |va| .{ .var_assign = va },
                        .expr => |expr| .{ .expression = expr },
                    };
                    c.statement(nir_increment);
                }
                c.emitLoop(loop_index, for_stmt.block.token.line);
                c.patchJump(jump_index);
            },

            .condition_for => |condition_for| {
                c.beginScope(condition_for.scope);
                defer c.endScope(condition_for.block.token.line);

                const loop_index = c.compiling_chunk.code.items.len;
                c.expression(condition_for.condition);
                const jump_index = c.emitJump(.op_jump_if_false, condition_for.token.line);

                c.statement(.{ .block = condition_for.block });

                c.emitLoop(loop_index, condition_for.block.token.line);
                c.patchJump(jump_index);
            },

            .infinite_for => |infinite_for| {
                c.beginScope(infinite_for.scope);
                defer c.endScope(infinite_for.block.token.line);

                const loop_index = c.compiling_chunk.code.items.len;

                c.statement(.{ .block = infinite_for.block });

                c.emitLoop(loop_index, infinite_for.block.token.line);
            },

            .break_stmt => |break_stmt| {
                const jump_index = c.emitJump(.op_jump, break_stmt.token.line);
                const gop = c.unpatched_jump_stmts.getOrPut(c.scratch, break_stmt.jump_scope) catch oom();
                if (!gop.found_existing) {
                    gop.value_ptr.* = .empty;
                }
                gop.value_ptr.append(c.scratch, jump_index) catch oom();
            },
            .continue_stmt => |continue_stmt| {
                const jump_index = c.emitJump(.op_jump, continue_stmt.token.line);
                const gop = c.unpatched_jump_stmts.getOrPut(c.scratch, continue_stmt.jump_scope) catch oom();
                if (!gop.found_existing) {
                    gop.value_ptr.* = .empty;
                }
                gop.value_ptr.append(c.scratch, jump_index) catch oom();
            },
        }
    }

    /// returns the index of the jump opcode
    fn emitLoop(c: *Compiler, loop_index: usize, line: u32) void {
        c.emitOpCode(.op_loop, line);
        // + 2 to account for the short being emitted
        const offset = c.compiling_chunk.code.items.len - loop_index + 2;
        c.emitShort(@intCast(offset), line);
    }

    /// returns the index of the jump opcode
    fn emitJump(c: *Compiler, jump: OpCode, line: u32) usize {
        const jump_index = c.compiling_chunk.code.items.len;
        c.emitOpCode(jump, line);
        c.emitShort(0xff, line);
        return jump_index;
    }

    fn patchJump(c: *Compiler, jump_index: usize) void {
        const code = c.compiling_chunk.code.items;
        const offset_arg = code[jump_index + 1 .. jump_index + 3][0..2];
        // the -3 is there to account for vm reading the offset short first.
        const offset = as(u16, code.len - jump_index - 3);
        mem.writeInt(u16, offset_arg, offset, .little);
    }

    fn findSym(c: *Compiler, name: []const u8) *Nir.Symbol {
        return c.sym_table.find(name, c.current_scope);
    }

    fn beginScope(c: *Compiler, scope: *Nir.Scope) void {
        c.current_scope = scope;
    }

    fn endScope(c: *Compiler, line: u32) void {
        defer c.current_scope = c.current_scope.parent;
        const locals = c.sym_table.locals.get(c.current_scope).?;
        const local_count = locals.locals.count();

        const kv_maybe = c.unpatched_jump_stmts.fetchRemove(c.current_scope);
        if (kv_maybe) |kv| {
            var stmts = kv.value;
            defer stmts.deinit(c.scratch);
            for (stmts.items) |stmt| {
                c.patchJump(stmt);
            }
        }

        switch (local_count) {
            0 => {},
            1 => c.emitOpCode(.op_pop, line),
            else => {
                c.emitOpCode(.op_pop_n, line);
                c.emitShort(@intCast(local_count), line);
            },
        }
    }

    fn decl(c: *Compiler, vd: Nir.Stmt.VarDecl) void {
        c.expression(vd.value);
    }

    fn identifier(c: *Compiler, i: *Nir.Expr.Identifier) void {
        const sym = c.sym_table.find(i.ident.lexeme, c.current_scope);
        const stack_slot: u16 = @intCast(sym.stack_slot);
        c.emitLoad(stack_slot, i.ident.line);
    }

    fn expression(c: *Compiler, expr: *Nir.Expr) void {
        switch (expr.kind) {
            .binary => |*b| c.binary(b),
            .unary => |*u| c.unary(u),
            .cast => |*ca| c.cast(ca, expr.type),
            .grouping => |*g| c.grouping(g),
            .literal => |*l| c.literal(l),
            .identifier => |*i| c.identifier(i),
        }
    }

    fn binary(c: *Compiler, b: *Nir.Expr.Binary) void {
        c.expression(b.left);
        c.expression(b.right);

        const line = b.operator.line;
        const operand_type = b.left.type;
        const op_code: OpCode = switch (b.operator.type) {
            .plus => switch (operand_type) {
                .n_string => .op_concat,
                .n_float => .op_add_float,
                .n_int => .op_add_int,
                else => unreachable,
            },
            .minus => switch (operand_type) {
                .n_float => .op_subtract_float,
                .n_int => .op_subtract_int,
                else => unreachable,
            },
            .star => switch (operand_type) {
                .n_float => .op_multiply_float,
                .n_int => .op_multiply_int,
                else => unreachable,
            },
            .slash => .op_divide_float,
            .equal_equal => switch (operand_type) {
                .n_string => .op_equal_string,
                .n_float => .op_equal_float,
                .n_int => .op_equal_int,
                .n_bool => .op_equal_bool,
                else => unreachable,
            },
            .bang_equal => switch (operand_type) {
                .n_string => .op_not_equal_string,
                .n_float => .op_not_equal_float,
                .n_int => .op_not_equal_int,
                .n_bool => .op_not_equal_bool,
                else => unreachable,
            },
            .greater => switch (operand_type) {
                .n_float => .op_greater_float,
                .n_int => .op_greater_int,
                else => unreachable,
            },
            .greater_equal => switch (operand_type) {
                .n_float => .op_greater_equal_float,
                .n_int => .op_greater_equal_int,
                else => unreachable,
            },
            .less => switch (operand_type) {
                .n_float => .op_less_float,
                .n_int => .op_less_int,
                else => unreachable,
            },
            .less_equal => switch (operand_type) {
                .n_float => .op_less_equal_float,
                .n_int => .op_less_equal_int,
                else => unreachable,
            },
            .kw_and => .op_and,
            .kw_or => .op_or,
            else => unreachable,
        };
        c.emitOpCode(op_code, line);
    }

    fn unary(c: *Compiler, u: *Nir.Expr.Unary) void {
        c.expression(u.expr);
        const line = u.operator.line;
        const ty = u.expr.type;
        switch (u.operator.type) {
            .minus => {
                const op_code: OpCode = switch (ty) {
                    .n_float => .op_negate_float,
                    .n_int => .op_negate_int,
                    else => unreachable,
                };
                c.emitOpCode(op_code, line);
            },
            .bang => c.emitOpCode(.op_not, line),
            else => unreachable,
        }
    }

    fn cast(c: *Compiler, cst: *Nir.Expr.Cast, target: NormType) void {
        c.expression(cst.expr);
        const line = cst.token.line;
        const op_code: OpCode = switch (target) {
            .n_int => .op_cast_to_int,
            .n_float => .op_cast_to_float,
            else => unreachable,
        };
        c.emitOpCode(op_code, line);
    }

    fn grouping(c: *Compiler, g: *Nir.Expr.Grouping) void {
        c.expression(g.expr);
    }

    fn literal(c: *Compiler, l: *Nir.Expr.Literal) void {
        const line = l.token.line;
        switch (l.value) {
            .float => |f| c.emitConstant(.{ .float = f }, line),
            .integer => |i| c.emitConstant(.{ .integer = i }, line),
            .boolean => |b| if (b)
                c.emitOpCode(.op_true, line)
            else
                c.emitOpCode(.op_false, line),
            .string => |s| c.emitString(s, line),
            .nil => c.emitOpCode(.op_nil, line),
        }
    }

    fn emitConstant(c: *Compiler, value: Value, line: u32) void {
        c.compiling_chunk.writeConstant(value, line);
    }

    fn emitString(c: *Compiler, s: []const u8, line: u32) void {
        c.compiling_chunk.writeString(s, line);
    }

    fn emitOpCode(c: *Compiler, op: OpCode, line: u32) void {
        c.emitByte(op.byte(), line);
    }

    fn emitOpCodes(c: *Compiler, op1: OpCode, op2: OpCode, line: u32) void {
        c.emitOpCode(op1, line);
        c.emitOpCode(op2, line);
    }

    fn emitStore(c: *Compiler, stack_slot: u16, line: u32) void {
        c.emitOpCode(.op_store, line);
        c.emitShort(stack_slot, line);
    }

    fn emitLoad(c: *Compiler, stack_slot: u16, line: u32) void {
        c.emitOpCode(.op_load, line);
        c.emitShort(stack_slot, line);
    }

    fn emitByte(c: *Compiler, b: u8, line: u32) void {
        c.compiling_chunk.write(b, line);
    }

    fn emitShort(c: *Compiler, b: u16, line: u32) void {
        c.compiling_chunk.writeShort(b, line);
    }

    fn emitBytes(c: *Compiler, a: u8, b: u8) void {
        c.emitByte(a);
        c.emitByte(b);
    }
};

pub fn compile(gpa: Allocator, nir: *Nir) Chunk {
    if (nir.errors.len > 0) return .init(gpa);

    var chunk: Chunk = .init(gpa);
    var c: Compiler = .init(gpa, gpa, nir, &chunk);
    defer c.deinit();

    c.compile();

    c.emitOpCode(.op_return, 0);

    return chunk;
}

test "basic chunk ops" {
    const gpa = testing.allocator;
    var chunk: Chunk = .init(gpa);
    defer chunk.deinit();

    chunk.writeConstant(.{ .integer = 10 }, 1);
    chunk.writeConstant(.{ .float = 3.14 }, 1);
    chunk.writeOp(.op_return, 2);

    const expected_code = [_]u8{
        OpCode.op_constant.byte(), 0,
        OpCode.op_constant.byte(), 1,
        OpCode.op_return.byte(),
    };
    const expected_lines = [_]u32{ 1, 1, 1, 1, 2 };
    const expected_constants = [_]Value{ .{ .integer = 10 }, .{ .float = 3.14 } };

    try testing.expectEqualSlices(u8, &expected_code, chunk.code.items);
    try testing.expectEqualSlices(u32, &expected_lines, chunk.lines.items);
    try testing.expectEqualSlices(Value, &expected_constants, chunk.constants.items);
}

fn testCompile(gpa: Allocator, source: []const u8) !Chunk {
    var l = Lexer.init(source);
    var tokens = l.scanTokens(gpa);
    defer tokens.deinit(gpa);
    if (tokens.errors.len > 0) {
        dbg("tokens.errors", tokens.errors);
        return error.LexerError;
    }

    var ast = parser.parse(gpa, tokens.tokens);
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

    return compile(gpa, &nir);
}

const TestCase = struct {
    source: []const u8,
    expected_code: []const u8,
    expected_lines: []const u32,
    expected_constants: []const Value,
};

const TestCaseMinimal = struct {
    source: []const u8,
    expected_code: []const u8,
    expected_constants: []const Value,
};

// TODO: test "chunk long instruction" {}

test "literals" {
    const gpa = testing.allocator;
    const tests: []const TestCase = &.{
        .{
            .source = "2",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_return }),
            .expected_lines = &.{ 1, 1, 0 },
            .expected_constants = &.{.{ .integer = 2 }},
        },
        .{
            .source = "3.0",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_return }),
            .expected_lines = &.{ 1, 1, 0 },
            .expected_constants = &.{.{ .float = 3.0 }},
        },
        .{
            .source = "true",
            .expected_code = &debug.opCodeToBytes(&.{ .op_true, .op_return }),
            .expected_lines = &.{ 1, 0 },
            .expected_constants = &.{},
        },
        .{
            .source = "false",
            .expected_code = &debug.opCodeToBytes(&.{ .op_false, .op_return }),
            .expected_lines = &.{ 1, 0 },
            .expected_constants = &.{},
        },
        // .{
        //     .source = "nil",
        //     .expected_code = &debug.opCodeToBytes(&.{ .op_nil, .op_return }),
        //     .expected_lines = &.{ 1, 0 },
        //     .expected_constants = &.{},
        // },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source = \"{s}\"\n", .{t.source});
        var chunk = try testCompile(gpa, t.source);
        defer chunk.deinit();

        try testing.expectEqualSlices(u8, t.expected_code, chunk.code.items);
        try testing.expectEqualSlices(u32, t.expected_lines, chunk.lines.items);
        try testing.expectEqualSlices(Value, t.expected_constants, chunk.constants.items);
    }
}

test "arithmetic expressions" {
    const gpa = testing.allocator;
    const tests: []const TestCase = &.{
        .{
            .source = "2 + 3",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_add_int, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .integer = 3 } },
        },
        .{
            .source = "2.0 + 3.0",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_add_float, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .float = 2 }, .{ .float = 3 } },
        },
        .{
            .source = "(2.0 + 3.0) - 4.0 / 2.0",
            .expected_code = &debug.opCodeToBytes(&.{
                .op_constant,
                0,
                .op_constant,
                1,
                .op_add_float,
                .op_constant,
                2,
                .op_constant,
                3,
                .op_divide_float,
                .op_subtract_float,
                .op_return,
            }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .float = 2.0 }, .{ .float = 3 }, .{ .float = 4.0 }, .{ .float = 2.0 } },
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source = \"{s}\"\n", .{t.source});
        var chunk = try testCompile(gpa, t.source);
        defer chunk.deinit();

        try testing.expectEqualSlices(u8, t.expected_code, chunk.code.items);
        try testing.expectEqualSlices(u32, t.expected_lines, chunk.lines.items);
        try testing.expectEqualSlices(Value, t.expected_constants, chunk.constants.items);
    }
}

test "auto cast arithmetic expressions" {
    const gpa = testing.allocator;
    const tests: []const TestCase = &.{
        .{
            .source = "2 + 3.0",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_cast_to_float, .op_constant, 1, .op_add_float, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .float = 3 } },
        },
        .{
            .source = "2 * 3.0",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_cast_to_float, .op_constant, 1, .op_multiply_float, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .float = 3 } },
        },
        .{
            .source = "2.0 / 3",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_cast_to_float, .op_divide_float, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .float = 2 }, .{ .integer = 3 } },
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source = \"{s}\"\n", .{t.source});
        var chunk = try testCompile(gpa, t.source);
        defer chunk.deinit();

        try testing.expectEqualSlices(u8, t.expected_code, chunk.code.items);
        try testing.expectEqualSlices(u32, t.expected_lines, chunk.lines.items);
        try testing.expectEqualSlices(Value, t.expected_constants, chunk.constants.items);
    }
}

test "comparison" {
    const gpa = testing.allocator;
    const tests: []const TestCase = &.{
        // TODO: use something other than numbers
        .{
            .source = "2 < 3",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_less_int, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .integer = 3 } },
        },
        .{
            .source = "2 <= 3",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_less_equal_int, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .integer = 3 } },
        },
        .{
            .source = "2 > 3",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_greater_int, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .integer = 3 } },
        },
        .{
            .source = "2 >= 3",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_greater_equal_int, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .integer = 3 } },
        },
        .{
            .source = "2 == 3",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_equal_int, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .integer = 3 } },
        },
        .{
            .source = "2 != 3",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_not_equal_int, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .integer = 2 }, .{ .integer = 3 } },
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source = \"{s}\"\n", .{t.source});
        var chunk = try testCompile(gpa, t.source);
        defer chunk.deinit();

        try testing.expectEqualSlices(u8, t.expected_code, chunk.code.items);
        try testing.expectEqualSlices(u32, t.expected_lines, chunk.lines.items);
        try testing.expectEqualSlices(Value, t.expected_constants, chunk.constants.items);
    }
}

test "logical" {
    const gpa = testing.allocator;
    const tests: []const TestCase = &.{
        .{
            .source = "true and true",
            .expected_code = &debug.opCodeToBytes(&.{ .op_true, .op_true, .op_and, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 0 },
            .expected_constants = &.{},
        },
        .{
            .source = "true or false",
            .expected_code = &debug.opCodeToBytes(&.{ .op_true, .op_false, .op_or, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 0 },
            .expected_constants = &.{},
        },
        .{
            .source = "!false",
            .expected_code = &debug.opCodeToBytes(&.{ .op_false, .op_not, .op_return }),
            .expected_lines = &.{ 1, 1, 0 },
            .expected_constants = &.{},
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source = \"{s}\"\n", .{t.source});
        var chunk = try testCompile(gpa, t.source);
        defer chunk.deinit();

        try testing.expectEqualSlices(u8, t.expected_code, chunk.code.items);
        try testing.expectEqualSlices(u32, t.expected_lines, chunk.lines.items);
        try testing.expectEqualSlices(Value, t.expected_constants, chunk.constants.items);
    }
}

test "casting" {
    const gpa = testing.allocator;
    const tests: []const TestCase = &.{
        .{
            .source = "float(2)",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_cast_to_float, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 0 },
            .expected_constants = &.{.{ .integer = 2 }},
        },
        .{
            .source = "int(2.0)",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_cast_to_int, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 0 },
            .expected_constants = &.{.{ .float = 2.0 }},
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source = \"{s}\"\n", .{t.source});
        var chunk = try testCompile(gpa, t.source);
        defer chunk.deinit();

        try testing.expectEqualSlices(u8, t.expected_code, chunk.code.items);
        try testing.expectEqualSlices(u32, t.expected_lines, chunk.lines.items);
        try testing.expectEqualSlices(Value, t.expected_constants, chunk.constants.items);
    }
}

test "string concatenation" {
    const gpa = testing.allocator;
    const tests: []const TestCase = &.{
        .{
            .source = "\"Hello, \" + \"World\"",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_constant, 1, .op_concat, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 1, 1, 0 },
            .expected_constants = &.{ .{ .string = .ref("Hello, ") }, .{ .string = .ref("World") } },
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source = \"{s}\"\n", .{t.source});
        var chunk = try testCompile(gpa, t.source);
        defer chunk.deinit();

        try testing.expectEqualSlices(u8, t.expected_code, chunk.code.items);
        try testing.expectEqualSlices(u32, t.expected_lines, chunk.lines.items);
        try testing.expectEqualDeep(t.expected_constants, chunk.constants.items);
    }
}

test "global variables - simple op_store" {
    const gpa = testing.allocator;
    const tests: []const TestCase = &.{
        .{
            .source = "x := 10;",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_return }),
            .expected_lines = &.{ 1, 1, 0 },
            .expected_constants = &.{.{ .integer = 10 }},
        },
        .{
            .source = "x := 10; y := false;",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_false, .op_return }),
            .expected_lines = &.{ 1, 1, 1, 0 },
            .expected_constants = &.{.{ .integer = 10 }},
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source = \"{s}\"\n", .{t.source});
        var chunk = try testCompile(gpa, t.source);
        defer chunk.deinit();

        try testing.expectEqualSlices(u8, t.expected_code, chunk.code.items);
        try testing.expectEqualSlices(u32, t.expected_lines, chunk.lines.items);
        try testing.expectEqualDeep(t.expected_constants, chunk.constants.items);
    }
}

test "global variables - op_load" {
    const gpa = testing.allocator;
    // zig fmt: off
    const tests: []const TestCaseMinimal = &.{
        .{
            .source = "x := 10; x;",
            .expected_code = &debug.opCodeToBytes(&.{
                .op_constant, 0,
                .op_load, 0, 0,
                .op_return,
            }),
            .expected_constants = &.{.{ .integer = 10 }},
        },
        .{
            .source = "x := 10; y := x;",
            .expected_code = &debug.opCodeToBytes(&.{
                .op_constant, 0,
                .op_load, 0, 0,
                .op_return,
            }),
            .expected_constants = &.{.{ .integer = 10 }},
        },
        .{
            .source = "x := 10; y := x; x + y",
            .expected_code = &debug.opCodeToBytes(&.{
                .op_constant, 0,
                .op_load, 0, 0,
                .op_load, 0, 0,
                .op_load, 1, 0,
                .op_add_int,
                .op_return,
            }),
            .expected_constants = &.{.{ .integer = 10 }},
        },
        .{
            .source =
            \\hello := "Hello";
            \\world := "World";
            \\hello
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                .op_constant, 0,
                .op_constant, 1,
                .op_load, 0, 0,
                .op_return,
            }),
            .expected_constants = &.{ .{ .string = .ref("Hello") }, .{ .string = .ref("World") } }
        }
    };
    // zig fmt: on

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source = \"{s}\"\n", .{t.source});
        var chunk = try testCompile(gpa, t.source);
        defer chunk.deinit();

        try testing.expectEqualSlices(u8, t.expected_code, chunk.code.items);
        try testing.expectEqualDeep(t.expected_constants, chunk.constants.items);
    }
}

test "local variables and scopes - op_load, op_store and scope popping" {
    @setEvalBranchQuota(20000);

    const gpa = testing.allocator;
    // zig fmt: off
    const tests: []const TestCaseMinimal = &.{
        .{
            .source =
            \\x := 1;
            \\{
            \\    y := x + 2;
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                // x
                .op_constant, 0,

                // y
                .op_load, 0, 0,
                .op_constant, 1,
                .op_add_int,

                .op_pop,

                .op_return,
            }),
            .expected_constants = &.{ .{ .integer = 1 }, .{ .integer = 2 } },
        },
        .{
            .source =
            \\x := 1;
            \\{
            \\    y := x + 2;
            \\}
            \\z := false;
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                // x
                .op_constant, 0,

                // z
                .op_false,

                // y
                .op_load, 0, 0,
                .op_constant, 1,
                .op_add_int,

                .op_pop,

                .op_return,
            }),
            .expected_constants = &.{ .{ .integer = 1 }, .{ .integer = 2 } },
        },
        .{
            .source =
            \\{
            \\    a := 1;
            \\    {
            \\        b := a + 1;
            \\        c := a + b;
            \\    }
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                // a
                .op_constant, 0,

                // b
                .op_load, 0, 0,
                .op_constant, 1,
                .op_add_int,

                // c
                .op_load, 0, 0,
                .op_load, 1, 0,
                .op_add_int,

                .op_pop_n, 2, 0,
                .op_pop,

                .op_return,
            }),
            .expected_constants = &.{ .{ .integer = 1 }, .{ .integer = 1 } },
        },
        .{
            .source =
            \\{
            \\    a := 1;
            \\    {
            \\        b := a + 1;
            \\        c := a + b;
            \\    }
            \\    c := true;
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                // a
                .op_constant, 0,

                // b
                .op_load, 0, 0,
                .op_constant, 1,
                .op_add_int,

                // c1
                .op_load, 0, 0,
                .op_load, 1, 0,
                .op_add_int,

                .op_pop_n, 2, 0,

                // c2
                .op_true,

                .op_pop_n, 2, 0,

                .op_return,
            }),
            .expected_constants = &.{ .{ .integer = 1 }, .{ .integer = 1 } },
        },
        .{
            .source =
            \\{
            \\    a := 1;
            \\    {
            \\        b := a + 1;
            \\        c := a + b;
            \\    }
            \\    c := true;
            \\}
            \\z := 0;
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                // z
                .op_constant, 0,

                // a
                .op_constant, 1,

                // b
                .op_load, 1, 0,
                .op_constant, 2,
                .op_add_int,

                // c1
                .op_load, 1, 0,
                .op_load, 2, 0,
                .op_add_int,

                .op_pop_n, 2, 0,

                // c2
                .op_true,

                .op_pop_n, 2, 0,

                .op_return,
            }),
            .expected_constants = &.{ .{ .integer = 0 }, .{ .integer = 1 }, .{ .integer = 1 } },
        },
    };
    // zig fmt: on

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source = \"{s}\"\n", .{t.source});
        var chunk = try testCompile(gpa, t.source);
        defer chunk.deinit();

        try testing.expectEqualSlices(u8, t.expected_code, chunk.code.items);
        try testing.expectEqualDeep(t.expected_constants, chunk.constants.items);
    }
}

test "local variable assignment" {
    @setEvalBranchQuota(20000);

    const gpa = testing.allocator;
    // zig fmt: off
    const tests: []const TestCaseMinimal = &.{
        .{
            .source =
            \\{
            \\    mut y := 1;
            \\    y = 2;
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                // mut y := 1
                .op_constant, 0,

                // y = 2;
                .op_constant, 1,
                .op_store, 0, 0,
                .op_pop,

                .op_pop,

                .op_return,
            }),
            .expected_constants = &.{ .{ .integer = 1 }, .{ .integer = 2 } },
        },
        .{
            .source =
            \\{
            \\    mut y := 1;
            \\    y += 1;
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                // mut y := 1
                .op_constant, 0,

                // y += 1;
                .op_load, 0, 0,
                .op_constant, 1,
                .op_add_int,
                .op_store, 0, 0,
                .op_pop,

                .op_pop,

                .op_return,
            }),
            .expected_constants = &.{ .{ .integer = 1 }, .{ .integer = 1 } },
        },
        .{
            .source =
            \\{
            \\    mut y := 1;
            \\    x := 3;
            \\    y += x;
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                // mut y := 1
                .op_constant, 0,

                // x := 3
                .op_constant, 1,

                // y += x;
                .op_load, 0, 0,
                .op_load, 1, 0,
                .op_add_int,
                .op_store, 0, 0,
                .op_pop,

                .op_pop_n, 2, 0,

                .op_return,
            }),
            .expected_constants = &.{ .{ .integer = 1 }, .{ .integer = 3 } },
        },
    };
    // zig fmt: on

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source = \"{s}\"\n", .{t.source});
        var chunk = try testCompile(gpa, t.source);
        defer chunk.deinit();

        try testing.expectEqualSlices(u8, t.expected_code, chunk.code.items);
        try testing.expectEqualDeep(t.expected_constants, chunk.constants.items);
    }
}

test "if statements no variables" {
    @setEvalBranchQuota(100000);
    const gpa = testing.allocator;
    // zig fmt: off
    const tests: []const TestCaseMinimal = &.{
        .{
            .source =
            \\if 2 > 1 {
            \\ print(2);
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                .op_constant, 0,
                .op_constant, 1,
                .op_greater_int,
                .op_jump_if_false, 3, 0,
                .op_constant, 2,
                .op_temp_print,
                .op_return,
            }),
            .expected_constants = &.{.{ .integer = 2 }, .{ .integer = 1 }, .{ .integer = 2 }},
        },
        .{
            .source =
            \\if 2 > 1 {
            \\ print(2);
            \\} else {
            \\ print(1);
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                .op_constant, 0,
                .op_constant, 1,
                .op_greater_int,
                .op_jump_if_false, 6, 0,
                .op_constant, 2,
                .op_temp_print,
                .op_jump, 3, 0,
                .op_constant, 3,
                .op_temp_print,
                .op_return,
            }),
            .expected_constants = &.{.{ .integer = 2 }, .{ .integer = 1 }, .{ .integer = 2 }, .{.integer = 1}},
        },
        .{
            .source =
            \\if 2 > 1 {
            \\ print(2);
            \\} else if 1 < 3 {
            \\ print(1);
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                .op_constant, 0,
                .op_constant, 1,
                .op_greater_int,
                .op_jump_if_false, 6, 0,
                .op_constant, 2,
                .op_temp_print,
                .op_jump, 11, 0,
                .op_constant, 3,
                .op_constant, 4,
                .op_less_int,
                .op_jump_if_false, 3, 0,
                .op_constant, 5,
                .op_temp_print,
                .op_return,
            }),
            .expected_constants = &.{
                .{ .integer = 2 },
                .{ .integer = 1 },
                .{ .integer = 2 },
                .{ .integer = 1 },
                .{ .integer = 3 },
                .{ .integer = 1 },
            },
        },
        .{
            .source =
            \\if 2 > 1 {
            \\ print(2);
            \\} else if 1 < 3 {
            \\ print(1);
            \\} else {
            \\  print(5);
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                .op_constant, 0,
                .op_constant, 1,
                .op_greater_int,
                .op_jump_if_false, 6, 0,
                .op_constant, 2,
                .op_temp_print,
                .op_jump, 17, 0,
                .op_constant, 3,
                .op_constant, 4,
                .op_less_int,
                .op_jump_if_false, 6, 0,
                .op_constant, 5,
                .op_temp_print,
                .op_jump, 3, 0,
                .op_constant, 6,
                .op_temp_print,
                .op_return,
            }),
            .expected_constants = &.{
                .{ .integer = 2 },
                .{ .integer = 1 },
                .{ .integer = 2 },
                .{ .integer = 1 },
                .{ .integer = 3 },
                .{ .integer = 1 },
                .{ .integer = 5 },
            },
        },
        .{
            .source =
            \\if true {
            \\    if false {
            \\        print(1);
            \\    }
            \\    print(2);
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                .op_true,
                .op_jump_if_false, 10, 0,
                .op_false,
                .op_jump_if_false, 3, 0,
                .op_constant, 0,
                .op_temp_print,
                .op_constant, 1,
                .op_temp_print,
                .op_return,
            }),
            .expected_constants = &.{ .{ .integer = 1 }, .{ .integer = 2 } },
        },
        .{
            .source =
            \\if true {
            \\    print(1);
            \\    print(2);
            \\    print(3);
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                .op_true,
                .op_jump_if_false, 9, 0,
                .op_constant, 0,
                .op_temp_print,
                .op_constant, 1,
                .op_temp_print,
                .op_constant, 2,
                .op_temp_print,
                .op_return,
            }),
            .expected_constants = &.{ .{ .integer = 1 }, .{ .integer = 2 }, .{ .integer = 3 } },
        },
        .{
            .source =
            \\if false {
            \\    print(1);
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                .op_false,
                .op_jump_if_false, 3, 0,
                .op_constant, 0,
                .op_temp_print,
                .op_return,
            }),
            .expected_constants = &.{.{ .integer = 1 }},
        },
        .{
            .source =
            \\if false {
            \\    print(1);
            \\} else {
            \\    print(2);
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                .op_false,
                .op_jump_if_false, 6, 0,
                .op_constant, 0,
                .op_temp_print,
                .op_jump, 3, 0,
                .op_constant, 1,
                .op_temp_print,
                .op_return,
            }),
            .expected_constants = &.{ .{ .integer = 1 }, .{ .integer = 2 } },
        },
        .{
            .source =
            \\if 1 > 2 {
            \\    print(1);
            \\} else if 2 > 3 {
            \\    print(2);
            \\} else if 3 > 4 {
            \\    print(3);
            \\} else {
            \\    print(4);
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                .op_constant, 0,
                .op_constant, 1,
                .op_greater_int,
                .op_jump_if_false, 6, 0,
                .op_constant, 2,
                .op_temp_print,
                .op_jump, 31, 0,
                .op_constant, 3,
                .op_constant, 4,
                .op_greater_int,
                .op_jump_if_false, 6, 0,
                .op_constant, 5,
                .op_temp_print,
                .op_jump, 17, 0,
                .op_constant, 6,
                .op_constant, 7,
                .op_greater_int,
                .op_jump_if_false, 6, 0,
                .op_constant, 8,
                .op_temp_print,
                .op_jump, 3, 0,
                .op_constant, 9,
                .op_temp_print,
                .op_return,
            }),
            .expected_constants = &.{
                .{ .integer = 1 },
                .{ .integer = 2 },
                .{ .integer = 1 },
                .{ .integer = 2 },
                .{ .integer = 3 },
                .{ .integer = 2 },
                .{ .integer = 3 },
                .{ .integer = 4 },
                .{ .integer = 3 },
                .{ .integer = 4 },
            },
        },
        .{
            .source =
            \\if true and false {
            \\    print(1);
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                .op_true,
                .op_false,
                .op_and,
                .op_jump_if_false, 3, 0,
                .op_constant, 0,
                .op_temp_print,
                .op_return,
            }),
            .expected_constants = &.{.{ .integer = 1 }},
        },
        .{
            .source =
            \\if true or false {
            \\    print(1);
            \\} else {
            \\    print(2);
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                .op_true,
                .op_false,
                .op_or,
                .op_jump_if_false, 6, 0,
                .op_constant, 0,
                .op_temp_print,
                .op_jump, 3, 0,
                .op_constant, 1,
                .op_temp_print,
                .op_return,
            }),
            .expected_constants = &.{ .{ .integer = 1 }, .{ .integer = 2 } },
        },
        .{
            .source =
            \\if false {
            \\    print(1);
            \\} else if false {
            \\    print(2);
            \\} else if false {
            \\    print(3);
            \\} else if false {
            \\    print(4);
            \\} else if true {
            \\    print(5);
            \\} else {
            \\    print(6);
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                .op_false,
                .op_jump_if_false, 6, 0,
                .op_constant, 0,
                .op_temp_print,
                .op_jump, 43, 0,
                .op_false,
                .op_jump_if_false, 6, 0,
                .op_constant, 1,
                .op_temp_print,
                .op_jump, 33, 0,
                .op_false,
                .op_jump_if_false, 6, 0,
                .op_constant, 2,
                .op_temp_print,
                .op_jump, 23, 0,
                .op_false,
                .op_jump_if_false, 6, 0,
                .op_constant, 3,
                .op_temp_print,
                .op_jump, 13, 0,
                .op_true,
                .op_jump_if_false, 6, 0,
                .op_constant, 4,
                .op_temp_print,
                .op_jump, 3, 0,
                .op_constant, 5,
                .op_temp_print,
                .op_return,
            }),
            .expected_constants = &.{
                .{ .integer = 1 },
                .{ .integer = 2 },
                .{ .integer = 3 },
                .{ .integer = 4 },
                .{ .integer = 5 },
                .{ .integer = 6 },
            },
        },
    };
    // zig fmt: on

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source = \"{s}\"\n", .{t.source});
        var chunk = try testCompile(gpa, t.source);
        defer chunk.deinit();

        try testing.expectEqualSlices(u8, t.expected_code, chunk.code.items);
        try testing.expectEqualDeep(t.expected_constants, chunk.constants.items);
    }
}

test "if statements with variable declarations - scope ends properly" {
    @setEvalBranchQuota(100000);
    const gpa = testing.allocator;
    // zig fmt: off
    const tests: []const TestCaseMinimal = &.{
        .{
            .source =
            \\if true {
            \\    x := 1;
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                .op_true,
                .op_jump_if_false, 3, 0,
                .op_constant, 0,
                .op_pop,
                .op_return,
            }),
            .expected_constants = &.{.{ .integer = 1 }},
        },
        .{
            .source =
            \\if false {
            \\    x := 1;
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                .op_false,
                .op_jump_if_false, 3, 0,
                .op_constant, 0,
                .op_pop,
                .op_return,
            }),
            .expected_constants = &.{.{ .integer = 1 }},
        },
        .{
            .source =
            \\x := 1;
            \\if true {
            \\    y := 2;
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                // x
                .op_constant, 0,
                // if block
                .op_true,
                .op_jump_if_false, 3, 0,
                .op_constant, 1,
                .op_pop,
                .op_return,
            }),
            .expected_constants = &.{ .{ .integer = 1 }, .{ .integer = 2 } },
        },
        .{
            .source =
            \\if true {
            \\    x := 1;
            \\} else {
            \\    x := 2;
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                .op_true,
                .op_jump_if_false, 6, 0,
                .op_constant, 0,
                .op_pop,
                .op_jump, 3, 0,
                .op_constant, 1,
                .op_pop,
                .op_return,
            }),
            .expected_constants = &.{ .{ .integer = 1 }, .{ .integer = 2 } },
        },
        .{
            .source =
            \\{
            \\    a := 1;
            \\    if true {
            \\        b := 2;
            \\    }
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                // a
                .op_constant, 0,
                // if block
                .op_true,
                .op_jump_if_false, 3, 0,
                .op_constant, 1,
                .op_pop,
                // scope end
                .op_pop,
                .op_return,
            }),
            .expected_constants = &.{ .{ .integer = 1 }, .{ .integer = 2 } },
        },
        .{
            .source =
            \\if true {
            \\    if false {
            \\        x := 1;
            \\    }
            \\    y := 2;
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                .op_true,
                .op_jump_if_false, 10, 0,
                .op_false,
                .op_jump_if_false, 3, 0,
                .op_constant, 0,
                .op_pop,
                .op_constant, 1,
                .op_pop,
                .op_return,
            }),
            .expected_constants = &.{ .{ .integer = 1 }, .{ .integer = 2 } },
        },
        .{
            .source =
            \\if true and false {
            \\    x := 1;
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                .op_true,
                .op_false,
                .op_and,
                .op_jump_if_false, 3, 0,
                .op_constant, 0,
                .op_pop,
                .op_return,
            }),
            .expected_constants = &.{.{ .integer = 1 }},
        },
        .{
            .source =
            \\x := 1;
            \\if x > 0 {
            \\    y := x + 1;
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                // x
                .op_constant, 0,
                // condition
                .op_load, 0, 0,
                .op_constant, 1,
                .op_greater_int,
                // if block
                .op_jump_if_false, 7, 0,
                .op_load, 0, 0,
                .op_constant, 2,
                .op_add_int,
                .op_pop,
                .op_return,
            }),
            .expected_constants = &.{ .{ .integer = 1 }, .{ .integer = 0 }, .{ .integer = 1 } },
        },
        .{
            .source =
            \\if true {
            \\    x := 1;
            \\    y := 2;
            \\    z := 3;
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                .op_true,
                .op_jump_if_false, 9, 0,
                .op_constant, 0,
                .op_constant, 1,
                .op_constant, 2,
                .op_pop_n, 3, 0,
                .op_return,
            }),
            .expected_constants = &.{ .{ .integer = 1 }, .{ .integer = 2 }, .{ .integer = 3 } },
        },
        .{
            .source =
            \\if true {
            \\    a := 1;
            \\    if true {
            \\        b := 2;
            \\        c := 3;
            \\    }
            \\    d := 4;
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                .op_true,
                .op_jump_if_false, 18, 0,
                // a
                .op_constant, 0,
                // nested if
                .op_true,
                .op_jump_if_false, 7, 0,
                // b, c
                .op_constant, 1,
                .op_constant, 2,
                .op_pop_n, 2, 0,
                // d
                .op_constant, 3,
                .op_pop_n, 2, 0,
                .op_return,
            }),
            .expected_constants = &.{ .{ .integer = 1 }, .{ .integer = 2 }, .{ .integer = 3 }, .{ .integer = 4 } },
        },

        .{
            .source =
            \\{
            \\    if true {
            \\        a := 1;
            \\        b := 2;
            \\    } else {
            \\        c := 3;
            \\        d := 4;
            \\    }
            \\    e := 5;
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                // if-else
                .op_true,
                .op_jump_if_false, 10, 0,
                // a, b
                .op_constant, 0,
                .op_constant, 1,
                .op_pop_n, 2, 0,
                .op_jump, 7, 0,
                // c, d
                .op_constant, 2,
                .op_constant, 3,
                .op_pop_n, 2, 0,
                // e
                .op_constant, 4,
                // scope end
                .op_pop,
                .op_return,
            }),
            .expected_constants = &.{ .{ .integer = 1 }, .{ .integer = 2 }, .{ .integer = 3 }, .{ .integer = 4 }, .{ .integer = 5 } },
        },
        .{
            .source =
            \\{
            \\    a := 1;
            \\    if a > 0 {
            \\        b := a + 1;
            \\    }
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                // a
                .op_constant, 0,
                // condition a > 0
                .op_load, 0, 0,
                .op_constant, 1,
                .op_greater_int,
                .op_jump_if_false, 7, 0,
                // b := a + 1
                .op_load, 0, 0,
                .op_constant, 2,
                .op_add_int,
                // pop b
                .op_pop,
                // pop a
                .op_pop,
                .op_return,
            }),
            .expected_constants = &.{ .{ .integer = 1 }, .{ .integer = 0 }, .{ .integer = 1 } },
        },
    };
    // zig fmt: on

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source = \"{s}\"\n", .{t.source});
        var chunk = try testCompile(gpa, t.source);
        defer chunk.deinit();

        try testing.expectEqualSlices(u8, t.expected_code, chunk.code.items);
        try testing.expectEqualDeep(t.expected_constants, chunk.constants.items);
    }
}

test "full for loops" {
    @setEvalBranchQuota(10000);
    const gpa = testing.allocator;
    // zig fmt: off
    const tests: []const TestCaseMinimal = &.{
        .{
            .source =
            \\for mut i := 0; i < 10; i += 1 {
            \\    print(i);
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                // mut i := 0
                .op_constant, 0,

                // i < 10
                .op_load, 0, 0,
                .op_constant, 1,
                .op_less_int,
                .op_jump_if_false, 17, 0,

                // print(i);
                .op_load, 0, 0,
                .op_temp_print,

                // i += 1
                .op_load, 0, 0,
                .op_constant, 2,
                .op_add_int,
                .op_store, 0, 0,
                .op_pop,
                .op_loop, 26, 0,

                .op_pop,

                .op_return,
            }),
            .expected_constants = &.{ .{ .integer = 0 }, .{ .integer = 10 }, .{ .integer = 1 } },
        },
    };
    // zig fmt: on

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source = \"{s}\"\n", .{t.source});

        var chunk = try testCompile(gpa, t.source);
        defer chunk.deinit();

        errdefer {
            var stderr = Io.File.stderr().writer(testing.io, &.{});
            debug.disassembleChunk(&stderr.interface, &chunk, "output chunk", t.source);
        }

        try testing.expectEqualSlices(u8, t.expected_code, chunk.code.items);
        try testing.expectEqualDeep(t.expected_constants, chunk.constants.items);
    }
}

test "condition for loops" {
    @setEvalBranchQuota(10000);
    const gpa = testing.allocator;
    // zig fmt: off
    const tests: []const TestCaseMinimal = &.{
        .{
            .source =
            \\mut i := 0;
            \\for i < 10 {
            \\    print(i);
            \\    i += 1;
            \\}
            ,
            .expected_code = &debug.opCodeToBytes(&.{
                // mut i := 0
                .op_constant, 0,

                // i < 10
                .op_load, 0, 0,
                .op_constant, 1,
                .op_less_int,
                .op_jump_if_false, 17, 0,

                // print(i)
                .op_load, 0, 0,
                .op_temp_print,

                // i += 1
                .op_load, 0, 0,
                .op_constant, 2,
                .op_add_int,
                .op_store, 0, 0,
                .op_pop,

                .op_loop, 26, 0,

                .op_return,
            }),
            .expected_constants = &.{ .{ .integer = 0 }, .{ .integer = 10 }, .{ .integer = 1 } },
        },
    };
    // zig fmt: on

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source = \"{s}\"\n", .{t.source});

        var chunk = try testCompile(gpa, t.source);
        defer chunk.deinit();

        errdefer {
            var stderr = Io.File.stderr().writer(testing.io, &.{});
            debug.disassembleChunk(&stderr.interface, &chunk, "output chunk", t.source);
        }

        try testing.expectEqualSlices(u8, t.expected_code, chunk.code.items);
        try testing.expectEqualDeep(t.expected_constants, chunk.constants.items);
    }
}

test "infinite for loops" {
    @setEvalBranchQuota(10000);
    const gpa = testing.allocator;
    const tests: []const TestCaseMinimal = &.{
        .{
            .source =
            \\for {
            \\}
            ,
            // zig fmt: off
            .expected_code = &debug.opCodeToBytes(&.{
                .op_loop, 3, 0,

                .op_return,
            }),
            // zig fmt: on
            .expected_constants = &.{},
        },
        .{
            .source =
            \\for {
            \\    print(1);
            \\}
            ,
            // zig fmt: off
            .expected_code = &debug.opCodeToBytes(&.{
                // print(1)
                .op_constant, 0,
                .op_temp_print,

                .op_loop, 6, 0,

                .op_return,
            }),
            // zig fmt: on
            .expected_constants = &.{.{ .integer = 1 }},
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source = \"{s}\"\n", .{t.source});

        var chunk = try testCompile(gpa, t.source);
        defer chunk.deinit();

        errdefer {
            var stderr = Io.File.stderr().writer(testing.io, &.{});
            debug.disassembleChunk(&stderr.interface, &chunk, "output chunk", t.source);
        }

        try testing.expectEqualSlices(u8, t.expected_code, chunk.code.items);
        try testing.expectEqualDeep(t.expected_constants, chunk.constants.items);
    }
}

test "break statements" {
    @setEvalBranchQuota(10000);
    const gpa = testing.allocator;
    const tests: []const TestCaseMinimal = &.{
        .{
            .source =
            \\for {
            \\    break;
            \\}
            ,
            // zig fmt: off
            .expected_code = &debug.opCodeToBytes(&.{
                .op_jump, 3, 0,

                .op_loop, 6, 0,

                .op_return,
            }),
            // zig fmt: on
            .expected_constants = &.{},
        },
        .{
            .source =
            \\mut i := 0;
            \\for {
            \\    if i >= 3 {
            \\        break;
            \\    }
            \\    i += 1;
            \\}
            ,
            // zig fmt: off
            .expected_code = &debug.opCodeToBytes(&.{
                .op_constant, 0,

                .op_load, 0, 0,
                .op_constant, 1,
                .op_greater_equal_int,
                .op_jump_if_false, 3, 0,

                // break
                .op_jump, 13, 0,

                .op_load, 0, 0,
                .op_constant, 2,
                .op_add_int,
                .op_store, 0, 0,
                .op_pop,
                
                .op_loop, 25, 0,

                .op_return,
            }),
            // zig fmt: on
            .expected_constants = &.{ .{ .integer = 0 }, .{ .integer = 3 }, .{ .integer = 1 } },
        },
        .{
            .source =
            \\mut i := 0;
            \\for i < 3 {
            \\    if i >= 3 {
            \\        break;
            \\    }
            \\    i += 1;
            \\}
            ,
            // zig fmt: off
            .expected_code = &debug.opCodeToBytes(&.{
                .op_constant, 0,

                .op_load, 0, 0,
                .op_constant, 1,
                .op_less_int,
                .op_jump_if_false, 25, 0,

                .op_load, 0, 0,
                .op_constant, 2,
                .op_greater_equal_int,
                .op_jump_if_false, 3, 0,

                // break
                .op_jump, 13, 0,

                .op_load, 0, 0,
                .op_constant, 3,
                .op_add_int,
                .op_store, 0, 0,
                .op_pop,
                
                .op_loop, 34, 0,

                .op_return,
            }),
            // zig fmt: on
            .expected_constants = &.{ .{ .integer = 0 }, .{ .integer = 3 }, .{ .integer = 3 }, .{ .integer = 1 } },
        },
        .{
            .source =
            \\for mut i := 0; i < 3; i += 1 {
            \\    if i >= 3 {
            \\        break;
            \\    }
            \\}
            ,
            // zig fmt: off
            .expected_code = &debug.opCodeToBytes(&.{
                .op_constant, 0,

                .op_load, 0, 0,
                .op_constant, 1,
                .op_less_int,
                .op_jump_if_false, 25, 0,


                .op_load, 0, 0,
                .op_constant, 2,
                .op_greater_equal_int,
                .op_jump_if_false, 3, 0,

                // break
                .op_jump, 13, 0,

                .op_load, 0, 0,
                .op_constant, 3,
                .op_add_int,
                .op_store, 0, 0,
                .op_pop,

                .op_loop, 34, 0,

                .op_pop,

                .op_return,
            }),
            // zig fmt: on
            .expected_constants = &.{ .{ .integer = 0 }, .{ .integer = 3 }, .{ .integer = 3 }, .{ .integer = 1 } },
        },
        .{
            .source =
            \\for mut i := 0; i < 3; i += 1 {
            \\    x := 2;
            \\    if i >= 3 {
            \\        break;
            \\    }
            \\    print(x + i);
            \\}
            ,
            // zig fmt: off
            .expected_code = &debug.opCodeToBytes(&.{
                .op_constant, 0,

                .op_load, 0, 0,
                .op_constant, 1,
                .op_less_int,
                .op_jump_if_false, 36, 0,

                // x := 2
                .op_constant, 2,

                .op_load, 0, 0,
                .op_constant, 3,
                .op_greater_equal_int,
                .op_jump_if_false, 3, 0,

                // break
                .op_jump, 22, 0,
                
                .op_load, 1, 0,
                .op_load, 0, 0,
                .op_add_int,
                .op_temp_print,

                // i += 1
                .op_load, 0, 0,
                .op_constant, 4,
                .op_add_int,
                .op_store, 0, 0,
                .op_pop,

                .op_loop, 45, 0,

                .op_pop,
                .op_pop,

                .op_return,
            }),
            // zig fmt: on
            .expected_constants = &.{ .{ .integer = 0 }, .{ .integer = 2 }, .{ .integer = 3 }, .{ .integer = 3 }, .{ .integer = 1 } },
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source = \"{s}\"\n", .{t.source});

        var chunk = try testCompile(gpa, t.source);
        defer chunk.deinit();

        errdefer {
            var stderr = Io.File.stderr().writer(testing.io, &.{});
            debug.disassembleChunk(&stderr.interface, &chunk, "output chunk", t.source);
        }

        try testing.expectEqualSlices(u8, t.expected_code, chunk.code.items);
        try testing.expectEqualDeep(t.expected_constants, chunk.constants.items);
    }
}

test "continue statements" {
    @setEvalBranchQuota(10000);
    const gpa = testing.allocator;
    const tests: []const TestCaseMinimal = &.{
        .{
            .source =
            \\for {
            \\    continue;
            \\}
            ,
            // zig fmt: off
            .expected_code = &debug.opCodeToBytes(&.{
                .op_jump, 0, 0,

                .op_loop, 6, 0,

                .op_return,
            }),
            // zig fmt: on
            .expected_constants = &.{},
        },
        .{
            .source =
            \\mut i := 0;
            \\for {
            \\    if i >= 3 {
            \\        continue;
            \\    }
            \\    i += 1;
            \\}
            ,
            // zig fmt: off
            .expected_code = &debug.opCodeToBytes(&.{
                .op_constant, 0,

                .op_load, 0, 0,
                .op_constant, 1,
                .op_greater_equal_int,
                .op_jump_if_false, 3, 0,

                // continue
                .op_jump, 10, 0,

                .op_load, 0, 0,
                .op_constant, 2,
                .op_add_int,
                .op_store, 0, 0,
                .op_pop,
                
                .op_loop, 25, 0,

                .op_return,
            }),
            // zig fmt: on
            .expected_constants = &.{ .{ .integer = 0 }, .{ .integer = 3 }, .{ .integer = 1 } },
        },
        .{
            .source =
            \\mut i := 0;
            \\for i < 3 {
            \\    if i >= 3 {
            \\        continue;
            \\    }
            \\    i += 1;
            \\}
            ,
            // zig fmt: off
            .expected_code = &debug.opCodeToBytes(&.{
                .op_constant, 0,

                .op_load, 0, 0,
                .op_constant, 1,
                .op_less_int,
                .op_jump_if_false, 25, 0,

                .op_load, 0, 0,
                .op_constant, 2,
                .op_greater_equal_int,
                .op_jump_if_false, 3, 0,

                // continue
                .op_jump, 10, 0,

                .op_load, 0, 0,
                .op_constant, 3,
                .op_add_int,
                .op_store, 0, 0,
                .op_pop,
                
                .op_loop, 34, 0,

                .op_return,
            }),
            // zig fmt: on
            .expected_constants = &.{ .{ .integer = 0 }, .{ .integer = 3 }, .{ .integer = 3 }, .{ .integer = 1 } },
        },
        .{
            .source =
            \\for mut i := 0; i < 3; i += 1 {
            \\    if i >= 3 {
            \\        continue;
            \\    }
            \\}
            ,
            // zig fmt: off
            .expected_code = &debug.opCodeToBytes(&.{
                .op_constant, 0,

                .op_load, 0, 0,
                .op_constant, 1,
                .op_less_int,
                .op_jump_if_false, 25, 0,


                .op_load, 0, 0,
                .op_constant, 2,
                .op_greater_equal_int,
                .op_jump_if_false, 3, 0,

                // continue
                .op_jump, 0, 0,

                .op_load, 0, 0,
                .op_constant, 3,
                .op_add_int,
                .op_store, 0, 0,
                .op_pop,

                .op_loop, 34, 0,

                .op_pop,

                .op_return,
            }),
            // zig fmt: on
            .expected_constants = &.{ .{ .integer = 0 }, .{ .integer = 3 }, .{ .integer = 3 }, .{ .integer = 1 } },
        },
        .{
            .source =
            \\mut i := 0; 
            \\for i < 5 {
            \\    if i == 3 {
            \\        i += 1;
            \\        continue;
            \\    }
            \\    print(i);
            \\    i += 1;
            \\}
            ,
            // zig fmt: off
            .expected_code = &debug.opCodeToBytes(&.{
                .op_constant, 0,

                .op_load, 0, 0,
                .op_constant, 1,
                .op_less_int,
                .op_jump_if_false, 39, 0,

                .op_load, 0, 0,
                .op_constant, 2,
                .op_equal_int,
                .op_jump_if_false, 13, 0,

                .op_load, 0, 0,
                .op_constant, 3,
                .op_add_int,
                .op_store, 0, 0,
                .op_pop,
                
                // continue
                .op_jump, 14, 0,

                .op_load, 0, 0,
                .op_temp_print,

                .op_load, 0, 0,
                .op_constant, 4,
                .op_add_int,
                .op_store, 0, 0,
                .op_pop,

                .op_loop, 48, 0,

                .op_return,
            }),
            // zig fmt: on
            .expected_constants = &.{ .{ .integer = 0 }, .{ .integer = 5 }, .{ .integer = 3 }, .{ .integer = 1 }, .{ .integer = 1 } },
        },
        .{
            .source =
            \\for mut i := 0; i < 5; i += 1 {
            \\    if i == 3 {
            \\        continue;
            \\    }
            \\    print(i);
            \\}
            ,
            // zig fmt: off
            .expected_code = &debug.opCodeToBytes(&.{
                .op_constant, 0,

                .op_load, 0, 0,
                .op_constant, 1,
                .op_less_int,
                .op_jump_if_false, 29, 0,

                .op_load, 0, 0,
                .op_constant, 2,
                .op_equal_int,
                .op_jump_if_false, 3, 0,

                // continue
                .op_jump, 4, 0,

                .op_load, 0, 0,
                .op_temp_print,

                .op_load, 0, 0,
                .op_constant, 3,
                .op_add_int,
                .op_store, 0, 0,
                .op_pop,

                .op_loop, 38, 0,

                .op_pop,

                .op_return,
            }),
            // zig fmt: on
            .expected_constants = &.{ .{ .integer = 0 }, .{ .integer = 5 }, .{ .integer = 3 }, .{ .integer = 1 } },
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source = \"{s}\"\n", .{t.source});

        var chunk = try testCompile(gpa, t.source);
        defer chunk.deinit();

        errdefer {
            var stderr = Io.File.stderr().writer(testing.io, &.{});
            debug.disassembleChunk(&stderr.interface, &chunk, "output chunk", t.source);
        }

        try testing.expectEqualSlices(u8, t.expected_code, chunk.code.items);
        try testing.expectEqualDeep(t.expected_constants, chunk.constants.items);
    }
}

test "temporary print opcode" {
    const gpa = testing.allocator;
    const tests: []const TestCaseMinimal = &.{
        .{
            .source = "print(10);",
            .expected_code = &debug.opCodeToBytes(&.{ .op_constant, 0, .op_temp_print, .op_return }),
            .expected_constants = &.{.{ .integer = 10 }},
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source = \"{s}\"\n", .{t.source});
        var chunk = try testCompile(gpa, t.source);
        defer chunk.deinit();

        try testing.expectEqualSlices(u8, t.expected_code, chunk.code.items);
        try testing.expectEqualDeep(t.expected_constants, chunk.constants.items);
    }
}
