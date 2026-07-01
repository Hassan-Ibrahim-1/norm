const std = @import("std");
const mem = std.mem;
const Io = std.Io;
const Allocator = mem.Allocator;

const Ast = @import("Ast.zig");
const debug = @import("debug.zig");
const ers = @import("errors.zig");
const Token = @import("Lexer.zig").Token;
const tagEql = @import("trait.zig").tagEql;

const Nir = @import("Nir.zig");
const NormType = Nir.NormType;

const Sema = struct {
    arena: Allocator,
    errors: std.ArrayList(Nir.Diagnostics),
    // TODO: reset on statement boundary
    panic_mode: bool,
    invalid_expr: *Nir.Expr,
    sym_table: Nir.SymbolTable,

    /// `level` is `top` if we are not in a for loop
    current_for_block_scope: *Nir.Scope,

    analyzing_stmt: AnalyzingStmt,

    /// `n_invalid` if in global scope
    function_type: NormType,

    const AnalyzingStmt = union(enum) {
        other,
        for_loop,
    };

    fn mask(ty: NormType) u64 {
        return @as(u64, 1) << @intFromEnum(std.meta.activeTag(ty));
    }

    // TODO: refactor this away
    const cast_map = init: {
        var m = [_]u64{0} ** @typeInfo(NormType).@"union".fields.len;
        const Tag = NormType.Tag;
        m[@intFromEnum(Tag.n_int)] = mask(.n_float);
        m[@intFromEnum(Tag.n_float)] = mask(.n_int);
        break :init m;
    };

    // TODO: refactor this away
    fn canCast(ty: NormType, target: NormType) bool {
        return (cast_map[@intFromEnum(std.meta.activeTag(ty))] & mask(target)) != 0;
    }

    // TODO: refactor this away
    fn commonType(l: NormType, r: NormType) NormType {
        if (tagEql(l, r)) return l;
        if (l.isNumeric() or r.isNumeric()) {
            return .n_float;
        }
        return .n_invalid;
    }

    fn init(gpa: Allocator, arena: Allocator) Sema {
        const sym_table: Nir.SymbolTable = .init(gpa);
        return .{
            .invalid_expr = makeInvalid(arena),
            .sym_table = sym_table,
            .arena = arena,
            .errors = .empty,
            .analyzing_stmt = .other,
            .current_for_block_scope = sym_table.top_scope,
            .panic_mode = false,
            .function_type = .n_invalid,
        };
    }

    fn analyzeTypeExpr(s: *Sema, expr: *Ast.Expr) NormType {
        // TODO: support struct/enum expressions
        if (expr.* != .identifier) {
            return s.expectedTypeExprErr(expr);
        }

        return switch (expr.identifier.ident.type) {
            .kw_int => .n_int,
            .kw_float => .n_float,
            .kw_string => .n_string,
            .kw_bool => .n_bool,

            .identifier => {
                dbg("expr", expr);
                @panic("TODO");
            },
            else => unreachable,
        };
    }

    fn inferGlobalType(s: *Sema, expr: *Ast.Expr) NormType {
        return switch (expr.*) {
            .binary => |*b| s.binary(b),
            .unary => |*u| s.unary(u),
            .cast => |*c| s.cast(c),
            .grouping => |*g| s.grouping(g),
            .identifier => |*i| s.identifier(i),
            .literal => |*l| s.literal(l),
            .function => |*f| s.analyzeFunctionSignature(f),
            .call => |*c| t: {
                s.globalCallError(c.token);
                break :t .n_invalid;
            },
        };
    }

    fn globalCallError(s: *Sema, token: Token) void {
        s.reportErrorLine(token.line, "Cannot call functions at compile time", .{});
    }

    fn analyzeFunctionSignature(s: *Sema, f: *Ast.Expr.Function) NormType {
        const parameters = s.arena.alloc(Nir.Expr.Function.Parameter, f.parameters.len) catch oom();
        for (f.parameters, 0..) |param, i| {
            const param_type = s.analyzeTypeExpr(param.type);
            parameters[i] = .{ .name = param.name, .type = param_type };
        }

        const return_type =
            if (f.return_type) |return_type| s.analyzeTypeExpr(return_type) else .n_void;

        const ty = s.arena.create(NormType.Function) catch oom();
        ty.* = .{
            .parameters = parameters,
            .return_type = return_type,
        };
        return ty;
    }

    fn analyzeGlobalSymbols(s: *Sema, stmts: []Ast.Stmt) void {
        for (stmts) |stmt| {
            switch (stmt) {
                // TODO: figure out how to be able to handle circular dependencies
                // I want variables to be able to refer to each other.
                // Main problem right now is that functions can't refer to each
                // other in their bodies. Everything else seems fine, just need
                // to write tests and then work on codegen and vm support for functions
                //
                // What I need exactly is to be able to infer the type of each global declaration
                // before analyze function bodies. The main problem here is just finding circular
                // dependencies.
                .var_decl => |vd| {
                    if (vd.type_expr) |type_expr| {
                        const var_type = s.analyzeTypeExpr(type_expr);
                        _ = var_type; // autofix
                    }
                    // const already_exists = s.sym_table.register(vd.ident.lexeme, var_type, vd.mutable);
                    // if (already_exists) {
                    //     s.redefinedVariableErr(vd.ident);
                    // }
                },
                .block => {
                    // TODO: disallow these as well
                },
                else => {
                    if (@import("builtin").is_test) continue;
                    s.disallowedGlobalStatementErr(stmt);
                },
            }
        }
    }

    fn analyze(s: *Sema, stmts: []Ast.Stmt) []Nir.Stmt {
        s.analyzeGlobalSymbols(stmts);
        if (s.errors.items.len > 0) return &.{};

        var nir_stmts: std.ArrayList(Nir.Stmt) = .empty;

        // Hoist up all the variable declarations. Makes it simpler for the
        // compiler, but I'm not sure if this is the best way to do things
        // though. Variables have to be initialized before declaration
        // but what if I just emit code for all global var decls before
        // codegen for other stuff. This also helps sema with variable
        // redefinition stuff.
        for (stmts) |stmt| {
            if (stmt != .var_decl) continue;
            const nir_stmt = s.statement(stmt);
            nir_stmts.append(s.arena, nir_stmt) catch oom();
        }

        for (stmts) |stmt| {
            if (stmt == .var_decl) continue;
            const nir_stmt = s.statement(stmt);
            nir_stmts.append(s.arena, nir_stmt) catch oom();
        }

        return nir_stmts.items;
    }

    fn statement(s: *Sema, stmt: Ast.Stmt) Nir.Stmt {
        switch (stmt) {
            .expression => |expr_stmt| {
                const nir_expr = s.expression(expr_stmt.expr);
                return .{ .expression = .{ .expr = nir_expr } };
            },
            .var_decl => |vd| {
                const sym = s.sym_table.findOrRegister(vd.ident.lexeme, vd.mutable) orelse {
                    s.redefinedVariableErr(vd.ident);
                    return .invalid;
                };

                const value =
                    if (vd.value) |v|
                        s.expectTypeTryCast(s.expression(v), sym.type)
                    else
                        @panic("todo: zero values");
                // Why would value every be null? This should be an error no?
                if (value == null) return .invalid;
                if (sym.type == .n_invalid) sym.type = value.?.type;
                if (sym.type == .n_void) {
                    s.voidAssignErr(vd.ident);
                    return .invalid;
                }

                return .{
                    .var_decl = .{
                        .ident = vd.ident,
                        .value = value.?,
                        .type = sym.type,
                        .mutable = vd.mutable,
                    },
                };
            },
            .print => |p| {
                const nir_expr = s.expression(p.expr);
                return .{ .print = .{ .token = p.token, .expr = nir_expr } };
            },
            .block => |block| {
                const previous_for_block_scope = s.current_for_block_scope;
                defer s.current_for_block_scope = previous_for_block_scope;

                const block_scope = s.beginScope();
                defer s.endScope();

                if (s.analyzing_stmt == .for_loop) {
                    s.analyzing_stmt = .other;
                    s.current_for_block_scope = block_scope;
                }

                var nir_stmts = s.arena.alloc(Nir.Stmt, block.stmts.len) catch oom();
                for (block.stmts, 0..) |b_stmt, i| {
                    nir_stmts[i] = s.statement(b_stmt);
                }
                return .{
                    .block = .{
                        .token = block.token,
                        .stmts = nir_stmts,
                        .scope = block_scope,
                        .end_token = block.end_token,
                    },
                };
            },
            .if_stmt => |if_stmt| {
                const if_condition = s.expression(if_stmt.condition);

                if (if_condition.type != .n_bool) {
                    s.reportError(if_condition, "Expected condition to be bool got {f}", .{if_condition.type});
                    return .invalid;
                }

                const if_then_block = s.statement(.{ .block = if_stmt.then_block }).block;

                const else_if_blocks =
                    s.arena.alloc(Nir.Stmt.If.ElseIf, if_stmt.else_if_blocks.len) catch oom();
                for (if_stmt.else_if_blocks, 0..) |else_if, i| {
                    const nir_condition = s.expression(else_if.condition);
                    if (nir_condition.type != .n_bool) {
                        s.reportError(nir_condition, "Expected condition to be bool got {f}", .{nir_condition.type});
                        return .invalid;
                    }

                    const nir_then_block = s.statement(.{ .block = else_if.then_block }).block;
                    else_if_blocks[i] = .{
                        .token = else_if.token,
                        .condition = nir_condition,
                        .then_block = nir_then_block,
                    };
                }

                const else_block =
                    if (if_stmt.else_block) |eb| s.statement(.{ .block = eb }).block else null;

                return .{
                    .if_stmt = .{
                        .token = if_stmt.token,
                        .condition = if_condition,
                        .then_block = if_then_block,
                        .else_if_blocks = else_if_blocks,
                        .else_block = else_block,
                    },
                };
            },
            .var_assign => |va| {
                if (s.sym_table.current_scope.level == .top) {
                    s.globalAssignErr(va.ident);
                    return .invalid;
                }

                const sym = s.sym_table.tryFind(va.ident.lexeme) orelse {
                    s.undefinedVariableErr(va.ident);
                    return .invalid;
                };
                if (!sym.mutable) {
                    s.immutableVarErr(va.ident);
                    return .invalid;
                }

                const nir_value = s.expression(va.value);
                const value = s.expectTypeTryCast(nir_value, sym.type);
                if (value == null) return .invalid;

                return .{
                    .var_assign = .{
                        .ident = va.ident,
                        .value = value.?,
                    },
                };
            },

            .infinite_for => |infinite_for| {
                s.analyzing_stmt = .for_loop;

                const for_scope = s.beginScope();
                defer s.endScope();

                const nir_block = s.statement(.{ .block = infinite_for.block }).block;

                return .{
                    .infinite_for = .{
                        .token = infinite_for.token,
                        .scope = for_scope,
                        .block = nir_block,
                    },
                };
            },

            .condition_for => |condition_for| {
                s.analyzing_stmt = .for_loop;

                const for_scope = s.beginScope();
                defer s.endScope();

                const condition = s.expression(condition_for.condition);
                if (condition.type != .n_bool) {
                    s.reportError(condition, "Expected condition to be bool got {f}", .{condition.type});
                    return .invalid;
                }

                const nir_block = s.statement(.{ .block = condition_for.block }).block;
                return .{
                    .condition_for = .{
                        .token = condition_for.token,
                        .scope = for_scope,
                        .condition = condition,
                        .block = nir_block,
                    },
                };
            },

            .for_stmt => |for_stmt| {
                s.analyzing_stmt = .for_loop;

                const for_scope = s.beginScope();
                defer s.endScope();

                const initializer: ?Nir.Stmt.For.InitializerStmt = if (for_stmt.initializer) |i| s: {
                    const ast_stmt: Ast.Stmt = switch (i) {
                        .var_assign => |va| .{ .var_assign = va },
                        .var_decl => |vd| .{ .var_decl = vd },
                        .expr => |expr| .{ .expression = expr },
                    };
                    const nir_stmt = s.statement(ast_stmt);
                    break :s switch (nir_stmt) {
                        .var_assign => |va| .{ .var_assign = va },
                        .var_decl => |vd| .{ .var_decl = vd },
                        .expression => |expr| .{ .expr = expr },
                        else => unreachable,
                    };
                } else null;

                const condition = s.expression(for_stmt.condition);
                if (condition.type != .n_bool) {
                    s.reportError(condition, "Expected condition to be bool got {f}", .{condition.type});
                    return .invalid;
                }

                const increment: ?Nir.Stmt.For.IncrementStmt = if (for_stmt.increment) |i| s: {
                    const ast_stmt: Ast.Stmt = switch (i) {
                        .var_assign => |va| .{ .var_assign = va },
                        .expr => |expr| .{ .expression = expr },
                    };
                    const nir_stmt = s.statement(ast_stmt);
                    if (s.errors.items.len > 0) {
                        dbg("errors", s.errors.items);
                        @panic("had an error");
                    }
                    break :s switch (nir_stmt) {
                        .var_assign => |va| .{ .var_assign = va },
                        .expression => |expr| .{ .expr = expr },
                        else => unreachable,
                    };
                } else null;
                const nir_block = s.statement(.{ .block = for_stmt.block }).block;

                return .{
                    .for_stmt = .{
                        .token = for_stmt.token,
                        .scope = for_scope,
                        .initializer = initializer,
                        .condition = condition,
                        .increment = increment,
                        .block = nir_block,
                    },
                };
            },
            .break_stmt => |break_stmt| {
                // Parser guarantees that loop control statements can only be present in loops
                std.debug.assert(s.current_for_block_scope.level != .top);
                return .{
                    .break_stmt = .{
                        .token = break_stmt.token,
                        .jump_scope = s.current_for_block_scope.parent,
                    },
                };
            },
            .continue_stmt => |continue_stmt| {
                // Parser guarantees that loop control statements can only be present in loops
                std.debug.assert(s.current_for_block_scope.level != .top);
                return .{
                    .continue_stmt = .{
                        .token = continue_stmt.token,
                        .jump_scope = s.current_for_block_scope,
                    },
                };
            },

            .return_stmt => |return_stmt| {
                const nir_expr = if (return_stmt.expr) |expr| s.expression(expr) else null;
                const return_type = if (nir_expr) |expr| expr.type else .n_void;
                if (!tagEql(return_type, s.function_type)) {
                    s.wrongReturnType(return_type, s.function_type, return_stmt.token);
                    return .invalid;
                }
                return .{ .return_stmt = .{ .token = return_stmt.token, .expr = nir_expr } };
            },
        }
    }

    fn beginScope(s: *Sema) *Nir.Scope {
        return s.sym_table.beginScope();
    }

    fn endScope(s: *Sema) void {
        s.sym_table.endScope();
    }

    fn expectedTypeExprErr(s: *Sema, expr: *Ast.Expr) NormType {
        s.reportErrorAst(
            expr,
            "Expected type identifier found {f}. TODO: support struct/enum expressions",
            .{expr},
        );
        return .n_invalid;
    }

    fn alwaysReturnsErr(s: *Sema, return_type: NormType, token: Token) *Nir.Expr {
        s.reportErrorLine(
            token.line,
            "Function does not return {f} in every branch",
            .{return_type},
        );
        return s.invalid_expr;
    }

    fn wrongReturnType(s: *Sema, actual: NormType, expected: NormType, token: Token) void {
        s.reportErrorLine(
            token.line,
            "Expected function to return {f}, instead got {f}",
            .{ expected, actual },
        );
    }

    fn expression(s: *Sema, expr: *Ast.Expr) *Nir.Expr {
        return switch (expr.*) {
            .binary => |*b| s.binary(b),
            .unary => |*u| s.unary(u),
            .cast => |*c| s.cast(c),
            .grouping => |*g| s.grouping(g),
            .identifier => |*i| s.identifier(i),
            .literal => |*l| s.literal(l),
            .function => |*f| s.function(f),
            .call => |*c| s.call(c),
        };
    }

    fn tryCast(s: *Sema, expr: *Nir.Expr, target: NormType) ?*Nir.Expr {
        if (target == .n_invalid) return null;
        if (tagEql(expr.type, target)) return expr;

        const arena = s.arena;
        if (canCast(expr.type, target)) {
            return makeAnonCast(arena, expr, target);
        }
        return null;
    }

    fn expectTypeTryCast(s: *Sema, expr: *Nir.Expr, target: NormType) ?*Nir.Expr {
        if (tagEql(expr.type, target) or target == .n_invalid) return expr;
        const casted = s.tryCast(expr, target) orelse {
            s.reportError(expr, "Expected {f} got {f}", .{ target, expr.type });
            return null;
        };
        return casted;
    }

    fn binary(s: *Sema, b: *Ast.Expr.Binary) *Nir.Expr {
        const left = s.expression(b.left);
        const right = s.expression(b.right);
        const arena = s.arena;

        switch (b.operator.type) {
            .plus => {
                if (left.type.isNumeric() and right.type.isNumeric()) {
                    const common_type = commonType(left.type, right.type);
                    const cast_left = s.tryCast(left, common_type) orelse return s.castErr(left, common_type);
                    const cast_right = s.tryCast(right, common_type) orelse return s.castErr(right, common_type);
                    return makeBinary(arena, cast_left, b.operator, cast_right, common_type);
                }
                if (left.type.isString() and right.type.isString()) {
                    return makeBinary(arena, left, b.operator, right, .n_string);
                }
                return s.invalidBinaryOp(left, b.operator, right);
            },
            .minus, .star => {
                if (left.type.isNumeric() and right.type.isNumeric()) {
                    const common_type = commonType(left.type, right.type);
                    const cast_left = s.tryCast(left, common_type) orelse return s.castErr(left, common_type);
                    const cast_right = s.tryCast(right, common_type) orelse return s.castErr(right, common_type);
                    return makeBinary(arena, cast_left, b.operator, cast_right, common_type);
                }
                return s.invalidBinaryOp(left, b.operator, right);
            },

            .slash => {
                if (left.type.isNumeric() and right.type.isNumeric()) {
                    const cast_left = s.tryCast(left, .n_float) orelse return s.invalid_expr;
                    const cast_right = s.tryCast(right, .n_float) orelse return s.invalid_expr;
                    return makeBinary(arena, cast_left, b.operator, cast_right, .n_float);
                }
                return s.invalidBinaryOp(left, b.operator, right);
            },

            .equal_equal, .bang_equal => {
                if (left.type.isComparable() and right.type.isComparable()) {
                    const common_type = commonType(left.type, right.type);
                    const cast_left = s.tryCast(left, common_type) orelse s.compareErr(left, right);
                    const cast_right = s.tryCast(right, common_type) orelse s.compareErr(left, right);
                    return makeBinary(arena, cast_left, b.operator, cast_right, .n_bool);
                }
                return s.compareErr(left, right);
            },
            .greater, .greater_equal, .less, .less_equal => {
                if (left.type.isOrderable() and right.type.isOrderable()) {
                    const common_type = commonType(left.type, right.type);
                    const cast_left = s.tryCast(left, common_type) orelse s.compareErr(left, right);
                    const cast_right = s.tryCast(right, common_type) orelse s.compareErr(left, right);
                    return makeBinary(arena, cast_left, b.operator, cast_right, .n_bool);
                }
                return s.compareErr(left, right);
            },
            .kw_and, .kw_or => {
                if (left.type != .n_bool or right.type != .n_bool) {
                    return s.compareErr(left, right);
                }
                return makeBinary(arena, left, b.operator, right, .n_bool);
            },
            else => unreachable,
        }
    }

    fn castErr(s: *Sema, expr: *Nir.Expr, target: NormType) *Nir.Expr {
        return s.reportErrorInv(expr, "Cannot cast {f} to {f}", .{ expr.type, target });
    }

    fn compareErr(s: *Sema, left: *Nir.Expr, right: *Nir.Expr) *Nir.Expr {
        return s.reportErrorInv(left, "Cannot compare {f} and {f}.", .{ left.type, right.type });
    }

    fn invalidBinaryOp(s: *Sema, left: *Nir.Expr, op: Token, right: *Nir.Expr) *Nir.Expr {
        return s.reportErrorInv(left, "{f} {s} {f} is not a valid operation", .{
            left.type,
            op.lexeme,
            right.type,
        });
    }

    fn unary(s: *Sema, u: *Ast.Expr.Unary) *Nir.Expr {
        const arena = s.arena;
        const expr = s.expression(u.expr);
        switch (u.operator.type) {
            .minus => {
                if (expr.type.isNumeric()) {
                    return makeUnary(arena, expr, u.operator, expr.type);
                }
                return s.reportErrorInv(expr, "Cannot negate {f}", .{expr.type});
            },
            .bang => {
                if (expr.type.isBool()) {
                    return makeUnary(arena, expr, u.operator, expr.type);
                }
                return s.reportErrorInv(expr, "! only supports bools, got {f}", .{expr.type});
            },
            else => unreachable,
        }
    }

    fn cast(s: *Sema, c: *Ast.Expr.Cast) *Nir.Expr {
        const arena = s.arena;

        const expr = s.expression(c.expr);
        if (expr.type == .n_invalid) return s.invalid_expr;

        const target_type: NormType = switch (c.token.type) {
            .kw_float => if (!s.expectType(expr, .n_int, "Cannot cast {f} to float, expected int", .{expr.type}))
                return s.invalid_expr
            else
                .n_float,
            .kw_int => if (!s.expectType(expr, .n_float, "Cannot cast {f} to int, expected float", .{expr.type}))
                return s.invalid_expr
            else
                .n_int,
            else => unreachable,
        };

        return makeCast(arena, expr, c.token, target_type);
    }

    fn grouping(s: *Sema, g: *Ast.Expr.Grouping) *Nir.Expr {
        const expr = s.expression(g.expr);
        if (expr.type == .n_invalid) return s.invalid_expr;
        return makeGrouping(s.arena, expr, g.paren, expr.type);
    }

    fn identifier(s: *Sema, i: *Ast.Expr.Identifier) *Nir.Expr {
        const sym = s.sym_table.tryFind(i.ident.lexeme) orelse {
            s.undefinedVariableErr(i.ident);
            return s.invalid_expr;
        };
        if (sym.type == .n_invalid and sym.scope.level == .top) {
            return s.useBeforeDefinitionErr(i.ident);
        }

        return makeIdentifier(s.arena, i.ident, sym.scope, sym.type);
    }

    fn undefinedVariableErr(s: *Sema, name: Token) void {
        s.reportErrorLine(name.line, "Undefined variable {s}", .{name.lexeme});
    }

    fn voidAssignErr(s: *Sema, ident: Token) void {
        s.reportErrorLine(ident.line, "Type void is not assignable", .{});
    }

    fn redefinedVariableErr(s: *Sema, name: Token) void {
        s.reportErrorLine(
            name.line,
            "Variable {s} is already defined",
            .{name.lexeme},
        );
    }

    fn useBeforeDefinitionErr(s: *Sema, name: Token) *Nir.Expr {
        s.reportErrorLine(
            name.line,
            "Variable {s} used before its definition",
            .{name.lexeme},
        );
        return s.invalid_expr;
    }

    fn immutableVarErr(s: *Sema, name: Token) void {
        // TODO: add hint here to declare with 'mut'
        s.reportErrorLine(
            name.line,
            "Variable {s} is not declared as mutable.",
            .{name.lexeme},
        );
    }

    fn globalAssignErr(s: *Sema, name: Token) void {
        s.reportErrorLine(
            name.line,
            "Variable assignments are not allowed in the global scope.",
            .{},
        );
    }

    fn disallowedGlobalStatementErr(s: *Sema, stmt: Ast.Stmt) void {
        const line = stmt.token().line;
        s.reportErrorLine(line, "A '{t}' statement is not allowed in global scope", .{stmt});
    }

    fn literal(s: *Sema, l: *Ast.Expr.Literal) *Nir.Expr {
        const ty: NormType = switch (l.value) {
            .float => .n_float,
            .integer => .n_int,
            .boolean => .n_bool,
            .string => .n_string,
            .nil => @panic("todo"),
        };
        return makeLiteral(s.arena, .fromAst(l.value), l.token, ty);
    }

    fn function(s: *Sema, f: *Ast.Expr.Function) *Nir.Expr {
        const previous_fn_type = s.function_type;
        defer s.function_type = previous_fn_type;

        const scope = s.beginScope();
        defer s.endScope();

        const parameters = s.arena.alloc(Nir.Expr.Function.Parameter, f.parameters.len) catch oom();
        for (f.parameters, 0..) |param, i| {
            const param_type = s.analyzeTypeExpr(param.type);
            parameters[i] = .{ .name = param.name, .type = param_type };
            const already_exists = s.sym_table.register(param.name.lexeme, param_type, false);
            if (already_exists) {
                s.redefinedVariableErr(param.name);
                return s.invalid_expr;
            }
        }

        const return_type =
            if (f.return_type) |return_type| s.analyzeTypeExpr(return_type) else .n_void;
        s.function_type = return_type;

        const body = s.statement(.{ .block = f.body }).block;

        if (return_type != .n_void) {
            const always_returns = alwaysReturns(body.stmts);
            if (!always_returns) {
                return s.alwaysReturnsErr(return_type, f.token);
            }
        }

        return makeFunction(s.arena, f.token, scope, parameters, return_type, body);
    }

    fn alwaysReturns(stmts: []Nir.Stmt) bool {
        for (stmts) |stmt| {
            switch (stmt) {
                .if_stmt => |if_stmt| {
                    if (if_stmt.else_block) |else_block| {
                        const else_returns = alwaysReturns(else_block.stmts);
                        if (else_returns) return true;
                    }
                },
                .block => |block| {
                    const always_returns = alwaysReturns(block.stmts);
                    if (always_returns) return true;
                },
                .return_stmt => return true,
                // TODO: we can guarantee returns here but only if there is no
                // preceding control statements in the loop
                .infinite_for => {},
                else => {},
            }
        }
        return false;
    }

    fn call(s: *Sema, c: *Ast.Expr.Call) *Nir.Expr {
        const callee = s.expression(c.callee);
        if (callee.type != .n_function) {
            return s.expectedFunctionCallee(callee.type, c.token);
        }

        // how do i actually access the function declaration? I only need
        // the type of the function nothing else, maybe I can add some info
        // to n_function in NormType like parameter list, return type etc.
        const callee_fn = callee.type.n_function;
        if (c.args.len != callee_fn.parameters.len) {
            return s.badArgumentLength(c.args.len, callee_fn.parameters.len, c.token);
        }

        const args = s.arena.alloc(*Nir.Expr, c.args.len) catch oom();
        for (c.args, 0..) |arg, i| {
            args[i] = s.expression(arg);
            if (!tagEql(args[i].type, callee_fn.parameters[i].type)) {
                return s.badArgumentType(i + 1, args[i].type, callee_fn.parameters[i].type, c.token);
            }
        }

        return makeCall(s.arena, callee, c.token, args);
    }

    fn expectedFunctionCallee(s: *Sema, callee_type: NormType, token: Token) *Nir.Expr {
        s.reportErrorLine(token.line, "Expected callee to be a function but got {f} instead", .{callee_type});
        return s.invalid_expr;
    }

    fn badArgumentLength(s: *Sema, actual: usize, expected: usize, token: Token) *Nir.Expr {
        s.reportErrorLine(token.line, "Expected {} arguments but got {} instead", .{ expected, actual });
        return s.invalid_expr;
    }

    fn badArgumentType(s: *Sema, arg_location: usize, actual: NormType, expected: NormType, token: Token) *Nir.Expr {
        s.reportErrorLine(
            token.line,
            "Expected argument {} to be of type {f} but got {f} instead",
            .{ arg_location, expected, actual },
        );
        return s.invalid_expr;
    }

    fn expectType(
        s: *Sema,
        expr: *Nir.Expr,
        ty: NormType,
        comptime error_msg: []const u8,
        args: anytype,
    ) bool {
        if (tagEql(expr.type, ty)) return true;
        s.reportError(expr, error_msg, args);
        return false;
    }

    fn expectTypes(
        s: *Sema,
        expr: *Nir.Expr,
        comptime types: []const NormType,
        comptime error_msg: []const u8,
        args: anytype,
    ) bool {
        if (checkTypes(expr, types)) return true;
        s.reportError(expr, error_msg, args);
        return false;
    }

    fn expectTypesEx(
        s: *Sema,
        left: *Nir.Expr,
        right: *Nir.Expr,
        comptime types: []const NormType,
        comptime error_msg: []const u8,
        args: anytype,
    ) bool {
        if (checkTypes(left, types) and checkTypes(right, types)) return true;
        s.reportError(left, error_msg, args);
        return false;
    }

    fn checkTypes(expr: *Nir.Expr, comptime types: []const NormType) bool {
        for (types) |ty|
            if (expr.type == ty) return true;
        return false;
    }

    fn diag(s: *Sema, line: u32, comptime error_msg: []const u8, args: anytype) Nir.Diagnostics {
        return .{
            .error_msg = std.fmt.allocPrint(s.arena, error_msg, args) catch oom(),
            .line = line,
        };
    }

    fn reportError(s: *Sema, expr: *Nir.Expr, comptime error_msg: []const u8, args: anytype) void {
        if (s.panic_mode) return;
        s.errors.append(s.arena, s.diag(expr.token().line, error_msg, args)) catch oom();
        s.panic_mode = true;
    }

    fn reportErrorAst(s: *Sema, expr: *Ast.Expr, comptime error_msg: []const u8, args: anytype) void {
        if (s.panic_mode) return;
        s.errors.append(s.arena, s.diag(expr.token().line, error_msg, args)) catch oom();
        s.panic_mode = true;
    }

    fn reportErrorLine(s: *Sema, line: u32, comptime error_msg: []const u8, args: anytype) void {
        if (s.panic_mode) return;
        s.errors.append(s.arena, s.diag(line, error_msg, args)) catch oom();
        s.panic_mode = true;
    }

    fn reportErrorInv(s: *Sema, expr: *Nir.Expr, comptime error_msg: []const u8, args: anytype) *Nir.Expr {
        s.reportError(expr, error_msg, args);
        return s.invalid_expr;
    }
};

pub fn analyze(gpa: Allocator, ast: *Ast) Nir {
    if (ast.errors.len > 0) return undefined;

    var arena: std.heap.ArenaAllocator = .init(gpa);

    var sema = Sema.init(gpa, arena.allocator());

    const stmts = sema.analyze(ast.stmts);

    return .{
        .arena = arena,
        .errors = sema.errors.items,
        .stmts = stmts,
        .sym_table = sema.sym_table,
    };
}

fn oom() noreturn {
    @panic("oom");
}

fn makeCall(
    arena: Allocator,
    callee: *Nir.Expr,
    left_paren: Token,
    args: []*Nir.Expr,
) *Nir.Expr {
    const e = makeExpr(arena);
    e.* = .{
        .kind = .{
            .call = .{
                .token = left_paren,
                .callee = callee,
                .args = args,
            },
        },
        .type = callee.type.n_function.return_type,
    };
    return e;
}

fn makeFunction(
    arena: Allocator,
    fn_token: Token,
    scope: *Nir.Scope,
    parameters: []Nir.Expr.Function.Parameter,
    return_type: NormType,
    body: Nir.Stmt.Block,
) *Nir.Expr {
    const e = makeExpr(arena);
    const ty = arena.create(NormType.Function) catch oom();
    ty.* = .{
        .parameters = parameters,
        .return_type = return_type,
    };
    e.* = .{
        .kind = .{
            .function = .{
                .token = fn_token,
                .scope = scope,
                .body = body,
            },
        },
        .type = .{ .n_function = ty },
    };
    return e;
}

fn makeIdentifier(arena: Allocator, ident: Token, scope: *Nir.Scope, ty: NormType) *Nir.Expr {
    const e = makeExpr(arena);
    e.* = .{ .kind = .{ .identifier = .{ .ident = ident, .scope = scope } }, .type = ty };
    return e;
}

fn makeGrouping(arena: Allocator, grping: *Nir.Expr, paren: Token, ty: NormType) *Nir.Expr {
    const e = makeExpr(arena);
    e.* = .{ .kind = .{ .grouping = .{ .paren = paren, .expr = grping } }, .type = ty };
    return e;
}

fn makeUnary(arena: Allocator, expr: *Nir.Expr, op: Token, ty: NormType) *Nir.Expr {
    const e = makeExpr(arena);
    e.* = .{
        .kind = .{ .unary = .{ .expr = expr, .operator = op } },
        .type = ty,
    };
    return e;
}

fn makeCast(arena: Allocator, expr: *Nir.Expr, token: Token, target: NormType) *Nir.Expr {
    const e = makeExpr(arena);
    e.* = .{
        .kind = .{ .cast = .{ .expr = expr, .token = token } },
        .type = target,
    };
    return e;
}

fn fakeTargetToken(target: NormType, line: u32) Token {
    return switch (target) {
        .n_int => .{ .type = .kw_int, .lexeme = "int", .line = line },
        .n_float => .{ .type = .kw_float, .lexeme = "float", .line = line },
        else => unreachable,
    };
}

fn makeAnonCast(arena: Allocator, expr: *Nir.Expr, target: NormType) *Nir.Expr {
    const expr_token = expr.token();
    const target_token = fakeTargetToken(target, expr_token.line);
    return makeCast(arena, expr, target_token, target);
}

fn makeBinary(arena: Allocator, left: *Nir.Expr, op: Token, right: *Nir.Expr, ty: NormType) *Nir.Expr {
    const e = makeExpr(arena);
    e.* = .{
        .kind = .{ .binary = .{ .left = left, .operator = op, .right = right } },
        .type = ty,
    };
    return e;
}

fn makeLiteral(arena: Allocator, value: Nir.Expr.Literal.Value, token: Token, ty: NormType) *Nir.Expr {
    const e = makeExpr(arena);
    e.* = .{
        .kind = .{ .literal = .{ .token = token, .value = value } },
        .type = ty,
    };
    return e;
}

fn makeInvalid(arena: Allocator) *Nir.Expr {
    const expr = makeExpr(arena);
    expr.* = .invalid;
    return expr;
}

fn makeExpr(arena: Allocator) *Nir.Expr {
    return arena.create(Nir.Expr) catch oom();
}

const Lexer = @import("Lexer.zig");
const parser = @import("parser.zig");
const dbg = @import("debug.zig").dbg;

fn testAnalyzeExpr(gpa: Allocator, source: []const u8) ![]const u8 {
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

    var nir = analyze(gpa, &ast);
    defer nir.deinit();
    if (nir.errors.len > 0) {
        debug.reportErrors(nir.errors, "test_runner", source);
        return error.SemaFailed;
    }

    var aw = Io.Writer.Allocating.init(gpa);
    try nir.stmts[0].expression.expr.format(&aw.writer);
    return aw.toOwnedSlice();
}

fn testAnalyzeExprFailure(gpa: Allocator, source: []const u8) !Nir {
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

    var nir = analyze(gpa, &ast);
    errdefer nir.deinit();
    if (nir.errors.len == 0) {
        std.debug.print("expected an error, sema passed with output=\"{f}\"\n", .{nir.stmts[0].expression.expr});
        return error.SemaPassed;
    }
    return nir;
}

fn testAnalyze(gpa: Allocator, source: []const u8) ![]const u8 {
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

    var nir = analyze(gpa, &ast);
    defer nir.deinit();
    if (nir.errors.len > 0) {
        debug.reportErrors(nir.errors, "test_runner", source);
        return error.SemaFailed;
    }

    return debug.printStmts(gpa, nir.stmts);
}

fn testAnalyzeFailure(gpa: Allocator, source: []const u8) !Nir {
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

    var nir = analyze(gpa, &ast);
    errdefer nir.deinit();
    if (nir.errors.len == 0) {
        const stmts = debug.printStmts(gpa, nir.stmts);
        defer gpa.free(stmts);
        std.debug.print("expected an error, sema passed with output=\"{s}\"\n", .{stmts});
        return error.SemaPassed;
    }

    return nir;
}

const testing = std.testing;

test "arithmetic" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{ .source = "2 + 2", .expected = "(2 + 2):int" },
        .{ .source = "2 / 2", .expected = "(float(2) / float(2)):float" },
        .{ .source = "-2", .expected = "(-2):int" },
        .{ .source = "-2", .expected = "(-2):int" },
        .{ .source = "2 * 3 + 4", .expected = "((2 * 3):int + 4):int" },
        .{ .source = "9.0 / 3.0 * 32.0", .expected = "((9.000 / 3.000):float * 32.000):float" },
    };

    for (tests) |t| {
        const actual = try testAnalyzeExpr(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}

test "arithmetic failure" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        error_msg: []const u8,
    } = &.{
        .{ .source = "1 + true", .error_msg = "int + bool is not a valid operation" },
        .{ .source = "1.0 + true", .error_msg = "float + bool is not a valid operation" },
        .{ .source = "true + 1.0", .error_msg = "bool + float is not a valid operation" },
        .{ .source = "false + 2", .error_msg = "bool + int is not a valid operation" },
        .{ .source = "1 + \"str\"", .error_msg = "int + string is not a valid operation" },
        .{ .source = "\"str\" + 1", .error_msg = "string + int is not a valid operation" },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"\n", .{t.source});

        var nir = try testAnalyzeExprFailure(gpa, t.source);
        defer nir.deinit();

        try testing.expect(nir.errors.len == 1);
        try testing.expectEqualStrings(t.error_msg, nir.errors[0].error_msg);
    }
}

test "string concat" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{
            .source = "\"Hello, \" + \"World\"",
            .expected = "(\"Hello, \" + \"World\"):string",
        },
    };

    for (tests) |t| {
        const actual = try testAnalyzeExpr(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}

test "string concat failure" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        error_msg: []const u8,
    } = &.{
        .{ .source = "\"h\" + true", .error_msg = "string + bool is not a valid operation" },
        .{ .source = "2.0 + \"h\" ", .error_msg = "float + string is not a valid operation" },
        .{ .source = "3 + \"h\" ", .error_msg = "int + string is not a valid operation" },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"\n", .{t.source});

        var nir = try testAnalyzeExprFailure(gpa, t.source);
        defer nir.deinit();

        try testing.expect(nir.errors.len == 1);
        try testing.expectEqualStrings(t.error_msg, nir.errors[0].error_msg);
    }
}

test "comparison" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{ .source = "2 > 1", .expected = "(2 > 1):bool" },
        .{ .source = "3 <= 2", .expected = "(3 <= 2):bool" },
        .{ .source = "1 == 1", .expected = "(1 == 1):bool" },
        .{ .source = "1 != 2", .expected = "(1 != 2):bool" },
        .{ .source = "true == true", .expected = "(true == true):bool" },
        .{ .source = "true != false", .expected = "(true != false):bool" },
        .{ .source = "\"Hey\" == \"Hey\"", .expected = "(\"Hey\" == \"Hey\"):bool" },
        .{ .source = "\"Hey\" != \"Hey\"", .expected = "(\"Hey\" != \"Hey\"):bool" },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"", .{t.source});

        const actual = try testAnalyzeExpr(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}

test "comparison failure" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        error_msg: []const u8,
    } = &.{
        .{ .source = "true > false", .error_msg = "Cannot compare bool and bool." },
        .{ .source = "true == 1", .error_msg = "Cannot compare bool and int." },
        .{ .source = "true != 2", .error_msg = "Cannot compare bool and int." },
        .{ .source = "2 < false", .error_msg = "Cannot compare int and bool." },
        .{ .source = "\"Hey\" == 2", .error_msg = "Cannot compare string and int." },
        .{ .source = "2 == \"Hey\"", .error_msg = "Cannot compare int and string." },
        .{ .source = "\"Hey\" != 2", .error_msg = "Cannot compare string and int." },
        .{ .source = "\"Hey\" != false", .error_msg = "Cannot compare string and bool." },
        .{ .source = "\"Hey\" == false", .error_msg = "Cannot compare string and bool." },
        .{ .source = "false == \"Hey\"", .error_msg = "Cannot compare bool and string." },
        .{ .source = "true != \"Hey\"", .error_msg = "Cannot compare bool and string." },
        .{ .source = "2 != \"Hey\"", .error_msg = "Cannot compare int and string." },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"\n", .{t.source});

        var nir = try testAnalyzeExprFailure(gpa, t.source);
        defer nir.deinit();

        try testing.expect(nir.errors.len == 1);
        try testing.expectEqualStrings(t.error_msg, nir.errors[0].error_msg);
    }
}

test "logical" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{ .source = "true and true", .expected = "(true and true):bool" },
        .{ .source = "true or false", .expected = "(true or false):bool" },
        .{ .source = "!true", .expected = "(!true):bool" },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"", .{t.source});

        const actual = try testAnalyzeExpr(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}

test "logical failure" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        error_msg: []const u8,
    } = &.{
        .{ .source = "true and 1", .error_msg = "Cannot compare bool and int." },
        .{ .source = "1 or true", .error_msg = "Cannot compare int and bool." },
        .{ .source = "1 and 1", .error_msg = "Cannot compare int and int." },
        .{ .source = "!1", .error_msg = "! only supports bools, got int" },
        .{ .source = "!1.0", .error_msg = "! only supports bools, got float" },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"\n", .{t.source});

        var nir = try testAnalyzeExprFailure(gpa, t.source);
        defer nir.deinit();

        try testing.expect(nir.errors.len == 1);
        try testing.expectEqualStrings(t.error_msg, nir.errors[0].error_msg);
    }
}

test "casting" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{ .source = "float(2)", .expected = "float(2)" },
        .{ .source = "int(2.0)", .expected = "int(2.000)" },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"", .{t.source});

        const actual = try testAnalyzeExpr(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}

test "auto casting" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{ .source = "2 + 3.0", .expected = "(float(2) + 3.000):float" },
        .{ .source = "3.0 * 4", .expected = "(3.000 * float(4)):float" },
        .{ .source = "2 - 3.0", .expected = "(float(2) - 3.000):float" },
        .{ .source = "3.0 - 4", .expected = "(3.000 - float(4)):float" },
        .{ .source = "2 / 3.0", .expected = "(float(2) / 3.000):float" },
        .{ .source = "3.0 / 4", .expected = "(3.000 / float(4)):float" },
        .{ .source = "2 == 3.0", .expected = "(float(2) == 3.000):bool" },
        .{ .source = "3.0 == 4", .expected = "(3.000 == float(4)):bool" },
        .{ .source = "2 != 3.0", .expected = "(float(2) != 3.000):bool" },
        .{ .source = "3.0 != 4", .expected = "(3.000 != float(4)):bool" },
        .{ .source = "2 > 3.0", .expected = "(float(2) > 3.000):bool" },
        .{ .source = "3.0 > 4", .expected = "(3.000 > float(4)):bool" },
        .{ .source = "2 >= 3.0", .expected = "(float(2) >= 3.000):bool" },
        .{ .source = "3.0 >= 4", .expected = "(3.000 >= float(4)):bool" },
        .{ .source = "2 < 3.0", .expected = "(float(2) < 3.000):bool" },
        .{ .source = "3.0 < 4", .expected = "(3.000 < float(4)):bool" },
        .{ .source = "2 <= 3.0", .expected = "(float(2) <= 3.000):bool" },
        .{ .source = "3.0 <= 4", .expected = "(3.000 <= float(4)):bool" },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"", .{t.source});

        const actual = try testAnalyzeExpr(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}

test "full variable declarations - simple values" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{
            .source = "x: int = 10;",
            .expected = "x: int = 10;",
        },
        .{
            .source = "x: float = 10.0;",
            .expected = "x: float = 10.000;",
        },
        .{
            .source = "x: bool = true;",
            .expected = "x: bool = true;",
        },
        .{
            .source = "x: string = \"hey\";",
            .expected = "x: string = \"hey\";",
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"", .{t.source});

        const actual = try testAnalyze(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}

test "full variable declarations - complex values" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{
            .source = "x: int = 10 + 2;",
            .expected = "x: int = (10 + 2):int;",
        },
        .{
            .source = "x: float = 10;",
            .expected = "x: float = float(10);",
        },
        .{
            .source = "x: int = 10 + 2.0;",
            .expected = "x: int = int((float(10) + 2.000):float);",
        },
        .{
            .source = "x: bool = 2 > 1;",
            .expected = "x: bool = (2 > 1):bool;",
        },
        .{
            .source = "x: string = \"Hello, \" + \"World\";",
            .expected = "x: string = (\"Hello, \" + \"World\"):string;",
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"", .{t.source});

        const actual = try testAnalyze(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}

test "tracking variables" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{
            .source = "x: int = 10; x;",
            .expected = "x: int = 10;\nx:int;",
        },
        .{
            .source = "x: int = 10; x + 10;",
            .expected = "x: int = 10;\n(x:int + 10):int;",
        },
        .{
            .source = "x: int = 10; y: int = x; y + 1;",
            .expected = "x: int = 10;\ny: int = x:int;\n(y:int + 1):int;",
        },
        .{
            .source = "x: int = 10; y: int = x; z: int = y * x;",
            .expected = "x: int = 10;\ny: int = x:int;\nz: int = (y:int * x:int):int;",
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"", .{t.source});

        const actual = try testAnalyze(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}

test "undefined variable" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        error_msg: []const u8,
    } = &.{
        .{
            .source = "x;",
            .error_msg = "Undefined variable x",
        },
        .{
            .source = "x: int = 10; x; y",
            .error_msg = "Undefined variable y",
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"\n", .{t.source});

        var nir = try testAnalyzeFailure(gpa, t.source);
        defer nir.deinit();

        try testing.expect(nir.errors.len == 1);
        try testing.expectEqualStrings(t.error_msg, nir.errors[0].error_msg);
    }
}

test "variable declaration - type inference" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{
            .source = "x := 10;",
            .expected = "x: int = 10;",
        },
        .{
            .source = "x := 10.0;",
            .expected = "x: float = 10.000;",
        },
        .{
            .source = "x := true;",
            .expected = "x: bool = true;",
        },
        .{
            .source = "x := \"hey\";",
            .expected = "x: string = \"hey\";",
        },
        .{
            .source = "x := 10 + 2;",
            .expected = "x: int = (10 + 2):int;",
        },
        .{
            .source = "x := float(10);",
            .expected = "x: float = float(10);",
        },
        .{
            .source = "x := int(10 + 2.0);",
            .expected = "x: int = int((float(10) + 2.000):float);",
        },
        .{
            .source = "x := 2 > 1;",
            .expected = "x: bool = (2 > 1):bool;",
        },
        .{
            .source = "x := \"Hello, \" + \"World\";",
            .expected = "x: string = (\"Hello, \" + \"World\"):string;",
        },
        .{
            .source = "x := 10 + 2; y := x * 3;",
            .expected = "x: int = (10 + 2):int;\ny: int = (x:int * 3):int;",
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"", .{t.source});

        const actual = try testAnalyze(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}

test "mut variable declaration" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{
            .source = "mut x: int = 10;",
            .expected = "mut x: int = 10;",
        },
        .{
            .source = "mut x: string = \"hey\";",
            .expected = "mut x: string = \"hey\";",
        },
        .{
            .source = "mut x := 10;",
            .expected = "mut x: int = 10;",
        },
        .{
            .source = "mut x := 10.0;",
            .expected = "mut x: float = 10.000;",
        },
        .{
            .source = "mut x := true;",
            .expected = "mut x: bool = true;",
        },
        .{
            .source = "mut x := \"hey\";",
            .expected = "mut x: string = \"hey\";",
        },
        .{
            .source = "mut x := 10 + 2;",
            .expected = "mut x: int = (10 + 2):int;",
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"", .{t.source});

        const actual = try testAnalyze(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}

test "variable declaration - already defined" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        error_msg: []const u8,
    } = &.{
        .{
            .source = "x := 10; x := true",
            .error_msg = "Variable x is already defined",
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"\n", .{t.source});

        var nir = try testAnalyzeFailure(gpa, t.source);
        defer nir.deinit();

        try testing.expect(nir.errors.len == 1);
        try testing.expectEqualStrings(t.error_msg, nir.errors[0].error_msg);
    }
}

test "mut variable declaration - already defined" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        error_msg: []const u8,
    } = &.{
        .{
            .source = "mut x := 10; x := true",
            .error_msg = "Variable x is already defined",
        },
        .{
            .source = "x := 10; mut x := true",
            .error_msg = "Variable x is already defined",
        },
        .{
            .source = "mut x := 10; mut x := true",
            .error_msg = "Variable x is already defined",
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"\n", .{t.source});

        var nir = try testAnalyzeFailure(gpa, t.source);
        defer nir.deinit();

        try testing.expect(nir.errors.len == 1);
        try testing.expectEqualStrings(t.error_msg, nir.errors[0].error_msg);
    }
}

test "block statements" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{
            .source = "{}",
            .expected =
            \\{
            \\}
            ,
        },
        .{
            .source =
            \\{
            \\    2 + 1;
            \\}
            ,
            .expected =
            \\{
            \\    (2 + 1):int;
            \\}
            ,
        },
        .{
            .source =
            \\{
            \\    2 + 1;
            \\    2 * 1;
            \\}
            ,
            .expected =
            \\{
            \\    (2 + 1):int;
            \\    (2 * 1):int;
            \\}
            ,
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"", .{t.source});

        const actual = try testAnalyze(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}

test "global declaration order error" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        error_msg: []const u8,
    } = &.{
        .{
            .source = "x := z; z := 1;",
            .error_msg = "Variable z used before its definition",
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"\n", .{t.source});

        var nir = try testAnalyzeFailure(gpa, t.source);
        defer nir.deinit();

        try testing.expect(nir.errors.len == 1);
        try testing.expectEqualStrings(t.error_msg, nir.errors[0].error_msg);
    }
}

test "block scopes" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{
            .source =
            \\{
            \\    x := 1;
            \\}
            ,
            .expected =
            \\{
            \\    x: int = 1;
            \\}
            ,
        },
        .{
            .source =
            \\x := 1;
            \\{
            \\    y := x + 2;
            \\}
            ,
            .expected =
            \\x: int = 1;
            \\{
            \\    y: int = (x:int + 2):int;
            \\}
            ,
        },
        .{
            .source =
            \\{
            \\    y := "Hey";
            \\}
            \\{
            \\    y := 1 + 2;
            \\}
            ,
            .expected =
            \\{
            \\    y: string = "Hey";
            \\}
            \\{
            \\    y: int = (1 + 2):int;
            \\}
            ,
        },
        .{
            .source =
            \\{
            \\    y := x;
            \\}
            \\x := 10;
            ,
            .expected =
            \\x: int = 10;
            \\{
            \\    y: int = x:int;
            \\}
            ,
        },
        .{
            .source =
            \\{
            \\    y := float(x) + 2;
            \\}
            \\x := 10;
            ,
            .expected =
            \\x: int = 10;
            \\{
            \\    y: float = (float(x:int) + float(2)):float;
            \\}
            ,
        },
        .{
            .source =
            \\{
            \\    {
            \\        x := 1;
            \\    }
            \\    x := 2;
            \\}
            ,
            .expected =
            \\{
            \\    {
            \\    x: int = 1;
            \\}
            \\    x: int = 2;
            \\}
            ,
        },
        .{
            .source =
            \\{
            \\    x := 1;
            \\    {
            \\        y := 2;
            \\        {
            \\            z := x + y;
            \\        }
            \\    }
            \\}
            ,
            .expected =
            \\{
            \\    x: int = 1;
            \\    {
            \\    y: int = 2;
            \\    {
            \\    z: int = (x:int + y:int):int;
            \\}
            \\}
            \\}
            ,
        },
        .{
            .source =
            \\x := 10;
            \\{
            \\    y := x * 2;
            \\    {
            \\        z := y + x;
            \\    }
            \\}
            ,
            .expected =
            \\x: int = 10;
            \\{
            \\    y: int = (x:int * 2):int;
            \\    {
            \\    z: int = (y:int + x:int):int;
            \\}
            \\}
            ,
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
            .expected =
            \\{
            \\    a: int = 1;
            \\    {
            \\    b: int = (a:int + 1):int;
            \\    c: int = (a:int + b:int):int;
            \\}
            \\}
            ,
        },
        .{
            .source =
            \\x := 1;
            \\{
            \\    y := x + 1;
            \\}
            ,
            .expected =
            \\x: int = 1;
            \\{
            \\    y: int = (x:int + 1):int;
            \\}
            ,
        },
        .{
            .source =
            \\{
            \\    y := 1;
            \\    z := 2;
            \\}
            \\x := 10;
            ,
            .expected =
            \\x: int = 10;
            \\{
            \\    y: int = 1;
            \\    z: int = 2;
            \\}
            ,
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"", .{t.source});

        const actual = try testAnalyze(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}

test "scoping errors - undefined and redefined variables" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        error_msg: []const u8,
    } = &.{
        .{
            .source =
            \\x := 1;
            \\{
            \\    y := x + 2;
            \\}
            \\y := false;
            ,
            .error_msg = "Variable y is already defined",
        },
        .{
            .source =
            \\x := 1;
            \\y := false;
            \\{
            \\    y := x + 2;
            \\}
            ,
            .error_msg = "Variable y is already defined",
        },
        .{
            .source =
            \\{
            \\    y := 1 + 2;
            \\}
            \\y + 3;
            ,
            .error_msg = "Undefined variable y",
        },
        .{
            .source =
            \\{
            \\    y := x + 2;
            \\}
            ,
            .error_msg = "Undefined variable x",
        },
        .{
            .source =
            \\{
            \\    x := 3;
            \\}
            \\{
            \\    y := x + 2;
            \\}
            ,
            .error_msg = "Undefined variable x",
        },
        .{
            .source =
            \\{
            \\    x := 1;
            \\    {
            \\        y := x + 2;
            \\    }
            \\    z := y + 1;
            \\}
            ,
            .error_msg = "Undefined variable y",
        },
        .{
            .source =
            \\x := 1;
            \\x := 2;
            ,
            .error_msg = "Variable x is already defined",
        },
        .{
            .source =
            \\{
            \\    x := 1;
            \\    x := 2;
            \\}
            ,
            .error_msg = "Variable x is already defined",
        },
        .{
            .source =
            \\{
            \\    a := 1;
            \\    a := 2;
            \\    {
            \\        b := a + 1;
            \\    }
            \\    b := 10;
            \\}
            ,
            .error_msg = "Variable a is already defined",
        },
        .{
            .source =
            \\x := 1;
            \\y := 2;
            \\z := w;
            ,
            .error_msg = "Undefined variable w",
        },
        .{
            .source =
            \\{
            \\    x := 1;
            \\}
            \\{
            \\    y := x + 1;
            \\}
            \\x := 2;
            ,
            .error_msg = "Variable x is already defined",
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"\n", .{t.source});

        var nir = try testAnalyzeFailure(gpa, t.source);
        defer nir.deinit();

        try testing.expect(nir.errors.len == 1);
        try testing.expectEqualStrings(t.error_msg, nir.errors[0].error_msg);
    }
}

test "if statements" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{
            .source = "if true {}",
            .expected = "if true {\n}",
        },
        .{
            .source = "if false { x := 1; }",
            .expected = "if false {\n    x: int = 1;\n}",
        },
        .{
            .source = "if true { x := 1; }",
            .expected = "if true {\n    x: int = 1;\n}",
        },
        .{
            .source = "if true { x := 1; } else { x := 2; }",
            .expected = "if true {\n    x: int = 1;\n} else {\n    x: int = 2;\n}",
        },
        .{
            .source = "if false { x := 1; } else { x := 2; }",
            .expected = "if false {\n    x: int = 1;\n} else {\n    x: int = 2;\n}",
        },
        .{
            .source = "if false { x := 1; } else if true { x := 2; }",
            .expected = "if false {\n    x: int = 1;\n} else if true {\n    x: int = 2;\n}",
        },
        .{
            .source = "if false { x := 1; } else if true { x := 2; } else { x := 3; }",
            .expected = "if false {\n    x: int = 1;\n} else if true {\n    x: int = 2;\n} else {\n    x: int = 3;\n}",
        },
        .{
            .source = "if false { x := 1; } else if 1 > 2 { x := 2; } else if 3 > 4 { x := 3; } else { x := 4; }",
            .expected = "if false {\n    x: int = 1;\n} else if (1 > 2):bool {\n    x: int = 2;\n} else if (3 > 4):bool {\n    x: int = 3;\n} else {\n    x: int = 4;\n}",
        },
        .{
            .source = "x := true; if x { y := 1; }",
            .expected = "x: bool = true;\nif x:bool {\n    y: int = 1;\n}",
        },
        .{
            .source = "x := 1; if x > 0 { y := 1; }",
            .expected = "x: int = 1;\nif (x:int > 0):bool {\n    y: int = 1;\n}",
        },
        .{
            .source = "if true and false { x := 1; }",
            .expected = "if (true and false):bool {\n    x: int = 1;\n}",
        },
        .{
            .source = "if true or false { x := 1; }",
            .expected = "if (true or false):bool {\n    x: int = 1;\n}",
        },
        .{
            .source = "if !false { x := 1; }",
            .expected = "if (!false):bool {\n    x: int = 1;\n}",
        },
        .{
            .source =
            \\x := 1;
            \\y := 2;
            \\if x > 0 {
            \\    if y > 0 {
            \\        z := 1;
            \\    }
            \\}
            ,
            .expected =
            \\x: int = 1;
            \\y: int = 2;
            \\if (x:int > 0):bool {
            \\    if (y:int > 0):bool {
            \\    z: int = 1;
            \\}
            \\}
            ,
        },
        .{
            .source =
            \\x := 1;
            \\if x > 0 {
            \\    y := 2;
            \\} else {
            \\    y := 3;
            \\}
            ,
            .expected =
            \\x: int = 1;
            \\if (x:int > 0):bool {
            \\    y: int = 2;
            \\} else {
            \\    y: int = 3;
            \\}
            ,
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"", .{t.source});

        const actual = try testAnalyze(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}

test "if statement failure" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        error_msg: []const u8,
    } = &.{
        .{
            .source =
            \\if 2 {
            \\print(2);
            \\}
            ,
            .error_msg = "Expected condition to be bool got int",
        },
        .{
            .source =
            \\if false {
            \\print(2);
            \\} else if "hey" {}
            ,
            .error_msg = "Expected condition to be bool got string",
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"\n", .{t.source});

        var nir = try testAnalyzeFailure(gpa, t.source);
        defer nir.deinit();

        try testing.expect(nir.errors.len == 1);
        try testing.expectEqualStrings(t.error_msg, nir.errors[0].error_msg);
    }
}

test "variable assignment" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{
            .source =
            \\{
            \\    mut x := 10;
            \\    x = 5;
            \\}
            ,
            .expected =
            \\{
            \\    mut x: int = 10;
            \\    x = 5;
            \\}
            ,
        },
        .{
            .source =
            \\{
            \\    mut x := 10;
            \\    y := 5;
            \\    x = y + 3;
            \\}
            ,
            .expected =
            \\{
            \\    mut x: int = 10;
            \\    y: int = 5;
            \\    x = (y:int + 3):int;
            \\}
            ,
        },
        .{
            .source =
            \\mut x := 10;
            \\{
            \\    x = 3;
            \\}
            ,
            .expected =
            \\mut x: int = 10;
            \\{
            \\    x = 3;
            \\}
            ,
        },
        .{
            .source =
            \\{
            \\    x = 3;
            \\}
            \\mut x := 10;
            ,
            .expected =
            \\mut x: int = 10;
            \\{
            \\    x = 3;
            \\}
            ,
        },
        .{
            .source =
            \\{
            \\    mut x := 10;
            \\    mut y := 5;
            \\    x = y;
            \\}
            ,
            .expected =
            \\{
            \\    mut x: int = 10;
            \\    mut y: int = 5;
            \\    x = y:int;
            \\}
            ,
        },
        .{
            .source =
            \\{
            \\    mut x := 10;
            \\    {
            \\        x = 5;
            \\    }
            \\}
            ,
            .expected =
            \\{
            \\    mut x: int = 10;
            \\    {
            \\    x = 5;
            \\}
            \\}
            ,
        },
        .{
            .source =
            \\{
            \\    mut x := 10;
            \\    if true {
            \\        x = 5;
            \\    }
            \\}
            ,
            .expected =
            \\{
            \\    mut x: int = 10;
            \\    if true {
            \\    x = 5;
            \\}
            \\}
            ,
        },
        .{
            .source =
            \\{
            \\    mut x := 10;
            \\    if false {
            \\        x = 5;
            \\    } else {
            \\        x = 3;
            \\    }
            \\}
            ,
            .expected =
            \\{
            \\    mut x: int = 10;
            \\    if false {
            \\    x = 5;
            \\} else {
            \\    x = 3;
            \\}
            \\}
            ,
        },
        .{
            .source =
            \\{
            \\    mut x := 10;
            \\    if false {
            \\        x = 5;
            \\    } else if true {
            \\        x = 3;
            \\    }
            \\}
            ,
            .expected =
            \\{
            \\    mut x: int = 10;
            \\    if false {
            \\    x = 5;
            \\} else if true {
            \\    x = 3;
            \\}
            \\}
            ,
        },
        .{
            .source =
            \\{
            \\    mut x := 10;
            \\    if false {
            \\        x = 5;
            \\    } else if false {
            \\        x = 3;
            \\    } else {
            \\        x = 7;
            \\    }
            \\}
            ,
            .expected =
            \\{
            \\    mut x: int = 10;
            \\    if false {
            \\    x = 5;
            \\} else if false {
            \\    x = 3;
            \\} else {
            \\    x = 7;
            \\}
            \\}
            ,
        },
        .{
            .source =
            \\{
            \\    mut x := 10;
            \\    {
            \\        mut y := 5;
            \\        x = y;
            \\    }
            \\}
            ,
            .expected =
            \\{
            \\    mut x: int = 10;
            \\    {
            \\    mut y: int = 5;
            \\    x = y:int;
            \\}
            \\}
            ,
        },
        .{
            .source =
            \\{
            \\    mut x := 10;
            \\    mut y := 5;
            \\    x = y + 2;
            \\}
            ,
            .expected =
            \\{
            \\    mut x: int = 10;
            \\    mut y: int = 5;
            \\    x = (y:int + 2):int;
            \\}
            ,
        },
        .{
            .source =
            \\{
            \\    mut x := 10;
            \\    mut y := 5;
            \\    y = x + 2;
            \\}
            ,
            .expected =
            \\{
            \\    mut x: int = 10;
            \\    mut y: int = 5;
            \\    y = (x:int + 2):int;
            \\}
            ,
        },
        .{
            .source =
            \\{
            \\    mut x := 10;
            \\    mut y := 5;
            \\    mut z := 3;
            \\    x = y + z;
            \\}
            ,
            .expected =
            \\{
            \\    mut x: int = 10;
            \\    mut y: int = 5;
            \\    mut z: int = 3;
            \\    x = (y:int + z:int):int;
            \\}
            ,
        },
        .{
            .source =
            \\{
            \\    mut x := 10;
            \\    x = x + 1;
            \\}
            ,
            .expected =
            \\{
            \\    mut x: int = 10;
            \\    x = (x:int + 1):int;
            \\}
            ,
        },
        .{
            .source =
            \\{
            \\    mut a := 1.5;
            \\    mut b := 2.5;
            \\    a = b;
            \\}
            ,
            .expected =
            \\{
            \\    mut a: float = 1.500;
            \\    mut b: float = 2.500;
            \\    a = b:float;
            \\}
            ,
        },
        .{
            .source =
            \\{
            \\    mut x := 10;
            \\    mut y := 5;
            \\    if x > y {
            \\        x = y;
            \\    }
            \\}
            ,
            .expected =
            \\{
            \\    mut x: int = 10;
            \\    mut y: int = 5;
            \\    if (x:int > y:int):bool {
            \\    x = y:int;
            \\}
            \\}
            ,
        },
        .{
            .source =
            \\{
            \\    mut x := 10;
            \\    if true {
            \\        mut y := 5;
            \\        x = y;
            \\    } else {
            \\        x = 3;
            \\    }
            \\}
            ,
            .expected =
            \\{
            \\    mut x: int = 10;
            \\    if true {
            \\    mut y: int = 5;
            \\    x = y:int;
            \\} else {
            \\    x = 3;
            \\}
            \\}
            ,
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"", .{t.source});

        const actual = try testAnalyze(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}

test "variable assignment failure" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        error_msg: []const u8,
    } = &.{
        .{
            .source = "mut x := 10; x = 1;",
            .error_msg = "Variable assignments are not allowed in the global scope.",
        },
        .{
            .source = "{x = 10;}",
            .error_msg = "Undefined variable x",
        },
        .{
            .source = "x := 3; { x = 10; }",
            .error_msg = "Variable x is not declared as mutable.",
        },
        .{
            .source = "{ x = 10; } x := 3;",
            .error_msg = "Variable x is not declared as mutable.",
        },
        .{
            .source = " { x := 3; x = 10; }",
            .error_msg = "Variable x is not declared as mutable.",
        },
        .{
            .source =
            \\{
            \\    mut x := 10;
            \\    x = false;
            \\}
            ,
            .error_msg = "Expected int got bool",
        },
        .{
            .source =
            \\{
            \\    mut x := 10;
            \\    if true {
            \\        x = false;
            \\    }
            \\}
            ,
            .error_msg = "Expected int got bool",
        },
        .{
            .source =
            \\{
            \\    mut x := 10;
            \\    if false {
            \\        x = 5;
            \\    } else {
            \\        x = false;
            \\    }
            \\}
            ,
            .error_msg = "Expected int got bool",
        },
        .{
            .source =
            \\{
            \\    mut x := 10;
            \\    if false {
            \\        x = 5;
            \\    } else if true {
            \\        x = false;
            \\    }
            \\}
            ,
            .error_msg = "Expected int got bool",
        },
        .{
            .source =
            \\{
            \\    mut x := 10;
            \\    {
            \\        x = false;
            \\    }
            \\}
            ,
            .error_msg = "Expected int got bool",
        },
        .{
            .source =
            \\{
            \\    {
            \\        mut x := 10;
            \\    }
            \\    x = 5;
            \\}
            ,
            .error_msg = "Undefined variable x",
        },
        .{
            .source =
            \\{
            \\    mut x := 10;
            \\    mut y := 5;
            \\    x = y;
            \\    z = 3;
            \\}
            ,
            .error_msg = "Undefined variable z",
        },
        .{
            .source =
            \\{
            \\    mut x := 10;
            \\    {
            \\        mut y := 5;
            \\        y = 3;
            \\    }
            \\    y = 7;
            \\}
            ,
            .error_msg = "Undefined variable y",
        },
        .{
            .source =
            \\{
            \\    mut x := 10;
            \\    if x > 5 {
            \\        mut y := 5;
            \\    }
            \\    y = 3;
            \\}
            ,
            .error_msg = "Undefined variable y",
        },
        .{
            .source =
            \\{
            \\    mut x := 10;
            \\    mut y := 5;
            \\    mut z := 3;
            \\    x = z;
            \\    w = 1;
            \\}
            ,
            .error_msg = "Undefined variable w",
        },
        .{
            .source =
            \\{
            \\    mut x := 10;
            \\    mut x := 5;
            \\    x = 3;
            \\}
            ,
            .error_msg = "Variable x is already defined",
        },
        .{
            .source =
            \\{
            \\    mut x := 10;
            \\    if false {
            \\        x = 5;
            \\    } else if false {
            \\        x = 3;
            \\    } else {
            \\        x = false;
            \\    }
            \\}
            ,
            .error_msg = "Expected int got bool",
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"\n", .{t.source});

        var nir = try testAnalyzeFailure(gpa, t.source);
        defer nir.deinit();

        try testing.expect(nir.errors.len == 1);
        try testing.expectEqualStrings(t.error_msg, nir.errors[0].error_msg);
    }
}

test "for loops" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{
            .source =
            \\for mut i := 0; i < 10; i += 1 {
            \\}
            ,
            .expected =
            \\for mut i: int = 0; (i:int < 10):bool; i = (i:int + 1):int; {
            \\}
            ,
        },
        .{
            .source =
            \\for mut i := 0; i < 10; {
            \\}
            ,
            .expected =
            \\for mut i: int = 0; (i:int < 10):bool; {
            \\}
            ,
        },
        .{
            .source =
            \\i := 10;
            \\for i < 10 {
            \\}
            ,
            .expected =
            \\i: int = 10;
            \\for (i:int < 10):bool {
            \\}
            ,
        },
        .{
            .source =
            \\for {
            \\}
            ,
            .expected =
            \\for {
            \\}
            ,
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"", .{t.source});

        const actual = try testAnalyze(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}

test "loop jump statements" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{
            .source =
            \\for {
            \\    break;
            \\}
            ,
            .expected =
            \\for {
            \\    break;
            \\}
            ,
        },
        .{
            .source =
            \\for {
            \\    continue;
            \\}
            ,
            .expected =
            \\for {
            \\    continue;
            \\}
            ,
        },
        .{
            .source =
            \\for mut i := 0; i < 3; i += 1 {
            \\    if true {
            \\        break;
            \\    }
            \\}
            ,
            .expected =
            \\for mut i: int = 0; (i:int < 3):bool; i = (i:int + 1):int; {
            \\    if true {
            \\    break;
            \\}
            \\}
            ,
        },
        .{
            .source =
            \\for {
            \\    for {
            \\        continue;
            \\    }
            \\}
            ,
            .expected =
            \\for {
            \\    for {
            \\    continue;
            \\}
            \\}
            ,
        },
        .{
            .source =
            \\for {
            \\    for {
            \\        continue;
            \\    }
            \\    break;
            \\}
            ,
            .expected =
            \\for {
            \\    for {
            \\    continue;
            \\}
            \\    break;
            \\}
            ,
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"", .{t.source});

        const actual = try testAnalyze(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}

test "functions, calls, and return stmts" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{
            .source =
            \\fn () {
            \\    return;
            \\}
            ,
            .expected =
            \\fn () {
            \\    return;
            \\};
            ,
        },
        .{
            .source =
            \\add := fn (a: int, b: int) int {
            \\    return a + b;
            \\}
            ,
            .expected =
            \\add: function = fn (a: int, b: int) int {
            \\    return (a:int + b:int):int;
            \\};
            ,
        },
        .{
            .source =
            \\fn (a: int, b: int) int {
            \\    return a + b;
            \\}
            ,
            .expected =
            \\fn (a: int, b: int) int {
            \\    return (a:int + b:int):int;
            \\};
            ,
        },
        .{
            .source =
            \\add := fn (a: int, b: int) int {
            \\    return a + b;
            \\}
            \\x := add(2, 3);
            ,
            .expected =
            \\add: function = fn (a: int, b: int) int {
            \\    return (a:int + b:int):int;
            \\};
            \\x: int = add(2, 3):int;
            ,
        },
        .{
            .source =
            \\add := fn (a: int, b: int) int {
            \\    return a + b;
            \\}
            \\x := fn () int {
            \\    return add(2,3);
            \\}
            ,
            .expected =
            \\add: function = fn (a: int, b: int) int {
            \\    return (a:int + b:int):int;
            \\};
            \\x: function = fn () int {
            \\    return add(2, 3):int;
            \\};
            ,
        },
        .{
            .source =
            \\x := fn () int {
            \\    return add(2,3);
            \\}
            \\add := fn (a: int, b: int) int {
            \\    return a + b;
            \\}
            ,
            .expected =
            \\x: function = fn () int {
            \\    return add(2, 3):int;
            \\};
            \\add: function = fn (a: int, b: int) int {
            \\    return (a:int + b:int):int;
            \\};
            ,
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"", .{t.source});

        const actual = try testAnalyze(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}

test "temporary print stmt" {
    const gpa = testing.allocator;
    const tests: []const struct {
        source: []const u8,
        expected: []const u8,
    } = &.{
        .{
            .source = "print(10);",
            .expected = "print(10);",
        },
        .{
            .source = "x := 10; print(x);",
            .expected = "x: int = 10;\nprint(x:int);",
        },
    };

    for (tests) |t| {
        errdefer std.debug.print("failed test case with source=\"{s}\"", .{t.source});

        const actual = try testAnalyze(gpa, t.source);
        defer gpa.free(actual);
        try testing.expectEqualStrings(t.expected, actual);
    }
}
