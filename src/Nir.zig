//! Norm intermediate representation
//!
//! Typed AST for a file

const std = @import("std");
const mem = std.mem;
const Io = std.Io;

const assert = std.debug.assert;
const Allocator = mem.Allocator;
const Ast = @import("Ast.zig");
const debug = @import("debug.zig");
const ers = @import("errors.zig");
const Token = @import("Lexer.zig").Token;

const trait = @import("trait.zig");

pub const Scope = struct {
    level: Level,
    // If `level` is .top this field is garbage
    parent: *Scope,

    pub const Level = enum {
        top,
        local,
    };
};

pub const Symbol = struct {
    type: NormType,
    scope: *Scope,
};

pub const SymbolTable = struct {
    gpa: Allocator,
    top: SymMap,
    scope_arena: std.heap.ArenaAllocator,
    locals: std.AutoHashMapUnmanaged(*Scope, Locals),
    top_scope: *Scope,
    current_scope: *Scope,

    pub const SymMap = std.StringHashMapUnmanaged(Symbol);

    pub const Locals = struct {
        locals: SymMap,
    };

    pub fn init(gpa: Allocator) SymbolTable {
        var scope_arena: std.heap.ArenaAllocator = .init(gpa);
        const top_scope = scope_arena.allocator().create(Scope) catch oom();
        top_scope.* = .{
            .parent = undefined,
            .level = .top,
        };

        return .{
            .gpa = gpa,
            .scope_arena = scope_arena,
            .top_scope = top_scope,
            .current_scope = top_scope,
            .top = .empty,
            .locals = .empty,
        };
    }

    pub fn deinit(st: *SymbolTable) void {
        st.scope_arena.deinit();
        st.top.deinit(st.gpa);
        st.locals.deinit(st.gpa);
        st.* = undefined;
    }

    /// returns true if the symbol already exists
    pub fn register(st: *SymbolTable, name: []const u8, ty: NormType) bool {
        if (st.symDefined(name, st.current_scope)) return true;

        const sym: Symbol = .{ .type = ty, .scope = st.current_scope };
        st.registerSym(name, sym);

        return false;
    }

    fn registerSym(st: *SymbolTable, name: []const u8, sym: Symbol) void {
        switch (st.current_scope.level) {
            .top => {
                st.top.put(st.gpa, name, sym) catch oom();
            },
            .local => {
                const locals = st.locals.getPtr(st.current_scope).?;
                locals.locals.put(st.gpa, name, sym) catch oom();
            },
        }
    }

    fn symDefined(st: *SymbolTable, name: []const u8, scope: *Scope) bool {
        switch (scope.level) {
            .top => {
                return st.top.contains(name);
            },
            .local => {
                const locals = st.locals.get(scope).?;
                return locals.locals.contains(name) or st.symDefined(name, scope.parent);
            },
        }
    }

    pub fn beginScope(st: *SymbolTable) void {
        const new_scope = st.newScope(.local, st.current_scope);
        st.locals.put(st.gpa, new_scope, .{ .locals = .empty }) catch oom();
        st.current_scope = new_scope;
    }

    pub fn endScope(st: *SymbolTable) void {
        assert(st.current_scope.level != .top);
        st.current_scope = st.current_scope.parent;
    }

    fn newScope(st: *SymbolTable, scope_level: Scope.Level, parent: *Scope) *Scope {
        const scope = st.scope_arena.allocator().create(Scope) catch oom();
        scope.* = .{
            .level = scope_level,
            .parent = parent,
        };
        return scope;
    }

    pub fn tryFind(st: *SymbolTable, name: []const u8) ?*Symbol {
        return st.findSym(name, st.current_scope);
    }

    pub fn find(st: *SymbolTable, name: []const u8) *Symbol {
        return st.findSym(name, st.current_scope).?;
    }

    fn findSym(st: *SymbolTable, name: []const u8, scope: *Scope) ?*Symbol {
        switch (scope.level) {
            .top => return st.top.getPtr(name),
            .local => {
                const locals = st.locals.getPtr(scope).?;
                return locals.locals.getPtr(name) orelse st.findSym(name, scope.parent);
            },
        }
    }

    /// If the return value is null then it means that the symbol
    /// has already been defined.
    pub fn findOrRegister(st: *SymbolTable, name: []const u8) ?*Symbol {
        const sym = st.tryFind(name) orelse {
            const already_exists = st.register(name, .n_invalid);
            if (already_exists) return null;
            return st.tryFind(name).?;
        };
        return sym;
    }
};

pub const NormType = enum {
    n_invalid,

    n_int,
    n_float,
    n_bool,
    n_string,

    pub fn format(nt: NormType, w: *Io.Writer) Io.Writer.Error!void {
        try w.print("{s}", .{@tagName(nt)[2..]});
    }

    pub fn isNumeric(ty: NormType) bool {
        return ty == .n_float or ty == .n_int;
    }

    pub fn isInt(ty: NormType) bool {
        return ty == .n_int;
    }

    pub fn isFloat(ty: NormType) bool {
        return ty == .n_float;
    }

    pub fn isBool(ty: NormType) bool {
        return ty == .n_bool;
    }

    pub fn isString(ty: NormType) bool {
        return ty == .n_string;
    }

    pub fn isComparable(ty: NormType) bool {
        return switch (ty) {
            .n_int, .n_float, .n_bool, .n_string => true,
            else => false,
        };
    }

    pub fn isOrderable(ty: NormType) bool {
        return switch (ty) {
            .n_int, .n_float => true,
            else => false,
        };
    }

    pub fn int(ty: NormType) trait.SmallestEnumBackingType(NormType) {
        return @intFromEnum(ty);
    }
};

pub const Stmt = union(enum) {
    pub const Expression = struct {
        expr: *Expr,

        pub fn format(e: *const Stmt.Expression, w: *Io.Writer) Io.Writer.Error!void {
            try w.print("{f};", .{e.expr});
        }
    };

    pub const VarDecl = struct {
        ident: Token,
        type: NormType,
        // sema could possibly automatically set this to the zero value
        value: *Expr,

        pub fn format(vd: *const Stmt.VarDecl, w: *Io.Writer) Io.Writer.Error!void {
            try w.print("{s}: {f} = {f};", .{ vd.ident.lexeme, vd.type, vd.value });
        }
    };

    pub const VarAssign = struct {
        ident: Token,
        value: *Expr,

        pub fn format(va: *const Stmt.VarAssign, w: *Io.Writer) Io.Writer.Error!void {
            try w.print("{s} = {f};", .{ va.ident.lexeme, va.value });
        }
    };

    pub const Block = struct {
        token: Token, // {
        stmts: []Stmt,

        pub fn format(b: *const Stmt.Block, w: *Io.Writer) Io.Writer.Error!void {
            try w.writeAll("{\n");
            for (b.stmts) |stmt| {
                try w.print("    {f}\n", .{stmt});
            }
            try w.writeAll("}");
        }
    };

    pub const Print = struct {
        print: Token,
        expr: *Expr,

        pub fn format(p: *const Stmt.Print, w: *Io.Writer) Io.Writer.Error!void {
            try w.print("print({f});", .{p.expr});
        }
    };

    pub const invalid: Stmt = undefined;

    expression: Expression,
    var_decl: VarDecl,
    var_assign: VarAssign,
    block: Block,
    print: Print,

    pub fn format(stmt: Stmt, w: *Io.Writer) Io.Writer.Error!void {
        switch (stmt) {
            inline else => |s| try w.print("{f}", .{s}),
        }
    }
};

pub const Expr = struct {
    pub const Binary = struct {
        left: *Expr,
        operator: Token,
        right: *Expr,

        pub fn format(expr: *const Binary, w: *Io.Writer) Io.Writer.Error!void {
            try w.print("({f} {s} {f})", .{ expr.left, expr.operator.lexeme, expr.right });
        }
    };

    pub const Unary = struct {
        operator: Token,
        expr: *Expr,

        pub fn format(expr: *const Unary, w: *Io.Writer) Io.Writer.Error!void {
            try w.print("({s}{f})", .{ expr.operator.lexeme, expr.expr });
        }
    };

    pub const Cast = struct {
        token: Token,
        expr: *Expr,

        pub fn format(expr: *const Cast, w: *Io.Writer) Io.Writer.Error!void {
            const target = switch (expr.token.type) {
                .kw_float => "float",
                .kw_int => "int",
                else => unreachable,
            };
            try w.print("{s}({f})", .{ target, expr.expr });
        }
    };

    pub const Literal = struct {
        pub const Value = union(enum) {
            integer: i32,
            float: f64,
            string: []const u8,
            boolean: bool,
            nil: void,

            pub fn format(value: *const Value, w: *Io.Writer) Io.Writer.Error!void {
                try switch (value.*) {
                    .integer => |i| w.print("{}", .{i}),
                    .float => |i| w.print("{d:.3}", .{i}),
                    .string => |i| w.print("{s}", .{i}),
                    .boolean => |i| w.print("{}", .{i}),
                    .nil => w.print("nil", .{}),
                };
            }

            pub fn fromAst(ast_val: Ast.Expr.Literal.Value) Value {
                return switch (ast_val) {
                    .integer => |x| .{ .integer = x },
                    .float => |x| .{ .float = x },
                    .string => |x| .{ .string = x },
                    .boolean => |x| .{ .boolean = x },
                    .nil => .nil,
                };
            }
        };

        value: Value,
        token: Token,

        pub fn format(expr: *const Literal, w: *Io.Writer) Io.Writer.Error!void {
            try switch (expr.value) {
                .string => |s| w.print("\"{s}\"", .{s}),
                else => w.print("{f}", .{expr.value}),
            };
        }
    };

    pub const Grouping = struct {
        paren: Token,
        expr: *Expr,

        pub fn format(expr: *const Grouping, w: *Io.Writer) Io.Writer.Error!void {
            try w.print("({f})", .{expr.expr});
        }
    };

    pub const Identifier = struct {
        ident: Token,
        scope: *Scope,

        pub fn format(i: *const Identifier, w: *Io.Writer) Io.Writer.Error!void {
            try w.print("{s}", .{i.ident.lexeme});
        }
    };

    type: NormType,
    kind: union(enum) {
        binary: Binary,
        unary: Unary,
        cast: Cast,
        grouping: Grouping,
        identifier: Identifier,
        literal: Literal,
    },

    pub const invalid: Expr = .{
        .type = .n_invalid,
        .kind = undefined,
    };

    pub fn token(e: *const Expr) Token {
        return switch (e.kind) {
            .binary => |*b| b.operator,
            .unary => |*u| u.operator,
            .cast => |*c| c.token,
            .literal => |*l| l.token,
            .identifier => |*i| i.ident,
            .grouping => |*g| g.paren,
        };
    }

    pub fn format(expr: *const Expr, w: *Io.Writer) Io.Writer.Error!void {
        if (expr.type == .n_invalid) {
            @panic("invalid - an error was not reported");
        }

        try switch (expr.kind) {
            inline .cast, .literal => |l| w.print("{f}", .{l}),
            inline else => |b| w.print("{f}:{f}", .{ b, expr.type }),
        };
    }
};

pub const Diagnostics = struct {
    line: u32,
    hints: []const []const u8 = &.{},
    notes: []const []const u8 = &.{},
    error_msg: []const u8,

    pub fn promote(
        d: *const Diagnostics,
        file_name: []const u8,
        source: []const u8,
    ) ers.Diagnostics {
        return .{
            .error_msg = d.error_msg,
            .line = d.line,
            .notes = d.notes,
            .hints = d.hints,
            .file_name = file_name,
            .source = source,
        };
    }
};

pub fn deinit(self: *@This()) void {
    self.arena.deinit();
    self.sym_table.deinit();
    self.* = undefined;
}

arena: std.heap.ArenaAllocator,
stmts: []Stmt,
sym_table: SymbolTable,
errors: []Diagnostics,

fn oom() noreturn {
    @panic("oom");
}
