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
const oom = debug.oom;
const ers = @import("errors.zig");
const Token = @import("Lexer.zig").Token;

const trait = @import("trait.zig");

pub const Scope = struct {
    level: Level,

    /// If `level` is .top then this is just the number of globals
    /// If `level` is .local then this is relative to the closest containing function
    /// If `level` is .local then this includes includes locals in all parent scopes
    local_count: usize,

    /// If `level` is .top this field is garbage
    parent: *Scope,

    pub const Level = union(enum) {
        top,
        local,
    };
};

pub const Symbol = struct {
    type: NormType,
    mutable: bool,
    scope: *Scope,
    stack_slot: usize,
};

pub const SymbolTable = struct {
    gpa: Allocator,
    arena: std.heap.ArenaAllocator,
    top: SymMap,
    locals: std.AutoHashMapUnmanaged(*Scope, SymMap),
    top_scope: *Scope,
    current_scope: *Scope,

    pub const SymMap = std.StringHashMapUnmanaged(Symbol);

    pub fn init(gpa: Allocator) SymbolTable {
        var arena: std.heap.ArenaAllocator = .init(gpa);
        const top_scope = arena.allocator().create(Scope) catch oom();
        top_scope.* = .{
            .local_count = 0,
            .parent = undefined,
            .level = .top,
        };

        return .{
            .gpa = gpa,
            .arena = arena,
            .top_scope = top_scope,
            .current_scope = top_scope,
            .top = .empty,
            .locals = .empty,
        };
    }

    pub fn deinit(st: *SymbolTable) void {
        st.arena.deinit();
        st.* = undefined;
    }

    /// returns true if the symbol already exists
    pub fn register(st: *SymbolTable, name: []const u8, ty: NormType, mutable: bool) bool {
        if (st.symDefined(name, st.current_scope)) return true;

        st.registerSym(name, ty, mutable);

        return false;
    }

    /// If the return value is null then it means that the symbol
    /// has already been defined.
    pub fn findOrRegister(st: *SymbolTable, name: []const u8, mutable: bool) ?*Symbol {
        if (st.current_scope.level == .top) {
            return st.tryFind(name);
        }
        const already_exists = st.register(name, .n_unknown, mutable);
        if (already_exists) return null;
        return st.tryFind(name).?;
    }

    fn registerSym(st: *SymbolTable, name: []const u8, ty: NormType, mutable: bool) void {
        switch (st.current_scope.level) {
            .top => {
                const stack_slot = st.top.count();
                const sym: Symbol = .{
                    .type = ty,
                    .mutable = mutable,
                    .scope = st.top_scope,
                    .stack_slot = stack_slot,
                };

                st.top.put(st.arena.allocator(), name, sym) catch oom();
            },
            .local => {
                const stack_slot = st.current_scope.local_count;
                const sym: Symbol = .{
                    .type = ty,
                    .scope = st.current_scope,
                    .stack_slot = stack_slot,
                    .mutable = mutable,
                };

                const locals = st.locals.getPtr(st.current_scope).?;
                locals.put(st.arena.allocator(), name, sym) catch oom();

                st.current_scope.local_count += 1;
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
                return locals.contains(name) or st.symDefined(name, scope.parent);
            },
        }
    }

    pub fn beginScope(st: *SymbolTable) *Scope {
        // we assume this is not a function scope so we use the parent scope's `local_count`
        const parent_local_count = st.current_scope.local_count;
        const new_scope = st.newScope(.local, parent_local_count, st.current_scope);
        st.locals.put(st.arena.allocator(), new_scope, .empty) catch oom();
        st.current_scope = new_scope;
        return new_scope;
    }

    pub fn endScope(st: *SymbolTable) void {
        assert(st.current_scope.level != .top);
        st.current_scope = st.current_scope.parent;
    }

    pub fn beginFn(st: *SymbolTable) *Scope {
        const new_scope = st.newScope(.local, 0, st.current_scope);
        st.locals.put(st.arena.allocator(), new_scope, .empty) catch oom();
        st.current_scope = new_scope;
        return new_scope;
    }

    pub fn endFn(st: *SymbolTable) void {
        assert(st.current_scope.level != .top);
        st.current_scope = st.current_scope.parent;
    }

    fn newScope(st: *SymbolTable, scope_level: Scope.Level, local_count: usize, parent: *Scope) *Scope {
        const scope = st.arena.allocator().create(Scope) catch oom();
        scope.* = .{
            .level = scope_level,
            .local_count = local_count,
            .parent = parent,
        };
        return scope;
    }

    pub fn tryFind(st: *SymbolTable, name: []const u8) ?*Symbol {
        return st.tryFindScoped(name, st.current_scope);
    }

    pub fn find(st: *SymbolTable, name: []const u8, scope: *Scope) *Symbol {
        return st.tryFindScoped(name, scope).?;
    }

    pub fn tryFindScoped(st: *SymbolTable, name: []const u8, scope: *Scope) ?*Symbol {
        switch (scope.level) {
            .top => return st.top.getPtr(name),
            .local => {
                const locals = st.locals.getPtr(scope).?;
                return locals.getPtr(name) orelse st.tryFindScoped(name, scope.parent);
            },
        }
    }
};

pub const NormType = union(enum) {
    pub const Function = struct {
        parameters: []Expr.Function.Parameter,
        return_type: NormType,
    };

    pub const Tag = std.meta.Tag(NormType);

    n_unknown,

    n_int,
    n_float,
    n_bool,
    n_string,
    n_void,

    n_function: *Function,

    pub fn format(nt: NormType, w: *Io.Writer) Io.Writer.Error!void {
        switch (nt) {
            // .n_function => |f| {
            //     try w.writeAll("fn (");
            //     for (f.parameters, 0..) |param, i| {
            //         if (i < f.parameters.len - 1) {
            //             try w.print("{s}:  {f}, ", .{ param.name.lexeme, param.type });
            //         } else {
            //             try w.print("{s}:  {f}", .{ param.name.lexeme, param.type });
            //         }
            //     }
            //     try w.print(") {f}", .{f.return_type});
            // },
            inline else => try w.print("{s}", .{@tagName(nt)[2..]}),
        }
    }

    pub fn isNumeric(ty: NormType) bool {
        return ty == .n_float or ty == .n_int;
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
};

pub const Stmt = union(enum) {
    pub const Expression = struct {
        expr: *Expr,
    };

    pub const VarDecl = struct {
        ident: Token,
        type: NormType,
        // TODO: zero values
        value: *Expr,
        // I don't think this field is necessary, I just use it for formatting and that's it.
        mutable: bool,
    };

    pub const VarAssign = struct {
        ident: Token,
        value: *Expr,
    };

    pub const Block = struct {
        token: Token, // {
        end_token: Token, // }
        stmts: []Stmt,
        scope: *Scope,
    };

    pub const Print = struct {
        token: Token,
        expr: *Expr,
    };

    pub const If = struct {
        pub const ElseIf = struct {
            token: Token, // if
            condition: *Expr,
            then_block: Block,
        };

        token: Token, // if
        condition: *Expr,
        then_block: Block,
        else_if_blocks: []ElseIf,
        else_block: ?Block,
    };

    pub const For = struct {
        pub const InitializerStmt = union(enum) {
            var_decl: VarDecl,
            var_assign: VarAssign,
            expr: Expression,
        };

        pub const IncrementStmt = union(enum) {
            var_assign: VarAssign,
            expr: Expression,
        };

        token: Token, // for
        scope: *Scope,

        // initializer and increment are guaranteed to not both be null
        initializer: ?InitializerStmt,
        condition: *Expr,
        increment: ?IncrementStmt,

        block: Block,
    };

    pub const ConditionFor = struct {
        token: Token, // for
        scope: *Scope,
        condition: *Expr,
        block: Block,
    };

    pub const InfiniteFor = struct {
        token: Token, // for
        scope: *Scope,
        block: Block,
    };

    pub const Break = struct {
        token: Token,
        jump_scope: *Scope,
    };

    pub const Continue = struct {
        token: Token,
        jump_scope: *Scope,
    };

    pub const Return = struct {
        token: Token, // return
        expr: ?*Expr,
    };

    pub const invalid: Stmt = undefined;

    expression: Expression,
    var_decl: VarDecl,
    var_assign: VarAssign,
    block: Block,
    if_stmt: If,
    for_stmt: For,
    condition_for: ConditionFor,
    infinite_for: InfiniteFor,
    break_stmt: Break,
    continue_stmt: Continue,
    return_stmt: Return,
    print: Print,
};

pub const Expr = struct {
    pub const Binary = struct {
        left: *Expr,
        operator: Token,
        right: *Expr,
    };

    pub const Unary = struct {
        operator: Token,
        expr: *Expr,
    };

    pub const Cast = struct {
        token: Token,
        expr: *Expr,
    };

    pub const Literal = struct {
        pub const Value = union(enum) {
            integer: i32,
            float: f64,
            string: []const u8,
            boolean: bool,
            nil: void,

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
    };

    pub const Grouping = struct {
        paren: Token,
        expr: *Expr,
    };

    pub const Identifier = struct {
        ident: Token,
        scope: *Scope,
    };

    pub const Function = struct {
        token: Token, // fn
        scope: *Scope,
        body: Stmt.Block,

        pub const Parameter = struct {
            name: Token,
            type: NormType,
        };
    };

    pub const Call = struct {
        token: Token, // left_paren
        callee: *Expr,
        args: []*Expr,
    };

    type: NormType,
    kind: union(enum) {
        binary: Binary,
        unary: Unary,
        cast: Cast,
        grouping: Grouping,
        identifier: Identifier,
        literal: Literal,
        function: Function,
        call: Call,
    },

    pub const invalid: Expr = .{
        .type = .n_unknown,
        .kind = undefined,
    };

    pub fn token(e: *const Expr) Token {
        if (e.type == .n_unknown) return .{ .type = .eof, .lexeme = "", .line = 0 };

        return switch (e.kind) {
            .binary => |*b| b.operator,
            .unary => |*u| u.operator,
            .cast => |*c| c.token,
            .literal => |*l| l.token,
            .identifier => |*i| i.ident,
            .grouping => |*g| g.paren,
            .function => |*f| f.token,
            .call => |*c| c.token,
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
