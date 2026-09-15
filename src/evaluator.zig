const std = @import("std");
const testing = std.testing;
const ast = @import("ast.zig");

pub const Value = union(enum) {
    null,
    boolean: bool,
    int: i64,
    string: []const u8,
    array: []const Value,
    object: std.StringHashMap(Value),

    pub fn truthy(self: Value) bool {
        return switch (self) {
            .null => false,
            .boolean => |b| b,
            .int => |i| i != 0,
            .string => |s| s.len != 0,
            .array => |a| a.len != 0,
            .object => |o| o.count() != 0,
        };
    }

    fn eql(a: Value, b: Value) bool {
        return switch (a) {
            .null => b == .null,
            .boolean => |x| b == .boolean and b.boolean == x,
            .int => |x| b == .int and b.int == x,
            .string => |x| b == .string and std.mem.eql(u8, b.string, x),
            .array, .object => false,
        };
    }
};

pub const Context = struct {
    values: std.StringHashMap(Value),

    pub fn init(allocator: std.mem.Allocator) Context {
        return .{ .values = std.StringHashMap(Value).init(allocator) };
    }

    pub fn deinit(self: *Context) void {
        self.values.deinit();
    }

    pub fn set(self: *Context, name: []const u8, value: Value) !void {
        try self.values.put(name, value);
    }
};

const Binding = struct { name: []const u8, value: Value };

const Scope = struct {
    parent: ?*const Scope = null,
    bindings: []const Binding = &.{},
    context: ?*const Context = null,

    fn lookup(self: *const Scope, name: []const u8) Value {
        for (self.bindings) |b| {
            if (std.mem.eql(u8, b.name, name)) return b.value;
        }
        if (self.parent) |parent| return parent.lookup(name);
        if (self.context) |ctx| return ctx.values.get(name) orelse .null;
        return .null;
    }
};

pub const Engine = struct {
    allocator: std.mem.Allocator,
    root: std.fs.Dir,
    arena: std.heap.ArenaAllocator,
    templates: std.StringHashMap(ast.Template),

    pub fn init(allocator: std.mem.Allocator, root_path: []const u8) anyerror!Engine {
        const root = try std.fs.cwd().openDir(root_path, .{});
        return .{
            .allocator = allocator,
            .root = root,
            .arena = std.heap.ArenaAllocator.init(allocator),
            .templates = std.StringHashMap(ast.Template).init(allocator),
        };
    }

    pub fn deinit(self: *Engine) void {
        var d = self.root;
        d.close();
        self.templates.deinit();
        self.arena.deinit();
    }

    fn load(self: *Engine, name: []const u8) anyerror!ast.Template {
        if (self.templates.get(name)) |cached| return cached;

        const source = try self.root.readFileAlloc(self.arena.allocator(), name, 4 * 1024 * 1024);
        const template = try ast.parse(self.arena.allocator(), source);
        try self.templates.put(try self.arena.allocator().dupe(u8, name), template);
        return template;
    }

    /// Walks a template's `extends` chain from most- to least-derived,
    /// collecting macros (first definition seen wins) and, per block name,
    /// the ordered chain of bodies from most- to least-derived -- so a
    /// `super()` call inside the winning body can render the next one up
    /// -- and returns the root ancestor's node list to actually render.
    fn resolveInheritance(
        self: *Engine,
        name: []const u8,
        overrides: *std.StringHashMap(std.ArrayList([]const ast.Node)),
        macros: *std.StringHashMap(ast.Node.Macro),
    ) anyerror!([]const ast.Node) {
        const template = try self.load(name);
        var extends_target: ?[]const u8 = null;

        for (template.nodes) |node| {
            switch (node) {
                .block => |b| {
                    const gop = try overrides.getOrPut(b.name);
                    if (!gop.found_existing) gop.value_ptr.* = .empty;
                    try gop.value_ptr.append(self.allocator, b.body);
                },
                .macro_def => |m| {
                    if (!macros.contains(m.name)) try macros.put(m.name, m);
                },
                .extends => |target| extends_target = target,
                else => {},
            }
        }

        if (extends_target) |target| return self.resolveInheritance(target, overrides, macros);
        return template.nodes;
    }

    pub fn render(self: *Engine, allocator: std.mem.Allocator, name: []const u8, context: *const Context) anyerror![]u8 {
        var aw = std.Io.Writer.Allocating.init(allocator);
        errdefer aw.deinit();
        try self.renderInto(&aw.writer, name, context);
        return aw.toOwnedSlice();
    }

    fn renderInto(self: *Engine, writer: *std.Io.Writer, name: []const u8, context: *const Context) anyerror!void {
        var overrides = std.StringHashMap(std.ArrayList([]const ast.Node)).init(self.allocator);
        defer freeOverrides(&overrides, self.allocator);
        var macros = std.StringHashMap(ast.Node.Macro).init(self.allocator);
        defer macros.deinit();

        const nodes = try self.resolveInheritance(name, &overrides, &macros);

        const root_scope = Scope{ .context = context };
        var eval = Eval{
            .engine = self,
            .macros = &macros,
            .overrides = &overrides,
            .writer = writer,
            .root_scope = &root_scope,
        };
        try eval.evalNodes(&root_scope, nodes);
    }
};

/// Which override body a `{{ super() }}` call inside it should render next.
const BlockChain = struct {
    bodies: []const []const ast.Node, // most-derived first
    depth: usize,
};

const Eval = struct {
    engine: *Engine,
    macros: *const std.StringHashMap(ast.Node.Macro),
    overrides: *const std.StringHashMap(std.ArrayList([]const ast.Node)),
    writer: *std.Io.Writer,
    root_scope: *const Scope,
    block_chain: ?BlockChain = null,

    fn evalNodes(self: *Eval, scope: *const Scope, nodes: []const ast.Node) anyerror!void {
        // `set` rebinds a name for the rest of this node list (and anything
        // nested under it), so it's handled here rather than in evalNode --
        // evalNode only ever sees one node and can't affect its siblings.
        var current = scope;
        for (nodes) |node| {
            if (node == .set) {
                current = try self.applySet(current, node.set);
                continue;
            }
            try self.evalNode(current, node);
        }
    }

    fn applySet(self: *Eval, scope: *const Scope, s: ast.Node.Set) anyerror!*const Scope {
        const value = try self.evalExpr(scope, s.value);
        const arena = self.engine.arena.allocator();
        const bindings = try arena.dupe(Binding, &.{.{ .name = s.name, .value = value }});
        const new_scope = try arena.create(Scope);
        new_scope.* = .{ .parent = scope, .bindings = bindings };
        return new_scope;
    }

    fn evalNode(self: *Eval, scope: *const Scope, node: ast.Node) anyerror!void {
        switch (node) {
            .text => |t| try self.writer.writeAll(t),
            .output => |expr| try self.writeValue(try self.evalExpr(scope, expr)),
            .if_stmt => |s| {
                for (s.branches) |branch| {
                    if ((try self.evalExpr(scope, branch.cond)).truthy()) {
                        return self.evalNodes(scope, branch.body);
                    }
                }
                if (s.else_body) |body| try self.evalNodes(scope, body);
            },
            .for_stmt => |f| try self.evalFor(scope, f),
            .extends => {}, // resolved up-front by Engine.resolveInheritance
            .macro_def => {}, // collected up-front, produces no direct output
            .set => unreachable, // intercepted by evalNodes before dispatch
            .include => |name| try self.evalInclude(scope, name),
            .block => |b| {
                if (self.overrides.get(b.name)) |chain| {
                    try self.renderBlockChain(scope, chain.items, 0);
                } else {
                    try self.evalNodes(scope, b.body);
                }
            },
        }
    }

    fn renderBlockChain(self: *Eval, scope: *const Scope, bodies: []const []const ast.Node, depth: usize) anyerror!void {
        var sub = self.*;
        sub.block_chain = .{ .bodies = bodies, .depth = depth };
        try sub.evalNodes(scope, bodies[depth]);
    }

    fn evalInclude(self: *Eval, scope: *const Scope, name: []const u8) anyerror!void {
        var overrides = std.StringHashMap(std.ArrayList([]const ast.Node)).init(self.engine.allocator);
        defer freeOverrides(&overrides, self.engine.allocator);
        var macros = std.StringHashMap(ast.Node.Macro).init(self.engine.allocator);
        defer macros.deinit();

        const nodes = try self.engine.resolveInheritance(name, &overrides, &macros);

        var sub = Eval{
            .engine = self.engine,
            .macros = &macros,
            .overrides = &overrides,
            .writer = self.writer,
            .root_scope = scope,
        };
        try sub.evalNodes(scope, nodes);
    }

    fn evalFor(self: *Eval, scope: *const Scope, f: ast.Node.For) anyerror!void {
        switch (f.iterable) {
            .range => |r| {
                const start = try asInt(try self.evalExpr(scope, r.start.*));
                const end = try asInt(try self.evalExpr(scope, r.end.*));
                var i = start;
                while (i < end) : (i += 1) {
                    const bindings = [_]Binding{.{ .name = f.name, .value = .{ .int = i } }};
                    const child = Scope{ .parent = scope, .bindings = &bindings };
                    try self.evalNodes(&child, f.body);
                }
            },
            else => {
                const iterable = try self.evalExpr(scope, f.iterable);
                const items: []const Value = switch (iterable) {
                    .array => |a| a,
                    else => return error.TypeMismatch,
                };
                for (items) |item| {
                    const bindings = [_]Binding{.{ .name = f.name, .value = item }};
                    const child = Scope{ .parent = scope, .bindings = &bindings };
                    try self.evalNodes(&child, f.body);
                }
            },
        }
    }

    fn writeValue(self: *Eval, value: Value) anyerror!void {
        switch (value) {
            .null => {},
            .boolean => |b| try self.writer.writeAll(if (b) "true" else "false"),
            .int => |i| try self.writer.print("{d}", .{i}),
            .string => |s| try self.writer.writeAll(s),
            .array, .object => return error.TypeMismatch,
        }
    }

    fn evalExpr(self: *Eval, scope: *const Scope, expr: ast.Expr) anyerror!Value {
        return switch (expr) {
            .int => |v| .{ .int = v },
            .boolean => |v| .{ .boolean = v },
            .string => |v| .{ .string = v },
            .path => |path| evalPath(scope, path),
            .binary => |b| try self.evalBinary(scope, b),
            .unary => |u| try self.evalUnary(scope, u),
            .call => |c| try self.evalCall(scope, c),
            .filter => |f| try self.evalFilter(scope, f),
            .range => error.TypeMismatch,
        };
    }

    fn evalBinary(self: *Eval, scope: *const Scope, b: ast.Expr.Binary) anyerror!Value {
        switch (b.op) {
            .logical_and => {
                const lhs = try self.evalExpr(scope, b.lhs.*);
                if (!lhs.truthy()) return .{ .boolean = false };
                return .{ .boolean = (try self.evalExpr(scope, b.rhs.*)).truthy() };
            },
            .logical_or => {
                const lhs = try self.evalExpr(scope, b.lhs.*);
                if (lhs.truthy()) return .{ .boolean = true };
                return .{ .boolean = (try self.evalExpr(scope, b.rhs.*)).truthy() };
            },
            else => {},
        }

        const lhs = try self.evalExpr(scope, b.lhs.*);
        const rhs = try self.evalExpr(scope, b.rhs.*);
        return switch (b.op) {
            .add => .{ .int = try asInt(lhs) + try asInt(rhs) },
            .sub => .{ .int = try asInt(lhs) - try asInt(rhs) },
            .mul => .{ .int = try asInt(lhs) * try asInt(rhs) },
            .div => .{ .int = @divTrunc(try asInt(lhs), try asInt(rhs)) },
            .floordiv => .{ .int = @divFloor(try asInt(lhs), try asInt(rhs)) },
            .mod => .{ .int = @mod(try asInt(lhs), try asInt(rhs)) },
            .pow => .{ .int = try intPow(try asInt(lhs), try asInt(rhs)) },
            .eq => .{ .boolean = lhs.eql(rhs) },
            .neq => .{ .boolean = !lhs.eql(rhs) },
            .lt => .{ .boolean = try asInt(lhs) < try asInt(rhs) },
            .lte => .{ .boolean = try asInt(lhs) <= try asInt(rhs) },
            .gt => .{ .boolean = try asInt(lhs) > try asInt(rhs) },
            .gte => .{ .boolean = try asInt(lhs) >= try asInt(rhs) },
            .concat => blk: {
                const arena = self.engine.arena.allocator();
                break :blk .{ .string = try std.fmt.allocPrint(arena, "{s}{s}", .{
                    try stringifyValue(arena, lhs),
                    try stringifyValue(arena, rhs),
                }) };
            },
            .logical_and, .logical_or => unreachable,
        };
    }

    fn evalUnary(self: *Eval, scope: *const Scope, u: ast.Expr.Unary) anyerror!Value {
        const operand = try self.evalExpr(scope, u.operand.*);
        return switch (u.op) {
            .logical_not => .{ .boolean = !operand.truthy() },
            .negate => .{ .int = -(try asInt(operand)) },
        };
    }

    fn evalCall(self: *Eval, scope: *const Scope, call: ast.Expr.Call) anyerror!Value {
        if (std.mem.eql(u8, call.name, "super")) return self.evalSuper(scope);

        const macro = self.macros.get(call.name) orelse return error.UnknownMacro;

        const bindings = try self.engine.allocator.alloc(Binding, macro.params.len);
        defer self.engine.allocator.free(bindings);
        for (macro.params, 0..) |param, i| {
            bindings[i] = .{
                .name = param,
                .value = if (i < call.args.len) try self.evalExpr(scope, call.args[i]) else .null,
            };
        }
        // Macros see their params and the base context, not the caller's
        // local variables -- matches typical macro/function scoping.
        const macro_scope = Scope{ .parent = self.root_scope, .bindings = bindings };

        var aw = std.Io.Writer.Allocating.init(self.engine.arena.allocator());
        defer aw.deinit();
        var sub = Eval{
            .engine = self.engine,
            .macros = self.macros,
            .overrides = self.overrides,
            .writer = &aw.writer,
            .root_scope = self.root_scope,
        };
        try sub.evalNodes(&macro_scope, macro.body);

        return .{ .string = try aw.toOwnedSlice() };
    }

    fn evalSuper(self: *Eval, scope: *const Scope) anyerror!Value {
        const chain = self.block_chain orelse return .{ .string = "" };
        if (chain.depth + 1 >= chain.bodies.len) return .{ .string = "" };

        var aw = std.Io.Writer.Allocating.init(self.engine.arena.allocator());
        defer aw.deinit();
        var sub = self.*;
        sub.writer = &aw.writer;
        sub.block_chain = .{ .bodies = chain.bodies, .depth = chain.depth + 1 };
        try sub.evalNodes(scope, chain.bodies[chain.depth + 1]);

        return .{ .string = try aw.toOwnedSlice() };
    }

    fn evalFilter(self: *Eval, scope: *const Scope, f: ast.Expr.Filter) anyerror!Value {
        const input = try self.evalExpr(scope, f.input.*);
        const name = std.meta.stringToEnum(FilterName, f.name) orelse return error.UnknownFilter;
        const arena = self.engine.arena.allocator();
        return switch (name) {
            .length => .{ .int = try filterLength(input) },
            .safe => input, // nothing auto-escapes by default, so there's nothing to opt out of yet
            .default => blk: {
                if (input != .null) break :blk input;
                break :blk if (f.args.len > 0) try self.evalExpr(scope, f.args[0]) else .null;
            },
            // Values are always integers -- these are exact already. No-ops
            // until a float Value variant exists to round/truncate.
            .round, .trunc, .ceil, .floor => .{ .int = try asInt(input) },
            .upper => .{ .string = try mapAscii(arena, try stringifyValue(arena, input), std.ascii.toUpper) },
            .lower => .{ .string = try mapAscii(arena, try stringifyValue(arena, input), std.ascii.toLower) },
            .title => .{ .string = try titleCase(arena, try stringifyValue(arena, input)) },
            .escape => .{ .string = try htmlEscape(arena, try stringifyValue(arena, input)) },
        };
    }
};

const FilterName = enum { length, safe, default, round, trunc, ceil, floor, upper, lower, title, escape };

fn filterLength(v: Value) anyerror!i64 {
    return switch (v) {
        .string => |s| @intCast(s.len),
        .array => |a| @intCast(a.len),
        .object => |o| @intCast(o.count()),
        else => error.TypeMismatch,
    };
}

fn mapAscii(allocator: std.mem.Allocator, s: []const u8, comptime f: fn (u8) u8) ![]const u8 {
    const buf = try allocator.alloc(u8, s.len);
    for (s, 0..) |c, i| buf[i] = f(c);
    return buf;
}

fn titleCase(allocator: std.mem.Allocator, s: []const u8) ![]const u8 {
    const buf = try allocator.alloc(u8, s.len);
    var start_of_word = true;
    for (s, 0..) |c, i| {
        buf[i] = if (start_of_word) std.ascii.toUpper(c) else std.ascii.toLower(c);
        start_of_word = std.ascii.isWhitespace(c);
    }
    return buf;
}

fn htmlEscape(allocator: std.mem.Allocator, s: []const u8) ![]const u8 {
    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();
    for (s) |c| {
        const entity: ?[]const u8 = switch (c) {
            '&' => "&amp;",
            '<' => "&lt;",
            '>' => "&gt;",
            '"' => "&quot;",
            '\'' => "&#39;",
            else => null,
        };
        if (entity) |e| try aw.writer.writeAll(e) else try aw.writer.writeByte(c);
    }
    return try aw.toOwnedSlice();
}

fn freeOverrides(overrides: *std.StringHashMap(std.ArrayList([]const ast.Node)), allocator: std.mem.Allocator) void {
    var it = overrides.valueIterator();
    while (it.next()) |chain| chain.deinit(allocator);
    overrides.deinit();
}

fn evalPath(scope: *const Scope, path: ast.Expr.Path) Value {
    var current: Value = scope.lookup(path.name);
    for (path.accessors) |accessor| {
        current = switch (accessor) {
            .field => |f| switch (current) {
                .object => |o| o.get(f) orelse .null,
                else => .null,
            },
            .index => |i| switch (current) {
                .array => |a| if (i >= 0 and i < a.len) a[@intCast(i)] else .null,
                else => .null,
            },
        };
    }
    return current;
}

fn stringifyValue(allocator: std.mem.Allocator, v: Value) anyerror![]const u8 {
    return switch (v) {
        .null => "",
        .boolean => |b| if (b) "true" else "false",
        .string => |s| s,
        .int => |i| try std.fmt.allocPrint(allocator, "{d}", .{i}),
        .array, .object => error.TypeMismatch,
    };
}

fn asInt(v: Value) anyerror!i64 {
    return switch (v) {
        .int => |i| i,
        else => error.TypeMismatch,
    };
}

fn intPow(base: i64, exp: i64) anyerror!i64 {
    if (exp < 0) return error.TypeMismatch;
    var result: i64 = 1;
    var i: i64 = 0;
    while (i < exp) : (i += 1) result *= base;
    return result;
}

fn writeFile(dir: std.fs.Dir, name: []const u8, contents: []const u8) !void {
    try dir.writeFile(.{ .sub_path = name, .data = contents });
}

test "variables, arithmetic, and paths" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try writeFile(tmp.dir, "index.html", "{{ name }} is {{ age + 1 }}, item0={{ items[0] }}, city={{ user.city }}");

    const path = try tmp.dir.realpathAlloc(testing.allocator, ".");
    defer testing.allocator.free(path);
    var engine = try Engine.init(testing.allocator, path);
    defer engine.deinit();

    var items = [_]Value{ .{ .string = "first" }, .{ .string = "second" } };
    var user = std.StringHashMap(Value).init(testing.allocator);
    defer user.deinit();
    try user.put("city", .{ .string = "NYC" });

    var ctx = Context.init(testing.allocator);
    defer ctx.deinit();
    try ctx.set("name", .{ .string = "Ada" });
    try ctx.set("age", .{ .int = 35 });
    try ctx.set("items", .{ .array = &items });
    try ctx.set("user", .{ .object = user });

    const out = try engine.render(testing.allocator, "index.html", &ctx);
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("Ada is 36, item0=first, city=NYC", out);
}

test "and, or, not, %, //, **, and unary minus" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try writeFile(
        tmp.dir,
        "t.html",
        "{{if a and b}}AB{{end}}" ++
            "{{if a or c}}A-or-C{{end}}" ++
            "{{if not c}}not-C{{end}}" ++
            "mod={{7 % 3}} floordiv={{-7 // 2}} pow={{2 ** 5}} " ++
            "neg_lit={{-5}} neg_var={{-n}} neg_expr={{-(2 + 3)}} double_neg={{-(-n)}}",
    );

    const path = try tmp.dir.realpathAlloc(testing.allocator, ".");
    defer testing.allocator.free(path);
    var engine = try Engine.init(testing.allocator, path);
    defer engine.deinit();

    var ctx = Context.init(testing.allocator);
    defer ctx.deinit();
    try ctx.set("a", .{ .boolean = true });
    try ctx.set("b", .{ .boolean = true });
    try ctx.set("c", .{ .boolean = false });
    try ctx.set("n", .{ .int = 5 });

    const out = try engine.render(testing.allocator, "t.html", &ctx);
    defer testing.allocator.free(out);
    try testing.expectEqualStrings(
        "ABA-or-Cnot-Cmod=1 floordiv=-4 pow=32 neg_lit=-5 neg_var=-5 neg_expr=-5 double_neg=5",
        out,
    );
}

test "filters: length, safe, default, round/trunc/ceil/floor, upper/lower/title, escape" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try writeFile(
        tmp.dir,
        "t.html",
        "len={{items | length}} " ++
            "safe={{raw | safe}} " ++
            "def1={{missing | default(\"fallback\")}} def2={{name | default(\"fallback\")}} " ++
            "round={{n | round(2)}} trunc={{n | trunc}} ceil={{n | ceil()}} floor={{n | floor}} " ++
            "upper={{name | upper}} lower={{name | lower}} title={{phrase | title}} " ++
            "escape={{html | escape}}",
    );

    const path = try tmp.dir.realpathAlloc(testing.allocator, ".");
    defer testing.allocator.free(path);
    var engine = try Engine.init(testing.allocator, path);
    defer engine.deinit();

    var items = [_]Value{ .{ .int = 1 }, .{ .int = 2 }, .{ .int = 3 } };
    var ctx = Context.init(testing.allocator);
    defer ctx.deinit();
    try ctx.set("items", .{ .array = &items });
    try ctx.set("raw", .{ .string = "<b>hi</b>" });
    try ctx.set("name", .{ .string = "Ada" });
    try ctx.set("n", .{ .int = 7 });
    try ctx.set("phrase", .{ .string = "hello world" });
    try ctx.set("html", .{ .string = "<a href=\"x\">&'\"</a>" });

    const out = try engine.render(testing.allocator, "t.html", &ctx);
    defer testing.allocator.free(out);
    try testing.expectEqualStrings(
        "len=3 safe=<b>hi</b> def1=fallback def2=Ada " ++
            "round=7 trunc=7 ceil=7 floor=7 " ++
            "upper=ADA lower=ada title=Hello World " ++
            "escape=&lt;a href=&quot;x&quot;&gt;&amp;&#39;&quot;&lt;/a&gt;",
        out,
    );
}

test "~ string concat" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try writeFile(tmp.dir, "t.html", "{{ \"https://github.com/namishh/\" ~ name ~ \"-\" ~ id }}");

    const path = try tmp.dir.realpathAlloc(testing.allocator, ".");
    defer testing.allocator.free(path);
    var engine = try Engine.init(testing.allocator, path);
    defer engine.deinit();

    var ctx = Context.init(testing.allocator);
    defer ctx.deinit();
    try ctx.set("name", .{ .string = "penelope" });
    try ctx.set("id", .{ .int = 7 });

    const out = try engine.render(testing.allocator, "t.html", &ctx);
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("https://github.com/namishh/penelope-7", out);
}

test "super() appends to the parent block instead of replacing it" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try writeFile(tmp.dir, "base.html", "<head>base-head</head>{{block content}}base-content{{end}}");
    try writeFile(
        tmp.dir,
        "child.html",
        "{{extends \"base.html\"}}" ++
            "{{block content}}{{ super() }}-and-child{{end}}",
    );

    const path = try tmp.dir.realpathAlloc(testing.allocator, ".");
    defer testing.allocator.free(path);
    var engine = try Engine.init(testing.allocator, path);
    defer engine.deinit();

    var ctx = Context.init(testing.allocator);
    defer ctx.deinit();

    const out = try engine.render(testing.allocator, "child.html", &ctx);
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("<head>base-head</head>base-content-and-child", out);
}

test "super() climbs a multi-level extends chain" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try writeFile(tmp.dir, "grandparent.html", "{{block x}}G{{end}}");
    try writeFile(tmp.dir, "parent.html", "{{extends \"grandparent.html\"}}{{block x}}{{ super() }}P{{end}}");
    try writeFile(tmp.dir, "child.html", "{{extends \"parent.html\"}}{{block x}}{{ super() }}C{{end}}");

    const path = try tmp.dir.realpathAlloc(testing.allocator, ".");
    defer testing.allocator.free(path);
    var engine = try Engine.init(testing.allocator, path);
    defer engine.deinit();

    var ctx = Context.init(testing.allocator);
    defer ctx.deinit();

    const out = try engine.render(testing.allocator, "child.html", &ctx);
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("GPC", out);
}

test "{{set}} rebinds a name for the rest of a for-loop body, recursively" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    // Mirrors a recursive file-tree template: `set` rebinds the loop var's
    // name to what the recursive include expects to find.
    try writeFile(
        tmp.dir,
        "tree.html",
        "{{node.name}}(" ++
            "{{for child in node.children}}{{set node = child}}{{include \"tree.html\"}}{{end}}" ++
            ")",
    );

    const path = try tmp.dir.realpathAlloc(testing.allocator, ".");
    defer testing.allocator.free(path);
    var engine = try Engine.init(testing.allocator, path);
    defer engine.deinit();

    var grandchild = std.StringHashMap(Value).init(testing.allocator);
    defer grandchild.deinit();
    try grandchild.put("name", .{ .string = "c" });
    try grandchild.put("children", .{ .array = &.{} });

    var children = [_]Value{.{ .object = grandchild }};

    var child = std.StringHashMap(Value).init(testing.allocator);
    defer child.deinit();
    try child.put("name", .{ .string = "b" });
    try child.put("children", .{ .array = &children });

    var roots = [_]Value{.{ .object = child }};

    var root = std.StringHashMap(Value).init(testing.allocator);
    defer root.deinit();
    try root.put("name", .{ .string = "a" });
    try root.put("children", .{ .array = &roots });

    var ctx = Context.init(testing.allocator);
    defer ctx.deinit();
    try ctx.set("node", .{ .object = root });

    const out = try engine.render(testing.allocator, "tree.html", &ctx);
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("a(b(c()))", out);
}

test "if / elseif / else" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try writeFile(tmp.dir, "t.html", "{{if score > 90}}A{{elseif score > 70}}B{{else}}C{{end}}");

    const path = try tmp.dir.realpathAlloc(testing.allocator, ".");
    defer testing.allocator.free(path);
    var engine = try Engine.init(testing.allocator, path);
    defer engine.deinit();

    var ctx = Context.init(testing.allocator);
    defer ctx.deinit();
    try ctx.set("score", .{ .int = 80 });

    const out = try engine.render(testing.allocator, "t.html", &ctx);
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("B", out);
}

test "for over an array and a range" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try writeFile(tmp.dir, "t.html", "{{for x in items}}[{{x}}]{{end}}{{for i in 1..4}}({{i}}){{end}}");

    const path = try tmp.dir.realpathAlloc(testing.allocator, ".");
    defer testing.allocator.free(path);
    var engine = try Engine.init(testing.allocator, path);
    defer engine.deinit();

    var items = [_]Value{ .{ .int = 1 }, .{ .int = 2 }, .{ .int = 3 } };
    var ctx = Context.init(testing.allocator);
    defer ctx.deinit();
    try ctx.set("items", .{ .array = &items });

    const out = try engine.render(testing.allocator, "t.html", &ctx);
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("[1][2][3](1)(2)(3)", out);
}

test "extends overrides a block, include splices a partial" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try writeFile(tmp.dir, "base.html", "<body>{{block content}}Default content{{end}}</body>");
    try writeFile(tmp.dir, "partial.html", "footer-for-{{name}}");
    try writeFile(tmp.dir, "child.html", "{{extends \"base.html\"}}{{block content}}Hello {{name}}! {{include \"partial.html\"}}{{end}}");

    const path = try tmp.dir.realpathAlloc(testing.allocator, ".");
    defer testing.allocator.free(path);
    var engine = try Engine.init(testing.allocator, path);
    defer engine.deinit();

    var ctx = Context.init(testing.allocator);
    defer ctx.deinit();
    try ctx.set("name", .{ .string = "Ada" });

    const out = try engine.render(testing.allocator, "child.html", &ctx);
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("<body>Hello Ada! footer-for-Ada</body>", out);

    const base_out = try engine.render(testing.allocator, "base.html", &ctx);
    defer testing.allocator.free(base_out);
    try testing.expectEqualStrings("<body>Default content</body>", base_out);
}

test "macro definition and call" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try writeFile(
        tmp.dir,
        "t.html",
        "{{macro button(text, url)}}<a href=\"{{url}}\">{{text}}</a>{{end}}" ++
            "{{ button(\"Click\", \"/go\") }}",
    );

    const path = try tmp.dir.realpathAlloc(testing.allocator, ".");
    defer testing.allocator.free(path);
    var engine = try Engine.init(testing.allocator, path);
    defer engine.deinit();

    var ctx = Context.init(testing.allocator);
    defer ctx.deinit();

    const out = try engine.render(testing.allocator, "t.html", &ctx);
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("<a href=\"/go\">Click</a>", out);
}

test "comment produces no output" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try writeFile(tmp.dir, "t.html", "a{# nothing here #}b{##}c");

    const path = try tmp.dir.realpathAlloc(testing.allocator, ".");
    defer testing.allocator.free(path);
    var engine = try Engine.init(testing.allocator, path);
    defer engine.deinit();

    var ctx = Context.init(testing.allocator);
    defer ctx.deinit();

    const out = try engine.render(testing.allocator, "t.html", &ctx);
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("abc", out);
}
