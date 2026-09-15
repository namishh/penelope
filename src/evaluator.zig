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
    /// collecting block overrides and macros along the way (first
    /// definition seen wins, since we walk child-before-parent), and
    /// returns the root ancestor's node list to actually render.
    fn resolveInheritance(
        self: *Engine,
        name: []const u8,
        overrides: *std.StringHashMap([]const ast.Node),
        macros: *std.StringHashMap(ast.Node.Macro),
    ) anyerror!([]const ast.Node) {
        const template = try self.load(name);
        var extends_target: ?[]const u8 = null;

        for (template.nodes) |node| {
            switch (node) {
                .block => |b| {
                    if (!overrides.contains(b.name)) try overrides.put(b.name, b.body);
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
        var overrides = std.StringHashMap([]const ast.Node).init(self.allocator);
        defer overrides.deinit();
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

const Eval = struct {
    engine: *Engine,
    macros: *const std.StringHashMap(ast.Node.Macro),
    overrides: *const std.StringHashMap([]const ast.Node),
    writer: *std.Io.Writer,
    root_scope: *const Scope,

    fn evalNodes(self: *Eval, scope: *const Scope, nodes: []const ast.Node) anyerror!void {
        for (nodes) |node| try self.evalNode(scope, node);
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
            .include => |name| try self.evalInclude(scope, name),
            .block => |b| {
                const body = self.overrides.get(b.name) orelse b.body;
                try self.evalNodes(scope, body);
            },
        }
    }

    fn evalInclude(self: *Eval, scope: *const Scope, name: []const u8) anyerror!void {
        var overrides = std.StringHashMap([]const ast.Node).init(self.engine.allocator);
        defer overrides.deinit();
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
            .call => |c| try self.evalCall(scope, c),
            .range => error.TypeMismatch, // only meaningful directly as a for-loop iterable
        };
    }

    fn evalBinary(self: *Eval, scope: *const Scope, b: ast.Expr.Binary) anyerror!Value {
        const lhs = try self.evalExpr(scope, b.lhs.*);
        const rhs = try self.evalExpr(scope, b.rhs.*);
        return switch (b.op) {
            .add => .{ .int = try asInt(lhs) + try asInt(rhs) },
            .sub => .{ .int = try asInt(lhs) - try asInt(rhs) },
            .mul => .{ .int = try asInt(lhs) * try asInt(rhs) },
            .div => .{ .int = @divTrunc(try asInt(lhs), try asInt(rhs)) },
            .eq => .{ .boolean = lhs.eql(rhs) },
            .neq => .{ .boolean = !lhs.eql(rhs) },
            .lt => .{ .boolean = try asInt(lhs) < try asInt(rhs) },
            .lte => .{ .boolean = try asInt(lhs) <= try asInt(rhs) },
            .gt => .{ .boolean = try asInt(lhs) > try asInt(rhs) },
            .gte => .{ .boolean = try asInt(lhs) >= try asInt(rhs) },
        };
    }

    fn evalCall(self: *Eval, scope: *const Scope, call: ast.Expr.Call) anyerror!Value {
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
};

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

fn asInt(v: Value) anyerror!i64 {
    return switch (v) {
        .int => |i| i,
        else => error.TypeMismatch,
    };
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
