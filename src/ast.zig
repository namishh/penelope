const std = @import("std");
const testing = std.testing;
const p = @import("parser.zig");

pub const BinOp = enum { add, sub, mul, div, eq, neq, lt, lte, gt, gte };

pub const Accessor = union(enum) {
    field: []const u8,
    index: i64,
};

pub const Expr = union(enum) {
    int: i64,
    boolean: bool,
    string: []const u8,
    path: Path,
    binary: Binary,
    call: Call,
    range: Range,

    pub const Path = struct { name: []const u8, accessors: []const Accessor };
    pub const Binary = struct { op: BinOp, lhs: *const Expr, rhs: *const Expr };
    pub const Call = struct { name: []const u8, args: []const Expr };
    pub const Range = struct { start: *const Expr, end: *const Expr };
};

pub const IfBranch = struct { cond: Expr, body: []const Node };

pub const Node = union(enum) {
    text: []const u8,
    output: Expr,
    if_stmt: If,
    for_stmt: For,
    extends: []const u8,
    include: []const u8,
    block: Block,
    macro_def: Macro,

    pub const If = struct { branches: []const IfBranch, else_body: ?[]const Node };
    pub const For = struct { name: []const u8, iterable: Expr, body: []const Node };
    pub const Block = struct { name: []const u8, body: []const Node };
    pub const Macro = struct { name: []const u8, params: []const []const u8, body: []const Node };
};

pub const Template = struct { nodes: []const Node };

pub const Error = p.Error || std.mem.Allocator.Error || error{UnexpectedToken};

pub fn parse(allocator: std.mem.Allocator, source: []const u8) Error!Template {
    var state = p.ParserState.init(source);
    const nodes = try parseNodes(allocator, &state, &.{});
    _ = try p.eof().parse(&state);
    return .{ .nodes = nodes };
}

const dquote = p.str("\"");
const string_body_elem = p.sequence(&.{ p.not(&dquote), p.any() });
const string_body = p.many(&string_body_elem);
const string_literal = p.sequence(&.{ dquote, string_body, dquote });

const ws_char = p.space();
const ws0 = p.many(&ws_char);

const open_tag = p.str("{{");
const close_tag = p.str("}}");
const comment_open = p.str("{#");
const comment_close = p.str("#}");
const comment_body_elem = p.sequence(&.{ p.not(&comment_close), p.any() });
const comment_body = p.many(&comment_body_elem);

const tag_start = p.choice(&.{ open_tag, comment_open });
const text_char = p.sequence(&.{ p.not(&tag_start), p.any() });
const text_run = p.many(&text_char);

fn skipWs(state: *p.ParserState) void {
    _ = ws0.parse(state) catch unreachable; // `space` always advances, so `many` can't fail here
}

fn atEof(state: *p.ParserState) bool {
    return state.index >= state.input.len;
}

fn containsStr(list: []const []const u8, needle: []const u8) bool {
    for (list) |item| {
        if (std.mem.eql(u8, item, needle)) return true;
    }
    return false;
}

fn keyword(state: *p.ParserState, word: []const u8) bool {
    const checkpoint = state.index;
    const span = p.identifier().parse(state) catch {
        state.index = checkpoint;
        return false;
    };
    if (std.mem.eql(u8, span.slice(state.input), word)) return true;
    state.index = checkpoint;
    return false;
}

// --- expressions: literals, var/var[0]/var.nesting, arithmetic, calls, ranges ---

fn parseBool(state: *p.ParserState) Error!bool {
    const checkpoint = state.index;
    inline for (.{ .{ "true", true }, .{ "false", false } }) |pair| {
        state.index = checkpoint;
        if (p.str(pair[0]).parse(state)) |_| {
            const boundary_ok = state.index >= state.input.len or !isIdentChar(state.input[state.index]);
            if (boundary_ok) return pair[1];
        } else |_| {}
    }
    state.index = checkpoint;
    return error.CouldNotMatch;
}

fn isIdentChar(c: u8) bool {
    return std.ascii.isAlphanumeric(c) or c == '_';
}

fn parseInt(state: *p.ParserState) Error!i64 {
    const checkpoint = state.index;
    var negative = false;
    if (p.str("-").parse(state)) |_| {
        negative = true;
    } else |_| {}

    const span = p.digits().parse(state) catch |err| {
        state.index = checkpoint;
        return err;
    };
    const value = std.fmt.parseInt(i64, span.slice(state.input), 10) catch return error.UnexpectedToken;
    return if (negative) -value else value;
}

fn parsePrimary(allocator: std.mem.Allocator, state: *p.ParserState) Error!Expr {
    skipWs(state);
    const checkpoint = state.index;

    if (p.str("(").parse(state)) |_| {
        skipWs(state);
        const inner = try parseExpr(allocator, state);
        skipWs(state);
        _ = try p.str(")").parse(state);
        return inner;
    } else |_| {
        state.index = checkpoint;
    }

    if (string_literal.parse(state)) |span| {
        const raw = span.slice(state.input);
        return Expr{ .string = raw[1 .. raw.len - 1] };
    } else |_| {
        state.index = checkpoint;
    }

    if (parseBool(state)) |value| {
        return Expr{ .boolean = value };
    } else |_| {
        state.index = checkpoint;
    }

    if (parseInt(state)) |value| {
        return Expr{ .int = value };
    } else |_| {
        state.index = checkpoint;
    }

    if (p.identifier().parse(state)) |span| {
        const name = span.slice(state.input);
        return try parsePathOrCall(allocator, state, name);
    } else |_| {
        state.index = checkpoint;
    }

    return error.UnexpectedToken;
}

fn parsePathOrCall(allocator: std.mem.Allocator, state: *p.ParserState, name: []const u8) Error!Expr {
    skipWs(state);

    if (p.str("(").parse(state)) |_| {
        var args: std.ArrayList(Expr) = .empty;
        skipWs(state);
        const before_args = state.index;
        if (p.str(")").parse(state)) |_| {
            // no arguments
        } else |_| {
            state.index = before_args;
            while (true) {
                const arg = try parseExpr(allocator, state);
                try args.append(allocator, arg);
                skipWs(state);
                if (p.str(",").parse(state)) |_| {
                    skipWs(state);
                    continue;
                } else |_| {}
                break;
            }
            skipWs(state);
            _ = try p.str(")").parse(state);
        }
        return Expr{ .call = .{ .name = name, .args = try args.toOwnedSlice(allocator) } };
    } else |_| {}

    var accessors: std.ArrayList(Accessor) = .empty;
    while (true) {
        const checkpoint = state.index;

        if (p.str(".").parse(state)) |_| {
            const field = try p.identifier().parse(state);
            try accessors.append(allocator, .{ .field = field.slice(state.input) });
            continue;
        } else |_| {
            state.index = checkpoint;
        }

        if (p.str("[").parse(state)) |_| {
            const idx_span = try p.digits().parse(state);
            const idx = std.fmt.parseInt(i64, idx_span.slice(state.input), 10) catch return error.UnexpectedToken;
            _ = try p.str("]").parse(state);
            try accessors.append(allocator, .{ .index = idx });
            continue;
        } else |_| {
            state.index = checkpoint;
        }

        break;
    }

    return Expr{ .path = .{ .name = name, .accessors = try accessors.toOwnedSlice(allocator) } };
}

fn makeBinary(allocator: std.mem.Allocator, op: BinOp, lhs: Expr, rhs: Expr) Error!Expr {
    const lhs_ptr = try allocator.create(Expr);
    lhs_ptr.* = lhs;
    const rhs_ptr = try allocator.create(Expr);
    rhs_ptr.* = rhs;
    return Expr{ .binary = .{ .op = op, .lhs = lhs_ptr, .rhs = rhs_ptr } };
}

const Op = struct { text: []const u8, op: BinOp };

fn parseBinaryLevel(
    allocator: std.mem.Allocator,
    state: *p.ParserState,
    comptime ops: []const Op,
    comptime next: fn (std.mem.Allocator, *p.ParserState) Error!Expr,
) Error!Expr {
    var lhs = try next(allocator, state);
    while (true) {
        skipWs(state);
        const checkpoint = state.index;
        const op = inline for (ops) |candidate| {
            if (p.str(candidate.text).parse(state)) |_| {
                break candidate.op;
            } else |_| {
                state.index = checkpoint;
            }
        } else {
            break;
        };
        skipWs(state);
        const rhs = try next(allocator, state);
        lhs = try makeBinary(allocator, op, lhs, rhs);
    }
    return lhs;
}

fn parseMultiplicative(allocator: std.mem.Allocator, state: *p.ParserState) Error!Expr {
    return parseBinaryLevel(allocator, state, &.{
        .{ .text = "*", .op = .mul },
        .{ .text = "/", .op = .div },
    }, parsePrimary);
}

fn parseAdditive(allocator: std.mem.Allocator, state: *p.ParserState) Error!Expr {
    return parseBinaryLevel(allocator, state, &.{
        .{ .text = "+", .op = .add },
        .{ .text = "-", .op = .sub },
    }, parseMultiplicative);
}

fn parseComparison(allocator: std.mem.Allocator, state: *p.ParserState) Error!Expr {
    const lhs = try parseAdditive(allocator, state);
    skipWs(state);
    const checkpoint = state.index;
    const comparisons = [_]Op{
        .{ .text = "==", .op = .eq },
        .{ .text = "!=", .op = .neq },
        .{ .text = "<=", .op = .lte },
        .{ .text = ">=", .op = .gte },
        .{ .text = "<", .op = .lt },
        .{ .text = ">", .op = .gt },
    };
    inline for (comparisons) |candidate| {
        if (p.str(candidate.text).parse(state)) |_| {
            skipWs(state);
            const rhs = try parseAdditive(allocator, state);
            return makeBinary(allocator, candidate.op, lhs, rhs);
        } else |_| {
            state.index = checkpoint;
        }
    }
    return lhs;
}

fn parseExpr(allocator: std.mem.Allocator, state: *p.ParserState) Error!Expr {
    const lhs = try parseComparison(allocator, state);
    skipWs(state);
    const checkpoint = state.index;
    if (p.str("..").parse(state)) |_| {
        skipWs(state);
        const rhs = try parseComparison(allocator, state);
        const lhs_ptr = try allocator.create(Expr);
        lhs_ptr.* = lhs;
        const rhs_ptr = try allocator.create(Expr);
        rhs_ptr.* = rhs;
        return Expr{ .range = .{ .start = lhs_ptr, .end = rhs_ptr } };
    } else |_| {
        state.index = checkpoint;
    }
    return lhs;
}

// --- statements / tags ---

fn parseNodes(allocator: std.mem.Allocator, state: *p.ParserState, stop_words: []const []const u8) Error![]const Node {
    var nodes: std.ArrayList(Node) = .empty;

    while (true) {
        const text_span = try text_run.parse(state);
        if (text_span.end > text_span.start) {
            try nodes.append(allocator, .{ .text = text_span.slice(state.input) });
        }

        if (atEof(state)) break;

        const tag_checkpoint = state.index;
        if (comment_open.parse(state)) |_| {
            _ = try comment_body.parse(state);
            _ = try comment_close.parse(state);
            continue;
        } else |_| {
            state.index = tag_checkpoint;
        }

        _ = try open_tag.parse(state);
        skipWs(state);

        const kw_checkpoint = state.index;
        if (p.identifier().parse(state)) |kw_span| {
            const kw = kw_span.slice(state.input);

            if (containsStr(stop_words, kw)) {
                state.index = tag_checkpoint;
                return try nodes.toOwnedSlice(allocator);
            }

            state.index = kw_checkpoint;
            if (std.mem.eql(u8, kw, "if")) {
                try nodes.append(allocator, try parseIf(allocator, state));
            } else if (std.mem.eql(u8, kw, "for")) {
                try nodes.append(allocator, try parseFor(allocator, state));
            } else if (std.mem.eql(u8, kw, "extends")) {
                try nodes.append(allocator, try parseExtends(state));
            } else if (std.mem.eql(u8, kw, "include")) {
                try nodes.append(allocator, try parseInclude(state));
            } else if (std.mem.eql(u8, kw, "block")) {
                try nodes.append(allocator, try parseBlock(allocator, state));
            } else if (std.mem.eql(u8, kw, "macro")) {
                try nodes.append(allocator, try parseMacro(allocator, state));
            } else {
                try nodes.append(allocator, try parseOutput(allocator, state));
            }
        } else |_| {
            state.index = kw_checkpoint;
            try nodes.append(allocator, try parseOutput(allocator, state));
        }
    }

    if (stop_words.len > 0) return error.UnexpectedToken;
    return try nodes.toOwnedSlice(allocator);
}

fn parseOutput(allocator: std.mem.Allocator, state: *p.ParserState) Error!Node {
    const expr = try parseExpr(allocator, state);
    skipWs(state);
    _ = try close_tag.parse(state);
    return Node{ .output = expr };
}

fn expectEnd(state: *p.ParserState) Error!void {
    _ = try open_tag.parse(state);
    skipWs(state);
    if (!keyword(state, "end")) return error.UnexpectedToken;
    skipWs(state);
    _ = try close_tag.parse(state);
}

fn parseIf(allocator: std.mem.Allocator, state: *p.ParserState) Error!Node {
    if (!keyword(state, "if")) return error.UnexpectedToken;
    skipWs(state);
    var cond = try parseExpr(allocator, state);
    skipWs(state);
    _ = try close_tag.parse(state);

    var branches: std.ArrayList(IfBranch) = .empty;
    var else_body: ?[]const Node = null;

    while (true) {
        const body = try parseNodes(allocator, state, &.{ "elseif", "else", "end" });
        try branches.append(allocator, .{ .cond = cond, .body = body });

        _ = try open_tag.parse(state);
        skipWs(state);

        if (keyword(state, "elseif")) {
            skipWs(state);
            cond = try parseExpr(allocator, state);
            skipWs(state);
            _ = try close_tag.parse(state);
            continue;
        }

        if (keyword(state, "else")) {
            skipWs(state);
            _ = try close_tag.parse(state);
            else_body = try parseNodes(allocator, state, &.{"end"});
            _ = try open_tag.parse(state);
            skipWs(state);
        }

        if (!keyword(state, "end")) return error.UnexpectedToken;
        skipWs(state);
        _ = try close_tag.parse(state);
        break;
    }

    return Node{ .if_stmt = .{ .branches = try branches.toOwnedSlice(allocator), .else_body = else_body } };
}

fn parseFor(allocator: std.mem.Allocator, state: *p.ParserState) Error!Node {
    if (!keyword(state, "for")) return error.UnexpectedToken;
    skipWs(state);
    const name_span = try p.identifier().parse(state);
    skipWs(state);
    if (!keyword(state, "in")) return error.UnexpectedToken;
    skipWs(state);
    const iterable = try parseExpr(allocator, state);
    skipWs(state);
    _ = try close_tag.parse(state);

    const body = try parseNodes(allocator, state, &.{"end"});
    try expectEnd(state);

    return Node{ .for_stmt = .{ .name = name_span.slice(state.input), .iterable = iterable, .body = body } };
}

fn parseQuotedPath(state: *p.ParserState) Error![]const u8 {
    const span = try string_literal.parse(state);
    const raw = span.slice(state.input);
    return raw[1 .. raw.len - 1];
}

fn parseExtends(state: *p.ParserState) Error!Node {
    if (!keyword(state, "extends")) return error.UnexpectedToken;
    skipWs(state);
    const path = try parseQuotedPath(state);
    skipWs(state);
    _ = try close_tag.parse(state);
    return Node{ .extends = path };
}

fn parseInclude(state: *p.ParserState) Error!Node {
    if (!keyword(state, "include")) return error.UnexpectedToken;
    skipWs(state);
    const path = try parseQuotedPath(state);
    skipWs(state);
    _ = try close_tag.parse(state);
    return Node{ .include = path };
}

fn parseBlock(allocator: std.mem.Allocator, state: *p.ParserState) Error!Node {
    if (!keyword(state, "block")) return error.UnexpectedToken;
    skipWs(state);
    const name_span = try p.identifier().parse(state);
    skipWs(state);
    _ = try close_tag.parse(state);

    const body = try parseNodes(allocator, state, &.{"end"});
    try expectEnd(state);

    return Node{ .block = .{ .name = name_span.slice(state.input), .body = body } };
}

fn parseMacro(allocator: std.mem.Allocator, state: *p.ParserState) Error!Node {
    if (!keyword(state, "macro")) return error.UnexpectedToken;
    skipWs(state);
    const name_span = try p.identifier().parse(state);
    skipWs(state);
    _ = try p.str("(").parse(state);
    skipWs(state);

    var params: std.ArrayList([]const u8) = .empty;
    const before_params = state.index;
    if (p.str(")").parse(state)) |_| {
        // no params
    } else |_| {
        state.index = before_params;
        while (true) {
            skipWs(state);
            const param_span = try p.identifier().parse(state);
            try params.append(allocator, param_span.slice(state.input));
            skipWs(state);
            if (p.str(",").parse(state)) |_| continue else |_| {}
            break;
        }
        skipWs(state);
        _ = try p.str(")").parse(state);
    }

    skipWs(state);
    _ = try close_tag.parse(state);

    const body = try parseNodes(allocator, state, &.{"end"});
    try expectEnd(state);

    return Node{ .macro_def = .{
        .name = name_span.slice(state.input),
        .params = try params.toOwnedSlice(allocator),
        .body = body,
    } };
}

// --- tests ---
// The AST owns lots of small allocations (node lists, accessor lists, boxed
// operands); tests parse into an arena and drop it all in one `deinit`
// rather than tracking each allocation individually.

fn parseFixture(arena: std.mem.Allocator, source: []const u8) !Template {
    return parse(arena, source);
}

test "comment: {##} produces no nodes" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const t = try parseFixture(arena.allocator(), "{##}");
    try testing.expectEqual(@as(usize, 0), t.nodes.len);
}

test "comment: text around a comment" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const t = try parseFixture(arena.allocator(), "hello {# skip this #} world");
    try testing.expectEqual(@as(usize, 2), t.nodes.len);
    try testing.expectEqualStrings("hello ", t.nodes[0].text);
    try testing.expectEqualStrings(" world", t.nodes[1].text);
}

test "variable: plain, indexed, nested, arithmetic, bool" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();
    {
        const t = try parseFixture(a, "{{ name }}");
        try testing.expectEqualStrings("name", t.nodes[0].output.path.name);
        try testing.expectEqual(@as(usize, 0), t.nodes[0].output.path.accessors.len);
    }
    {
        const t = try parseFixture(a, "{{ items[0] }}");
        try testing.expectEqualStrings("items", t.nodes[0].output.path.name);
        try testing.expectEqual(@as(i64, 0), t.nodes[0].output.path.accessors[0].index);
    }
    {
        const t = try parseFixture(a, "{{ user.nesting }}");
        try testing.expectEqualStrings("user", t.nodes[0].output.path.name);
        try testing.expectEqualStrings("nesting", t.nodes[0].output.path.accessors[0].field);
    }
    {
        const t = try parseFixture(a, "{{ 12 + 3 }}");
        const bin = t.nodes[0].output.binary;
        try testing.expectEqual(BinOp.add, bin.op);
        try testing.expectEqual(@as(i64, 12), bin.lhs.int);
        try testing.expectEqual(@as(i64, 3), bin.rhs.int);
    }
    {
        const t = try parseFixture(a, "{{ true }}");
        try testing.expectEqual(true, t.nodes[0].output.boolean);
    }
}

test "if / elseif / else" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const t = try parseFixture(arena.allocator(), "{{if x}}A{{elseif y}}B{{else}}C{{end}}");
    const s = t.nodes[0].if_stmt;
    try testing.expectEqual(@as(usize, 2), s.branches.len);
    try testing.expectEqualStrings("x", s.branches[0].cond.path.name);
    try testing.expectEqualStrings("A", s.branches[0].body[0].text);
    try testing.expectEqualStrings("y", s.branches[1].cond.path.name);
    try testing.expectEqualStrings("B", s.branches[1].body[0].text);
    try testing.expectEqualStrings("C", s.else_body.?[0].text);
}

test "for over a variable and over a range" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();
    {
        const t = try parseFixture(a, "{{for item in items}}{{item}}{{end}}");
        const f = t.nodes[0].for_stmt;
        try testing.expectEqualStrings("item", f.name);
        try testing.expectEqualStrings("items", f.iterable.path.name);
        try testing.expectEqualStrings("item", f.body[0].output.path.name);
    }
    {
        const t = try parseFixture(a, "{{for i in 1..3}}{{i}}{{end}}");
        const f = t.nodes[0].for_stmt;
        try testing.expectEqual(@as(i64, 1), f.iterable.range.start.int);
        try testing.expectEqual(@as(i64, 3), f.iterable.range.end.int);
    }
}

test "extends and include" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();
    {
        const t = try parseFixture(a, "{{extends \"base.html\"}}");
        try testing.expectEqualStrings("base.html", t.nodes[0].extends);
    }
    {
        const t = try parseFixture(a, "{{include \"partial.html\"}}");
        try testing.expectEqualStrings("partial.html", t.nodes[0].include);
    }
}

test "block with default content" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const t = try parseFixture(arena.allocator(), "{{block content}}Default content{{end}}");
    const b = t.nodes[0].block;
    try testing.expectEqualStrings("content", b.name);
    try testing.expectEqualStrings("Default content", b.body[0].text);
}

test "macro definition and call" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const t = try parseFixture(
        arena.allocator(),
        "{{macro button(text, url)}}<a href=\"{{url}}\">{{text}}</a>{{end}}" ++
            "{{ button(\"Click\", \"/go\") }}",
    );

    const m = t.nodes[0].macro_def;
    try testing.expectEqualStrings("button", m.name);
    try testing.expectEqual(@as(usize, 2), m.params.len);
    try testing.expectEqualStrings("text", m.params[0]);
    try testing.expectEqualStrings("url", m.params[1]);

    const call = t.nodes[1].output.call;
    try testing.expectEqualStrings("button", call.name);
    try testing.expectEqualStrings("Click", call.args[0].string);
    try testing.expectEqualStrings("/go", call.args[1].string);
}
