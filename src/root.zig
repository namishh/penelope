const std = @import("std");

pub const parser = @import("parser.zig");
pub const ast = @import("ast.zig");
pub const evaluator = @import("evaluator.zig");

pub const Context = evaluator.Context;
pub const Value = evaluator.Value;
pub const Engine = evaluator.Engine;

pub fn new(allocator: std.mem.Allocator, templates_dir: []const u8) !Engine {
    return Engine.init(allocator, templates_dir);
}

pub fn render(allocator: std.mem.Allocator, path: []const u8, context: *const Context) ![]u8 {
    const dir = std.fs.path.dirname(path) orelse ".";
    const name = std.fs.path.basename(path);

    var engine = try new(allocator, dir);
    defer engine.deinit();
    return engine.render(allocator, name, context);
}

test {
    std.testing.refAllDecls(@This());
}

test "render() a standalone template file" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(.{ .sub_path = "hello.html", .data = "Hello, {{ name }}!" });

    const path = try tmp.dir.realpathAlloc(std.testing.allocator, "hello.html");
    defer std.testing.allocator.free(path);

    var ctx = Context.init(std.testing.allocator);
    defer ctx.deinit();
    try ctx.set("name", .{ .string = "World" });

    const out = try render(std.testing.allocator, path, &ctx);
    defer std.testing.allocator.free(out);
    try std.testing.expectEqualStrings("Hello, World!", out);
}
