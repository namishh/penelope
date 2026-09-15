const std = @import("std");
const penelope = @import("penelope");

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var ctx = penelope.Context.init(allocator);
    defer ctx.deinit();
    try ctx.set("name", .{ .string = "world" });

    std.debug.print("penelope: a templating engine, importable as a module.\n", .{});
    std.debug.print("  const html = try penelope.render(allocator, \"index.html\", &ctx);\n", .{});
}
