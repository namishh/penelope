const std = @import("std");

const parser = @import("parser.zig");
const template = @import("ast.zig");

test {
    std.testing.refAllDecls(parser);
    std.testing.refAllDecls(template);
}

pub fn main() !void {
    std.debug.print("All your {s} are belong to us.\n", .{"codebase"});
}
