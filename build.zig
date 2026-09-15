const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // The library, published under the module name consumers import via
    // `zig fetch --save` + `b.dependency("penelope", .{}).module("penelope")`.
    const penelope = b.addModule("penelope", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = "penelope",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "penelope", .module = penelope },
            },
        }),
    });

    b.installArtifact(exe);

    const run_step = b.step("run", "Run the app");
    const run_cmd = b.addRunArtifact(exe);
    run_step.dependOn(&run_cmd.step);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // The `penelope` module's own import graph (parser/ast/evaluator) is
    // where all real tests live; the exe is just a thin demo consumer.
    const penelope_tests = b.addTest(.{
        .root_module = penelope,
    });

    const run_penelope_tests = b.addRunArtifact(penelope_tests);

    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_penelope_tests.step);
}
