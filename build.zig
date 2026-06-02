const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    _ = b.addModule("parser", .{ .root_source_file = b.path("src/parser.zig") });

    _ = b.addModule("lexer", .{ .root_source_file = b.path("src/lexer.zig") });

    _ = b.addModule("ast", .{ .root_source_file = b.path("src/ast.zig") });

    _ = b.addModule("object", .{ .root_source_file = b.path("src/object.zig") });

    const exe = b.addExecutable(.{
        .name = "monkey",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the monkey interpreter");
    run_step.dependOn(&run_cmd.step);

    const unit_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/tests.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    const run_unit_tests = b.addRunArtifact(unit_tests);

    const tests_step = b.step("test", "Run MonkeyLang unit tests");
    tests_step.dependOn(&run_unit_tests.step);
}
