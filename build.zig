const std = @import("std");
const Build = std.Build;

pub fn build(b: *Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const mod = b.addModule("zlex", .{
        .root_source_file = b.path("src/zlex.zig"),
        .target = target,
        .optimize = optimize,
    });

    buildExamples(b, mod, target, optimize);
}

fn buildExamples(
    b: *Build,
    mod: *Build.Module,
    target: Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
) void {
    const ex = b.addExecutable(.{
        .name = "zlex_example",
        .target = target,
        .optimize = optimize,
        .root_source_file = b.path("examples/example.zig"),
    });
    ex.root_module.addImport("zlex", mod);

    b.installArtifact(ex);
    const run_ex = b.addRunArtifact(ex);
    const run_step = b.step("example", "run zlex example");
    run_step.dependOn(&run_ex.step);
}
