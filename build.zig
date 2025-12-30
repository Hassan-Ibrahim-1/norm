const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe_mod = b.createModule(.{
        .target = target,
        .optimize = optimize,
        .link_libcpp = true,
    });

    exe_mod.addCSourceFiles(.{
        .files = &.{
            "src/main.cpp",
            "src/debug.cpp",
        },
        .flags = switch (optimize) {
            .Debug => &.{ "-std=c++17", "-Wall", "-Werror", "-g", "-O0" },

            .ReleaseFast,
            .ReleaseSafe,
            .ReleaseSmall,
            => &.{ "-std=c++17", "-Wall", "-Werror", "-O3" },
        },
    });

    const exe = b.addExecutable(.{
        .name = "norm",
        .root_module = exe_mod,
    });
    b.installArtifact(exe);

    const run_step = b.step("run", "Run the app");

    const run_cmd = b.addRunArtifact(exe);
    run_step.dependOn(&run_cmd.step);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
}
