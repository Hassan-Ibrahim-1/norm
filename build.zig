const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});

    const optimize = b.standardOptimizeOption(.{});

    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const no_bin = b.option(bool, "no-bin", "skip emitting binary") orelse false;
    const debug = b.option(bool, "debug", "enable, debug logs, etc") orelse true;
    const use_llvm = b.option(bool, "llvm", "force use of llvm backend");

    const opts = b.addOptions();
    opts.addOption(bool, "debug", debug);
    exe_mod.addOptions("opts", opts);

    const exe = b.addExecutable(.{
        .name = "norm",
        .root_module = exe_mod,
        .use_llvm = use_llvm,
    });

    if (no_bin) {
        b.getInstallStep().dependOn(&exe.step);
    } else {
        b.installArtifact(exe);
    }

    const run_cmd = b.addRunArtifact(exe);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const exe_unit_tests = b.addTest(.{
        .root_module = exe_mod,
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);

    const build_lib_unit_tests = b.addInstallArtifact(exe_unit_tests, .{});
    const build_test_step = b.step("build-test", "Build unit tests");
    build_test_step.dependOn(&build_lib_unit_tests.step);
}
