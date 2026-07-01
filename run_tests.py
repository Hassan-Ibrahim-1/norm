#!/usr/bin/env python3
import argparse
import difflib
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path


RED = "\033[31m"
RESET = "\033[0m"


@dataclass
class Expectation:
    kind: str
    lines: list[str]
    description: str | None


@dataclass
class Failure:
    path: Path
    source: str
    expected: str
    actual: str
    diff_expected: list[str]
    diff_actual: list[str]
    reason: str


def strip_comment_prefix(line: str) -> str | None:
    stripped = line.lstrip()
    if not stripped.startswith("//"):
        return None

    value = stripped[2:]
    if value.startswith(" "):
        value = value[1:]
    return value.rstrip("\n")


def parse_expectation(source: str) -> Expectation | None:
    description = None
    lines = source.splitlines()

    for i, line in enumerate(lines):
        comment = strip_comment_prefix(line)
        if comment is None:
            if line.strip() == "":
                continue
            break

        if comment.startswith("test:"):
            description = comment[len("test:") :].strip()
            continue

        if comment == "expect:" or comment == "expect error:":
            kind = "error" if comment == "expect error:" else "stdout"
            expected_lines: list[str] = []

            for expected_line in lines[i + 1 :]:
                expected_comment = strip_comment_prefix(expected_line)
                if expected_comment is None:
                    break
                expected_lines.append(expected_comment)

            return Expectation(kind=kind, lines=expected_lines, description=description)

    return None


def format_block(text: str) -> str:
    if text == "":
        return "(empty)"
    return text.rstrip("\n")


def numbered_source(source: str) -> str:
    return "\n".join(f"{i:4} | {line}" for i, line in enumerate(source.splitlines(), 1))


def red(text: str, enabled: bool) -> str:
    if not enabled:
        return text
    return f"{RED}{text}{RESET}"


def unified_diff(expected: list[str], actual: list[str], color: bool) -> str:
    lines = difflib.unified_diff(
        expected,
        actual,
        fromfile="expected",
        tofile="actual",
        lineterm="",
    )

    rendered: list[str] = []
    for line in lines:
        if line.startswith("+") or line.startswith("-"):
            rendered.append(red(line, color))
        else:
            rendered.append(line)
    return "\n".join(rendered)


def run_norm(compiler: Path, source_path: Path, timeout: float) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [str(compiler), str(source_path)],
        capture_output=True,
        text=True,
        timeout=timeout,
        check=False,
    )


def check_file(compiler: Path, path: Path, timeout: float) -> Failure | None:
    source = path.read_text()
    expectation = parse_expectation(source)
    if expectation is None:
        return Failure(
            path=path,
            source=source,
            expected="// expect: or // expect error: header",
            actual="missing expectation header",
            diff_expected=["// expect: or // expect error: header"],
            diff_actual=["missing expectation header"],
            reason="missing expectation header",
        )

    expected_text = "\n".join(expectation.lines)

    try:
        result = run_norm(compiler, path, timeout)
    except subprocess.TimeoutExpired as err:
        actual = ""
        if err.stdout:
            actual += err.stdout
        if err.stderr:
            actual += err.stderr
        actual += f"\nprocess timed out after {timeout:g}s"
        return Failure(
            path=path,
            source=source,
            expected=expected_text,
            actual=actual,
            diff_expected=expectation.lines,
            diff_actual=actual.splitlines(),
            reason="process timed out",
        )

    if expectation.kind == "stdout":
        actual_lines = result.stdout.splitlines()
        if result.returncode == 0 and expectation.lines == actual_lines:
            return None

        actual = result.stdout if result.returncode == 0 else result.stdout + result.stderr
        reason = "stdout did not match expected output"
        if result.returncode != 0:
            reason = f"compiler exited with status {result.returncode}"
        return Failure(
            path=path,
            source=source,
            expected=expected_text,
            actual=actual,
            diff_expected=expectation.lines,
            diff_actual=actual.splitlines(),
            reason=reason,
        )

    actual = result.stdout + result.stderr
    if expected_text in actual:
        return None

    return Failure(
        path=path,
        source=source,
        expected=expected_text,
        actual=actual,
        diff_expected=expectation.lines,
        diff_actual=actual.splitlines(),
        reason="expected error text was not found in compiler output",
    )


def print_failure(failure: Failure, color: bool) -> None:
    print(red(f"FAIL {failure.path}", color))
    print(f"Reason: {failure.reason}")
    print("\nSource:")
    print(numbered_source(failure.source))
    print("\nExpected:")
    print(format_block(failure.expected))
    print("\nActual:")
    print(format_block(failure.actual))
    print("\nDiff:")
    diff = unified_diff(failure.diff_expected, failure.diff_actual, color)
    print(diff if diff else red("(no textual diff)", color))
    print()


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Run .norm files and compare against header expectations.")
    parser.add_argument("compiler", type=Path, help="path to the norm compiler binary")
    parser.add_argument("test_path", type=Path, help=".norm file or directory containing .norm tests")
    parser.add_argument("--timeout", type=float, default=5.0, help="per-test timeout in seconds")
    parser.add_argument("--no-color", action="store_true", help="disable ANSI color output")
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    compiler = args.compiler
    test_path = args.test_path

    if not compiler.is_file():
        print(f"compiler not found: {compiler}", file=sys.stderr)
        return 2
    if not test_path.exists():
        print(f"test path not found: {test_path}", file=sys.stderr)
        return 2

    if test_path.is_file():
        if test_path.suffix != ".norm":
            print(f"test file is not a .norm file: {test_path}", file=sys.stderr)
            return 2
        test_files = [test_path]
    elif test_path.is_dir():
        test_files = sorted(test_path.rglob("*.norm"))
    else:
        print(f"test path is not a file or directory: {test_path}", file=sys.stderr)
        return 2

    if not test_files:
        print(f"no .norm files found in {test_path}")
        return 1

    failures: list[Failure] = []
    for path in test_files:
        failure = check_file(compiler, path, args.timeout)
        if failure is not None:
            failures.append(failure)

    color = not args.no_color
    for failure in failures:
        print_failure(failure, color)

    passed = len(test_files) - len(failures)
    print(f"{passed}/{len(test_files)} tests passed")
    if failures:
        print(red(f"{len(failures)} test(s) failed", color))
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
