# Norm Edge Case Bug Report

No implementation fixes were attempted. This report describes the failures exposed by the new `.norm` files in this directory.

## Added Runnable Regression Tests

These programs currently produce the expected stdout from their file headers:

- `tests/condition_for_nested_control.norm`
- `tests/for_increment_continue_cleanup.norm`
- `tests/for_scope_reuse_after_loop.norm`
- `tests/if_loop_scope_slots.norm`
- `tests/infinite_for_nested_control.norm`
- `tests/loop_continue_after_inner_for.norm`

The current CLI also prints `constants length: ...` to stderr for successful runs. The `expect` comments only describe program stdout.

## Bug 1: Loop Parser State Leaks Past A Completed Loop

Repro files:

- `tests/loop_state_leak_break_after_loop.norm`
- `tests/loop_state_leak_continue_after_loop.norm`

Expected behavior:

- `break;` after a completed loop should report `Found break statement outside a loop`.
- `continue;` after a completed loop should report `Found continue statement outside a loop`.

Actual behavior:

- Both programs reach sema and panic on an assertion in `src/sema.zig`.
- `break` panics at the assertion in the `break_stmt` path.
- `continue` panics at the assertion in the `continue_stmt` path.

Likely cause:

- `Parser.parsing_loop` is a single boolean.
- `src/parser.zig:175-177` sets `p.parsing_loop = true` when parsing any `for` statement.
- `src/parser.zig:178-185` trusts that boolean when accepting `break` or `continue`.
- The boolean is reset only by the top-level parse loop defer at `src/parser.zig:647-648`.
- If a `for` appears inside a block, the whole block is one top-level statement, so `parsing_loop` remains true while the parser continues parsing later statements in that same block.
- Sema assumes the parser has already rejected loop-control statements outside loops. Once the completed loop is no longer active, `s.current_for_block_scope` is back at the top scope, so the assertion in `src/sema.zig:357` or `src/sema.zig:367` fires.

## Bug 2: Typed Forward Global Initializer Reads An Uninitialized Slot

Repro file:

- `tests/typed_global_forward_initializer.norm`

Expected behavior:

- The initializer `y: int = z + 1;` should report `Variable z used before it's definition` because `z` has not been initialized yet.

Actual behavior:

- Sema accepts the program.
- The VM panics with an invalid union field/enum value while executing integer addition.

Likely cause:

- `src/sema.zig:86-103` pre-registers all global declarations before analyzing initializers.
- If a global has an explicit type annotation, `analyzeGlobalSymbols` records that type immediately instead of `.n_invalid`.
- `src/sema.zig:552-559` only reports use-before-definition for top-level symbols whose type is still `.n_invalid`.
- Therefore `z: int = 5;` makes `z` look semantically usable before its initializer has run.
- `src/compiler.zig:502-505` compiles a declaration by only emitting code for the initializer expression. Globals live on the VM stack by declaration slot/order rather than through an explicit store in this path.
- Disassembly confirms the problem: `y`'s initializer emits `op_load 1` for `z` before `z`'s initializer emits its constant. That reads `vm.stack[1]` before it contains a valid `Value`.
- `src/vm.zig:265-268` loads the stack slot without an initialization check, so the later `op_add_int` sees an invalid `Value` and panics.

## Bug 3: String And Bool Equality Panics Instead Of Reporting A Type Error

Repro file:

- `tests/cross_type_string_bool_equality.norm`

Expected behavior:

- The expression `"ready" == true` should report `Cannot compare string and bool.`.

Actual behavior:

- Sema panics with `panic: hmm` from `commonType()`.

Likely cause:

- `src/sema.zig:461-467` handles `==` and `!=` by first checking whether both operands are individually comparable.
- Both `string` and `bool` return true from `NormType.isComparable()`.
- The equality path then calls `commonType(left.type, right.type)`.
- `src/sema.zig:48-53` only handles equal types or numeric mixes. If the types are different and neither side is numeric, it panics.
- Other invalid mixed comparisons such as `string == int` happen to avoid the panic because the numeric side makes `commonType()` return `float`, then casting fails and reports a diagnostic. The `string`/`bool` pair has no numeric side, so it reaches the panic directly.
