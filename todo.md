## Norm

- [x] Lexer
- [ ] Parser (astgen)
- [ ] Error report system (do this early)
- [ ] Semantic anaylsis + type checking
- [ ] Codegen + optimization
- [ ] Virtual machine + garbage collector

Think about how a garbage collector fits here.

### Doing now

- [x] Parse literals, arithmetic expressions
- [x] Emit opcode for a basic AST
- [x] Setup a VM to run that opcode
- [x] Write test cases for the parser
- [x] Setup an error reporting system with line numbers, code snippet etc
- [x] boolean expressions
- [x] nil
- [x] comparison operators
- [x] logical operators (not, and, or)
- [x] Precedence tests for logical and comparison operators
- [x] type checking for basic operations
- [x] How will basic types work (how should `true + false` be disallowed)
compiler should also convert (1 + 2.0) to (1.0 + 2.0)
- [x] cast syntax + cast operation
- [x] test casting
- [x] test failures in sema
- [x] sema auto casts in arithmetic
- [x] string literal support
- [x] string concatenation
- [x] string interning for comparisons? look at go for inspiration
- [x] global constant string table
- [x] consider adding making specific instructions for each type. right now op_add et al have to deal
- [ ] variable declartions + statements + scopes
- [ ] variable mutability + assignment
- [ ] sync points at statement boundaries for parser and sema
- [ ] if statements
- [ ] rewrite logical operators to have short circuit behavior
- [ ] loops
- [ ] read up on type systems and type inference. there's quite a lot i don't fully understand.
    * whats an elegant way of determining types, i just use a lot of if statements right now.
    * how do i handle user defined types? right now im just thinking of interning them
    * how do i handle multiple integer types?
    * how do i handle casting?
    * traits? interfaces?
    * i think the stack should also change. there's no need for a Value type at runtime. just
    have an array of bytes to index into and opcodes define how many bytes you need and what to do with them.
    there should be no types at runtime whatsoever. maybe have specialized opcodes for reading bytes and have registers?
    this would probably make various number types easier to deal with i think?
    * https://mukulrathi.com/create-your-own-programming-language/intro-to-type-checking/
    * https://blog.polybdenum.com/2020/07/04/subtype-inference-by-example-part-1-introducing-cubiml.html
with both floats and integers
- [ ] Think about reusing Ast stuff in Nir. There's a lot of duplicated code
- [ ] look at other type checking implementations, mine is unreadable and naive.
- [ ] Think about debug info, instead of each chunk having line numbers there
could be pointers to a debug info table. Would allow variable/function names, types etc.
- [ ] think about garbage collection

#### Notes for next session

Each pass should take ownership over the last IR

There should be a constant string table like how compiled languages do it. No
need for extra allocations.

#### Notes on heap allocation

All strings should obviously be heap allocated but what about structs? Not all
structs should be heap allocated. This probably requires escape analysis. To
keep it simple right now im just going to heap allocate everything.

Since the compiler decides what goes on the heap or stack, just emit different op codes
for different cases. For objects which can be stored on the stack like a struct, we can have
different op codes for field access.

#### Variables

I want variables to work the same way as they do in zig. So no shadowing at
all. This lets me resolve what each variable refers to at compile time. But, I
do want to allow global variables to be used before they are declared. The package
system in this language should also work just like odin's so that means global
variables are allowed to be used across a package as well.

Sema should make sure all variables are unique and don't overlap in the same
scope / parent scope. Create a symbol table with names, types, and scope.

Go through the AST and collect all top level declarations.

Then in a second pass, go through the AST again and define all local variables
and their scopes and types, go through function bodies.

How will structs and enums fit into this?

#### Language demo project

Making a custom engine and doing some small demo in it would be nice. Radiance
cascades seems interesting: [radiance cascades](https://radiance-cascades.com/)
Since zig compiles to wasm, I could theoretically get this language running in a web browser.
It'd be awesome if I could get an engine running in a browser.

#### Language Ideas

Type system, type inference, closures, go-like import system,

How do you inline a function? This would be a fun optimization. Having variable
levels of inlining to balance compilation speed and runtime performance would
also be interesting.


```

import "fmt"

add := fn (x, y: int) int {
    return x + y
}

main := fn () {
    value := add(2, 3) // This is a constant
    fmt.println("{value}")

    // This is a comment
    mut x: int = 0
    x += 1
    fmt.println("{x}")

    // no mut allowed here
    for i := range 10 {
        fmt.println("{}", i)
    }
}

read_all := fn () fmt.FileError!string {
    f := try fmt.open_file("file.txt", "r")
    return try f.read_all()
}

cast_to_float := fn (i: int) float {
    return float(i)
}

index_of := fn (s: string, target: char) ?int {
    for i, c := range s {
        if c == target {
            return i
        }
    }
    return nil
}

replace_with := fn (s: []mut int, old, new: int) {
    for i := range s {
        if s[i] == old {
            s[i] = new
        }
    }
}

```


```

// tuples
x: (int, int) := 0, 0

Vec2 := struct {
    x: float,
    y: float,
    
    // just a namespaced function
    // example call: `Vec2.new(..)`
    new := fn (x, y: float) Self {
        // variables that have the same name as a field don't need specifier
        // otherwise {x: x, y: y}
        return {x, y}
    }
}

// all enum tags have a predefined `int` field
// Direction.up.int
Direction := enum(u32) {
    up,
    down,
    right,
    left,
}

switch_stmt := fn () {
    x := Direction.up

    is_up := switch x {
        .up => true,
        else => false,
    }

    // OR
    is_up2 := if x == .up {
        ret_if true
    } else {
        ret_if false
    }
}

// `any...` is equivalent to `[]any` but the caller can put the arguments in
// without having to allocate a slice themselves when the callee wants to
// pass in a list for tho they have to do so with the `...` operator, so same as go
var_args := fn (fmt: string, x: any...) {
    fmt.println(fmt, x)
}

```

```
// collections
slice_functions := fn () {
    slice := []int{}
    fmt.println("len: {slice.len}, cap: {slice.cap}")

    slice.append_slice({1, 2, 3, 4})
    slice.append(5)
    assert(slice.index_of(1) == 0)
}

```


```

// Generics?

Eq := trait {
    eql: fn (a, b: Self) bool
}

Vec2 := struct {
    x: f32,
    y: f32,

    // Eq
    eql := fn (a, b: Self) bool {
        return a.x == b.x and a.y == b.y
    }
}

index_of := fn (s: []T, target: T) ?int
    where T: Eq
{
    for i, n := range s {
        if n == target {
            return i
        }
    }
    return nil
}

main := fn () {
    a := Vec2{0, 0}
    b := Vec2{1, 2}
    
    slice := []Vec2{a, b}

    assert(index_of(slice, a) == 0)
    assert(index_of(slice, b) == 1)
}

```
