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
- [ ] Setup an error reporting system with line numbers, code snippet etc
- [ ] boolean expressions
- [ ] comparison operators
- [ ] logical operators (not, and, or)
- [ ] strings + string interning for comparisons? look at go for inspiration
- [ ] global constant string table
- [ ] think about garbage collection
- [ ] variable declartions + expressions
- [ ] variable mutability + assignment
- [ ] what now? booleans, strings, variables, functions, closures, structs, types

#### Notes for next session

There should be a constant string table like how compiled languages do it. No
need for extra allocations.

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
        // variables that have the same name
        // as a field don't need specifier
        // otherwise {x: x, y: y}
        return {x, y}
    }
}

// all enum tags have a predefined `int` field
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

var_args := fn(fmt: string, x: any...) {
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
    eql := (a, b: Self) bool {
        if a.x == b.x and a.y == b.y {
            return true
        }
        return false
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
    a := Vec2{1, 2}
    b := Vec2{1, 2}
    
    slice := []Vec2{a, b}

    assert(index_of(slice, a) == 0)
    assert(index_of(slice, b) == 1)
}

```
