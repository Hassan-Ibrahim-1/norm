# TODO

Investigate having a minimum page size for arena buffers
=======
[x] Lexer
[ ] Parser (astgen)
[ ] Semantic anaylsis + type checking
[ ] Codegen + optimization
[ ] Virtual machine + garbage collector

Produce a tree that just parses given types (doesn't infer yet), expressions,
statements, ...

### Now

[ ] Parse literals, arithmetic expressions
[ ] Emit opcode for a basic AST
[ ] Setup a VM to run that opcode

Think about error reporting alongside all of this as well

### Norm

Type system, type inference, closures, go-like import system,

```

import "io"

add := fn (x, y: int) int {
    return x + y
}

main := fn () {
    value := add(2, 3) // This is a constant
    io.println("{value}")

    // This is a comment
    mut x: int = 0
    x += 1
    io.println("{x}")

    // no mut allowed here
    for i := range 10 {
        io.println("{}", i)
    }
}

read_all := fn () io.FileError!string {
    f := try io.open_file("file.txt", "r")
    return try f.read_all()
}

index_of := fn (s: string, target: u8) ?int {
    for i, c := range s {
        if c == target {
            return i
        }
    }
    return nil
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
    io.println(fmt, x)
}

```
