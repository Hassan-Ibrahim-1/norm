### Prereq stuff

** Slice type
** Result type
** Option type
** String type
* Allocators
* Testing
* Dynamic array
* Hash map (can do this later)

Read a bunch about memory management and allocators. then implement. do it
right.


### Norm

Type system, type inference, closures, go-like import system,

```

import "io"

add := fn (x, y: int) int {
    return x + y
}

main := fn () {
    value := add(2, 3) // This is a constant
    io.print("{value}")

    mut x := 0
    x += 1
    io.print("{x}")

    // no mut allowed here
    for i := range 10 {
        io.print("{}", i)
    }
}

```


```

// tuples
x: (int, int) := 0, 0

Vec2 := struct {
    x: float,
    y: float,
    
    // first parameter can have no type and is assigned
    // to Self, can add pointer syntax
    new := fn (*mut self, x, y: f32) Self {
        // variables that have the same name
        // as a field don't need specifier
        // otherwise {x: x, y: y}
        return {x, y}
    }
}

```


### Arena Allocator implementation

Linked list of buffers. If current buf gets full allocate a new one and start
using that.

Can only free the top of a buffer (pop).

Can allocate any type.

### Memory Pool implementation

Wraps an arena allocator but only allocates for one type.

When a type is 'freed', add it to a linked list. When allocating a new object
check that linked list first and return that address.


What should my base allocator be? Just a simple page allocator? Maybe if I abstract
over this I can add the same memory checks that zig has. Have a really fast allocator
for release builds and a slower allocator with checks for debugging.

Maybe there's a better c++ specific way of doing those checks though.
