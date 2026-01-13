#include <iostream>

#include "common.h"

int main() {
    ArenaAllocator alloc;

    Slice<u64> slice = alloc.alloc<u64>(20);

    alloc.pop(slice);

    assert(alloc.end_index == 0);

    alloc.free();
}
