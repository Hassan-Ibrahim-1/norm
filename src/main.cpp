#include <iostream>

#include "common.h"

int main() {
    ArenaAllocator alloc;

    Slice<u64> slice = alloc.alloc<u64>(20);

    Slice<u64> slice2 = alloc.alloc<u64>(20);

    auto slice2_index = alloc.end_index;
    alloc.pop(slice);
    assert(slice2_index == alloc.end_index);

    alloc.pop(slice2);

    assert(alloc.end_index != slice2_index);
    assert(slice2_index - sizeof(u64) * slice2.len == alloc.end_index);

    alloc.free();
}
