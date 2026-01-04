#include <iostream>

#include "common.h"

int main() {
    auto cap = 1024 * sizeof(u64);
    BumpAllocator alloc(cap);

    assert(alloc.capacity == cap);
    assert(alloc.data != nullptr);

    Slice<u64> slice = alloc.alloc<u64>(1024).unwrap();

    for (int i = 0; i < slice.len; i++) {
        slice[i] = i + 1;
    }

    std::cout << slice.to_string() << "\n";

    alloc.free();
}
