#include <iostream>

#include "common.h"

int main() {
    ArenaAllocator alloc;

    Slice<u64> slice = alloc.alloc<u64>(1024);

    for (int i = 0; i < slice.len; i++) {
        slice[i] = i + 1;
    }

    std::cout << slice.to_string() << "\n";

    alloc.free();
}
