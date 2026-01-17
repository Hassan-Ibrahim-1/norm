#include "common.h"

int main() {
    ArenaAllocator alloc;

    alloc.alloc<u64>(20);
    alloc.alloc<u64>(20);
    alloc.alloc<u64>(500);
    alloc.alloc<u64>(750);

    assert(alloc.buffer_list.len() == 4);

    alloc.clear();
}
