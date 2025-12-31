#include <iostream>

#include "common.h"

int main() {
    int data[] = {1, 2, 3, 4, 5};
    Slice<int> slice(data, 5);

    std::cout << slice.index_of(9).unwrap() << "\n";

    return 0;
}
