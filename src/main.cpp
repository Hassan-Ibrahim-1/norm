#include <iostream>

#include "common.h"

int main() {
    int data[5] = {1, 2, 3, 4, 5};

    Slice<int> slice(data, 5);

    std::cout << slice.to_string() << "\n";

    return 0;
}
