#include <iostream>

#include "common.h"

int main() {
    char data[] = "1234";
    String slice(data, 4);

    slice[3] = '5';

    std::cout << slice.to_string() << "\n";

    return 0;
}
