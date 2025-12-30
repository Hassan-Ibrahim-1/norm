#include <iostream>

#include "common.h"

enum class Error {
    ERR,
};

std::string to_string(Error e) {
    return "ERR";
}

int main() {
    Result<int, Error> res(Error::ERR);
    std::cout << to_string(res.err().unwrap()) << "\n";
    return 0;
}
