#include "debug.h"

#include <iomanip>
#include <iostream>
#include <sstream>

namespace debug {

void print_stack_trace() {
    void* callstack[128];
    int frames = backtrace(callstack, 128);
    char** strs = backtrace_symbols(callstack, frames);

    std::cerr << "\nStack trace:\n";

    for (int i = 0; i < frames; i++) {
        std::cerr << "  " << i << ": ";

        // Try to demangle the symbol
        std::string symbol(strs[i]);
        size_t start = symbol.find('(');
        size_t end = symbol.find('+', start);

        if (start != std::string::npos && end != std::string::npos
            && end > start) {
            std::string mangled = symbol.substr(start + 1, end - start - 1);
            int status;
            char* demangled =
                abi::__cxa_demangle(mangled.c_str(), nullptr, nullptr, &status);

            if (status == 0 && demangled) {
                std::cerr << symbol.substr(0, start) << "(" << demangled;
                if (symbol.find('+', end) != std::string::npos) {
                    std::cerr << symbol.substr(end);
                } else {
                    std::cerr << ")";
                }
                std::cerr << "\n";
                free(demangled);
            } else {
                std::cerr << strs[i] << "\n";
            }
        } else {
            std::cerr << strs[i] << "\n";
        }
    }

    free(strs);
}

} // namespace debug
