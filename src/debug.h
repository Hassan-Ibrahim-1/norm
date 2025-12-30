#pragma once

#include <cxxabi.h>
#include <dlfcn.h>
#include <execinfo.h>

namespace debug {

void print_stack_trace();

} // namespace debug

#define DEBUG_LOG
#define DEBUG_ERROR

#ifdef DEBUG_LOG

    #define DEBUG_YELLOW "\033[33;33m"
    #define DEBUG_RED "\033[1;31m"
    #define DEBUG_RESET "\033[0m"

    #define LOG(msg, ...) \
        do { \
            printf( \
                "%s[%s]%s: ", \
                DEBUG_YELLOW, \
                __PRETTY_FUNCTION__, \
                DEBUG_RESET \
            ); \
            printf(msg, ##__VA_ARGS__); \
            printf("\n"); \
        } while (0)
    #define LOG_ERROR(msg, ...) \
        do { \
            fprintf( \
                stderr, \
                "%s[ERROR -> %s]%s: ", \
                DEBUG_RED, \
                __PRETTY_FUNCTION__, \
                DEBUG_RESET \
            ); \
            fprintf(stderr, msg, ##__VA_ARGS__); \
            fprintf(stderr, "\n"); \
            debug::print_stack_trace(); \
        } while (0)

#else
    #undef LOG
    #undef LOG_ERROR
    #define LOG(msg, ...)
    #define LOG_ERROR(msg, ...)
#endif // DEBUG_LOG

#ifdef DEBUG_ERROR

    #define ERROR(msg, ...) \
        do { \
            fprintf( \
                stderr, \
                "%s[%s]%s: ", \
                DEBUG_RED, \
                __PRETTY_FUNCTION__, \
                DEBUG_RESET \
            ); \
            fprintf(stderr, msg, ##__VA_ARGS__); \
            fprintf(stderr, "\n"); \
            debug::print_stack_trace(); \
            std::abort(); \
        } while (0)

#else
    #define ERROR(msg, ...)
#endif // DEBUG_ERROR
