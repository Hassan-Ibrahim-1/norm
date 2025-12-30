#include <cassert>
#include <cstddef>
#include <string>

template<typename T>
struct Slice {
    T* data;
    std::size_t len;

    Slice(T* data, std::size_t len) : data(data), len(len) {}

    Slice<T> subslice(std::size_t start, std::size_t end) {
        assert(end <= len);
        assert(start <= end);

        return Slice(data + start, end - start);
    }

    T& at(std::size_t index) {
        assert(index < len);
        return *(data + index);
    }

    const T& at(std::size_t index) const {
        assert(index < len);
        return *(data + index);
    }

    T& operator[](std::size_t index) {
        return at(index);
    }

    const T& operator[](std::size_t index) const {
        return at(index);
    }

    std::string to_string() const {
        std::string result = "{ ";
        for (std::size_t i = 0; i < len; ++i) {
            if (i > 0) {
                result += ", ";
            }
            result += std::to_string(data[i]);
        }
        result += " }";
        return result;
    }
};
