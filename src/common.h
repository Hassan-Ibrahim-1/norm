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

template<typename T>
class Option {
    T value;
    bool has_value;

  public:
    Option<T>() : has_value(false) {}

    Option<T>(T value) : value(value), has_value(true) {}

    T unwrap() const {
        assert(has_value);
        return value;
    }

    T unwrap_or(T or_else) const {
        return has_value ? value : or_else;
    }

    bool is_null() const {
        return !has_value;
    }
};

template<typename T, typename E>
class Result {
    union U {
        T ok;
        E error;

        U(T ok) : ok(ok) {}

        U(E error) : error(error) {}

    } value;

    bool _ok;

  public:
    Result<T, E>(T value) : value(value), _ok(true) {}

    Result<T, E>(E error) : value(error), _ok(false) {}

    T unwrap() const {
        assert(_ok);
        return value.ok;
    }

    T unwrap_or(T or_else) const {
        return _ok ? value.ok : or_else;
    }

    Option<T> ok() const {
        return _ok ? Option(value.ok) : Option<T>();
    }

    Option<E> err() const {
        return !_ok ? Option(value.error) : Option<E>();
    }

    bool is_ok() const {
        return _ok;
    }

    bool is_err() const {
        return !_ok;
    }
};
