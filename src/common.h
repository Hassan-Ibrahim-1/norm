#include <cassert>
#include <cstddef>
#include <cstdint>
#include <string>
#include <type_traits>

using u8 = std::uint8_t;
using u16 = std::uint16_t;
using u32 = std::uint32_t;
using u64 = std::uint64_t;

using i8 = std::int8_t;
using i16 = std::int16_t;
using i32 = std::int32_t;
using i64 = std::int64_t;

template<typename T>
class Option {
    T value;
    bool _has_value;

  public:
    Option<T>() : value {}, _has_value(false) {}

    Option<T>(T value) : value(value), _has_value(true) {}

    T unwrap() const {
        assert(_has_value);
        return value;
    }

    T unwrap_or(T or_else) const {
        return _has_value ? value : or_else;
    }

    bool is_null() const {
        return !_has_value;
    }

    bool has_value() const {
        return _has_value;
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

template<typename T>
struct Slice {
    T* data;
    u64 len;

    using iterator = T*;
    using const_iterator = const T*;

    Slice(T* data, u64 len) : data(data), len(len) {}

    Slice<T> subslice(u64 start, std::size_t end) {
        assert(end <= len);
        assert(start <= end);

        return Slice(data + start, end - start);
    }

    T& at(u64 index) {
        assert(index < len);
        return *(data + index);
    }

    const T& at(u64 index) const {
        assert(index < len);
        return *(data + index);
    }

    T& operator[](u64 index) {
        return at(index);
    }

    const T& operator[](u64 index) const {
        return at(index);
    }

    std::string to_string() const {
        if constexpr (std::is_same_v<T, char>) {
            return std::string(data, len);
        }
        std::string result = "{ ";
        for (u64 i = 0; i < len; ++i) {
            if (i > 0) {
                result += ", ";
            }
            result += std::to_string(data[i]);
        }
        result += " }";
        return result;
    }

    iterator begin() {
        return data;
    }

    iterator end() {
        return data + len;
    }

    const_iterator begin() const {
        return data;
    }

    const_iterator end() const {
        return data + len;
    }

    const_iterator cbegin() const {
        return data;
    }

    const_iterator cend() const {
        return data + len;
    }

    Option<u64> index_of(T target) const {
        for (int i = 0; i < len; i += 1) {
            if (target == data[i])
                return static_cast<u64>(i);
        }
        return {};
    }
};

using String = Slice<char>;

template<typename T>
struct SinglyLinkedList {
    struct Node {
        T value;
        Option<Node*> next;

        Node(T value) : value(value), next() {}

        void insert_after(Node* new_node) {
            new_node->next = next;
            next = new_node;
        }

        Option<Node*> remove_next() {
            if (next.is_null())
                return {};
            Node* n = next.unwrap();
            next = n->next;
            return n;
        }
    };

    Option<Node*> first;

    SinglyLinkedList() : first() {}

    void prepend(Node* new_node) {
        new_node->next = first;
        first = new_node;
    }

    void remove(Node* node) {
        Node* current = first.unwrap();
        while (current != node) {
            current = current->next.unwrap();
        }
        current->next = node->next;
    }

    Option<Node*> pop_first() {
        if (first.is_null())
            return {};
        Node* f = first.unwrap();
        first = f->next;
        return f;
    }
};
