#pragma once

#include <sys/mman.h>

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

inline bool is_pow2(u64 n) {
    return (n & (n - 1)) == 0;
}

inline u64 align_pow2_backward(u64 address, u64 alignment) {
    assert(is_pow2(alignment));
    // 000010000 // example alignment
    // 000001111 // subtract 1
    // 111110000 // binary not
    return address & ~(alignment - 1);
}

inline u64 align_pow2_forward(u64 address, u64 alignment) {
    assert(is_pow2(alignment));
    return align_pow2_backward(address + (alignment - 1), alignment);
}

inline u64 alignment_to_bytes(u8 alignment) {
    return 1 << alignment;
}

template<typename T>
class Option {
    T value;
    bool _has_value;

  public:
    // null
    Option<T>() : value(), _has_value(false) {}

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

    Slice() : data(nullptr), len(0) {}

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
        Option<Node*> next;
        T value;

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

// can't expand
struct BumpAllocator {
    u64 capacity;
    u64 end_index;
    u8* data;

    BumpAllocator(u64 capacity) :
        capacity(capacity),
        end_index(0),
        data(nullptr) {
        data = static_cast<u8*>(mmap(
            NULL,
            capacity,
            PROT_READ | PROT_WRITE,
            MAP_PRIVATE | MAP_ANONYMOUS,
            -1,
            0
        ));
        assert(data != MAP_FAILED);
    }

    void free() {
        munmap(data, capacity);
        data = nullptr;
        capacity = 0;
        end_index = 0;
    }

    template<typename T>
    [[nodiscard]]
    Option<Slice<T>> alloc_aligned(u64 count, u64 alignment) {
        alignment = std::max<u64>(alignof(T), alignment);
        auto base = reinterpret_cast<u64>(data);
        auto current = base + end_index;
        auto aligned = align_pow2_forward(current, alignment);
        u64 new_index = aligned - base;
        auto size = count * sizeof(T);

        if (new_index + size > capacity) {
            return {};
        }

        end_index = new_index + size;
        return Slice(reinterpret_cast<T*>(data + new_index), count);
    }

    template<typename T>
    [[nodiscard]]
    Option<Slice<T>> alloc(u64 count) {
        return alloc_aligned<T>(count, alignof(T));
    }

    void reset() {
        end_index = 0;
    }

    template<typename T>
    Option<T*> create_aligned(u64 alignment) {
        alignment = std::max<u64>(alignof(T), alignment);
        auto base = reinterpret_cast<u64>(data);
        auto current = base + end_index;
        auto aligned = align_pow2_forward(current, alignment);
        u64 new_index = aligned - base;
        auto size = sizeof(T);

        if (new_index + size > capacity) {
            return {};
        }

        end_index = new_index + size;
        return reinterpret_cast<T*>(data + new_index);
    }

    template<typename T>
    Option<T*> create() {
        return create_aligned<T>(alignof(T));
    }
};

class ArenaAllocator {
  public:
    static constexpr u64 DEFAULT_BUFNODE_CAP = 1024 * 8;

    struct BufNode {
        u64 capacity;
        u8 data[];
    };

    SinglyLinkedList<BufNode> buffer_list;
    u64 end_index;

    template<typename T>
    Slice<T> alloc(u64 count, u64 alignment);

    template<typename T>
    T* create();

    template<typename T>
    Option<Slice<T>> resize(Slice<T> current, u64 new_len);

    template<typename T>
    void free(T* ptr);

  private:
    SinglyLinkedList<BufNode>::Node*
    create_node(u64 prev_len, u64 minimum_size);

    void free_node(SinglyLinkedList<BufNode>::Node* node);
};
