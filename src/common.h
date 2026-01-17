#pragma once

#include <sys/mman.h>
#include <valgrind/valgrind.h>

#ifdef __linux__
    #include <valgrind/memcheck.h>
#endif // __linux__

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

inline void* raw_alloc(u64 len) {
    auto ptr = mmap(
        NULL,
        len,
        PROT_READ | PROT_WRITE,
        MAP_PRIVATE | MAP_ANONYMOUS,
        -1,
        0
    );
    assert(ptr != MAP_FAILED);

#ifdef __linux__
    if (RUNNING_ON_VALGRIND) {
        VALGRIND_MALLOCLIKE_BLOCK(ptr, len, 0, 0);
    }
#endif // __linux__

    return ptr;
}

inline void raw_free(void* ptr, u64 size) {
    int err = munmap(ptr, size);
    assert(err != -1);

#ifdef __linux__
    if (RUNNING_ON_VALGRIND) {
        VALGRIND_FREELIKE_BLOCK(ptr, size);
    }
#endif // __linux__
}

inline u64 align_pow2_backward(u64 address, u64 alignment) {
    assert(is_pow2(alignment));
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

    using Iterator = T*;
    using ConstIterator = const T*;

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

    Iterator begin() {
        return data;
    }

    Iterator end() {
        return data + len;
    }

    ConstIterator begin() const {
        return data;
    }

    ConstIterator end() const {
        return data + len;
    }

    ConstIterator cbegin() const {
        return data;
    }

    ConstIterator cend() const {
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

        Node(T value) : next(), value(value) {}

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

    u64 len() {
        auto it = first;
        u64 size = 0;
        while (it.has_value()) {
            size += 1;
            it = it.unwrap()->next;
        }
        return size;
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
    [[nodiscard]] Option<Slice<T>> alloc_aligned(u64 count, u64 alignment) {
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
        return Slice(static_cast<T*>(data + new_index), count);
    }

    template<typename T>
    [[nodiscard]] Option<Slice<T>> alloc(u64 count) {
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
        return static_cast<T*>(data + new_index);
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

    struct AllocationHeader {
        u64 padding;
    };

    ArenaAllocator() {}

    SinglyLinkedList<BufNode> buffer_list;
    u64 end_index;

    template<typename T>
    Slice<T> alloc_aligned(u64 count, u64 alignment) {
        assert(count > 0);

        alignment = std::max<u64>(alignof(T), alignment);
        auto size = count * sizeof(T);

        if (buffer_list.first.is_null()) {
            buffer_list.first = create_node(0, size);
        }

        BufNode* node = &buffer_list.first.unwrap()->value;

        while (true) {
            auto base = reinterpret_cast<u64>(node->data);
            auto current = base + end_index;
            auto aligned = align_pow2_forward(
                current + sizeof(AllocationHeader),
                alignment
            );
            u64 padding = aligned - current;
            auto adjusted_index = end_index + padding;
            auto new_end_index = adjusted_index + size;

            auto* header = reinterpret_cast<AllocationHeader*>(
                aligned - sizeof(AllocationHeader)
            );
            header->padding = padding;

            if (new_end_index <= node->capacity) {
                end_index = new_end_index;
                return Slice<T>(
                    reinterpret_cast<T*>(node->data + adjusted_index),
                    count
                );
            }
            node = &create_node(node->capacity, size + alignment)->value;
        }
    }

    template<typename T>
    Slice<T> alloc(u64 count) {
        return alloc_aligned<T>(count, alignof(T));
    }

    template<typename T>
    void pop_aligned(Slice<T> mem, u64 alignment) {
        if (buffer_list.first.is_null()) {
            printf("empty buffer list\n");
            return;
        }

        auto len_bytes = mem.len * sizeof(T);
        auto mem_ptr = reinterpret_cast<u64>(mem.data);
        auto* cur_buf = &buffer_list.first.unwrap()->value;
        auto cur_end = reinterpret_cast<u64>(cur_buf->data) + end_index;

        if (cur_end == mem_ptr + len_bytes) {
            auto* header = reinterpret_cast<AllocationHeader*>(
                mem_ptr - sizeof(AllocationHeader)
            );
            end_index -= len_bytes + header->padding;
        }
    }

    template<typename T>
    void pop(Slice<T> mem) {
        pop_aligned<T>(mem, alignof(T));
    }

    u64 query_capacity() {
        u64 size = 0;
        auto it = buffer_list.first;
        while (it.has_value()) {
            auto node = it.unwrap();
            size += node->value.capacity - sizeof(Node);
            it = node->next;
        }
        return size;
    }

    // retains capacity
    void reset() {
        auto target_capacity = query_capacity();
        auto total_size = target_capacity + sizeof(Node);

        auto it = buffer_list.first;
        Option<Node*> maybe_first_node = {};
        while (it.has_value()) {
            auto node = it.unwrap();
            auto next_it = node->next;

            if (next_it.is_null()) {
                maybe_first_node = node;
                break;
            }

            it = next_it;
        }
        assert(
            maybe_first_node.is_null()
            || maybe_first_node.unwrap()->next.is_null()
        );
        end_index = 0;
        if (maybe_first_node.has_value()) {
            auto first_node = maybe_first_node.unwrap();
            buffer_list.first = first_node;

            auto* first_buf_node = &maybe_first_node.unwrap()->value;
            if (first_buf_node->capacity == total_size) {
                return;
            }
            auto new_ptr = raw_alloc(total_size);
            free_node(first_node);
            auto node = static_cast<Node*>(new_ptr);
            node->value.capacity = total_size;
            buffer_list.first = node;
        }
    }

    template<typename T>
    T* create();

    void clear() {
        auto it = buffer_list.first;
        while (it.has_value()) {
            auto* node = it.unwrap();
            it = node->next;
            free_node(node);
        }
        buffer_list.first = {};
        end_index = 0;
    }

  private:
    using BufNode = ArenaAllocator::BufNode;
    using Node = SinglyLinkedList<BufNode>::Node;

    Node* create_node(u64 prev_len, u64 minimum_size) {
        auto actual_min_size = minimum_size + sizeof(Node);
        auto big_enough_len = prev_len + actual_min_size;
        auto len = big_enough_len + big_enough_len / 2;
        Node* node = static_cast<Node*>(raw_alloc(len));
        node->value.capacity = len - sizeof(Node);
        buffer_list.prepend(node);
        end_index = 0;
        return node;
    }

    void free_node(Node* node) {
        raw_free(node, sizeof(Node) + node->value.capacity);
    }
};
