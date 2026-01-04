#include "common.h"

#include <cstdlib>

using BufNode = ArenaAllocator::BufNode;
using Node = SinglyLinkedList<BufNode>::Node;

template<typename T>
Slice<T> ArenaAllocator::alloc(u64 count, u64 alignment) {
    const auto ptr_align = alignment_to_bytes(alignment);
    auto cur_node =
        buffer_list.first.unwrap_or(create_node(0, count + ptr_align))->value;

    while (true) {
        Slice<u8> cur_alloc_buf =
            Slice(static_cast<u8*>(cur_node.data), cur_node.capacity);
        auto cur_buf = cur_alloc_buf.subslice(sizeof(Node), cur_alloc_buf.len);
        auto addr = reinterpret_cast<u64>(cur_buf.data + end_index);
        auto new_addr = align_pow2_forward(addr, ptr_align);
        auto adjusted_index = end_index + (new_addr - addr);
        auto new_end_index = adjusted_index + count;

        if (new_end_index <= cur_buf.len) {
            auto result = cur_buf.subslice(adjusted_index, new_end_index);
            end_index = new_end_index;
            return result;
        }

        auto bigger_buffer_size = sizeof(Node) + new_end_index;
    }
}

template<typename T>
T* ArenaAllocator::create() {}

template<typename T>
Option<Slice<T>> resize(Slice<T> current, u64 new_len);

template<typename T>
void ArenaAllocator::free(T* ptr) {}

Node* ArenaAllocator::create_node(u64 prev_len, u64 minimum_size) {
    auto actual_min_size = minimum_size + sizeof(Node);
    auto big_enough_len = prev_len + actual_min_size;
    auto len = big_enough_len + big_enough_len / 2;
    Node* node = static_cast<Node*>(malloc(len));
    node->value.capacity = len;
    buffer_list.prepend(node);
    end_index = 0;
    return node;
}

void ArenaAllocator::free_node(Node* node) {
    free(node);
}
