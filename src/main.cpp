#include <iostream>

#include "common.h"

int main() {
    SinglyLinkedList<int> ll = {};
    SinglyLinkedList<int>::Node node(10);
    SinglyLinkedList<int>::Node node2(20);

    ll.prepend(&node);
    node.insert_after(&node2);

    auto n = node.remove_next();
    assert(n.unwrap() == &node2);

    return 0;
}
