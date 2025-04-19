module [
    empty,
    add,
]

import LinkedListBase exposing [
    LinkedListBase
]

LinkedList a := LinkedListBase a

empty : {} -> LinkedList a
empty = |{}|
    @LinkedList(LinkedListBase.empty({}))

add : LinkedList a, a -> LinkedList a
add = |@LinkedList(list), value|
    @LinkedList(LinkedListBase.add(list, value))

head : LinkedList a -> Result a [EmptyList]
head = |@LinkedList(list)|
    LinkedListBase.head(list)
