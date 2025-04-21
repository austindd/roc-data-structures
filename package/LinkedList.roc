module [
    LinkedList,
    empty,
    head,
    tail,
    add,
    get,
    reverse,
]

import LinkedListBase exposing [
    LinkedListBase,
]

LinkedList a := LinkedListBase a

empty : {} -> LinkedList a
empty = |{}|
    @LinkedList(LinkedListBase.empty({}))

head : LinkedList a -> Result a [EmptyList]
head = |@LinkedList(list)|
    LinkedListBase.head(list)

tail : LinkedList a -> Result (LinkedList a) [EmptyList]
tail = |@LinkedList(list)|
    Result.map_ok(LinkedListBase.tail(list), @LinkedList)

add : LinkedList a, a -> LinkedList a
add = |@LinkedList(list), value|
    @LinkedList(LinkedListBase.add(list, value))

get : LinkedList a, U64 -> Result a [EmptyList, IndexOutOfBounds]
get = |@LinkedList(list), index|
    LinkedListBase.get(list, index)

reverse : LinkedList a -> LinkedList a
reverse = |@LinkedList(list)|
    @LinkedList(LinkedListBase.reverse(list))
