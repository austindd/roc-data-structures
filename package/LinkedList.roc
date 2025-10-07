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

# Tests

expect # Empty list head
    list = empty({})
    head(list) == Err(EmptyList)

expect # Empty list tail
    list = empty({})
    tail(list) |> Result.is_err

expect # Single element
    list = empty({}) |> add(42)
    head(list) == Ok(42)

expect # Multiple elements - LIFO order
    list = empty({})
        |> add(1)
        |> add(2)
        |> add(3)
    head(list) == Ok(3)

expect # Tail returns remaining list
    list = empty({})
        |> add(1)
        |> add(2)
        |> add(3)
    remaining = tail(list)
    when remaining is
        Ok(rest) -> head(rest) == Ok(2)
        Err(_) -> Bool.false

expect # Get by index
    list = empty({})
        |> add(1)
        |> add(2)
        |> add(3)
    get(list, 0) == Ok(3) &&  # Head is index 0
    get(list, 1) == Ok(2) &&
    get(list, 2) == Ok(1)

expect # Get out of bounds
    list = empty({}) |> add(1)
    get(list, 5) == Err(IndexOutOfBounds)

expect # Reverse list
    list = empty({})
        |> add(1)
        |> add(2)
        |> add(3)
    reversed = reverse(list)
    get(reversed, 0) == Ok(1) &&
    get(reversed, 1) == Ok(2) &&
    get(reversed, 2) == Ok(3)
