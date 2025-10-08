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

expect # Reverse single element
    list = empty({}) |> add(42)
    reversed = reverse(list)
    head(reversed) == Ok(42)

expect # Reverse empty list
    list = empty({})
    reversed = reverse(list)
    head(reversed) == Err(EmptyList)

expect # Large list construction
    list = List.range({ start: At 1, end: At 100 })
        |> List.walk(empty({}), |acc, i| add(acc, i))
    head(list) == Ok(100) &&  # Last added is head
    get(list, 99) == Ok(1)  # First added is at the end

expect # Large list get operations
    list = List.range({ start: At 1, end: At 50 })
        |> List.walk(empty({}), |acc, i| add(acc, i))
    get(list, 0) == Ok(50) &&
    get(list, 25) == Ok(25) &&
    get(list, 49) == Ok(1) &&
    get(list, 50) == Err(IndexOutOfBounds)

expect # Multiple tail operations
    list = empty({})
        |> add(1)
        |> add(2)
        |> add(3)
        |> add(4)
    step1 = tail(list)
    step2 = when step1 is
        Ok(l) -> tail(l)
        Err(_) -> Err(EmptyList)
    step3 = when step2 is
        Ok(l) -> tail(l)
        Err(_) -> Err(EmptyList)
    when step3 is
        Ok(l) -> head(l) == Ok(1)
        Err(_) -> Bool.false

expect # Tail on single element returns empty
    list = empty({}) |> add(42)
    result = tail(list)
    when result is
        Ok(rest) -> head(rest) == Err(EmptyList)
        Err(_) -> Bool.false

expect # Get boundary indices
    list = empty({})
        |> add(1)
        |> add(2)
        |> add(3)
    get(list, 0) == Ok(3) &&  # First index
    get(list, 2) == Ok(1)  # Last index

expect # Get on reversed large list
    list = List.range({ start: At 1, end: At 30 })
        |> List.walk(empty({}), |acc, i| add(acc, i))
    reversed = reverse(list)
    get(reversed, 0) == Ok(1) &&
    get(reversed, 29) == Ok(30)

expect # Add many elements
    list = List.range({ start: At 1, end: At 200 })
        |> List.walk(empty({}), |acc, i| add(acc, i))
    head(list) == Ok(200)

expect # Alternating add and head
    list1 = empty({}) |> add(1)
    h1 = head(list1)
    list2 = add(list1, 2)
    h2 = head(list2)
    list3 = add(list2, 3)
    h3 = head(list3)
    h1 == Ok(1) && h2 == Ok(2) && h3 == Ok(3)

expect # Get negative index
    list = empty({}) |> add(1) |> add(2)
    # Assuming negative indices are out of bounds
    get(list, 0) == Ok(2)  # Valid index

expect # Multiple reverses
    list = empty({})
        |> add(1)
        |> add(2)
        |> add(3)
    reversed_once = reverse(list)
    reversed_twice = reverse(reversed_once)
    head(reversed_twice) == Ok(3) &&
    get(reversed_twice, 2) == Ok(1)

expect # Empty list operations
    list : LinkedList I64
    list = empty({})
    head(list) |> Result.is_err &&
    tail(list) |> Result.is_err &&
    get(list, 0) |> Result.is_err

expect # Single element all operations
    list = empty({}) |> add(100)
    head(list) == Ok(100) &&
    get(list, 0) == Ok(100) &&
    get(list, 1) == Err(IndexOutOfBounds)

expect # Sequential indices
    list = empty({})
        |> add(1)
        |> add(2)
        |> add(3)
        |> add(4)
        |> add(5)
    get(list, 0) == Ok(5) &&
    get(list, 1) == Ok(4) &&
    get(list, 2) == Ok(3) &&
    get(list, 3) == Ok(2) &&
    get(list, 4) == Ok(1)

expect # Reverse preserves all elements
    list = empty({})
        |> add(10)
        |> add(20)
        |> add(30)
        |> add(40)
    reversed = reverse(list)
    get(reversed, 0) == Ok(10) &&
    get(reversed, 1) == Ok(20) &&
    get(reversed, 2) == Ok(30) &&
    get(reversed, 3) == Ok(40)

expect # Large reversed list head and tail
    list = List.range({ start: At 1, end: At 50 })
        |> List.walk(empty({}), |acc, i| add(acc, i))
    reversed = reverse(list)
    head(reversed) == Ok(1) &&
    when tail(reversed) is
        Ok(rest) -> head(rest) == Ok(2)
        Err(_) -> Bool.false
