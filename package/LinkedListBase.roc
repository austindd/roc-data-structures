module [
    empty,
    head,
    tail,
    add,
    concat,
    set,
    LinkedListBase,
]

LinkedListBase a : [
    Cons a (LinkedListBase a),
    Nil,
]

empty : {} -> LinkedListBase a
empty = |{}| Nil

head : LinkedListBase a -> Result a [EmptyList]
head = |linked_list|
    when linked_list is
        Cons(element, _) -> Ok(element)
        Nil -> Err(EmptyList)

tail : LinkedListBase a -> Result (LinkedListBase a) [EmptyList]
tail = |linked_list|
    when linked_list is
        Cons(_, next) -> Ok(next)
        Nil -> Err(EmptyList)

add : LinkedListBase a, a -> LinkedListBase a
add = |list, element|
    Cons(element, list)

concat : LinkedListBase a, LinkedListBase a -> LinkedListBase a
concat = |list1, list2|
    when list1 is
        Cons(element, next) -> Cons(element, concat(next, list2))
        Nil -> list2

set : LinkedListBase a, U64, a -> Result (LinkedListBase a) [OutOfBounds]
set = |list, index, element|
    when list is
        Cons(hd, next) ->
            if index == 0 then
                Ok(Cons(element, next))
            else
                set(next, index - 1, element)

        Nil -> Err(OutOfBounds)

expect
    list = empty({})
        |> add(1)
        |> add(2)
        |> add(3)

    new_list = set(list, 0, 42)

    new_list == Ok(
        empty({})
            |> add(1)
            |> add(2)
            |> add(42)
        )
