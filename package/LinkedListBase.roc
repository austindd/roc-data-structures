module [
    LinkedListBase,
    empty,
    head,
    tail,
    add,
    concat,
    set,
    get,
    reverse,
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
        Cons(_, next) ->
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

get : LinkedListBase a, U64 -> Result a [EmptyList, IndexOutOfBounds]
get = |list, index|
    get_inner = |list_, index_|
        when list_ is
            Cons(element, next) ->
                if index_ == 0 then
                    Ok(element)
                else
                    get(next, index_ - 1)

            Nil -> Err(IndexOutOfBounds)
    get_inner(list, index)

expect
    list = empty({})
        |> add(1)
        |> add(2)
        |> add(3)

    element = get(list, 1)

    element == Ok(2)

reverse : LinkedListBase a -> LinkedListBase a
reverse = |list|
    reverse_inner = |list_, acc|
        when list_ is
            Cons(element, next) ->
                reverse_inner(next, Cons(element, acc))

            Nil -> acc
    reverse_inner(list, Nil)

expect
    list = empty({})
        |> add(1)
        |> add(2)
        |> add(3)

    reversed_list = reverse(list)

    reversed_list == empty({})
        |> add(3)
        |> add(2)
        |> add(1)
