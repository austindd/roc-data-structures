module [
    empty,
    head,
    tail,
    append,
    concat,
    # set_at,
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

append : LinkedListBase a, a -> LinkedListBase a
append = |list, element|
    Cons(element, list)

concat : LinkedListBase a, LinkedListBase a -> LinkedListBase a
concat = |list1, list2|
    when list1 is
        Cons(element, next) -> Cons(element, concat(next, list2))
        Nil -> list2

# set_at : LinkedListBase a, Int i, a -> Result (LinkedListBase a) [OutOfBounds]
# set_at = |list, index, element|
#    if index == 0 then
#        Ok(Cons(element, list))
#    else
#        when list is
#            Cons(head, next) -> Ok(Cons(head, set_at(next, index - 1, element)))
#            Nil -> Err(OutOfBounds)
