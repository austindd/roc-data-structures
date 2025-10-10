module [
    HeapNum,
    empty,
    insert,
    peek,
    pop,
    from_list,
    to_list,
    size,
    is_empty,
    merge,
]

import HeapBase exposing [HeapBase]
import Ord exposing [Ord, Ordering]

debug_heapnum : HeapNum a -> Inspector _ where a implements Ord & Inspect
debug_heapnum = |@HeapNum(value)|
    Inspect.to_inspector(value)

num_compare : NumVal a, NumVal a -> Ordering where a implements Ord
num_compare = |@NumVal(a), @NumVal(b)|
    Num.compare(a, b)

NumVal a := Num a implements [
        Ord {
            compare: num_compare,
        },
        Eq,
        Hash,
        Inspect,
    ]

## A binary min-heap specialized for numeric types
HeapNum a := HeapBase (NumVal a) implements [
        Eq,
        Hash,
        Inspect {
            to_inspector: debug_heapnum,
        },
    ]

empty : {} -> HeapNum a
empty = |{}|
    HeapBase.empty({}) |> @HeapNum

insert : HeapNum a, Num a -> HeapNum a
insert = |@HeapNum(heap), value|
    HeapBase.insert(heap, @NumVal(value)) |> @HeapNum

peek : HeapNum a -> Result (Num a) [ListWasEmpty]
peek = |@HeapNum(heap)|
    when HeapBase.peek(heap) is
        Err(ListWasEmpty) -> Err(ListWasEmpty)
        Ok(@NumVal(value)) -> Ok(value)

pop : HeapNum a -> Result { value : Num a, heap : HeapNum a } [ListWasEmpty]
pop = |@HeapNum(heap)|
    when HeapBase.pop(heap) is
        Err(ListWasEmpty) -> Err(ListWasEmpty)
        Ok({ value: @NumVal(v), heap: new_heap }) ->
            Ok({ value: v, heap: @HeapNum(new_heap) })

from_list : List (Num a) -> HeapNum a
from_list = |list|
    list
    |> List.map(@NumVal)
    |> HeapBase.from_list
    |> @HeapNum

to_list : HeapNum a -> List (Num a)
to_list = |@HeapNum(heap)|
    HeapBase.to_list(heap)
    |> List.map(|@NumVal(v)| v)

size : HeapNum a -> U64
size = |@HeapNum(heap)|
    HeapBase.size(heap)

is_empty : HeapNum a -> Bool
is_empty = |@HeapNum(heap)|
    HeapBase.is_empty(heap)

merge : HeapNum a, HeapNum a -> HeapNum a
merge = |@HeapNum(heap1), @HeapNum(heap2)|
    HeapBase.merge(heap1, heap2) |> @HeapNum

# Tests

expect # Empty heap
    heap : HeapNum I64
    heap = empty({})
    is_empty(heap) && size(heap) == 0

expect # Single element
    heap = empty({}) |> insert(42)
    peek(heap) == Ok(42) && size(heap) == 1

expect # Insert maintains min property
    heap = empty({})
        |> insert(5)
        |> insert(3)
        |> insert(7)
        |> insert(1)
    peek(heap) == Ok(1)

expect # Pop returns minimum
    heap = empty({})
        |> insert(5)
        |> insert(3)
        |> insert(7)
    when pop(heap) is
        Ok({ value, heap: new_heap }) ->
            value == 3 && peek(new_heap) == Ok(5)

        _ -> Bool.false

expect # From list and to list
    list = [9, 3, 7, 1, 5]
    heap = from_list(list)
    sorted = to_list(heap)
    sorted == [1, 3, 5, 7, 9]

expect # Merge heaps
    heap1 = empty({})
        |> insert(5)
        |> insert(3)
    heap2 = empty({})
        |> insert(7)
        |> insert(1)
    merged = merge(heap1, heap2)
    peek(merged) == Ok(1) && size(merged) == 4

expect # Heap equality
    heap1 = from_list([3, 1, 2])
    heap2 = from_list([2, 3, 1])
    heap1 == heap2

expect # Large heap
    heap = List.range({ start: At 100, end: At 1 })
        |> List.walk(empty({}), |acc, i| insert(acc, i))
    peek(heap) == Ok(1) && size(heap) == 100

expect # Pop until empty
    heap = from_list([3, 1, 2])
    result1 = pop(heap)
    result2 = when result1 is
        Ok({ heap: h }) -> pop(h)
        _ -> Err(ListWasEmpty)
    result3 = when result2 is
        Ok({ heap: h }) -> pop(h)
        _ -> Err(ListWasEmpty)
    result4 = when result3 is
        Ok({ heap: h }) -> pop(h)
        _ -> Err(ListWasEmpty)

    when (result1, result2, result3, result4) is
        (Ok({ value: v1 }), Ok({ value: v2 }), Ok({ value: v3 }), Err(ListWasEmpty)) ->
            v1 == 1 && v2 == 2 && v3 == 3

        _ -> Bool.false

expect # Empty heap operations
    heap : HeapNum I64
    heap = empty({})
    peek(heap) == Err(ListWasEmpty) && pop(heap) == Err(ListWasEmpty)

expect # Merge with empty
    heap = from_list([1, 2])
    empty_heap = empty({})
    merged1 = merge(heap, empty_heap)
    merged2 = merge(empty_heap, heap)
    merged1 == heap && merged2 == heap

expect # Decimal numbers
    heap = empty({})
        |> insert(3.14)
        |> insert(2.71)
        |> insert(1.41)
    peek(heap) == Ok(1.41)

expect # Negative numbers
    heap = from_list([5, -3, 7, -10, 2])
    sorted = to_list(heap)
    sorted == [-10, -3, 2, 5, 7]

expect # Duplicate values
    heap = from_list([5, 3, 5, 3, 1, 1])
    sorted = to_list(heap)
    sorted == [1, 1, 3, 3, 5, 5]

expect # Heap sort performance on larger dataset
    list = List.range({ start: At 1, end: At 50 })
        |> List.map(|n| 51 - n)  # Reverse order
    heap = from_list(list)
    sorted = to_list(heap)
    sorted == List.range({ start: At 1, end: At 50 })
