module [
    Heap,
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

## A binary min-heap where elements must implement the Ord ability
Heap a := HeapBase a where a implements Ord
    implements [
        Eq { is_eq: heap_eq },
    ]

heap_eq : Heap a, Heap a -> Bool where a implements Eq & Ord & Inspect
heap_eq = |@Heap(heap_a), @Heap(heap_b)|
    HeapBase.to_list(heap_a) == HeapBase.to_list(heap_b)

empty : {} -> Heap a
empty = |{}|
    @Heap(HeapBase.empty({}))

insert : Heap a, a -> Heap a
insert = |@Heap(heap), value|
    @Heap(HeapBase.insert(heap, value))

peek : Heap a -> Result a [ListWasEmpty]
peek = |@Heap(heap)|
    HeapBase.peek(heap)

pop : Heap a -> Result { value : a, heap : Heap a } [ListWasEmpty]
pop = |@Heap(heap)|
    when HeapBase.pop(heap) is
        Err(ListWasEmpty) -> Err(ListWasEmpty)
        Ok({ value, heap: new_heap }) ->
            Ok({ value, heap: @Heap(new_heap) })

from_list : List a -> Heap a
from_list = |list|
    @Heap(HeapBase.from_list(list))

to_list : Heap a -> List a
to_list = |@Heap(heap)|
    HeapBase.to_list(heap)

size : Heap a -> U64
size = |@Heap(heap)|
    HeapBase.size(heap)

is_empty : Heap a -> Bool
is_empty = |@Heap(heap)|
    HeapBase.is_empty(heap)

merge : Heap a, Heap a -> Heap a
merge = |@Heap(heap1), @Heap(heap2)|
    @Heap(HeapBase.merge(heap1, heap2))

# Tests

# Helper wrapper for testing since raw numbers don't implement Ord
TestVal := I64 implements [Ord { compare: test_val_compare }, Eq, Inspect]

test_val_compare : TestVal, TestVal -> Ordering
test_val_compare = |@TestVal(a), @TestVal(b)| Num.compare(a, b)

expect # Empty heap
    heap : Heap TestVal
    heap = empty({})
    is_empty(heap) && size(heap) == 0

expect # Single element
    heap = empty({}) |> insert(@TestVal(42))
    peek(heap) == Ok(@TestVal(42)) && size(heap) == 1

expect # Insert maintains min property
    heap = empty({})
        |> insert(@TestVal(5))
        |> insert(@TestVal(3))
        |> insert(@TestVal(7))
        |> insert(@TestVal(1))
    peek(heap) == Ok(@TestVal(1))

expect # Pop returns minimum
    heap = empty({})
        |> insert(@TestVal(5))
        |> insert(@TestVal(3))
        |> insert(@TestVal(7))
    when pop(heap) is
        Ok({ value, heap: new_heap }) ->
            value == @TestVal(3) && peek(new_heap) == Ok(@TestVal(5))

        _ -> Bool.false

expect # From list and to list
    list = [@TestVal(9), @TestVal(3), @TestVal(7), @TestVal(1), @TestVal(5)]
    heap = from_list(list)
    sorted = to_list(heap)
    sorted == [@TestVal(1), @TestVal(3), @TestVal(5), @TestVal(7), @TestVal(9)]

expect # Merge heaps
    heap1 = empty({})
        |> insert(@TestVal(5))
        |> insert(@TestVal(3))
    heap2 = empty({})
        |> insert(@TestVal(7))
        |> insert(@TestVal(1))
    merged = merge(heap1, heap2)
    peek(merged) == Ok(@TestVal(1)) && size(merged) == 4

expect # Heap equality
    heap1 = from_list([@TestVal(3), @TestVal(1), @TestVal(2)])
    heap2 = from_list([@TestVal(2), @TestVal(3), @TestVal(1)])
    heap1 == heap2

expect # Large heap
    heap = List.range({ start: At 100, end: At 1 })
        |> List.walk(empty({}), |acc, i| insert(acc, @TestVal(i)))
    peek(heap) == Ok(@TestVal(1)) && size(heap) == 100

expect # Pop until empty
    heap = from_list([@TestVal(3), @TestVal(1), @TestVal(2)])
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
            v1 == @TestVal(1) && v2 == @TestVal(2) && v3 == @TestVal(3)

        _ -> Bool.false

expect # Empty heap operations
    heap : Heap TestVal
    heap = empty({})
    peek(heap) == Err(ListWasEmpty) && pop(heap) == Err(ListWasEmpty)

expect # Merge with empty
    heap = from_list([@TestVal(1), @TestVal(2)])
    empty_heap = empty({})
    merged1 = merge(heap, empty_heap)
    merged2 = merge(empty_heap, heap)
    merged1 == heap && merged2 == heap
