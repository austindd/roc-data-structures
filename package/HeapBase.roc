module [
    HeapBase,
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

import Ord exposing [
    Ord,
    compare,
    Ordering,
]

## A binary min-heap represented as a list
## For element at index i:
## - Left child at: 2*i + 1
## - Right child at: 2*i + 2
## - Parent at: (i - 1) / 2
HeapBase a : List a where a implements Ord & Inspect

empty : {} -> HeapBase a
empty = |{}| []

is_empty : HeapBase a -> Bool
is_empty = |heap|
    List.is_empty(heap)

size : HeapBase a -> U64
size = |heap|
    List.len(heap)

peek : HeapBase a -> Result a [ListWasEmpty]
peek = |heap|
    List.first(heap)

## Insert an element into the heap and maintain heap property
insert : HeapBase a, a -> HeapBase a
insert = |heap, value|
    new_heap = List.append(heap, value)
    bubble_up(new_heap, List.len(new_heap) - 1)

## Remove and return the minimum element
pop : HeapBase a -> Result { value : a, heap : HeapBase a } [ListWasEmpty]
pop = |heap|
    when List.first(heap) is
        Err(ListWasEmpty) -> Err(ListWasEmpty)
        Ok(min_val) ->
            when List.last(heap) is
                Err(ListWasEmpty) -> Err(ListWasEmpty)  # Should not happen if first succeeded
                Ok(last_val) ->
                    if List.len(heap) == 1 then
                        Ok({ value: min_val, heap: [] })
                    else
                        # Replace root with last element, remove last, then bubble down
                        new_heap =
                            heap
                            |> List.set(0, last_val)
                            |> List.drop_last(1)

                        Ok({ value: min_val, heap: bubble_down(new_heap, 0) })

## Build a heap from a list in O(n) time using Floyd's algorithm
from_list : List a -> HeapBase a
from_list = |list|
    len = List.len(list)
    if len <= 1 then
        list
    else
        # Start from last non-leaf node and heapify down
        last_parent = (len - 2) // 2
        heapify_helper(list, last_parent)

heapify_helper : List a, U64 -> List a where a implements Ord & Inspect
heapify_helper = |heap, index|
    heapified = bubble_down(heap, index)
    if index == 0 then
        heapified
    else
        heapify_helper(heapified, index - 1)

## Extract all elements in sorted order
to_list : HeapBase a -> List a
to_list = |heap|
    to_list_helper(heap, [])

to_list_helper : HeapBase a, List a -> List a
to_list_helper = |heap, acc|
    when pop(heap) is
        Err(ListWasEmpty) -> acc
        Ok({ value, heap: new_heap }) ->
            to_list_helper(new_heap, List.append(acc, value))

## Merge two heaps
merge : HeapBase a, HeapBase a -> HeapBase a
merge = |heap1, heap2|
    # Simple approach: combine lists and heapify
    # Could be optimized but this is correct and reasonably efficient
    List.concat(heap1, heap2)
    |> from_list

## Restore heap property by bubbling element up
bubble_up : HeapBase a, U64 -> HeapBase a
bubble_up = |heap, index|
    if index == 0 then
        heap
    else
        parent_idx = (index - 1) // 2
        when (List.get(heap, index), List.get(heap, parent_idx)) is
            (Ok(child), Ok(parent)) ->
                when compare(child, parent) is
                    LT ->
                        # Child is smaller, swap with parent
                        swapped =
                            heap
                            |> List.set(index, parent)
                            |> List.set(parent_idx, child)
                        bubble_up(swapped, parent_idx)

                    _ -> heap

            _ -> heap

## Restore heap property by bubbling element down
bubble_down : HeapBase a, U64 -> HeapBase a
bubble_down = |heap, index|
    len = List.len(heap)
    left_idx = 2 * index + 1
    right_idx = 2 * index + 2

    # Find smallest among parent, left child, and right child
    when List.get(heap, index) is
        Err(OutOfBounds) -> heap
        Ok(current) ->
            smallest_idx = find_smallest(heap, index, left_idx, right_idx, len)

            if smallest_idx == index then
                heap
            else
                when List.get(heap, smallest_idx) is
                    Err(OutOfBounds) -> heap
                    Ok(smallest) ->
                        swapped =
                            heap
                            |> List.set(index, smallest)
                            |> List.set(smallest_idx, current)
                        bubble_down(swapped, smallest_idx)

find_smallest : HeapBase a, U64, U64, U64, U64 -> U64
find_smallest = |heap, current_idx, left_idx, right_idx, len|
    # Start with current as smallest
    smallest_idx = current_idx

    # Check left child
    smallest_with_left =
        if left_idx < len then
            when (List.get(heap, smallest_idx), List.get(heap, left_idx)) is
                (Ok(smallest), Ok(left)) ->
                    when compare(left, smallest) is
                        LT -> left_idx
                        _ -> smallest_idx

                _ -> smallest_idx
        else
            smallest_idx

    # Check right child
    if right_idx < len then
        when (List.get(heap, smallest_with_left), List.get(heap, right_idx)) is
            (Ok(smallest), Ok(right)) ->
                when compare(right, smallest) is
                    LT -> right_idx
                    _ -> smallest_with_left

            _ -> smallest_with_left
    else
        smallest_with_left

# Tests - using TestKey wrapper since raw numbers don't implement Ord

TestKey := I64 implements [Ord { compare: test_key_compare }, Eq, Inspect]

test_key_compare : TestKey, TestKey -> Ordering
test_key_compare = |@TestKey(a), @TestKey(b)| Num.compare(a, b)

expect # Empty heap
    heap = empty({})
    is_empty(heap) && size(heap) == 0

expect # Single element
    heap = empty({}) |> insert(@TestKey(42))
    peek(heap) == Ok(@TestKey(42)) && size(heap) == 1

expect # Insert maintains min property
    heap = empty({})
        |> insert(@TestKey(5))
        |> insert(@TestKey(3))
        |> insert(@TestKey(7))
        |> insert(@TestKey(1))
    peek(heap) == Ok(@TestKey(1))

expect # Pop returns minimum
    heap = empty({})
        |> insert(@TestKey(5))
        |> insert(@TestKey(3))
        |> insert(@TestKey(7))
    when pop(heap) is
        Ok({ value, heap: new_heap }) ->
            value == @TestKey(3) && peek(new_heap) == Ok(@TestKey(5))

        _ -> Bool.false

expect # Pop empty heap
    heap : HeapBase TestKey
    heap = empty({})
    pop(heap) == Err(ListWasEmpty)

expect # Multiple pops in order
    heap = empty({})
        |> insert(@TestKey(5))
        |> insert(@TestKey(3))
        |> insert(@TestKey(7))
        |> insert(@TestKey(1))

    result1 = pop(heap)
    result2 = when result1 is
        Ok({ heap: h1 }) -> pop(h1)
        _ -> Err(ListWasEmpty)
    result3 = when result2 is
        Ok({ heap: h2 }) -> pop(h2)
        _ -> Err(ListWasEmpty)

    when (result1, result2, result3) is
        (Ok({ value: v1 }), Ok({ value: v2 }), Ok({ value: v3 })) ->
            v1 == @TestKey(1) && v2 == @TestKey(3) && v3 == @TestKey(5)

        _ -> Bool.false

expect # From list creates valid heap
    list = [@TestKey(5), @TestKey(3), @TestKey(7), @TestKey(1), @TestKey(9), @TestKey(2)]
    heap = from_list(list)
    peek(heap) == Ok(@TestKey(1))

expect # To list returns sorted elements
    heap = empty({})
        |> insert(@TestKey(5))
        |> insert(@TestKey(3))
        |> insert(@TestKey(7))
        |> insert(@TestKey(1))
    sorted = to_list(heap)
    sorted == [@TestKey(1), @TestKey(3), @TestKey(5), @TestKey(7)]

expect # Merge two heaps
    heap1 = empty({})
        |> insert(@TestKey(5))
        |> insert(@TestKey(3))
    heap2 = empty({})
        |> insert(@TestKey(7))
        |> insert(@TestKey(1))
    merged = merge(heap1, heap2)
    peek(merged) == Ok(@TestKey(1)) && size(merged) == 4

expect # Large heap operations
    heap = List.range({ start: At 100, end: At 1 })
        |> List.walk(empty({}), |acc, i| insert(acc, @TestKey(i)))
    peek(heap) == Ok(@TestKey(1)) && size(heap) == 100

expect # Heap sort via to_list
    list = [@TestKey(9), @TestKey(3), @TestKey(7), @TestKey(1), @TestKey(5), @TestKey(2), @TestKey(8), @TestKey(4), @TestKey(6)]
    sorted = from_list(list) |> to_list
    sorted == [@TestKey(1), @TestKey(2), @TestKey(3), @TestKey(4), @TestKey(5), @TestKey(6), @TestKey(7), @TestKey(8), @TestKey(9)]

expect # Single element pop
    heap = empty({}) |> insert(@TestKey(42))
    when pop(heap) is
        Ok({ value, heap: new_heap }) ->
            value == @TestKey(42) && is_empty(new_heap)

        _ -> Bool.false

expect # Duplicate elements
    heap = empty({})
        |> insert(@TestKey(5))
        |> insert(@TestKey(3))
        |> insert(@TestKey(5))
        |> insert(@TestKey(3))
    sorted = to_list(heap)
    sorted == [@TestKey(3), @TestKey(3), @TestKey(5), @TestKey(5)]

expect # Empty heap peek
    heap : HeapBase TestKey
    heap = empty({})
    peek(heap) == Err(ListWasEmpty)

expect # Size after operations
    heap = empty({})
        |> insert(@TestKey(1))
        |> insert(@TestKey(2))
        |> insert(@TestKey(3))
    size_before = size(heap)
    result = pop(heap)
    size_after = when result is
        Ok({ heap: h }) -> size(h)
        _ -> 0
    size_before == 3 && size_after == 2
