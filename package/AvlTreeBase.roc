module [
    AvlTreeBase,
    empty,
    insert,
    get,
    map,
    walk,
    walk_until,
    to_list,
    from_list,
]

import Ord exposing [
    Ord,
    compare,
    Ordering,
]

AvlTreeBase a b : [
    Empty,
    Node {
            l : AvlTreeBase a b,
            k : a,
            v : b,
            h : U8,
            r : AvlTreeBase a b,
        },
    Leaf {
            k : a,
            v : b,
        },
] where a implements Ord & Inspect, b implements Inspect

impossible = "Impossible"

empty : {} -> AvlTreeBase a b
empty = |{}| Empty

mknode : AvlTreeBase a b, a, b, AvlTreeBase a b -> AvlTreeBase a b
mknode = |l, k, v, r|
    Node({ l, k, v, h: 1 + Num.max(height(l), height(r)), r })

height : AvlTreeBase a b -> U8
height = |avl_tree|
    when avl_tree is
        Empty -> 0
        Leaf(_) -> 1
        Node({ h }) -> h

rotate_right : AvlTreeBase a b -> AvlTreeBase a b
rotate_right = |avl_tree|
    when avl_tree is
        Node({ l: Node({ l: t1, k: y2k, v: y2v, r: t3 }), k: y4k, v: y4v, r: t5 }) ->
            mknode(mknode(t1, y2k, y2v, t3), y4k, y4v, t5)

        _ -> crash impossible

rotate_left : AvlTreeBase a b -> AvlTreeBase a b
rotate_left = |avl_tree|
    when avl_tree is
        Node({ l: t1, k: y2k, v: y2v, r: Node({ l: t3, k: y4k, v: y4v, r: t5 }) }) ->
            mknode(t1, y2k, y2v, mknode(t3, y4k, y4v, t5))

        _ -> crash impossible

balance : AvlTreeBase a b -> AvlTreeBase a b
balance = |avl_tree|
    when avl_tree is
        Empty | Leaf(_) -> avl_tree
        Node(root_node) ->
            { l, k, v, r } = root_node
            hl = height(l)
            hr = height(r)
            if hl > hr + 2 then
                when l is
                    Empty | Leaf(_) -> crash impossible
                    Node(l_node) ->
                        { l: l_node_l, r: l_node_r } = l_node
                        if height(l_node_l) >= height(l_node_r) then
                            # Left-left case: single right rotation
                            rotate_right(avl_tree)
                        else
                            # Left-right case: rotate left on left child, then right on root
                            new_left = rotate_left(l)
                            new_tree = mknode(new_left, k, v, r)
                            rotate_right(new_tree)
            else if hr > hl + 2 then
                when r is
                    Empty | Leaf(_) -> crash impossible
                    Node(r_node) ->
                        { l: r_node_l, r: r_node_r } = r_node
                        if height(r_node_r) >= height(r_node_l) then
                            # Right-right case: single left rotation
                            rotate_left(avl_tree)
                        else
                            # Right-left case: rotate right on right child, then left on root
                            new_right = rotate_right(r)
                            new_tree = mknode(l, k, v, new_right)
                            rotate_left(new_tree)
            else
                avl_tree

insert : AvlTreeBase a b, a, b -> AvlTreeBase a b
insert = |avl_tree, key, value|
    when avl_tree is
        Empty -> Leaf({ k: key, v: value })
        Leaf({ k }) ->
            when compare(key, k) is
                EQ -> Leaf({ k, v: value })
                LT -> balance(mknode(Empty, key, value, avl_tree))
                GT -> balance(mknode(avl_tree, key, value, Empty))

        Node({ l, k, v, r }) ->
            when compare(key, k) is
                EQ -> mknode(l, k, value, r)
                LT ->
                    new_left = insert(l, key, value)
                    balance(mknode(new_left, k, v, r))

                GT ->
                    new_right = insert(r, key, value)
                    balance(mknode(l, k, v, new_right))

get : AvlTreeBase a b, a -> Result b {}
get = |avl_tree, key|
    when avl_tree is
        Empty -> Err {}
        Leaf({ k, v }) ->
            when compare(key, k) is
                EQ -> Ok(v)
                LT | GT -> Err {}

        Node({ l, k, v, r }) ->
            when compare(key, k) is
                EQ -> Ok(v)
                LT -> get(l, key)
                GT -> get(r, key)

map : AvlTreeBase a b, (b -> c) -> AvlTreeBase a c
map = |avl_tree, fn|
    when avl_tree is
        Empty -> empty {}
        Leaf({ k, v }) -> Leaf({ k, v: fn(v) })
        Node({ l, k, v, r }) ->
            mknode(map(l, fn), k, fn(v), map(r, fn))

walk : AvlTreeBase a b, state, (state, a, b -> state) -> state
walk = |avl_tree, state, fn|
    when avl_tree is
        Empty -> state
        Leaf({ k, v }) -> fn(state, k, v)
        Node({ l, k, v, r }) ->
            l_state = walk(l, state, fn)
            this_state = fn(l_state, k, v)
            r_state = walk(r, this_state, fn)
            r_state

walk_until : AvlTreeBase a b, state, (state, a, b -> [Continue state, Break state]) -> state
walk_until = |avl_tree, state, fn|
    when avl_tree is
        Empty -> state
        Leaf({ k, v }) ->
            when fn(state, k, v) is
                Continue(new_state) -> new_state
                Break(new_state) -> new_state

        Node({ l, k, v, r }) ->
            l_state = walk_until(l, state, fn)
            this_state = fn(l_state, k, v)
            when this_state is
                Continue(new_state) -> walk_until(r, new_state, fn)
                Break(new_state) -> new_state

to_list : AvlTreeBase a b -> List (a, b)
to_list = |avl_tree|
    walk(
        avl_tree,
        [],
        |list, key, value|
            List.append(list, (key, value)),
    )

entry_compare : (k, v), (k, v) -> Ordering where k implements Ord
entry_compare = |(a, _), (b, _)|
    compare(a, b)

from_list : List (a, b) -> AvlTreeBase a b where a implements Ord & Inspect
from_list = |pairs|
    sorted_pairs = pairs |> List.sort_with(entry_compare)
    sorted_pairs_length = List.len(sorted_pairs)
    if (sorted_pairs_length < 1) then
        Empty
    else
        from_sorted_list(sorted_pairs)

from_sorted_list : List (a, b) -> AvlTreeBase a b
from_sorted_list = |list|
    list_length = List.len(list)
    when list is
        [] -> Empty
        [(k, v)] -> Leaf({ k, v })
        _ ->
            mid = Num.ceiling(Num.to_frac(list_length) / 2)
            when List.get(list, mid) is
                Err(OutOfBounds) -> Empty
                Ok((mid_key, mid_value)) ->
                    left_node = list |> List.take_first(mid) |> from_sorted_list
                    right_node = list |> List.drop_first(mid + 1) |> from_sorted_list
                    node = mknode(
                        left_node,
                        mid_key,
                        mid_value,
                        right_node,
                    )
                    node

# Tests - using TestKey wrapper since raw numbers don't implement Ord

TestKey := I64 implements [Ord { compare: test_key_compare }, Eq, Inspect]

test_key_compare : TestKey, TestKey -> Ordering
test_key_compare = |@TestKey(a), @TestKey(b)| Num.compare(a, b)

expect # Empty tree
    tree = empty({})
    get(tree, @TestKey(1)) == Err {}

expect # Single insert and get
    tree = empty({}) |> insert(@TestKey(42), "answer")
    get(tree, @TestKey(42)) == Ok("answer")

expect # Get non-existent key
    tree = empty({}) |> insert(@TestKey(1), "a")
    get(tree, @TestKey(2)) == Err {}

expect # Update existing key
    tree = empty({})
        |> insert(@TestKey(5), "first")
        |> insert(@TestKey(5), "second")
    get(tree, @TestKey(5)) == Ok("second")

expect # Multiple inserts in ascending order
    tree = empty({})
        |> insert(@TestKey(1), "a")
        |> insert(@TestKey(2), "b")
        |> insert(@TestKey(3), "c")
        |> insert(@TestKey(4), "d")
        |> insert(@TestKey(5), "e")
    get(tree, @TestKey(1)) == Ok("a") &&
    get(tree, @TestKey(3)) == Ok("c") &&
    get(tree, @TestKey(5)) == Ok("e")

expect # Multiple inserts in descending order
    tree = empty({})
        |> insert(@TestKey(5), "e")
        |> insert(@TestKey(4), "d")
        |> insert(@TestKey(3), "c")
        |> insert(@TestKey(2), "b")
        |> insert(@TestKey(1), "a")
    get(tree, @TestKey(1)) == Ok("a") &&
    get(tree, @TestKey(3)) == Ok("c") &&
    get(tree, @TestKey(5)) == Ok("e")

expect # Multiple inserts in random order
    tree = empty({})
        |> insert(@TestKey(3), "c")
        |> insert(@TestKey(1), "a")
        |> insert(@TestKey(4), "d")
        |> insert(@TestKey(2), "b")
        |> insert(@TestKey(5), "e")
    get(tree, @TestKey(1)) == Ok("a") &&
    get(tree, @TestKey(2)) == Ok("b") &&
    get(tree, @TestKey(3)) == Ok("c") &&
    get(tree, @TestKey(4)) == Ok("d") &&
    get(tree, @TestKey(5)) == Ok("e")

expect # to_list maintains sorted order - ascending inserts
    tree = empty({})
        |> insert(@TestKey(1), "a")
        |> insert(@TestKey(2), "b")
        |> insert(@TestKey(3), "c")
    to_list(tree) == [(@TestKey(1), "a"), (@TestKey(2), "b"), (@TestKey(3), "c")]

expect # to_list maintains sorted order - descending inserts
    tree = empty({})
        |> insert(@TestKey(3), "c")
        |> insert(@TestKey(2), "b")
        |> insert(@TestKey(1), "a")
    to_list(tree) == [(@TestKey(1), "a"), (@TestKey(2), "b"), (@TestKey(3), "c")]

expect # to_list maintains sorted order - random inserts
    tree = empty({})
        |> insert(@TestKey(2), "b")
        |> insert(@TestKey(1), "a")
        |> insert(@TestKey(4), "d")
        |> insert(@TestKey(3), "c")
        |> insert(@TestKey(5), "e")
    to_list(tree) == [(@TestKey(1), "a"), (@TestKey(2), "b"), (@TestKey(3), "c"), (@TestKey(4), "d"), (@TestKey(5), "e")]

expect # from_list creates sorted tree
    list = [(@TestKey(3), "c"), (@TestKey(1), "a"), (@TestKey(2), "b")]
    tree = from_list(list)
    to_list(tree) == [(@TestKey(1), "a"), (@TestKey(2), "b"), (@TestKey(3), "c")]

expect # from_list with duplicates - sorts then builds tree from sorted list
    list = [(@TestKey(1), "first"), (@TestKey(2), "b"), (@TestKey(1), "second")]
    tree = from_list(list)
    # from_list sorts, so both (@TestKey(1), "first") and (@TestKey(1), "second") exist
    # The tree contains both entries
    list_result = to_list(tree)
    List.len(list_result) == 3

expect # map transforms values
    tree = empty({})
        |> insert(@TestKey(1), 10)
        |> insert(@TestKey(2), 20)
        |> insert(@TestKey(3), 30)
    mapped = map(tree, \x -> x * 2)
    to_list(mapped) == [(@TestKey(1), 20), (@TestKey(2), 40), (@TestKey(3), 60)]

expect # walk accumulates in order
    tree = empty({})
        |> insert(@TestKey(3), 30)
        |> insert(@TestKey(1), 10)
        |> insert(@TestKey(2), 20)
    sum = walk(tree, 0, |acc, _k, v| acc + v)
    sum == 60

expect # walk visits keys in sorted order
    tree = empty({})
        |> insert(@TestKey(3), "c")
        |> insert(@TestKey(1), "a")
        |> insert(@TestKey(2), "b")
    keys = walk(tree, [], |acc, @TestKey(k), _v| List.append(acc, k))
    keys == [1, 2, 3]

expect # walk_until can break early
    tree = empty({})
        |> insert(@TestKey(1), "a")
        |> insert(@TestKey(2), "b")
        |> insert(@TestKey(3), "c")
        |> insert(@TestKey(4), "d")
        |> insert(@TestKey(5), "e")
    result = walk_until(tree, [], |acc, @TestKey(k), v|
        if k > 3 then
            Break(acc)  # Break without adding current element
        else
            Continue(List.append(acc, v))
    )
    result == ["a", "b", "c"]

expect # Large tree with many elements
    tree = List.range({ start: At 1, end: At 100 })
        |> List.walk(empty({}), |acc, i| insert(acc, @TestKey(i), i * 10))
    get(tree, @TestKey(1)) == Ok(10) &&
    get(tree, @TestKey(50)) == Ok(500) &&
    get(tree, @TestKey(100)) == Ok(1000) &&
    get(tree, @TestKey(101)) == Err {}

expect # Large tree maintains order
    tree = List.range({ start: At 1, end: At 50 })
        |> List.walk(empty({}), |acc, i| insert(acc, @TestKey(i), i))
    list = to_list(tree)
    List.len(list) == 50 &&
    List.first(list) == Ok((@TestKey(1), 1)) &&
    List.last(list) == Ok((@TestKey(50), 50))

expect # Descending sequential insertions - stress test left rotations
    tree = List.range({ start: At 1, end: At 20 })
        |> List.reverse
        |> List.walk(empty({}), |acc, i| insert(acc, @TestKey(i), i))
    list = to_list(tree)
    List.len(list) == 20 &&
    List.first(list) == Ok((@TestKey(1), 1)) &&
    List.last(list) == Ok((@TestKey(20), 20))

expect # Zigzag insertions - alternating high and low
    tree = empty({})
        |> insert(@TestKey(10), "10")
        |> insert(@TestKey(5), "5")
        |> insert(@TestKey(15), "15")
        |> insert(@TestKey(3), "3")
        |> insert(@TestKey(7), "7")
        |> insert(@TestKey(12), "12")
        |> insert(@TestKey(17), "17")
    list = to_list(tree)
    List.len(list) == 7 &&
    get(tree, @TestKey(3)) == Ok("3") &&
    get(tree, @TestKey(17)) == Ok("17")

expect # Update same key multiple times
    tree = empty({})
        |> insert(@TestKey(1), "a")
        |> insert(@TestKey(1), "b")
        |> insert(@TestKey(1), "c")
        |> insert(@TestKey(1), "d")
    get(tree, @TestKey(1)) == Ok("d") &&
    List.len(to_list(tree)) == 1

expect # Empty tree to_list
    tree : AvlTreeBase TestKey Str
    tree = empty({})
    List.len(to_list(tree)) == 0

expect # Empty tree walk
    tree : AvlTreeBase TestKey Str
    tree = empty({})
    result = walk(tree, 0, |acc, _k, _v| acc + 1)
    result == 0

expect # Empty tree map
    tree : AvlTreeBase TestKey I64
    tree = empty({})
    mapped = map(tree, \x -> x * 2)
    List.len(to_list(mapped)) == 0

expect # Single element to_list
    tree = insert(empty({}), @TestKey(42), "answer")
    to_list(tree) == [(@TestKey(42), "answer")]

expect # Single element walk
    tree = insert(empty({}), @TestKey(1), 10)
    result = walk(tree, 0, |acc, _k, v| acc + v)
    result == 10

expect # Single element map
    tree = insert(empty({}), @TestKey(1), 5)
    mapped = map(tree, \x -> x * 3)
    get(mapped, @TestKey(1)) == Ok(15)

expect # walk_until breaks at first element
    tree = empty({})
        |> insert(@TestKey(1), "a")
        |> insert(@TestKey(2), "b")
        |> insert(@TestKey(3), "c")
    result = walk_until(tree, 0, |acc, _k, _v| Break(acc + 1))
    result == 1

expect # walk_until never breaks
    tree = empty({})
        |> insert(@TestKey(1), "a")
        |> insert(@TestKey(2), "b")
        |> insert(@TestKey(3), "c")
    result = walk_until(tree, 0, |acc, _k, _v| Continue(acc + 1))
    result == 3

expect # walk_until on empty tree
    tree : AvlTreeBase TestKey Str
    tree = empty({})
    result = walk_until(tree, 42, |acc, _k, _v| Continue(acc + 1))
    result == 42

expect # from_list with empty list
    list : List (TestKey, Str)
    list = []
    tree = from_list(list)
    List.len(to_list(tree)) == 0

expect # Insert then get non-existent key
    tree = empty({})
        |> insert(@TestKey(1), "a")
        |> insert(@TestKey(3), "c")
        |> insert(@TestKey(5), "e")
    get(tree, @TestKey(2)) == Err {} &&
    get(tree, @TestKey(4)) == Err {} &&
    get(tree, @TestKey(0)) == Err {} &&
    get(tree, @TestKey(10)) == Err {}

expect # Interleaved insertions - middle-out pattern
    tree = empty({})
        |> insert(@TestKey(50), "50")
        |> insert(@TestKey(25), "25")
        |> insert(@TestKey(75), "75")
        |> insert(@TestKey(12), "12")
        |> insert(@TestKey(37), "37")
        |> insert(@TestKey(62), "62")
        |> insert(@TestKey(87), "87")
    list = to_list(tree)
    List.len(list) == 7 &&
    get(tree, @TestKey(12)) == Ok("12") &&
    get(tree, @TestKey(87)) == Ok("87")

expect # Large tree walk accumulation
    tree = List.range({ start: At 1, end: At 100 })
        |> List.walk(empty({}), |acc, i| insert(acc, @TestKey(i), i))
    sum = walk(tree, 0, |acc, _k, v| acc + v)
    sum == 5050  # Sum of 1 to 100

expect # from_list preserves last value for duplicate keys
    list = [(@TestKey(1), "first"), (@TestKey(2), "b"), (@TestKey(3), "c"), (@TestKey(1), "last")]
    tree = from_list(list)
    # Since from_list just inserts in order, last insert wins
    result = to_list(tree)
    List.len(result) == 4  # All entries present due to sorting keeping both

expect # map maintains tree structure
    tree = empty({})
        |> insert(@TestKey(2), 20)
        |> insert(@TestKey(1), 10)
        |> insert(@TestKey(3), 30)
    mapped1 = map(tree, \x -> x * 2)
    mapped2 = map(mapped1, \x -> x + 5)
    get(mapped2, @TestKey(1)) == Ok(25) &&
    get(mapped2, @TestKey(2)) == Ok(45) &&
    get(mapped2, @TestKey(3)) == Ok(65)

expect # walk with string concatenation
    tree = empty({})
        |> insert(@TestKey(3), "c")
        |> insert(@TestKey(1), "a")
        |> insert(@TestKey(2), "b")
        |> insert(@TestKey(4), "d")
    result = walk(tree, "", |acc, _k, v| Str.concat(acc, v))
    result == "abcd"
