module [
    AvlTreeNum,
    empty,
    insert,
    get,
    map,
    walk,
    walk_until,
    to_list,
    from_list,
]

import AvlTreeBase exposing [AvlTreeBase]
import Ord exposing [Ord, Ordering]

debug_avltreenum : AvlTreeNum a b -> Inspector _ where a implements Ord & Inspect
debug_avltreenum = |@AvlTreeNum(value)|
    Inspect.to_inspector(value)

num_compare : NumKey a, NumKey a -> Ordering where a implements Ord
num_compare = |@NumKey(a), @NumKey(b)|
    Num.compare(a, b)

NumKey a := Num a implements [
        Ord {
            compare: num_compare,
        },
        Eq,
        Hash,
        Inspect,
    ]

AvlTreeNum k v := AvlTreeBase (NumKey k) v implements [
        Eq,
        Hash,
        Inspect {
            to_inspector: debug_avltreenum,
        },
    ]

empty : {} -> AvlTreeNum k v
empty = |{}| AvlTreeBase.empty({}) |> @AvlTreeNum

get : AvlTreeNum k v, Num k -> Result v {}
get = |@AvlTreeNum(tree), key|
    AvlTreeBase.get(tree, @NumKey(key))

insert : AvlTreeNum k v, Num k, v -> AvlTreeNum k v
insert = |@AvlTreeNum(tree), key, value|
    AvlTreeBase.insert(tree, @NumKey(key), value) |> @AvlTreeNum

map : AvlTreeNum k v, (v -> w) -> AvlTreeNum k w
map = |@AvlTreeNum(tree), fn|
    AvlTreeBase.map(tree, fn) |> @AvlTreeNum

walk : AvlTreeNum k v, state, (state, Num k, v -> state) -> state
walk = |@AvlTreeNum(tree), state, fn|
    AvlTreeBase.walk(
        tree,
        state,
        |s, @NumKey(k), v| fn(s, k, v),
    )

walk_until : AvlTreeNum k v, state, (state, Num k, v -> [Continue state, Break state]) -> state
walk_until = |@AvlTreeNum(tree), state, fn|
    AvlTreeBase.walk_until(
        tree,
        state,
        |s, @NumKey(k), v| fn(s, k, v),
    )

to_list : AvlTreeNum k v -> List (Num k, v)
to_list = |@AvlTreeNum(tree)|
    AvlTreeBase.walk(
        tree,
        [],
        |list, @NumKey(key), value|
            List.append(list, (key, value)),
    )

from_list : List (Num k, v) -> AvlTreeNum k v
from_list = |list|
    new_list = List.map(list, |(k, v)| (@NumKey(k), v))
    avl_tree = AvlTreeBase.from_list(new_list)
    @AvlTreeNum(avl_tree)

expect
    tree_a =
        from_list(
            [
                (1, "1"),
                (2, "2"),
                (3, "3"),
            ],
        )
        |> to_list
    tree_b =
        empty({})
        |> insert(1, "1")
        |> insert(2, "2")
        |> insert(3, "3")
        |> to_list
    tree_a == tree_b

expect
    tree_a =
        from_list(
            [
                (1, "1"),
                (2, "2"),
                (3, "3"),
                (4, "4"),
                (5, "5"),
                (6, "6"),
            ],
        )
        |> to_list
    tree_b =
        empty({})
        |> insert(1, "1")
        |> insert(2, "2")
        |> insert(3, "3")
        |> insert(4, "4")
        |> insert(5, "5")
        |> insert(6, "6")
        |> to_list
    tree_a == tree_b

expect
    tree_a =
        from_list(
            [
                (1, "1"),
                (2, "2"),
                (3, "3"),
                (4, "4"),
                (5, "5"),
                (6, "6"),
                (7, "7"),
            ],
        )
        |> to_list
    tree_b =
        empty({})
        |> insert(1, "1")
        |> insert(2, "2")
        |> insert(3, "3")
        |> insert(4, "4")
        |> insert(5, "5")
        |> insert(6, "6")
        |> insert(7, "7")
        |> to_list
    tree_a == tree_b

expect # Large number keys
    tree = empty({})
        |> insert(1000000, "million")
        |> insert(2000000, "two million")
        |> insert(500000, "half million")
    get(tree, 1000000) == Ok("million") &&
    get(tree, 2000000) == Ok("two million") &&
    get(tree, 500000) == Ok("half million")

expect # Negative number keys
    tree = empty({})
        |> insert(-5, "neg five")
        |> insert(-1, "neg one")
        |> insert(0, "zero")
        |> insert(1, "one")
        |> insert(5, "five")
    list = to_list(tree)
    List.len(list) == 5 &&
    get(tree, -5) == Ok("neg five") &&
    get(tree, 5) == Ok("five")

expect # Mixed positive and negative keys ordered correctly
    tree = empty({})
        |> insert(3, "c")
        |> insert(-2, "b")
        |> insert(-5, "a")
        |> insert(0, "d")
        |> insert(7, "e")
    result = walk(tree, [], |acc, k, _v| List.append(acc, k))
    result == [-5, -2, 0, 3, 7]

# expect # Floating point keys (U64 specific)
#     tree : AvlTreeNum U64 Str
#     tree = empty({})
#         |> insert(100, "hundred")
#         |> insert(50, "fifty")
#         |> insert(200, "two hundred")
#     get(tree, 100) == Ok("hundred")

# expect # U8 keys - boundary values
#     tree : AvlTreeNum U8 Str
#     tree = empty({})
#         |> insert(0, "zero")
#         |> insert(255, "max")
#         |> insert(128, "mid")
#     get(tree, 0) == Ok("zero") &&
#     get(tree, 255) == Ok("max") &&
#     get(tree, 128) == Ok("mid")

expect # Sequential numeric insertions
    tree = List.range({ start: At 1, end: At 100 })
        |> List.walk(empty({}), |acc, i| insert(acc, i, i * i))
    get(tree, 1) == Ok(1) &&
    get(tree, 10) == Ok(100) &&
    get(tree, 100) == Ok(10000)

expect # Reverse sequential numeric insertions
    tree = List.range({ start: At 1, end: At 50 })
        |> List.reverse
        |> List.walk(empty({}), |acc, i| insert(acc, i, Num.to_str(i)))
    list = to_list(tree)
    List.len(list) == 50 &&
    List.first(list) == Ok((1, "1"))

expect # walk_until with numeric accumulation
    tree = empty({})
        |> insert(10, 1)
        |> insert(20, 2)
        |> insert(30, 3)
        |> insert(40, 4)
    result = walk_until(tree, 0, |acc, k, v|
        if k >= 30 then
            Break(acc)
        else
            Continue(acc + v)
    )
    result == 3

expect # map with numeric transformation
    tree = empty({})
        |> insert(1, 10)
        |> insert(2, 20)
        |> insert(3, 30)
    doubled = map(tree, \x -> x * 2)
    sum = walk(doubled, 0, |acc, _k, v| acc + v)
    sum == 120

# expect # Empty tree operations
#     tree : AvlTreeNum I64 Str
#     tree = empty({})
#     to_list(tree) == [] &&
#     get(tree, 1) == Err {}

expect # Single element numeric tree
    tree = empty({}) |> insert(42, "answer")
    to_list(tree) == [(42, "answer")] &&
    get(tree, 42) == Ok("answer")

expect # Update numeric key multiple times
    tree = empty({})
        |> insert(100, 1)
        |> insert(100, 2)
        |> insert(100, 3)
    get(tree, 100) == Ok(3) &&
    List.len(to_list(tree)) == 1

expect # Large tree numeric walk
    tree = List.range({ start: At 1, end: At 200 })
        |> List.walk(empty({}), |acc, i| insert(acc, i, i))
    sum = walk(tree, 0, |acc, _k, v| acc + v)
    sum == 20100  # Sum of 1 to 200

expect # from_list with numeric keys
    pairs = List.range({ start: At 1, end: At 30 })
        |> List.map(\i -> (i, i * 2))
    tree = from_list(pairs)
    get(tree, 15) == Ok(30) &&
    List.len(to_list(tree)) == 30

expect # Sparse numeric keys
    tree = empty({})
        |> insert(1, "a")
        |> insert(100, "b")
        |> insert(10000, "c")
        |> insert(1000000, "d")
    get(tree, 1) == Ok("a") &&
    get(tree, 1000000) == Ok("d") &&
    get(tree, 50) == Err {}

expect # Zero as key
    tree = empty({})
        |> insert(0, "zero")
        |> insert(-1, "neg")
        |> insert(1, "pos")
    get(tree, 0) == Ok("zero")

expect # walk with key transformation
    tree = empty({})
        |> insert(1, "a")
        |> insert(2, "b")
        |> insert(3, "c")
    keys_doubled = walk(tree, [], |acc, k, _v| List.append(acc, k * 2))
    keys_doubled == [2, 4, 6]

# expect # I32 specific range
#     tree : AvlTreeNum I32 Str
#     tree = empty({})
#         |> insert(2147483647, "max")  # Max I32
#         |> insert(-2147483648, "min")  # Min I32
#         |> insert(0, "zero")
#     List.len(to_list(tree)) == 3
