module [
    BTreeNum,
    empty,
    insert,
    get,
    map,
    walk,
    walk_until,
    to_list,
    from_list,
]

import BTreeBase exposing [BTreeBase]
import Ord exposing [Ord, Ordering]

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

BTreeNum k v := BTreeBase (NumKey k) v implements [
        Eq,
    ]

empty : {} -> BTreeNum k v
empty = |{}| BTreeBase.empty {} |> @BTreeNum

get : BTreeNum k v, Num k -> Result v [NotFound]
get = |@BTreeNum(tree), key|
    BTreeBase.get(tree, @NumKey(key))

insert : BTreeNum k v, Num k, v -> BTreeNum k v
insert = |@BTreeNum(tree), key, value|
    BTreeBase.insert(@NumKey(key), value, tree) |> @BTreeNum

map : BTreeNum k v, (v -> w) -> BTreeNum k w
map = |@BTreeNum(tree), fn|
    BTreeBase.map(tree, fn) |> @BTreeNum

walk : BTreeNum k v, state, (state, Num k, v -> state) -> state
walk = |@BTreeNum(tree), state, fn|
    BTreeBase.walk(
        tree,
        state,
        |s, @NumKey(k), v| fn(s, k, v),
    )

walk_until : BTreeNum k v, state, (state, Num k, v -> [Continue state, Break state]) -> state
walk_until = |@BTreeNum(tree), state, fn|
    BTreeBase.walk_until(
        tree,
        state,
        |s, @NumKey(k), v| fn(s, k, v),
    )

to_list : BTreeNum k v -> List (Num k, v)
to_list = |@BTreeNum(tree)|
    BTreeBase.walk(
        tree,
        [],
        |list, @NumKey(key), value|
            List.append(list, (key, value)),
    )

from_list : List (Num k, v) -> BTreeNum k v
from_list = |list|
    new_list = List.map(list, |(k, v)| (@NumKey(k), v))
    btree = BTreeBase.from_list(new_list)
    @BTreeNum(btree)

# --- Tests ---

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
        empty {}
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
        empty {}
        |> insert(1, "1")
        |> insert(2, "2")
        |> insert(3, "3")
        |> insert(4, "4")
        |> insert(5, "5")
        |> insert(6, "6")
        |> to_list
    tree_a == tree_b

expect # Large tree test
    tree = List.range({ start: At 1, end: At 100 })
        |> List.walk(empty {}, |acc, i| insert(acc, i, i * 10))
    get(tree, 1) == Ok(10) &&
    get(tree, 50) == Ok(500) &&
    get(tree, 100) == Ok(1000)

expect # Get non-existent key
    tree = empty {}
        |> insert(5, "five")
    get(tree, 10) == Err NotFound

expect # Update existing key
    tree = empty {}
        |> insert(1, "first")
        |> insert(1, "updated")
    get(tree, 1) == Ok("updated")

expect # Walk accumulates correctly
    tree = empty {}
        |> insert(1, 10)
        |> insert(2, 20)
        |> insert(3, 30)
    sum = walk(tree, 0, |acc, _k, v| acc + v)
    sum == 60

expect # Map transforms values
    tree = empty {}
        |> insert(1, 5)
        |> insert(2, 10)
    mapped = map(tree, \x -> x * 2)
    get(mapped, 1) == Ok(10) &&
    get(mapped, 2) == Ok(20)

expect # walk_until can break early
    tree = empty {}
        |> insert(1, 1)
        |> insert(2, 2)
        |> insert(3, 3)
        |> insert(4, 4)
    result = walk_until(tree, 0, |acc, _k, v|
        if v > 2 then
            Break(acc)
        else
            Continue(acc + v)
    )
    result == 3

expect # Descending insertions
    tree = List.range({ start: At 1, end: At 30 })
        |> List.reverse
        |> List.walk(empty {}, |acc, i| insert(acc, i, Num.to_str(i)))
    list = to_list(tree)
    List.len(list) == 30
