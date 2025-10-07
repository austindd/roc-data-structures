module [
    RbTreeNum,
    empty,
    insert,
    get,
    map,
    walk,
    walk_until,
    to_list,
    from_list,
]

import RbTreeBase exposing [RbTreeBase]
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

RbTreeNum k v := RbTreeBase (NumKey k) v implements [
        Eq,
        Hash,
        Inspect,
    ]


empty : {} -> RbTreeNum k v
empty = |{}| RbTreeBase.empty({}) |> @RbTreeNum

get : RbTreeNum k v, Num k -> Result v {}
get = |@RbTreeNum(tree), key|
    RbTreeBase.get(tree, @NumKey(key))

insert : RbTreeNum k v, Num k, v -> RbTreeNum k v
insert = |@RbTreeNum(tree), key, value|
    RbTreeBase.insert(@NumKey(key), value, tree) |> @RbTreeNum

map : RbTreeNum k v, (v -> w) -> RbTreeNum k w
map = |@RbTreeNum(tree), fn|
    RbTreeBase.map(tree, fn) |> @RbTreeNum

walk : RbTreeNum k v, state, (state, Num k, v -> state) -> state
walk = |@RbTreeNum(tree), state, fn|
    RbTreeBase.walk(
        tree,
        state,
        |s, @NumKey(k), v| fn(s, k, v),
    )

walk_until : RbTreeNum k v, state, (state, Num k, v -> [Continue state, Break state]) -> state
walk_until = |@RbTreeNum(tree), state, fn|
    RbTreeBase.walk_until(
        tree,
        state,
        |s, @NumKey(k), v| fn(s, k, v),
    )

to_list : RbTreeNum k v -> List (Num k, v)
to_list = |@RbTreeNum(tree)|
    RbTreeBase.walk(
        tree,
        [],
        |list, @NumKey(key), value|
            List.append(list, (key, value)),
    )

from_list : List (Num k, v) -> RbTreeNum k v
from_list = |list|
    new_list = List.map(list, |(k, v)| (@NumKey(k), v))
    rb_tree = RbTreeBase.from_list(new_list)
    @RbTreeNum(rb_tree)


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
