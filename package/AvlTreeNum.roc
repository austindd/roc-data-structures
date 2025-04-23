module [
    AvlTreeNum,
    empty,
    insert,
    get,
    map,
    walk,
    to_list,
    from_list,
]

import AvlTreeBase exposing [AvlTreeBase]
import Ord exposing [Ord, compare, Ordering]

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
