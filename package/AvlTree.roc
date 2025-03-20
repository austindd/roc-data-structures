module [
    AvlTree,
    empty,
]

import AvlTreeBase exposing [AvlTreeBase]
import Ord exposing [Ord, Ordering, compare]

AvlTree a b := AvlTreeBase a b where a implements Ord implements [
    Eq {
        is_eq : avl_tree_eq
    }
]

avl_tree_eq : AvlTree a b, AvlTree a b -> Bool where a implements Eq & Ord & Inspect, b implements Eq & Inspect
avl_tree_eq = |@AvlTree(tree_a), @AvlTree(tree_b)|
    AvlTreeBase.to_list(tree_a) == AvlTreeBase.to_list(tree_b)

Key a := Num a implements [
    Eq,
    Ord {
        compare: key_compare
    },
]

key_compare : Key a, Key a -> Ordering
key_compare = |@Key(a), @Key(b)|
    Num.compare(a, b)

expect
    tree_1 = empty({})
        |> insert(@Key(1), "1")
        |> insert(@Key(2), "2")
        |> insert(@Key(3), "3")
        |> insert(@Key(4), "4")
        |> insert(@Key(5), "5")
    tree_2 = from_list([
            (@Key(1), "1"),
            (@Key(2), "2"),
            (@Key(3), "3"),
            (@Key(4), "4"),
            (@Key(5), "5"),
        ])
    tree_1 == tree_2

empty : {} -> AvlTree a b
empty = |{}| AvlTreeBase.empty({}) |> @AvlTree

insert : AvlTree a b, a, b -> AvlTree a b
insert = |@AvlTree(tree), key, value|
    AvlTreeBase.insert(tree, key, value) |> @AvlTree

get : AvlTree a b, a -> Result b {}
get = |@AvlTree(tree), key|
    AvlTreeBase.get(tree, key)

map : AvlTree a b, (b -> c) -> AvlTree a c
map = |@AvlTree(tree), fn|
    AvlTreeBase.map(tree, fn) |> @AvlTree

walk : AvlTree a b, state, (state, a, b -> state) -> state
walk = |@AvlTree(tree), state, fn|
    AvlTreeBase.walk(tree, state, fn)

walk_until : AvlTree a b, state, (state, a, b -> [Continue(state), Break(state)]) -> state
walk_until = |@AvlTree(tree), state, fn|
    AvlTreeBase.walk_until(tree, state, fn)

to_list : AvlTree a b -> List (a, b)
to_list = |@AvlTree(avl_tree)|
    AvlTreeBase.to_list(avl_tree)

from_list : List (a, b) -> AvlTree a b
from_list = |list|
    AvlTreeBase.from_list(list) |> @AvlTree
