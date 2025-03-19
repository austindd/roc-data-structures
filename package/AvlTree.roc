module [
    AvlTree,
    empty,
]

import AvlTreeBase exposing [AvlTreeBase]
import Ord exposing [Ord, Ordering, compare]

AvlTree a b := AvlTreeBase a b where a implements Ord

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
