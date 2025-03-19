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
    AvlTreeBase.get(tree, key) |> @AvlTree
