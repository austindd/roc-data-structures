module [
    AvlTree,
]

import AvlTreeBase exposing [AvlTreeBase]
import Ord exposing [Ord, Ordering, compare]

AvlTree a b := AvlTreeBase a b where a implements Ord
