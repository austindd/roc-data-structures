module [
    AvlTree,
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

AvlTree a b := AvlTreeBase a b where a implements Ord
    implements [
        Eq { is_eq: avl_tree_eq },
    ]

avl_tree_eq : AvlTree a b, AvlTree a b -> Bool where a implements Eq & Ord & Inspect, b implements Eq & Inspect
avl_tree_eq = |@AvlTree(tree_a), @AvlTree(tree_b)|
    AvlTreeBase.to_list(tree_a) == AvlTreeBase.to_list(tree_b)

Key a := Num a implements [
        Eq,
        Ord {
            compare: key_compare,
        },
    ]

key_compare : Key a, Key a -> Ordering
key_compare = |@Key(a), @Key(b)|
    Num.compare(a, b)

expect
    tree_1 =
        empty({})
        |> insert(@Key(1), "1")
        |> insert(@Key(2), "2")
        |> insert(@Key(3), "3")
        |> insert(@Key(4), "4")
        |> insert(@Key(5), "5")
    tree_2 = from_list(
        [
            (@Key(1), "1"),
            (@Key(2), "2"),
            (@Key(3), "3"),
            (@Key(4), "4"),
            (@Key(5), "5"),
        ],
    )
    tree_1 == tree_2

expect # Empty tree operations
    tree : AvlTree (Key I64) Str
    tree = empty({})
    get(tree, @Key(1)) == Err {}

expect # Single element
    tree = empty({})
        |> insert(@Key(100), "hundred")
    get(tree, @Key(100)) == Ok("hundred")

expect # Multiple elements with custom Ord
    tree = empty({})
        |> insert(@Key(50), "fifty")
        |> insert(@Key(25), "twenty-five")
        |> insert(@Key(75), "seventy-five")
        |> insert(@Key(10), "ten")
    get(tree, @Key(25)) == Ok("twenty-five") &&
    get(tree, @Key(75)) == Ok("seventy-five") &&
    get(tree, @Key(10)) == Ok("ten")

expect # Ordered enumeration with custom keys
    tree = empty({})
        |> insert(@Key(3), "c")
        |> insert(@Key(1), "a")
        |> insert(@Key(2), "b")
    to_list(tree) == [(@Key(1), "a"), (@Key(2), "b"), (@Key(3), "c")]

expect # Map operation preserves structure
    tree = empty({})
        |> insert(@Key(1), 10)
        |> insert(@Key(2), 20)
    mapped = map(tree, \x -> x * 2)
    to_list(mapped) == [(@Key(1), 20), (@Key(2), 40)]

expect # Walk accumulates correctly
    tree = empty({})
        |> insert(@Key(1), 5)
        |> insert(@Key(2), 10)
        |> insert(@Key(3), 15)
    sum = walk(tree, 0, |acc, _k, v| acc + v)
    sum == 30

expect # Walk_until can terminate early
    tree = empty({})
        |> insert(@Key(1), "a")
        |> insert(@Key(2), "b")
        |> insert(@Key(3), "c")
        |> insert(@Key(4), "d")
        |> insert(@Key(5), "e")
    result = walk_until(tree, 0, |count, k, _v|
        if k == @Key(4) then
            Break(count)  # Stop before processing key 4
        else
            Continue(count + 1)
    )
    result == 3

expect # Equality works correctly
    tree1 = empty({})
        |> insert(@Key(1), "a")
        |> insert(@Key(2), "b")
    tree2 = empty({})
        |> insert(@Key(2), "b")
        |> insert(@Key(1), "a")
    tree3 = empty({})
        |> insert(@Key(1), "x")
        |> insert(@Key(2), "b")
    tree1 == tree2 && tree1 != tree3

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

walk_until : AvlTree a b, state, (state, a, b -> [Continue state, Break state]) -> state
walk_until = |@AvlTree(tree), state, fn|
    AvlTreeBase.walk_until(tree, state, fn)

to_list : AvlTree a b -> List (a, b)
to_list = |@AvlTree(avl_tree)|
    AvlTreeBase.to_list(avl_tree)

from_list : List (a, b) -> AvlTree a b
from_list = |list|
    AvlTreeBase.from_list(list) |> @AvlTree
