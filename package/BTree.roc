module [
    BTree,
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
import Ord exposing [Ord]

## A Map implemented using a 2-3 tree (a type of B-tree).
## 2-3 trees are self-balancing search trees where:
## - Each internal node has either 2 or 3 children
## - All leaves are at the same depth
## - Operations maintain O(log n) time complexity
##
## Keys (`k`) must implement the `Ord` ability for comparison and ordering.
## Keys (`k`) and Values (`v`) must implement `Inspect` for debugging/display.
BTree k v := BTreeBase k v where k implements Ord
    implements [
        Eq { is_eq: btree_eq },
    ]

btree_eq : BTree k v, BTree k v -> Bool where k implements Eq & Ord & Inspect, v implements Eq & Inspect
btree_eq = |@BTree(tree_a), @BTree(tree_b)|
    BTreeBase.to_list(tree_a) == BTreeBase.to_list(tree_b)

## Creates an empty `BTree`.
empty : {} -> BTree k v
empty = |{}|
    BTreeBase.empty {} |> @BTree

## Inserts a key-value pair into the `BTree`.
## If the key already exists, its associated value is updated to the new value.
## Returns the modified tree.
insert : BTree k v, k, v -> BTree k v
insert = |@BTree(tree), key, value|
    BTreeBase.insert(key, value, tree) |> @BTree

## Retrieves the value associated with the given key.
## Returns `Ok value` if the key is found, otherwise returns `Err NotFound`.
get : BTree k v, k -> Result v [NotFound]
get = |@BTree(tree), key|
    BTreeBase.get(tree, key)

## Transforms the values in the `BTree` using a given function `fn`,
## while keeping the keys the same.
## Returns a new tree with the transformed values.
map : BTree k v, (v -> w) -> BTree k w
map = |@BTree(tree), fn|
    BTreeBase.map(tree, fn) |> @BTree

## Iterates through the key-value pairs of the `BTree` in ascending key order,
## applying the function `fn` to accumulate a `state`.
## `fn` takes the current state, key, and value, and returns the next state.
walk : BTree k v, state, (state, k, v -> state) -> state
walk = |@BTree(tree), state, fn|
    BTreeBase.walk(tree, state, fn)

## Iterates through the key-value pairs like `walk`, but allows early termination.
## `fn` returns `Continue state` to proceed or `Break state` to stop and return the final state.
walk_until : BTree k v, state, (state, k, v -> [Continue state, Break state]) -> state
walk_until = |@BTree(tree), state, fn|
    BTreeBase.walk_until(tree, state, fn)

## Converts the `BTree` into a `List` of (key, value) pairs, sorted by key.
to_list : BTree k v -> List (k, v)
to_list = |@BTree(tree)|
    BTreeBase.to_list(tree)

## Creates a `BTree` from a `List` of (key, value) pairs.
## If duplicate keys exist in the list, the value associated with the last occurrence
## of the key in the list will be the one stored in the tree.
from_list : List (k, v) -> BTree k v where k implements Ord
from_list = |list|
    BTreeBase.from_list(list) |> @BTree

# --- Tests ---

import Ord exposing [Ordering]

WrapperTestKey a := Num a implements [
        Eq,
        Ord {
            compare: wrapper_test_key_compare,
        },
        Inspect,
    ]

wrapper_test_key_compare : WrapperTestKey a, WrapperTestKey a -> Ordering
wrapper_test_key_compare = |@WrapperTestKey(a), @WrapperTestKey(b)|
    Num.compare(a, b)

expect # Empty tree get returns error
    tree : BTree (WrapperTestKey I64) Str
    tree = empty {}
    get(tree, @WrapperTestKey(1)) |> Result.is_err

expect # Single insert and get works
    tree = empty {} |> insert(@WrapperTestKey(10), "ten")
    get(tree, @WrapperTestKey(10)) |> Result.is_ok

expect # Multiple inserts work
    tree = empty {}
        |> insert(@WrapperTestKey(3), "c")
        |> insert(@WrapperTestKey(1), "a")
        |> insert(@WrapperTestKey(2), "b")
    when (get(tree, @WrapperTestKey(1)), get(tree, @WrapperTestKey(2)), get(tree, @WrapperTestKey(3))) is
        (Ok(_), Ok(_), Ok(_)) -> Bool.true
        _ -> Bool.false

expect # Map transforms values
    tree = empty {}
        |> insert(@WrapperTestKey(1), 5)
        |> insert(@WrapperTestKey(2), 10)
    mapped : BTree (WrapperTestKey I64) I64
    mapped = map(tree, \x -> x * 2)
    when get(mapped, @WrapperTestKey(1)) is
        Ok(10) -> Bool.true
        _ -> Bool.false

expect # Walk accumulates correctly
    tree = empty {}
        |> insert(@WrapperTestKey(1), 10)
        |> insert(@WrapperTestKey(2), 20)
        |> insert(@WrapperTestKey(3), 30)
    sum = walk(tree, 0, |acc, _k, v| acc + v)
    sum == 60

expect # walk_until can break early
    tree = empty {}
        |> insert(@WrapperTestKey(1), 1)
        |> insert(@WrapperTestKey(2), 2)
        |> insert(@WrapperTestKey(3), 3)
        |> insert(@WrapperTestKey(4), 4)
    result = walk_until(tree, 0, |acc, _k, v|
        if v > 2 then
            Break(acc)
        else
            Continue(acc + v)
    )
    result == 3

expect # Large tree sequential inserts
    tree = List.range({ start: At 1, end: At 50 })
        |> List.walk(empty {}, |acc, i| insert(acc, @WrapperTestKey(i), i * 2))
    get(tree, @WrapperTestKey(1)) == Ok(2) &&
    get(tree, @WrapperTestKey(25)) == Ok(50) &&
    get(tree, @WrapperTestKey(50)) == Ok(100)

expect # Descending sequential inserts
    tree = List.range({ start: At 1, end: At 30 })
        |> List.reverse
        |> List.walk(empty {}, |acc, i| insert(acc, @WrapperTestKey(i), Num.to_str(i)))
    list = to_list(tree)
    List.len(list) == 30

expect # Update key multiple times
    tree = empty {}
        |> insert(@WrapperTestKey(5), "first")
        |> insert(@WrapperTestKey(5), "second")
        |> insert(@WrapperTestKey(5), "third")
    get(tree, @WrapperTestKey(5)) == Ok("third") &&
    List.len(to_list(tree)) == 1

expect # Get non-existent keys
    tree = empty {}
        |> insert(@WrapperTestKey(2), "b")
        |> insert(@WrapperTestKey(4), "d")
    get(tree, @WrapperTestKey(1)) == Err NotFound &&
    get(tree, @WrapperTestKey(3)) == Err NotFound &&
    get(tree, @WrapperTestKey(5)) == Err NotFound

expect # Equality despite insertion order
    tree1 = empty {}
        |> insert(@WrapperTestKey(1), "a")
        |> insert(@WrapperTestKey(2), "b")
        |> insert(@WrapperTestKey(3), "c")
    tree2 = empty {}
        |> insert(@WrapperTestKey(3), "c")
        |> insert(@WrapperTestKey(1), "a")
        |> insert(@WrapperTestKey(2), "b")
    tree1 == tree2

expect # Inequality due to different values
    tree1 = empty {}
        |> insert(@WrapperTestKey(1), "a")
        |> insert(@WrapperTestKey(2), "b")
    tree2 = empty {}
        |> insert(@WrapperTestKey(1), "x")
        |> insert(@WrapperTestKey(2), "b")
    tree1 != tree2
