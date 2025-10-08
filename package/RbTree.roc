module [
    RbTree, # The user-facing type
    empty,
    insert,
    get,
    map,
    walk,
    walk_until,
    to_list,
    from_list,
]

# Import the base implementation and necessary abilities/types
import RbTreeBase exposing [RbTreeBase]
import Ord exposing [Ord]

## A Map implemented using a self-balancing Red-Black Tree.
## Keys (`k`) must implement the `Ord` ability for comparison and ordering.
## Keys (`k`) and Values (`v`) must implement `Inspect` for debugging/display.
RbTree k v := RbTreeBase k v where k implements Ord
    implements [
        Eq { is_eq: rb_tree_eq },
    ]

rb_tree_eq : RbTree k v, RbTree k v -> Bool where k implements Eq & Ord & Inspect, v implements Eq & Inspect
rb_tree_eq = |@RbTree(tree_a), @RbTree(tree_b)|
    # Rely on the base module's to_list for comparison
    RbTreeBase.to_list(tree_a) == RbTreeBase.to_list(tree_b)

## Creates an empty `RbTree`.
empty : {} -> RbTree k v
empty = |{}|
    RbTreeBase.empty {} |> @RbTree

## Inserts a key-value pair into the `RbTree`.
## If the key already exists, its associated value is updated to the new value.
## Returns the modified tree.
insert : RbTree k v, k, v -> RbTree k v
insert = |@RbTree(tree), key, value|
    RbTreeBase.insert(key, value, tree) |> @RbTree

## Retrieves the value associated with the given key.
## Returns `Ok value` if the key is found, otherwise returns `Err {}`.
get : RbTree k v, k -> Result v {}
get = |@RbTree(tree), key|
    RbTreeBase.get(tree, key)

## Transforms the values in the `RbTree` using a given function `fn`,
## while keeping the keys the same.
## Returns a new tree with the transformed values.
map : RbTree k v, (v -> w) -> RbTree k w # New value type must also be Inspectable
map = |@RbTree(tree), fn|
    RbTreeBase.map(tree, fn) |> @RbTree

## Iterates through the key-value pairs of the `RbTree` in ascending key order,
## applying the function `fn` to accumulate a `state`.
## `fn` takes the current state, key, and value, and returns the next state.
walk : RbTree k v, state, (state, k, v -> state) -> state
walk = |@RbTree(tree), state, fn|
    RbTreeBase.walk(tree, state, fn)

## Iterates through the key-value pairs like `walk`, but allows early termination.
## `fn` returns `Continue state` to proceed or `Break state` to stop and return the final state.
walk_until : RbTree k v, state, (state, k, v -> [Continue state, Break state]) -> state
walk_until = |@RbTree(tree), state, fn|
    RbTreeBase.walk_until(tree, state, fn)

## Converts the `RbTree` into a `List` of (key, value) pairs, sorted by key.
to_list : RbTree k v -> List (k, v)
to_list = |@RbTree(tree)|
    RbTreeBase.to_list(tree)

## Creates an `RbTree` from a `List` of (key, value) pairs.
## If duplicate keys exist in the list, the value associated with the last occurrence
## of the key in the list will be the one stored in the tree.
from_list : List (k, v) -> RbTree k v where k implements Ord
from_list = |list|
    RbTreeBase.from_list(list) |> @RbTree

# Test helper - Key type for testing with numeric keys
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

# --- Expectations for the Wrapper ---

# expect # Test empty, insert, get
#    tree1 : RbTree I64 Str
#    tree1 = empty {} |> insert(5, "E") |> insert(2, "B") |> insert(8, "H") |> insert(2, "BB") # Update key 2
#    get(tree1, 5) == Ok("E") && get(tree1, 2) == Ok("BB") && get(tree1, 8) == Ok("H") && get(tree1, 99) == Err {}

# expect # Test map
#    tree2 : RbTree I64 Str
#    tree2 = empty {} |> insert(1, "x") |> insert(2, "yy")
#    treeMapped : RbTree I64 U64
#    treeMapped = map(tree2, Str.count_utf8_bytes)
#    get(treeMapped, 1) == Ok(1) && get(treeMapped, 2) == Ok(2)

# expect # Test walk (concatenate values in key order)
#    tree3 : RbTree I64 Str
#    tree3 = empty {} |> insert(3, "c") |> insert(1, "a") |> insert(2, "b")
#    concatVals = walk(tree3, "", |acc, _k, v| Str.concat(acc, v))
#    concatVals == "abc"

# expect # Test walk_until (find first value starting with 'b')
#    tree4 : RbTree I64 Str
#    tree4 = empty {} |> insert(3, "c") |> insert(1, "a") |> insert(2, "b") |> insert(4, "b2")
#    findVal = walk_until(tree4, Err {}, |_, _k, v| if Str.starts_with(v, "b") then Break(Ok v) else Continue(Err {}))
#    findVal == Ok("b") # Stops at key 2

# expect # Test to_list
#    tree5 : RbTree I64 Str
#    tree5 = empty {} |> insert(30, "z") |> insert(10, "x") |> insert(20, "y")
#    to_list(tree5) == [(10, "x"), (20, "y"), (30, "z")]

# expect # Test from_list and to_list round trip
#    listIn = [(5, "e"), (1, "a"), (3, "c"), (2, "b"), (4, "d")]
#    tree6 = from_list(listIn)
#    listOut = to_list(tree6)
#    listOut == [(1, "a"), (2, "b"), (3, "c"), (4, "d"), (5, "e")]

# expect # Test Eq implementation
#    treeA = from_list([(1, "a"), (2, "b")])
#    treeB = empty {} |> insert(2, "b") |> insert(1, "a") # Different insertion order
#    treeC = from_list([(1, "a"), (2, "different")])
#    treeD = from_list([(1, "a")])
#    treeA == treeB # Should be equal despite order
#    &&
#    treeA != treeC # Should be different due to value
#    &&
#    treeA != treeD # Should be different due to missing element

# expect # Test Inspect implementation (indirectly via to_list)
#    treeInsp = from_list([(10, "ten")])
#    # This doesn't directly test the inspector output format easily in `expect`,
#    # but verifies that `to_list` (used by the inspector) works.
#    # A manual inspection via `dbg` or `roc repl` would show the list format.
#    to_list(treeInsp) == [(10, "ten")]

expect # Empty tree get returns error
    tree : RbTree (WrapperTestKey I64) Str
    tree = empty({})
    get(tree, @WrapperTestKey(1)) |> Result.is_err

expect # Single insert and get works
    tree = empty({}) |> insert(@WrapperTestKey(10), "ten")
    get(tree, @WrapperTestKey(10)) |> Result.is_ok

expect # Multiple inserts work
    tree = empty({})
        |> insert(@WrapperTestKey(3), "c")
        |> insert(@WrapperTestKey(1), "a")
        |> insert(@WrapperTestKey(2), "b")
    when (get(tree, @WrapperTestKey(1)), get(tree, @WrapperTestKey(2)), get(tree, @WrapperTestKey(3))) is
        (Ok(_), Ok(_), Ok(_)) -> Bool.true
        _ -> Bool.false

expect # Map transforms values
    tree = empty({})
        |> insert(@WrapperTestKey(1), 5)
        |> insert(@WrapperTestKey(2), 10)
    mapped : RbTree (WrapperTestKey I64) I64
    mapped = map(tree, \x -> x * 2)
    when get(mapped, @WrapperTestKey(1)) is
        Ok(10) -> Bool.true
        _ -> Bool.false

expect # Walk accumulates correctly
    tree = empty({})
        |> insert(@WrapperTestKey(1), 10)
        |> insert(@WrapperTestKey(2), 20)
        |> insert(@WrapperTestKey(3), 30)
    sum = walk(tree, 0, |acc, _k, v| acc + v)
    sum == 60

expect # walk_until can break early
    tree = empty({})
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
        |> List.walk(empty({}), |acc, i| insert(acc, @WrapperTestKey(i), i * 2))
    get(tree, @WrapperTestKey(1)) == Ok(2) &&
    get(tree, @WrapperTestKey(25)) == Ok(50) &&
    get(tree, @WrapperTestKey(50)) == Ok(100)

expect # Descending sequential inserts
    tree = List.range({ start: At 1, end: At 30 })
        |> List.reverse
        |> List.walk(empty({}), |acc, i| insert(acc, @WrapperTestKey(i), Num.to_str(i)))
    list = to_list(tree)
    List.len(list) == 30

expect # to_list on empty tree
    tree : RbTree (WrapperTestKey I64) Str
    tree = empty({})
    List.len(to_list(tree)) == 0

expect # from_list on empty list
    list : List (WrapperTestKey I64, Str)
    list = []
    tree = from_list(list)
    List.len(to_list(tree)) == 0

expect # from_list with single element
    tree = from_list([(@WrapperTestKey(42), "answer")])
    get(tree, @WrapperTestKey(42)) == Ok("answer")

expect # Equality of empty trees
    tree1 : RbTree (WrapperTestKey I64) Str
    tree1 = empty({})
    tree2 : RbTree (WrapperTestKey I64) Str
    tree2 = empty({})
    tree1 == tree2

expect # Inequality due to different values
    tree1 = empty({})
        |> insert(@WrapperTestKey(1), "a")
        |> insert(@WrapperTestKey(2), "b")
    tree2 = empty({})
        |> insert(@WrapperTestKey(1), "x")
        |> insert(@WrapperTestKey(2), "b")
    tree1 != tree2

expect # Equality despite insertion order
    tree1 = empty({})
        |> insert(@WrapperTestKey(1), "a")
        |> insert(@WrapperTestKey(2), "b")
        |> insert(@WrapperTestKey(3), "c")
    tree2 = empty({})
        |> insert(@WrapperTestKey(3), "c")
        |> insert(@WrapperTestKey(1), "a")
        |> insert(@WrapperTestKey(2), "b")
    tree1 == tree2

expect # Update key multiple times
    tree = empty({})
        |> insert(@WrapperTestKey(5), "first")
        |> insert(@WrapperTestKey(5), "second")
        |> insert(@WrapperTestKey(5), "third")
    get(tree, @WrapperTestKey(5)) == Ok("third") &&
    List.len(to_list(tree)) == 1

expect # Get non-existent keys
    tree = empty({})
        |> insert(@WrapperTestKey(2), "b")
        |> insert(@WrapperTestKey(4), "d")
    get(tree, @WrapperTestKey(1)) == Err {} &&
    get(tree, @WrapperTestKey(3)) == Err {} &&
    get(tree, @WrapperTestKey(5)) == Err {}

expect # map on empty tree
    tree : RbTree (WrapperTestKey I64) I64
    tree = empty({})
    mapped = map(tree, \x -> x * 2)
    to_list(mapped) == []

expect # Chained map operations
    tree = empty({})
        |> insert(@WrapperTestKey(1), 10)
        |> insert(@WrapperTestKey(2), 20)
    result = tree
        |> map(\x -> x * 2)
        |> map(\x -> x + 5)
    get(result, @WrapperTestKey(1)) == Ok(25) &&
    get(result, @WrapperTestKey(2)) == Ok(45)

expect # walk with string concatenation
    tree = empty({})
        |> insert(@WrapperTestKey(3), "c")
        |> insert(@WrapperTestKey(1), "a")
        |> insert(@WrapperTestKey(2), "b")
    result = walk(tree, "", |acc, _k, v| Str.concat(acc, v))
    result == "abc"

expect # walk_until never breaks
    tree = empty({})
        |> insert(@WrapperTestKey(1), 1)
        |> insert(@WrapperTestKey(2), 2)
        |> insert(@WrapperTestKey(3), 3)
    result = walk_until(tree, 0, |acc, _k, v| Continue(acc + v))
    result == 6

expect # walk_until breaks immediately
    tree = empty({})
        |> insert(@WrapperTestKey(1), "a")
        |> insert(@WrapperTestKey(2), "b")
    result = walk_until(tree, [], |acc, _k, _v| Break(acc))
    result == []

expect # Large tree from_list
    pairs = List.range({ start: At 1, end: At 50 })
        |> List.map(\i -> (@WrapperTestKey(i), i * 10))
    tree = from_list(pairs)
    get(tree, @WrapperTestKey(1)) == Ok(10) &&
    get(tree, @WrapperTestKey(50)) == Ok(500)

expect # to_list maintains order
    tree = empty({})
        |> insert(@WrapperTestKey(5), "e")
        |> insert(@WrapperTestKey(2), "b")
        |> insert(@WrapperTestKey(8), "h")
        |> insert(@WrapperTestKey(1), "a")
    list = to_list(tree)
    List.first(list) == Ok((@WrapperTestKey(1), "a")) &&
    List.last(list) == Ok((@WrapperTestKey(8), "h"))

expect # Zigzag insertion
    tree = empty({})
        |> insert(@WrapperTestKey(10), "10")
        |> insert(@WrapperTestKey(5), "5")
        |> insert(@WrapperTestKey(15), "15")
        |> insert(@WrapperTestKey(3), "3")
        |> insert(@WrapperTestKey(7), "7")
    List.len(to_list(tree)) == 5 &&
    get(tree, @WrapperTestKey(3)) == Ok("3")

expect # Large tree walk accumulation
    tree = List.range({ start: At 1, end: At 100 })
        |> List.walk(empty({}), |acc, i| insert(acc, @WrapperTestKey(i), i))
    sum = walk(tree, 0, |acc, _k, v| acc + v)
    sum == 5050
