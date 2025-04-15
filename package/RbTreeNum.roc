module [
    RbTreeNum, # The user-facing type for numeric keys
    empty,
    insert,
    get,
    map,
    walk,
    walk_until, # Added to match RbTree's interface
    to_list,
    from_list,
]

# Import the general RbTree wrapper and necessary abilities/types
import RbTree exposing [RbTree]
import Ord exposing [Ord, Ordering, compare]
import Num exposing [Num] # Needed for Num constraint and compare
import List # Needed for list operations
import Inspect exposing [Inspect, Inspector, InspectFormatter] # For Inspect ability
import Hash exposing [Hash, Hasher] # For Hash ability
import Eq exposing [Eq] # For Eq ability

# --- Opaque Wrapper for Numeric Keys ---

# Helper function to compare numeric keys
num_compare : NumKey a, NumKey a -> Ordering where a implements Ord
num_compare = |@NumKey(a), @NumKey(b)|
    Num.compare(a, b)

# Opaque type to wrap numeric keys, allowing them to satisfy RbTree's `k implements Ord` constraint.
NumKey a := Num a implements [
        # Ord is crucial for the underlying RbTree
        Ord { compare: num_compare },
        # Implement other common abilities for the key wrapper
        Eq,
        Hash,
        Inspect,
    ] where a implements Ord & Eq & Hash & Inspect

# --- RbTreeNum Definition ---

# Helper function to inspect RbTreeNum by delegating to the underlying RbTree's inspector
inspect_rb_tree_num : RbTreeNum k v -> Inspector f where k implements Ord & Inspect, v implements Inspect, f implements InspectFormatter
inspect_rb_tree_num = |@RbTreeNum(tree)|
    # Delegate inspection to the wrapped RbTree (which inspects as a list of NumKey pairs)
    Inspect.to_inspector(tree)

# Helper function for equality by delegating to the underlying RbTree's Eq
eq_rb_tree_num : RbTreeNum k v, RbTreeNum k v -> Bool where k implements Eq, v implements Eq
eq_rb_tree_num = |@RbTreeNum(tree_a), @RbTreeNum(tree_b)|
    # Delegate equality to the wrapped RbTree (which compares lists of NumKey pairs)
    tree_a == tree_b

# Helper function for hashing by delegating to the underlying RbTree's Hash
hash_rb_tree_num : hasher, RbTreeNum k v -> hasher where k implements Hash, v implements Hash, hasher implements Hasher
hash_rb_tree_num = |hasher, @RbTreeNum(tree)|
    # Delegate hashing to the wrapped RbTree (which likely hashes the list of NumKey pairs)
    Hash.hash(hasher, tree)


## A Map implemented using a Red-Black Tree where keys are numeric (`Num k`).
## Values (`v`) must implement `Inspect`.
RbTreeNum k v := RbTree (NumKey k) v where v implements Inspect # Underlying RbTree handles k constraints
    implements [
        # Delegate common abilities to the underlying RbTree
        Eq { is_eq: eq_rb_tree_num },
        Hash { hash: hash_rb_tree_num },
        Inspect { to_inspector: inspect_rb_tree_num },
    ]


# --- Public Functions ---

## Creates an empty `RbTreeNum`.
empty : {} -> RbTreeNum k v
empty = |{}|
    RbTree.empty {} |> @RbTreeNum


## Inserts a numeric key-value pair into the `RbTreeNum`.
## If the key already exists, its value is updated.
insert : RbTreeNum k v, Num k, v -> RbTreeNum k v
insert = |@RbTreeNum(tree), key, value|
    # Wrap the numeric key with NumKey before passing to RbTree.insert
    RbTree.insert(tree, @NumKey(key), value) |> @RbTreeNum


## Retrieves the value associated with the given numeric key.
## Returns `Ok value` if the key exists, `Err {}` otherwise.
get : RbTreeNum k v, Num k -> Result v {}
get = |@RbTreeNum(tree), key|
    # Wrap the numeric key with NumKey before passing to RbTree.get
    RbTree.get(tree, @NumKey(key))


## Transforms the values in the `RbTreeNum` using a function `fn`, keeping keys the same.
map : RbTreeNum k v, (v -> w) -> RbTreeNum k w where w implements Inspect # New value type must also be Inspectable
map = |@RbTreeNum(tree), fn|
    # Delegate mapping directly to the underlying RbTree.map
    RbTree.map(tree, fn) |> @RbTreeNum


## Iterates through the key-value pairs (in key order), accumulating a state.
walk : RbTreeNum k v, state, (state, Num k, v -> state) -> state
walk = |@RbTreeNum(tree), state, fn|
    # Delegate to RbTree.walk, unwrapping the NumKey within the walker function
    RbTree.walk(tree, state, |currentState, @NumKey(key), value|
        fn(currentState, key, value) # Pass the unwrapped Num k to the user's function
    )


## Iterates like `walk`, but allows early termination using `Continue` or `Break`.
walk_until : RbTreeNum k v, state, (state, Num k, v -> [Continue state, Break state]) -> state
walk_until = |@RbTreeNum(tree), state, fn|
    # Delegate to RbTree.walk_until, unwrapping the NumKey within the walker function
    RbTree.walk_until(tree, state, |currentState, @NumKey(key), value|
        fn(currentState, key, value) # Pass the unwrapped Num k to the user's function
    )


## Converts the `RbTreeNum` into a `List` of (Num k, v) pairs, sorted by key.
to_list : RbTreeNum k v -> List (Num k, v)
to_list = |@RbTreeNum(tree)|
    # Get the list from the underlying RbTree (which has NumKey pairs)
    numKeyList = RbTree.to_list(tree)
    # Map over the list to unwrap NumKey back to Num k for the user
    List.map(numKeyList, |(@NumKey(key), value)| (key, value))


## Creates an `RbTreeNum` from a `List` of (Num k, v) pairs.
from_list : List (Num k, v) -> RbTreeNum k v where v implements Inspect
from_list = |list|
    # Map over the user's list to wrap Num k keys into NumKey
    numKeyList = List.map(list, |(key, value)| (@NumKey(key), value))
    # Create the underlying RbTree using the list of NumKey pairs
    RbTree.from_list(numKeyList) |> @RbTreeNum


# --- Expectations ---

expect # Test empty, insert, get, update
    tree1 : RbTreeNum I64 Str
    tree1 =
        empty {}
        |> insert(10, "A")
        |> insert(5, "B")
        |> insert(15, "C")
        |> insert(5, "BB") # Update key 5

    get(tree1, 10) == Ok("A") &&
    get(tree1, 5) == Ok("BB") &&
    get(tree1, 15) == Ok("C") &&
    get(tree1, 99) == Err {}

expect # Test map
    tree2 : RbTreeNum I32 I64
    tree2 = from_list([(1i32, 10), (2i32, 20)])
    treeMapped : RbTreeNum I32 I64
    treeMapped = map(tree2, \x -> x * 2)
    get(treeMapped, 1i32) == Ok(20) && get(treeMapped, 2i32) == Ok(40)

expect # Test walk (sum values)
    tree3 : RbTreeNum U8 U8
    tree3 = from_list([(1u8, 100), (2u8, 50), (3u8, 25)])
    sumVals = walk(tree3, 0u8, |acc, _k, v| Num.add_wrap(acc, v)) # Use add_wrap for safety
    sumVals == 175u8

expect # Test walk_until (stop when key is even)
    tree4 : RbTreeNum I16 Str
    tree4 = from_list([(1i16, "odd1"), (3i16, "odd2"), (2i16, "even1"), (4i16, "even2")])
    findEven = walk_until(tree4, Err {}, |_, key, val| if Num.is_even(key) then Break(Ok (key, val)) else Continue(Err {}))
    findEven == Ok(2i16, "even1") # Should stop at key 2

expect # Test to_list preserves order
    tree5 : RbTreeNum Dec Str
    tree5 = from_list([(3.0dec, "c"), (1.5dec, "a"), (2.2dec, "b")])
    to_list(tree5) == [(1.5dec, "a"), (2.2dec, "b"), (3.0dec, "c")]

expect # Test from_list and to_list round trip
    listIn : List (I64, Str)
    listIn = [(50, "z"), (10, "x"), (30, "y")]
    tree6 = from_list(listIn)
    listOut = to_list(tree6)
    listOut == [(10, "x"), (30, "y"), (50, "z")]

expect # Test Eq implementation via from_list
    treeA : RbTreeNum I64 Str
    treeA = from_list([(1, "a"), (2, "b")])
    treeB : RbTreeNum I64 Str
    treeB = from_list([(2, "b"), (1, "a")]) # Different order, same content
    treeC : RbTreeNum I64 Str
    treeC = from_list([(1, "DIFFERENT"), (2, "b")])
    treeA == treeB && treeA != treeC

# RbTreeNum inspection is delegated, verify underlying list conversion works
expect # Test Inspect implementation (indirectly)
    treeInsp : RbTreeNum I64 Str
    treeInsp = from_list([(10, "ten")])
    to_list(treeInsp) == [(10, "ten")] # If this works, inspection delegate works
