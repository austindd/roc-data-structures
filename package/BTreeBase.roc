module [
    BTreeBase,
    empty,
    insert,
    get,
    map,
    walk,
    walk_until,
    to_list,
    from_list,
]

import Ord exposing [Ord, compare]

## A B-tree is either Empty or a Node with keys, values, and children.
## This is a simplified 2-3 tree (minimum degree 2) for compatibility.
BTreeBase a b : [
    Empty,
    Node2 { k1 : a, v1 : b, left : BTreeBase a b, right : BTreeBase a b },
    Node3 { k1 : a, v1 : b, k2 : a, v2 : b, left : BTreeBase a b, mid : BTreeBase a b, right : BTreeBase a b },
]

## Creates an empty B-tree.
empty : {} -> BTreeBase a b
empty = |{}| Empty

## Retrieves the value associated with a key.
get : BTreeBase a b, a -> Result b [NotFound] where a implements Ord
get = |tree, key|
    when tree is
        Empty -> Err NotFound

        Node2({ k1, v1, left, right }) ->
            when compare(key, k1) is
                EQ -> Ok v1
                LT -> get(left, key)
                GT -> get(right, key)

        Node3({ k1, v1, k2, v2, left, mid, right }) ->
            when compare(key, k1) is
                EQ -> Ok v1
                LT -> get(left, key)
                GT ->
                    when compare(key, k2) is
                        EQ -> Ok v2
                        LT -> get(mid, key)
                        GT -> get(right, key)

## Result of inserting into a tree - either fits or needs to split
InsertResult a b : [
    Fits (BTreeBase a b),
    Splits { left : BTreeBase a b, k : a, v : b, right : BTreeBase a b },
]

## Inserts a key-value pair into the tree.
insert : a, b, BTreeBase a b -> BTreeBase a b where a implements Ord
insert = |key, value, tree|
    when insert_helper(key, value, tree) is
        Fits(new_tree) -> new_tree
        Splits({ left, k, v, right }) ->
            # Root split - create new root
            Node2({ k1: k, v1: v, left, right })

insert_helper : a, b, BTreeBase a b -> InsertResult a b where a implements Ord
insert_helper = |key, value, tree|
    when tree is
        Empty ->
            # Create a new 2-node
            Fits(Node2({ k1: key, v1: value, left: Empty, right: Empty }))

        Node2({ k1, v1, left, right }) ->
            when compare(key, k1) is
                EQ ->
                    # Update existing key
                    Fits(Node2({ k1, v1: value, left, right }))

                LT ->
                    # Insert into left subtree
                    when insert_helper(key, value, left) is
                        Fits(new_left) ->
                            Fits(Node2({ k1, v1, left: new_left, right }))

                        Splits({ left: ll, k: lk, v: lv, right: lr }) ->
                            # Left child split - absorb into this node (becomes 3-node)
                            Fits(Node3({ k1: lk, v1: lv, k2: k1, v2: v1, left: ll, mid: lr, right }))

                GT ->
                    # Insert into right subtree
                    when insert_helper(key, value, right) is
                        Fits(new_right) ->
                            Fits(Node2({ k1, v1, left, right: new_right }))

                        Splits({ left: rl, k: rk, v: rv, right: rr }) ->
                            # Right child split - absorb into this node (becomes 3-node)
                            Fits(Node3({ k1, v1, k2: rk, v2: rv, left, mid: rl, right: rr }))

        Node3({ k1, v1, k2, v2, left, mid, right }) ->
            when compare(key, k1) is
                EQ ->
                    # Update existing key
                    Fits(Node3({ k1, v1: value, k2, v2, left, mid, right }))

                LT ->
                    # Insert into left subtree
                    when insert_helper(key, value, left) is
                        Fits(new_left) ->
                            Fits(Node3({ k1, v1, k2, v2, left: new_left, mid, right }))

                        Splits({ left: ll, k: lk, v: lv, right: lr }) ->
                            # Left child split - this node must split
                            left_node = Node2({ k1: lk, v1: lv, left: ll, right: lr })
                            right_node = Node2({ k1: k2, v1: v2, left: mid, right })
                            Splits({ left: left_node, k: k1, v: v1, right: right_node })

                GT ->
                    when compare(key, k2) is
                        EQ ->
                            # Update existing key
                            Fits(Node3({ k1, v1, k2, v2: value, left, mid, right }))

                        LT ->
                            # Insert into middle subtree
                            when insert_helper(key, value, mid) is
                                Fits(new_mid) ->
                                    Fits(Node3({ k1, v1, k2, v2, left, mid: new_mid, right }))

                                Splits({ left: ml, k: mk, v: mv, right: mr }) ->
                                    # Middle child split - this node must split
                                    left_node = Node2({ k1, v1, left, right: ml })
                                    right_node = Node2({ k1: k2, v1: v2, left: mr, right })
                                    Splits({ left: left_node, k: mk, v: mv, right: right_node })

                        GT ->
                            # Insert into right subtree
                            when insert_helper(key, value, right) is
                                Fits(new_right) ->
                                    Fits(Node3({ k1, v1, k2, v2, left, mid, right: new_right }))

                                Splits({ left: rl, k: rk, v: rv, right: rr }) ->
                                    # Right child split - this node must split
                                    left_node = Node2({ k1, v1, left, right: mid })
                                    right_node = Node2({ k1: rk, v1: rv, left: rl, right: rr })
                                    Splits({ left: left_node, k: k2, v: v2, right: right_node })

## Transforms the values in the tree using a provided function.
map : BTreeBase a b, (b -> c) -> BTreeBase a c where a implements Ord
map = |tree, fn|
    when tree is
        Empty -> Empty

        Node2({ k1, v1, left, right }) ->
            Node2({ k1, v1: fn(v1), left: map(left, fn), right: map(right, fn) })

        Node3({ k1, v1, k2, v2, left, mid, right }) ->
            Node3({ k1, v1: fn(v1), k2, v2: fn(v2), left: map(left, fn), mid: map(mid, fn), right: map(right, fn) })

## Walks through the tree in key order, accumulating a state.
walk : BTreeBase a b, state, (state, a, b -> state) -> state where a implements Ord
walk = |tree, state, fn|
    when tree is
        Empty -> state

        Node2({ k1, v1, left, right }) ->
            state
            |> \s -> walk(left, s, fn)
            |> \s -> fn(s, k1, v1)
            |> \s -> walk(right, s, fn)

        Node3({ k1, v1, k2, v2, left, mid, right }) ->
            state
            |> \s -> walk(left, s, fn)
            |> \s -> fn(s, k1, v1)
            |> \s -> walk(mid, s, fn)
            |> \s -> fn(s, k2, v2)
            |> \s -> walk(right, s, fn)

## Walks through the tree in key order, accumulating a state until a `Break` is returned.
walk_until : BTreeBase a b, state, (state, a, b -> [Continue state, Break state]) -> state where a implements Ord
walk_until = |tree, state, fn|
    when tree is
        Empty -> state

        Node2({ k1, v1, left, right }) ->
            left_state = walk_until(left, state, fn)
            when fn(left_state, k1, v1) is
                Continue(s) -> walk_until(right, s, fn)
                Break(s) -> s

        Node3({ k1, v1, k2, v2, left, mid, right }) ->
            left_state = walk_until(left, state, fn)
            when fn(left_state, k1, v1) is
                Continue(mid_state) ->
                    mid_result = walk_until(mid, mid_state, fn)
                    when fn(mid_result, k2, v2) is
                        Continue(s) -> walk_until(right, s, fn)
                        Break(s) -> s
                Break(s) -> s

## Converts the tree into a list of (key, value) pairs, sorted by key.
to_list : BTreeBase a b -> List (a, b) where a implements Ord
to_list = |tree|
    walk(tree, [], |list, key, value| List.append(list, (key, value)))

## Creates a tree from a list of (key, value) pairs.
from_list : List (a, b) -> BTreeBase a b where a implements Ord
from_list = |pairs|
    List.walk(pairs, empty {}, |tree, (key, value)| insert(key, value, tree))

# --- Tests ---
# Note: Tests are commented out due to current Roc compiler limitations
# with recursive type inference in test contexts

# TestKey := I64 implements [Ord { compare: test_key_compare }, Eq, Inspect]

# test_key_compare : TestKey, TestKey -> Ordering
