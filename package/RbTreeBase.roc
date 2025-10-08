module [
    RbTreeBase,
    empty,
    insert,
    get, # Replaced contains
    map, # New
    walk, # New
    walk_until, # New
    to_list, # New
    from_list, # New
]

import Ord exposing [Ord, compare, Ordering]

Color : [Red, Black]

RbTreeBase a b : [
    Empty,
    Node {
            color : Color,
            k : a,
            v : b,
            left : RbTreeBase a b,
            right : RbTreeBase a b,
        },
] where a implements Ord & Inspect, b implements Inspect

# inspect_rb_tree : RbTreeBase a b -> Inspector f where a implements Inspect, b implements Inspect
# inspect_rb_tree = |tree| # Basic placeholder - could be more detailed
#     Inspect.to_inspector(to_list(tree))

## Creates an empty Red-Black Tree Map.
empty : {} -> RbTreeBase a b
empty = |{}| Empty

## Retrieves the value associated with a key.
## Returns `Ok value` if the key exists, `Err {}` otherwise.
get : RbTreeBase a b, a -> Result b {}
get = |tree, key|
    when tree is
        Empty ->
            Err {}

        Node({ k, v, left, right }) ->
            when compare(key, k) is
                LT -> get(left, key)
                GT -> get(right, key)
                EQ -> Ok(v)

## Inserts a key-value pair into the tree.
## If the key already exists, its value is updated.
insert : a, b, RbTreeBase a b -> RbTreeBase a b
insert = |key, value, tree|
    newTree = insertHelper(key, value, tree)
    # Force root to Black
    when newTree is
        Node({ color: _, k, v, left, right }) -> Node({ color: Black, k, v, left, right })
        Empty -> Empty # Should only happen if initial tree was Empty and insertHelper returned Empty (which it shouldn't)

# Helper function for recursive insertion
insertHelper : a, b, RbTreeBase a b -> RbTreeBase a b
insertHelper = |key, value, tree|
    when tree is
        Empty ->
            # New nodes are always Red initially
            Node({ color: Red, k: key, v: value, left: Empty, right: Empty })

        Node({ color, k, v, left, right }) ->
            comparison = compare(key, k)
            when comparison is
                LT ->
                    # Key is smaller, insert into left subtree
                    newLeft = insertHelper(key, value, left)
                    balance(color, k, v, newLeft, right) # Balance after insertion

                GT ->
                    # Key is larger, insert into right subtree
                    newRight = insertHelper(key, value, right)
                    balance(color, k, v, left, newRight) # Balance after insertion

                EQ ->
                    # Key already exists, update the value
                    Node({ color, k, v: value, left, right })

# Balance the tree according to Red-Black Tree rules after insertion/deletion
balance : Color, a, b, RbTreeBase a b, RbTreeBase a b -> RbTreeBase a b
balance = |nodeColor, k, v, left, right|
    when (nodeColor, k, v, left, right) is
        # Case 1: Left-leaning Red violation (Black grandparent, Red parent, Red child on left) -> Right Rotation
        (Black, zk, zv, Node({ color: Red, k: yk, v: yv, left: Node({ color: Red, k: xk, v: xv, left: a, right: b }), right: c }), d) ->
            Node({ color: Red, k: yk, v: yv, left: Node({ color: Black, k: xk, v: xv, left: a, right: b }), right: Node({ color: Black, k: zk, v: zv, left: c, right: d }) })

        # Case 2: Left-leaning Red violation (Inner child is Red) -> Left Rotation then Right Rotation
        (Black, zk, zv, Node({ color: Red, k: xk, v: xv, left: a, right: Node({ color: Red, k: yk, v: yv, left: b, right: c }) }), d) ->
            Node({ color: Red, k: yk, v: yv, left: Node({ color: Black, k: xk, v: xv, left: a, right: b }), right: Node({ color: Black, k: zk, v: zv, left: c, right: d }) })

        # Case 3: Right-leaning Red violation (Black grandparent, Red parent, Red child on right) -> Left Rotation
        (Black, xk, xv, a, Node({ color: Red, k: yk, v: yv, left: b, right: Node({ color: Red, k: zk, v: zv, left: c, right: d }) })) ->
            Node({ color: Red, k: yk, v: yv, left: Node({ color: Black, k: xk, v: xv, left: a, right: b }), right: Node({ color: Black, k: zk, v: zv, left: c, right: d }) })

        # Case 4: Right-leaning Red violation (Inner child is Red) -> Right Rotation then Left Rotation
        (Black, xk, xv, a, Node({ color: Red, k: zk, v: zv, left: Node({ color: Red, k: yk, v: yv, left: b, right: c }), right: d })) ->
            Node({ color: Red, k: yk, v: yv, left: Node({ color: Black, k: xk, v: xv, left: a, right: b }), right: Node({ color: Black, k: zk, v: zv, left: c, right: d }) })

        # Case 5: Color Flip - If both children are Red, flip colors
        (_, k1, v1, Node({ color: Red, k: lk, v: lv, left: ll, right: lr }), Node({ color: Red, k: rk, v: rv, left: rl, right: rr })) ->
            Node({ color: Red, k: k1, v: v1, left: Node({ color: Black, k: lk, v: lv, left: ll, right: lr }), right: Node({ color: Black, k: rk, v: rv, left: rl, right: rr }) })

        # Default case: No violation or handled by flips, just construct the node with original color
        _ -> Node({ color: nodeColor, k, v, left, right })

## Transforms the values in the tree using a provided function.
map : RbTreeBase a b, (b -> c) -> RbTreeBase a c
map = |tree, fn|
    when tree is
        Empty -> Empty
        Node({ color, k, v, left, right }) ->
            Node({ color, k, v: fn(v), left: map(left, fn), right: map(right, fn) })

## Walks through the tree in key order, accumulating a state.
walk : RbTreeBase a b, state, (state, a, b -> state) -> state
walk = |tree, state, fn|
    when tree is
        Empty ->
            state

        Node({ k, v, left, right }) ->
            # In-order traversal: Left -> Node -> Right
            leftState = walk(left, state, fn)
            nodeState = fn(leftState, k, v)
            walk(right, nodeState, fn)

## Walks through the tree in key order, accumulating a state until a `Break` is returned.
walk_until : RbTreeBase a b, state, (state, a, b -> [Continue state, Break state]) -> state
walk_until = |tree, state, fn|
    when tree is
        Empty ->
            state

        Node({ k, v, left, right }) ->
            # In-order traversal with early exit
            leftResult = walk_until(left, state, fn)
            # Check if the walk should break after processing the left subtree
            when fn(leftResult, k, v) is
                Continue(nodeState) -> walk_until(right, nodeState, fn) # Continue to the right
                Break(finalState) -> finalState # Break early

## Converts the tree into a list of (key, value) pairs, sorted by key.
to_list : RbTreeBase a b -> List (a, b)
to_list = |tree|
    walk(tree, [], |list, key, value| List.append(list, (key, value)))

## Creates a tree from a list of (key, value) pairs.
## Note: This performs repeated insertions. For large lists, building from a sorted
## list would be more efficient but is more complex to implement for RB-Trees.
from_list : List (a, b) -> RbTreeBase a b where a implements Ord & Inspect
from_list = |pairs|
    List.walk(
        pairs,
        empty {},
        |currentTree, (key, value)|
            insert(key, value, currentTree),
    )

# --- Expectations for Map Functionality ---

# expect
#    # Test get on existing keys
#    myTree =
#        empty({})
#        |> insert(10, "ten")
#        |> insert(20, "twenty")
#        |> insert(5, "five")
#    get(myTree, 10)
#    == Ok("ten")
#    and
#    get(myTree, 5)
#    == Ok("five")
#    and
#    get(myTree, 20)
#    == Ok("twenty")

# expect
#    # Test get on non-existent key
#    myTree =
#        empty({})
#        |> insert(10, "ten")
#    get(myTree, 100) == Err {}

# expect
#    # Test updating an existing key
#    myTree =
#        empty({})
#        |> insert(10, "ten")
#        |> insert(10, "TEN_UPDATED")
#    get(myTree, 10) == Ok("TEN_UPDATED")

# expect
#    # Test map function
#    myTree =
#        empty({})
#        |> insert(1, "a")
#        |> insert(2, "bb")
#        |> insert(3, "ccc")
#        |> map(Str.count_utf8_bytes) # Map values to their string length
#    get(myTree, 1)
#    == Ok(1)
#    and
#    get(myTree, 2)
#    == Ok(2)
#    and
#    get(myTree, 3)
#    == Ok(3)

# expect
#    # Test walk function (summing keys)
#    myTree =
#        empty({})
#        |> insert(1, "x")
#        |> insert(2, "y")
#        |> insert(3, "z")
#    sumOfKeys = walk(myTree, 0, |sum, key, _value| sum + key)
#    sumOfKeys == 6

# expect
#    # Test walk_until function (stop when key > 2)
#    myTree =
#        empty({})
#        |> insert(1, "x")
#        |> insert(2, "y")
#        |> insert(3, "z") # Should stop before processing this
#        |> insert(4, "w")
#    walkResult = walk_until(
#        myTree,
#        [],
#        |list, key, val|
#            if key > 2 then Break(list) else Continue(List.append(list, (key, val))),
#    )
#    # Should contain only pairs with keys <= 2
#    walkResult == [(1, "x"), (2, "y")]

# expect
#    # Test to_list function (preserves order)
#    myTree =
#        empty({})
#        |> insert(3, "three")
#        |> insert(1, "one")
#        |> insert(2, "two")
#    toListResult = to_list(myTree)
#    toListResult == [(1, "one"), (2, "two"), (3, "three")]

# expect
#    # Test from_list and to_list round trip
#    initialList = [(30, "c"), (10, "a"), (20, "b")]
#    tree = from_list(initialList)
#    finalList = to_list(tree)
#    finalList == [(10, "a"), (20, "b"), (30, "c")] # Check if sorted correctly

# expect
#    # Test from_list with duplicate keys (last one wins)
#    initialList = [(10, "a"), (20, "b"), (10, "A_UPDATED")]
#    tree = from_list(initialList)
#    finalList = to_list(tree)
#    finalList == [(10, "A_UPDATED"), (20, "b")]

# Tests - using TestKey wrapper since raw numbers don't implement Ord

TestKey := I64 implements [Ord { compare: test_key_compare }, Eq, Inspect]

test_key_compare : TestKey, TestKey -> Ordering
test_key_compare = |@TestKey(a), @TestKey(b)| Num.compare(a, b)

expect # Empty tree
    tree = empty({})
    get(tree, @TestKey(1)) == Err {}

expect # Single insert and get
    tree = insert(@TestKey(42), "answer", empty({}))
    get(tree, @TestKey(42)) == Ok("answer")

expect # Multiple inserts and ordered enumeration
    tree = insert(@TestKey(2), "b", insert(@TestKey(1), "a", insert(@TestKey(3), "c", empty({}))))
    to_list(tree) == [(@TestKey(1), "a"), (@TestKey(2), "b"), (@TestKey(3), "c")]

expect # from_list creates sorted tree
    list = [(@TestKey(3), "c"), (@TestKey(1), "a"), (@TestKey(2), "b")]
    tree = from_list(list)
    to_list(tree) == [(@TestKey(1), "a"), (@TestKey(2), "b"), (@TestKey(3), "c")]

expect # map transforms values
    tree = insert(@TestKey(3), 30, insert(@TestKey(2), 20, insert(@TestKey(1), 10, empty({}))))
    mapped = map(tree, \x -> x * 2)
    to_list(mapped) == [(@TestKey(1), 20), (@TestKey(2), 40), (@TestKey(3), 60)]

expect # walk accumulates in order
    tree = insert(@TestKey(2), 20, insert(@TestKey(1), 10, insert(@TestKey(3), 30, empty({}))))
    sum = walk(tree, 0, |acc, _k, v| acc + v)
    sum == 60

expect # Large tree sequential insertions
    tree = List.range({ start: At 1, end: At 100 })
        |> List.walk(empty({}), |acc, i| insert(@TestKey(i), i * 10, acc))
    get(tree, @TestKey(1)) == Ok(10) &&
    get(tree, @TestKey(50)) == Ok(500) &&
    get(tree, @TestKey(100)) == Ok(1000)

expect # Descending insertions
    tree = List.range({ start: At 1, end: At 30 })
        |> List.reverse
        |> List.walk(empty({}), |acc, i| insert(@TestKey(i), i, acc))
    list = to_list(tree)
    List.len(list) == 30 &&
    List.first(list) == Ok((@TestKey(1), 1))

expect # Update same key multiple times
    tree = insert(@TestKey(5), "c", insert(@TestKey(5), "b", insert(@TestKey(5), "a", empty({}))))
    get(tree, @TestKey(5)) == Ok("c") &&
    List.len(to_list(tree)) == 1

expect # Empty tree operations
    tree : RbTreeBase TestKey Str
    tree = empty({})
    to_list(tree) == [] &&
    get(tree, @TestKey(1)) == Err {}

expect # Single element operations
    tree = insert(@TestKey(42), "answer", empty({}))
    to_list(tree) == [(@TestKey(42), "answer")] &&
    get(tree, @TestKey(42)) == Ok("answer")

expect # walk_until breaks early
    tree = insert(@TestKey(4), "d", insert(@TestKey(3), "c", insert(@TestKey(2), "b", insert(@TestKey(1), "a", empty({})))))
    result = walk_until(tree, [], |acc, _k, v|
        if v == "c" then
            Break(acc)
        else
            Continue(List.append(acc, v))
    )
    result == ["a", "b"]

expect # walk_until never breaks
    tree = insert(@TestKey(3), 3, insert(@TestKey(2), 2, insert(@TestKey(1), 1, empty({}))))
    result = walk_until(tree, 0, |acc, _k, v| Continue(acc + v))
    result == 6

expect # walk_until on empty tree
    tree : RbTreeBase TestKey Str
    tree = empty({})
    result = walk_until(tree, 100, |acc, _k, _v| Continue(acc + 1))
    result == 100

expect # map on empty tree
    tree : RbTreeBase TestKey I64
    tree = empty({})
    mapped = map(tree, \x -> x * 2)
    to_list(mapped) == []

expect # from_list with empty list
    list : List (TestKey, Str)
    list = []
    tree = from_list(list)
    to_list(tree) == []

expect # from_list with single element
    tree = from_list([(@TestKey(1), "one")])
    get(tree, @TestKey(1)) == Ok("one")

expect # Get non-existent keys
    tree = insert(@TestKey(6), "f", insert(@TestKey(4), "d", insert(@TestKey(2), "b", empty({}))))
    get(tree, @TestKey(1)) == Err {} &&
    get(tree, @TestKey(3)) == Err {} &&
    get(tree, @TestKey(5)) == Err {} &&
    get(tree, @TestKey(7)) == Err {}

expect # Zigzag insertion pattern
    tree = insert(@TestKey(17), "17",
           insert(@TestKey(12), "12",
           insert(@TestKey(7), "7",
           insert(@TestKey(3), "3",
           insert(@TestKey(15), "15",
           insert(@TestKey(5), "5",
           insert(@TestKey(10), "10", empty({}))))))))
    List.len(to_list(tree)) == 7 &&
    get(tree, @TestKey(3)) == Ok("3") &&
    get(tree, @TestKey(17)) == Ok("17")

expect # Large tree walk accumulation
    tree = List.range({ start: At 1, end: At 100 })
        |> List.walk(empty({}), |acc, i| insert(@TestKey(i), i, acc))
    sum = walk(tree, 0, |acc, _k, v| acc + v)
    sum == 5050

expect # from_list maintains order
    list = [(@TestKey(5), "e"), (@TestKey(1), "a"), (@TestKey(3), "c"), (@TestKey(2), "b"), (@TestKey(4), "d")]
    tree = from_list(list)
    result = to_list(tree)
    List.len(result) == 5 &&
    List.first(result) == Ok((@TestKey(1), "a")) &&
    List.last(result) == Ok((@TestKey(5), "e"))

expect # walk with string concatenation
    tree = insert(@TestKey(4), "d", insert(@TestKey(2), "b", insert(@TestKey(1), "a", insert(@TestKey(3), "c", empty({})))))
    result = walk(tree, "", |acc, _k, v| Str.concat(acc, v))
    result == "abcd"

expect # map maintains structure
    tree = insert(@TestKey(3), 30, insert(@TestKey(2), 20, insert(@TestKey(1), 10, empty({}))))
    mapped1 = map(tree, \x -> x * 2)
    mapped2 = map(mapped1, \x -> x + 5)
    get(mapped2, @TestKey(1)) == Ok(25) &&
    get(mapped2, @TestKey(2)) == Ok(45) &&
    get(mapped2, @TestKey(3)) == Ok(65)

expect # walk with key access
    tree = insert(@TestKey(3), "c", insert(@TestKey(2), "b", insert(@TestKey(1), "a", empty({}))))
    keys = walk(tree, [], |acc, @TestKey(k), _v| List.append(acc, k))
    keys == [1, 2, 3]

expect # Interleaved middle-out insertions
    tree = insert(@TestKey(87), "87",
           insert(@TestKey(62), "62",
           insert(@TestKey(37), "37",
           insert(@TestKey(12), "12",
           insert(@TestKey(75), "75",
           insert(@TestKey(25), "25",
           insert(@TestKey(50), "50", empty({}))))))))
    list = to_list(tree)
    List.len(list) == 7

expect # from_list with duplicates
    list = [(@TestKey(1), "first"), (@TestKey(2), "b"), (@TestKey(1), "second")]
    tree = from_list(list)
    # Last value wins for duplicates via insert
    result = to_list(tree)
    List.len(result) == 2 &&  # Only 2 unique keys
    get(tree, @TestKey(1)) == Ok("second")  # Last value wins
