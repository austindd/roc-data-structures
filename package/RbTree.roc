module [
    RBTree,
    empty,
    insert,
    contains,
]

import Ord exposing [Ord, compare, Ordering]

# Define the possible colors for a node
Color : [Red, Black]

# Define the recursive tree structure
# RBTree a means a Red-Black Tree holding values of type 'a'
# It can be Empty or a Node containing:
# - Color (Red or Black)
# - value of type 'a'
# - left child (RBTree a)
# - right child (RBTree a)
RBTree a : [
    Empty,
    Node {
            color : Color,
            nodeVal : a,
            left : RBTree a,
            right : RBTree a,
        },
]

# Create an empty tree
empty : {} -> RBTree a
empty = |{}| Empty

# Check if a value exists in the tree
# Requires the type 'a' to implement the Ord ability for comparison
contains : RBTree a, a -> Bool where a implements Ord
contains = |tree, value|
    when tree is
        Empty ->
            # Value not found in an empty tree
            Bool.false

        Node({ nodeVal, left, right }) ->
            # Compare the target value with the current node's value
            when compare(value, nodeVal) is
                LT ->
                    # If target is less, search in the left subtree
                    contains(left, value)

                GT ->
                    # If target is greater, search in the right subtree
                    contains(right, value)

                EQ ->
                    # If equal, the value is found
                    Bool.true

# Core insertion function
# Requires the type 'a' to implement the Ord ability for comparison
insert : a, RBTree a -> RBTree a where a implements Ord
insert = |value, tree|
    # insertHelper does the recursive insertion and balancing
    newTree = insertHelper(value, tree)

    # The root of the Red-Black Tree must always be Black.
    # insertHelper might return a Red root if the tree was initially empty
    # or if balancing propagates Red up to the root.
    when newTree is
        Node({ color, nodeVal, left, right }) -> Node({ color: Black, nodeVal, left, right }) # Force root to Black
        Empty -> Empty # Should not happen if insertHelper is correct, but handle defensively

# Recursive helper function for insertion and balancing
insertHelper : a, RBTree a -> RBTree a where a implements Ord
insertHelper = |value, tree|
    when tree is
        Empty ->
            # Base case: Inserted element becomes a new Red leaf node.
            Node({ color: Red, nodeVal: value, left: Empty, right: Empty })

        Node({ color, nodeVal, left, right }) ->
            # Compare the value to insert with the current node's value
            comparison = compare(value, nodeVal)
            when comparison is
                LT ->
                    # Value is less, insert into the left subtree recursively
                    newLeft = insertHelper(value, left)
                    # Balance the current node after left insertion
                    balance(color, nodeVal, newLeft, right)

                GT ->
                    # Value is greater, insert into the right subtree recursively
                    newRight = insertHelper(value, right)
                    # Balance the current node after right insertion
                    balance(color, nodeVal, left, newRight)

                EQ ->
                    # Value already exists, return the original node (no duplicates)
                    # Since Roc is immutable, returning the same node is efficient.
                    Node({ color, nodeVal, left, right })

# Balance function: Maintains Red-Black Tree properties after insertion.
# This function checks for violations (like Red parent with Red child)
# and performs rotations and color flips to fix them.
# This is based on the standard Red-Black Tree balancing cases (Okasaki style).
balance : Color, a, RBTree a, RBTree a -> RBTree a where a implements Ord
balance = |nodeColor, nodeVal, left, right|
    when (nodeColor, nodeVal, left, right) is
        # Case 1: Left-Leaning Red Violation (Black grandparent, Red parent, Red child)
        # Corrected by right rotation and color flips.
        (Black, z, Node({ color: Red, nodeVal: y, left: Node({ color: Red, nodeVal: x, left: a, right: b }), right: c }), d) ->
            Node(
                {
                    color: Red,
                    nodeVal: y,
                    left: Node(
                        {
                            color: Black,
                            nodeVal: x,
                            left: a,
                            right: b,
                        },
                    ),
                    right: Node(
                        {
                            color: Black,
                            nodeVal: z,
                            left: c,
                            right: d,
                        },
                    ),
                },
            )

        # Case 2: Left-Leaning Red Violation (Inner child is Red)
        # Corrected by left rotation (at x), then handled like Case 1.
        (Black, z, Node({ color: Red, nodeVal: x, left: a, right: Node({ color: Red, nodeVal: y, left: b, right: c }) }), d) ->
            Node(
                {
                    color: Red,
                    nodeVal: y,
                    left: Node(
                        {
                            color: Black,
                            nodeVal: x,
                            left: a,
                            right: b,
                        },
                    ),
                    right: Node(
                        {
                            color: Black,
                            nodeVal: z,
                            left: c,
                            right: d,
                        },
                    ),
                },
            )

        # Case 3: Right-Leaning Red Violation (Black grandparent, Red parent, Red child)
        # Corrected by left rotation and color flips.
        (Black, x, a, Node({ color: Red, nodeVal: z, left: b, right: Node({ color: Red, nodeVal: y, left: c, right: d }) })) ->
            Node({ color: Red, nodeVal: y, left: Node({ color: Black, nodeVal: x, left: a, right: b }), right: Node({ color: Black, nodeVal: z, left: c, right: d }) })

        # Case 4: Right-Leaning Red Violation (Inner child is Red)
        # Corrected by right rotation (at z), then handled like Case 3.
        (Black, x, a, Node({ color: Red, nodeVal: y, left: Node({ color: Red, nodeVal: z, left: b, right: c }), right: d })) ->
            Node({ color: Red, nodeVal: y, left: Node({ color: Black, nodeVal: x, left: a, right: b }), right: Node({ color: Black, nodeVal: z, left: c, right: d }) })

        # Case 5: Color Flip - If both children are Red, flip their colors to Black
        # and the parent's color to Red (unless the parent is the root, handled by `insert`).
        # This case is implicitly handled by the recursive nature; if insertHelper returns
        # a Red node into a position where its sibling is also Red, the patterns above
        # (when called on the grandparent) will trigger the necessary rotations/flips.
        # Okasaki's algorithm sometimes separates this, but it gets handled.
        # Default case: No violation detected at this node, just construct the node.
        _ -> Node({ color: Black, nodeVal, left, right })

#expect
#    myTree =
#        empty({})
#        |> insert(10)
#        |> insert(20)
#        |> insert(5)
#        |> insert(15)
#        |> insert(25)
#        |> insert(3)
#        |> insert(7)

#    (contains(myTree, 15) && !contains(myTree, 100))
