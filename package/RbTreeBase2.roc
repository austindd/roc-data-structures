module [
    RbTreeBase,
    empty,
    insert,
    contains,
]

import Ord exposing [Ord, compare, Ordering]

Color : [Red, Black]

RbTreeBase a : [
    Empty,
    Node {
            color : Color,
            nodeVal : a,
            left : RbTreeBase a,
            right : RbTreeBase a,
        },
]

empty : {} -> RbTreeBase a
empty = |{}| Empty

contains : RbTreeBase a, a -> Bool where a implements Ord
contains = |tree, value|
    when tree is
        Empty ->
            Bool.false

        Node({ nodeVal, left, right }) ->
            when compare(value, nodeVal) is
                LT ->
                    contains(left, value)

                GT ->
                    contains(right, value)

                EQ ->
                    Bool.true

insert : a, RbTreeBase a -> RbTreeBase a where a implements Ord
insert = |value, tree|
    newTree = insertHelper(value, tree)
    when newTree is
        Node({ color, nodeVal, left, right }) -> Node({ color: Black, nodeVal, left, right }) # Force root to Black
        Empty -> Empty # Should not happen if insertHelper is correct, but handle defensively

insertHelper : a, RbTreeBase a -> RbTreeBase a where a implements Ord
insertHelper = |value, tree|
    when tree is
        Empty ->
            Node({ color: Red, nodeVal: value, left: Empty, right: Empty })

        Node({ color, nodeVal, left, right }) ->
            comparison = compare(value, nodeVal)
            when comparison is
                LT ->
                    newLeft = insertHelper(value, left)
                    balance(color, nodeVal, newLeft, right)

                GT ->
                    newRight = insertHelper(value, right)
                    balance(color, nodeVal, left, newRight)

                EQ ->
                    Node({ color, nodeVal, left, right })

balance : Color, a, RbTreeBase a, RbTreeBase a -> RbTreeBase a where a implements Ord
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
