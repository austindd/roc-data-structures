module [
    AvlTreeBase,
    empty,
    insert,
    get,
    map,
    walk,
    walk_until,
    to_list,
    from_list,
]

import Ord exposing [
    Ord,
    compare,
    Ordering,
]

AvlTreeBase a b : [
    Empty,
    Node {
            l : AvlTreeBase a b,
            k : a,
            v : b,
            h : U64,
            r : AvlTreeBase a b,
        },
    Leaf {
            k : a,
            v : b,
        },
] where a implements Ord & Inspect, b implements Inspect

impossible = "Impossible"

empty : {} -> AvlTreeBase a b
empty = |{}| Empty

mknode : AvlTreeBase a b, a, b, AvlTreeBase a b -> AvlTreeBase a b
mknode = |l, k, v, r|
    Node({ l, k, v, h: 1 + Num.max(height(l), height(r)), r })

height : AvlTreeBase a b -> U64
height = |avl_tree|
    when avl_tree is
        Empty -> 0
        Leaf(_) -> 1
        Node({ h }) -> h

rotate_right : AvlTreeBase a b -> AvlTreeBase a b
rotate_right = |avl_tree|
    when avl_tree is
        Node({ l: Node({ l: t1, k: y2k, v: y2v, r: t3 }), k: y4k, v: y4v, r: t5 }) ->
            mknode(mknode(t1, y2k, y2v, t3), y4k, y4v, t5)

        _ -> crash impossible

rotate_left : AvlTreeBase a b -> AvlTreeBase a b
rotate_left = |avl_tree|
    when avl_tree is
        Node({ l: t1, k: y2k, v: y2v, r: Node({ l: t3, k: y4k, v: y4v, r: t5 }) }) ->
            mknode(t1, y2k, y2v, mknode(t3, y4k, y4v, t5))

        _ -> crash impossible

balance : AvlTreeBase a b -> AvlTreeBase a b
balance = |avl_tree|
    when avl_tree is
        Empty | Leaf(_) -> avl_tree
        Node(root_node) ->
            { l, r } = root_node
            hl = height(l)
            hr = height(r)
            if hl > hr + 2 then
                when l is
                    Empty | Leaf(_) -> crash impossible
                    Node(l_node) ->
                        { l: l_node_l, r: l_node_r } = l_node
                        if height(l_node_l) >= height(l_node_r) then
                            rotate_right(avl_tree)
                        else
                            avl_tree
            else if hr > hl + 2 then
                when r is
                    Empty | Leaf(_) -> crash impossible
                    Node(r_node) ->
                        { l: r_node_l, r: r_node_r } = r_node
                        if height(r_node_r) >= height(r_node_l) then
                            rotate_left(avl_tree)
                        else
                            avl_tree
            else
                avl_tree

insert : AvlTreeBase a b, a, b -> AvlTreeBase a b
insert = |avl_tree, key, value|
    when avl_tree is
        Empty -> Leaf({ k: key, v: value })
        Leaf({ k }) ->
            when compare(key, k) is
                EQ -> Leaf({ k, v: value })
                LT -> balance(mknode(Empty, key, value, avl_tree))
                GT -> balance(mknode(avl_tree, key, value, Empty))

        Node({ l, k, v, r }) ->
            when compare(key, k) is
                EQ -> mknode(l, k, value, r)
                LT ->
                    new_left = insert(l, key, value)
                    mknode(new_left, k, v, r)

                GT ->
                    new_right = insert(r, key, value)
                    mknode(l, k, v, new_right)

get : AvlTreeBase a b, a -> Result b {}
get = |avl_tree, key|
    when avl_tree is
        Empty -> Err {}
        Leaf({ k, v }) ->
            when compare(key, k) is
                EQ -> Ok(v)
                LT | GT -> Err {}

        Node({ l, k, v, r }) ->
            when compare(key, k) is
                EQ -> Ok(v)
                LT -> get(l, key)
                GT -> get(r, key)

map : AvlTreeBase a b, (b -> c) -> AvlTreeBase a c
map = |avl_tree, fn|
    when avl_tree is
        Empty -> empty {}
        Leaf({ k, v }) -> Leaf({ k, v: fn(v) })
        Node({ l, k, v, r }) ->
            mknode(map(l, fn), k, fn(v), map(r, fn))

walk : AvlTreeBase a b, state, (state, a, b -> state) -> state
walk = |avl_tree, state, fn|
    when avl_tree is
        Empty -> state
        Leaf({ k, v }) -> fn(state, k, v)
        Node({ l, k, v, r }) ->
            l_state = walk(l, state, fn)
            this_state = fn(l_state, k, v)
            r_state = walk(r, this_state, fn)
            r_state

walk_until : AvlTreeBase a b, state, (state, a, b -> [Continue state, Break state]) -> state
walk_until = |avl_tree, state, fn|
    when avl_tree is
        Empty -> state
        Leaf({ k, v }) ->
            when fn(state, k, v) is
                Continue(new_state) -> new_state
                Break(new_state) -> new_state

        Node({ l, k, v, r }) ->
            l_state = walk_until(l, state, fn)
            this_state = fn(l_state, k, v)
            when this_state is
                Continue(new_state) -> walk_until(r, new_state, fn)
                Break(new_state) -> new_state

to_list : AvlTreeBase a b -> List (a, b)
to_list = |avl_tree|
    walk(
        avl_tree,
        [],
        |list, key, value|
            List.append(list, (key, value)),
    )

Entry a b := (a, b) implements [
        Ord {
            compare: entry_compare,
        },
    ]

entry_compare : Entry k v, Entry k v -> Ordering where k implements Ord
entry_compare = |@Entry(a), @Entry(b)|
    (k1, _) = a
    (k2, _) = b
    compare(k1, k2)

entry_compare_2 : (k, v), (k, v) -> Ordering where k implements Ord
entry_compare_2 = |(a, _), (b, _)|
    compare(a, b)

from_list : List (a, b) -> AvlTreeBase a b where a implements Ord & Inspect
from_list = |pairs|
    sorted_pairs = pairs |> List.sort_with(entry_compare_2)
    sorted_pairs_length = List.len(sorted_pairs)
    if (sorted_pairs_length < 1) then
        Empty
    else
        from_sorted_list(sorted_pairs)

from_sorted_list : List (a, b) -> AvlTreeBase a b
from_sorted_list = |list|
    list_length = List.len(list)
    when list is
        [] -> Empty
        [(k, v)] -> Leaf({ k, v })
        _ ->
            mid = Num.ceiling(Num.to_frac(list_length) / 2)
            when List.get(list, mid) is
                Err(OutOfBounds) -> Empty
                Ok((mid_key, mid_value)) ->
                    left_node = list |> List.take_first(mid) |> from_sorted_list
                    right_node = list |> List.drop_first(mid + 1) |> from_sorted_list
                    node = mknode(
                        left_node,
                        mid_key,
                        mid_value,
                        right_node,
                    )
                    node
