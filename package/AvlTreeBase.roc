module [
    AvlTreeBase,
    empty,
    insert,
    get,
    map,
    walk_entries,
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

walk_entries : AvlTreeBase a b, state, (state, a, b -> state) -> state
walk_entries = |avl_tree, state, fn|
    when avl_tree is
        Empty -> state
        Leaf({ k, v }) -> fn(state, k, v)
        Node({ l, k, v, r }) ->
            l_state = walk_entries(l, state, fn)
            this_state = fn(l_state, k, v)
            r_state = walk_entries(r, this_state, fn)
            r_state

to_list : AvlTreeBase a b -> List (a, b)
to_list = |avl_tree|
    walk_entries(
        avl_tree,
        [],
        |list, key, value|
            List.append(list, (key, value)),
    )

build_balanced_tree : List (Entry a b), U64, U64 -> AvlTreeBase a b
build_balanced_tree = |items, start, end|
    if start >= end then
        Empty
    else
        when (end - start) is
            0 -> Empty
            1 ->
                @Entry((k, v)) =
                    when List.get(items, start) is
                        Err(_) -> crash impossible
                        Ok(value) -> value
                Leaf({ k, v })

            2 ->
                @Entry((k1, v1)) =
                    when List.get(items, start) is
                        Err(_) -> crash impossible
                        Ok(value) -> value
                @Entry((k2, v2)) =
                    when List.get(items, start + 1) is
                        Err(_) -> crash impossible
                        Ok(value) -> value
                mknode(Leaf({ k: k1, v: v1 }), k2, v2, Empty)

            3 ->
                @Entry((k1, v1)) =
                    when List.get(items, start) is
                        Err(_) -> crash impossible
                        Ok(value) -> value
                @Entry((k2, v2)) =
                    when List.get(items, start + 1) is
                        Err(_) -> crash impossible
                        Ok(value) -> value
                @Entry((k3, v3)) =
                    when List.get(items, start + 2) is
                        Err(_) -> crash impossible
                        Ok(value) -> value
                mknode(Leaf({ k: k1, v: v1 }), k2, v2, Leaf({ k: k3, v: v3 }))

            _ ->
                mid = start + Num.ceiling(Num.to_frac(end - start) / 2)
                @Entry((k, v)) =
                    when List.get(items, mid) is
                        Err(_) -> crash impossible
                        Ok(value) -> value
                node = mknode(
                    build_balanced_tree(items, start, mid - 1),
                    k,
                    v,
                    build_balanced_tree(items, mid + 1, end),
                )
                node

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

from_list : List (a, b) -> AvlTreeBase a b where a implements Ord
from_list = |pairs|
    sorted_pairs = pairs |> List.map(@Entry) |> List.sort_with(entry_compare)
    sorted_pairs_length = List.len(sorted_pairs)
    if (sorted_pairs_length < 1) then
        Empty
    else
        build_balanced_tree(sorted_pairs, 0, List.len(sorted_pairs) - 1)
