module [
    ListMap,
    insert,
    get,
    map,
    walk,
    to_list,
    from_list,
]

ListMap a b := {
    count : U16,
    list : List (Result (a, b) {}),
} where a implements Eq
    implements [
        Inspect {
            to_inspector: list_map_to_inspector,
        },
    ]

list_map_to_inspector : ListMap a b -> Inspector _ where a implements Eq & Inspect, b implements Inspect
list_map_to_inspector = |@ListMap(list_map)|
    Inspect.to_inspector(list_map)

compaction_threshold : U16
compaction_threshold = 255

maybe_compact2 : List (Result (a, b) {}), U16 -> List (Result (a, b) {})
maybe_compact2 = |list, count|
    if count > compaction_threshold then
        List.keep_if(list, Result.is_ok)
    else
        list

remove_entry_if_exists2 : List (Result (a, b) {}), a -> List (Result (a, b) {}) where a implements Eq
remove_entry_if_exists2 = |list, key|
    List.drop_if(
        list,
        |result|
            when result is
                Ok((k, _)) -> k == key
                Err(_) -> Bool.false,
    )

insert : ListMap a b, a, b -> ListMap a b
insert = |@ListMap(list_map), key, value|
    new_list =
        list_map.list
        |> remove_entry_if_exists2(key)
        |> maybe_compact2(list_map.count)
        |> List.append(Ok((key, value)))

    @ListMap(
        {
            count: (
                if list_map.count > compaction_threshold then
                    0
                else
                    list_map.count + 1
            ),
            list: new_list,
        },
    )

get : ListMap a b, a -> Result b {}
get = |@ListMap(list_map), key|
    list_map.list
    |> List.walk_until(
        Err({}),
        |_, result|
            when result is
                Err(_) -> Continue(Err {})
                Ok((k, v)) ->
                    if k == key then
                        Break(Ok(v))
                    else
                        Continue(Err({})),
    )

walk : ListMap a b, state, (state, a, b -> state) -> state
walk = |@ListMap(list_map), state, fn|
    list_map.list
    |> List.walk(
        state,
        |s, result|
            when result is
                Ok((k, v)) -> fn(s, k, v)
                Err(_) -> s,
    )

to_list : ListMap a b -> List (a, b)
to_list = |@ListMap(list_map)|
    List.keep_oks(list_map.list, |result| result)

from_list : List (a, b) -> ListMap a b where a implements Eq
from_list = |list|
    @ListMap(
        {
            count: 0,
            list: List.map(list, |pair| Ok(pair)),
        },
    )

map : ListMap a b, (b -> c) -> ListMap a c
map = |@ListMap(list_map), fn|
    @ListMap(
        {
            count: list_map.count,
            list: List.map(
                list_map.list,
                |result|
                    when result is
                        Ok((k, v)) -> Ok((k, fn(v)))
                        Err({}) -> Err({}),
            ),
        },
    )

# Tests

expect # Insert and get
    lm = from_list([])
        |> insert(1, "one")
        |> insert(2, "two")
    get(lm, 1) == Ok("one") &&
    get(lm, 2) == Ok("two")

expect # Get non-existent key
    lm = from_list([(1, "a")])
    get(lm, 2) == Err {}

expect # Update existing key
    lm = from_list([])
        |> insert(5, "first")
        |> insert(5, "second")
    get(lm, 5) == Ok("second")

expect # to_list and from_list
    list = [(1, "a"), (2, "b"), (3, "c")]
    lm = from_list(list)
    result = to_list(lm)
    # Order may vary since ListMap doesn't guarantee order
    List.len(result) == 3

expect # map transforms values
    initial = from_list([(1, 10), (2, 20), (3, 30)])
    mapped = map(initial, \x -> x * 2)
    get(mapped, 1) == Ok(20) &&
    get(mapped, 2) == Ok(40) &&
    get(mapped, 3) == Ok(60)

expect # walk accumulates
    initial = from_list([(1, 10), (2, 20), (3, 30)])
    sum = walk(initial, 0, |acc, _k, v| acc + v)
    sum == 60

expect # Empty map operations
    lm = from_list([])
    get(lm, 1) == Err {} &&
    to_list(lm) == []

expect # Single element operations
    lm = from_list([(42, "answer")])
    get(lm, 42) == Ok("answer") &&
    List.len(to_list(lm)) == 1

expect # Update same key many times
    lm = from_list([])
        |> insert(1, "a")
        |> insert(1, "b")
        |> insert(1, "c")
        |> insert(1, "d")
        |> insert(1, "e")
    get(lm, 1) == Ok("e")

expect # Many unique keys
    lm = List.range({ start: At 1, end: At 50 })
        |> List.walk(from_list([]), |acc, i| insert(acc, i, i * 10))
    get(lm, 1) == Ok(10) &&
    get(lm, 25) == Ok(250) &&
    get(lm, 50) == Ok(500)

expect # Update many keys multiple times
    lm = from_list([])
        |> insert(1, "a")
        |> insert(2, "b")
        |> insert(3, "c")
        |> insert(1, "A")
        |> insert(2, "B")
        |> insert(3, "C")
    get(lm, 1) == Ok("A") &&
    get(lm, 2) == Ok("B") &&
    get(lm, 3) == Ok("C")

expect # to_list with multiple elements
    lm = from_list([(1, "a"), (2, "b"), (3, "c")])
    result = to_list(lm)
    List.len(result) == 3

expect # from_list with duplicates
    lm = from_list([(1, "first"), (2, "b"), (1, "second")])
    # from_list doesn't deduplicate, both entries are stored
    result = to_list(lm)
    List.len(result) == 3

expect # map on empty map
    lm = from_list([])
    mapped = map(lm, \x -> x * 2)
    to_list(mapped) == []

expect # map changes value types
    lm = from_list([(1, "a"), (2, "bb"), (3, "ccc")])
    lengths = map(lm, Str.count_utf8_bytes)
    get(lengths, 1) == Ok(1) &&
    get(lengths, 2) == Ok(2) &&
    get(lengths, 3) == Ok(3)

expect # walk on empty map
    lm = from_list([])
    result = walk(lm, 0, |acc, _k, _v| acc + 1)
    result == 0

expect # walk counts elements
    lm = from_list([(1, "a"), (2, "b"), (3, "c"), (4, "d")])
    count = walk(lm, 0, |acc, _k, _v| acc + 1)
    count == 4

expect # Get non-existent keys
    lm = from_list([(1, "a"), (3, "c"), (5, "e")])
    get(lm, 2) == Err {} &&
    get(lm, 4) == Err {} &&
    get(lm, 6) == Err {}

expect # Large map with many updates
    lm = List.range({ start: At 1, end: At 100 })
        |> List.walk(from_list([]), |acc, i| insert(acc, i, i))
    # Update every key
    lm2 = List.range({ start: At 1, end: At 100 })
        |> List.walk(lm, |acc, i| insert(acc, i, i * 2))
    get(lm2, 50) == Ok(100)

expect # Stress test with many inserts
    lm = List.range({ start: At 1, end: At 300 })
        |> List.walk(from_list([]), |acc, i| insert(acc, i, Num.to_str(i)))
    get(lm, 1) == Ok("1") &&
    get(lm, 150) == Ok("150") &&
    get(lm, 300) == Ok("300")

expect # Map after many operations
    lm = from_list([])
        |> insert(1, 10)
        |> insert(2, 20)
        |> insert(3, 30)
        |> insert(1, 15)  # Update
        |> insert(2, 25)  # Update
    doubled = map(lm, \x -> x * 2)
    get(doubled, 1) == Ok(30) &&
    get(doubled, 2) == Ok(50)

expect # walk accumulates keys
    lm = from_list([(1, "a"), (2, "b"), (3, "c")])
    key_sum = walk(lm, 0, |acc, k, _v| acc + k)
    key_sum == 6

expect # to_list returns all elements
    lm = from_list([(5, "e"), (3, "c"), (1, "a"), (4, "d"), (2, "b")])
    result = to_list(lm)
    List.len(result) == 5

expect # Chained map operations
    lm = from_list([(1, 5), (2, 10)])
    result = lm
        |> map(\x -> x * 2)
        |> map(\x -> x + 1)
    get(result, 1) == Ok(11) &&
    get(result, 2) == Ok(21)

expect # Insert then walk
    lm = from_list([])
        |> insert(10, "ten")
        |> insert(20, "twenty")
        |> insert(30, "thirty")
    concat = walk(lm, "", |acc, _k, v| Str.concat(acc, v))
    Str.count_utf8_bytes(concat) > 0  # Has content

expect # Update overwrites previous value
    lm = from_list([(1, "first")])
        |> insert(1, "second")
        |> insert(1, "third")
    list = to_list(lm)
    List.len(list) == 1  # Only one entry
