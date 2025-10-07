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
