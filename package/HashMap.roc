module [
    HashMap,
    empty,
    insert,
    get,
    remove,
    map_values,
    walk,
    walk_until,
    to_list,
    from_list,
    is_empty,
    len,
]

## A simple hash map implementation using buckets.
## Uses I64 keys and a bucket-based approach.
HashMap v := {
    buckets : List (List (I64, v)),
    count : U64,
}

bucket_count : U64
bucket_count = 64

## Hash a numeric key to get a bucket index.
hash_key : I64 -> U64
hash_key = |key|
    u = Num.to_u64(key)
    # Simple hash mixing
    mixed = Num.bitwise_xor(u, Num.shift_right_by(u, 32u8))
    Num.rem(mixed, bucket_count)

## Creates an empty hash map.
empty : {} -> HashMap v
empty = |{}|
    @HashMap({ buckets: List.repeat([], bucket_count), count: 0 })

## Checks if the hash map is empty.
is_empty : HashMap v -> Bool
is_empty = |@HashMap(map)|
    map.count == 0

## Inserts a key-value pair into the hash map.
insert : HashMap v, I64, v -> HashMap v
insert = |@HashMap(map), key, value|
    bucket_idx = hash_key(key)

    when List.get(map.buckets, bucket_idx) is
        Ok(bucket) ->
            # Check if key exists
            (new_bucket, was_update) = insert_in_bucket(bucket, key, value)
            new_buckets = List.set(map.buckets, bucket_idx, new_bucket)
            new_count = if was_update then map.count else map.count + 1
            @HashMap({ buckets: new_buckets, count: new_count })

        Err(OutOfBounds) ->
            # Should never happen with fixed bucket count
            @HashMap(map)

insert_in_bucket : List (I64, v), I64, v -> (List (I64, v), Bool)
insert_in_bucket = |bucket, key, value|
    (new_bucket, found) = List.walk(
        bucket,
        ([], Bool.false),
        |(acc, was_found), (k, v)|
            if k == key then
                (List.append(acc, (key, value)), Bool.true)
            else
                (List.append(acc, (k, v)), was_found),
    )

    if found then
        (new_bucket, Bool.true)
    else
        (List.append(bucket, (key, value)), Bool.false)

## Retrieves the value associated with a key.
get : HashMap v, I64 -> Result v {}
get = |@HashMap(map), key|
    bucket_idx = hash_key(key)

    when List.get(map.buckets, bucket_idx) is
        Ok(bucket) ->
            List.walk_until(
                bucket,
                Err {},
                |_, (k, v)|
                    if k == key then
                        Break(Ok(v))
                    else
                        Continue(Err {}),
            )

        Err(OutOfBounds) ->
            Err {}

## Removes a key-value pair from the hash map.
remove : HashMap v, I64 -> HashMap v
remove = |@HashMap(map), key|
    bucket_idx = hash_key(key)

    when List.get(map.buckets, bucket_idx) is
        Ok(bucket) ->
            new_bucket = List.drop_if(bucket, |(k, _)| k == key)
            removed_count = List.len(bucket) - List.len(new_bucket)
            new_buckets = List.set(map.buckets, bucket_idx, new_bucket)
            new_count = Num.sub_saturated(map.count, removed_count)
            @HashMap({ buckets: new_buckets, count: new_count })

        Err(OutOfBounds) ->
            @HashMap(map)

## Transforms all values in the hash map.
map_values : HashMap v, (v -> w) -> HashMap w
map_values = |@HashMap(hash_map), fn|
    new_buckets = List.map(
        hash_map.buckets,
        |bucket| List.map(bucket, |(k, v)| (k, fn(v))),
    )
    @HashMap({ buckets: new_buckets, count: hash_map.count })

## Iterates over all key-value pairs, accumulating a state.
walk : HashMap v, state, (state, I64, v -> state) -> state
walk = |@HashMap(map), initial_state, fn|
    List.walk(
        map.buckets,
        initial_state,
        |state, bucket|
            List.walk(bucket, state, |s, (k, v)| fn(s, k, v)),
    )

## Iterates over all key-value pairs with early termination support.
walk_until : HashMap v, state, (state, I64, v -> [Continue state, Break state]) -> state
walk_until = |@HashMap(map), initial_state, fn|
    List.walk_until(
        map.buckets,
        initial_state,
        |state, bucket|
            result = List.walk_until(bucket, state, |s, (k, v)| fn(s, k, v))
            Continue(result),
    )

## Converts the hash map to a list of key-value pairs.
to_list : HashMap v -> List (I64, v)
to_list = |@HashMap(map)|
    to_list_internal(map)

to_list_internal : { buckets : List (List (I64, v)), count : U64 } -> List (I64, v)
to_list_internal = |map|
    List.walk(
        map.buckets,
        [],
        |acc, bucket|
            List.concat(acc, bucket),
    )

## Creates a hash map from a list of key-value pairs.
from_list : List (I64, v) -> HashMap v
from_list = |list|
    List.walk(list, empty({}), |acc, (k, v)| insert(acc, k, v))

## Returns the number of key-value pairs in the hash map.
len : HashMap v -> U64
len = |@HashMap(map)|
    map.count

# Tests

expect # Empty hash map
    map = empty({})
    is_empty(map)

expect # Single insert and get
    map = empty({})
        |> insert(42, "answer")
    get(map, 42) == Ok("answer")

expect # Get non-existent key
    map = empty({})
        |> insert(1, "a")
    get(map, 2) == Err {}

expect # Update existing key
    map = empty({})
        |> insert(5, "first")
        |> insert(5, "second")
    get(map, 5) == Ok("second")

expect # Multiple inserts
    map = empty({})
        |> insert(1, "a")
        |> insert(2, "b")
        |> insert(3, "c")
    get(map, 1) == Ok("a") &&
    get(map, 2) == Ok("b") &&
    get(map, 3) == Ok("c")

expect # Negative numbers
    map = empty({})
        |> insert(-5, "neg five")
        |> insert(0, "zero")
        |> insert(5, "pos five")
    get(map, -5) == Ok("neg five") &&
    get(map, 0) == Ok("zero") &&
    get(map, 5) == Ok("pos five")

expect # Remove key
    map = empty({})
        |> insert(1, "a")
        |> insert(2, "b")
        |> remove(1)
    get(map, 1) == Err {} &&
    get(map, 2) == Ok("b")

expect # Map transforms values
    hash_map = empty({})
        |> insert(1, 5)
        |> insert(2, 10)
    mapped = map_values(hash_map, \x -> x * 2)
    get(mapped, 1) == Ok(10) &&
    get(mapped, 2) == Ok(20)

expect # Walk accumulates
    map = empty({})
        |> insert(1, 10)
        |> insert(2, 20)
        |> insert(3, 30)
    sum = walk(map, 0, |acc, _k, v| acc + v)
    sum == 60

expect # walk_until can break early
    map = empty({})
        |> insert(1, 1)
        |> insert(2, 2)
        |> insert(3, 3)
    result = walk_until(map, 0, |acc, _k, v|
        if v > 1 && acc == 0 then
            Break(v)
        else
            Continue(acc)
    )
    result > 0

expect # to_list returns all entries
    map = empty({})
        |> insert(1, "a")
        |> insert(2, "b")
    list = to_list(map)
    List.len(list) == 2

expect # from_list creates hash map
    list = [(1, "a"), (2, "b"), (3, "c")]
    map = from_list(list)
    get(map, 1) == Ok("a") &&
    get(map, 2) == Ok("b") &&
    get(map, 3) == Ok("c")

expect # len counts entries
    map = empty({})
        |> insert(1, "a")
        |> insert(2, "b")
        |> insert(3, "c")
    len(map) == 3

expect # Large hash map
    map = List.range({ start: At 1, end: At 100 })
        |> List.walk(empty({}), |acc, i| insert(acc, i, i * 2))
    get(map, 1) == Ok(2) &&
    get(map, 50) == Ok(100) &&
    get(map, 100) == Ok(200) &&
    len(map) == 100

expect # Sequential removes
    map = List.range({ start: At 1, end: At 20 })
        |> List.walk(empty({}), |acc, i| insert(acc, i, i))
    reduced = List.range({ start: At 1, end: At 10 })
        |> List.walk(map, |acc, i| remove(acc, i))
    len(reduced) == 10 &&
    get(reduced, 5) == Err {} &&
    get(reduced, 15) == Ok(15)

expect # Large keys
    map = empty({})
        |> insert(0, "zero")
        |> insert(255, "max")
        |> insert(128, "mid")
    get(map, 0) == Ok("zero") &&
    get(map, 255) == Ok("max") &&
    get(map, 128) == Ok("mid")

expect # Walk collects keys
    map = empty({})
        |> insert(3, "c")
        |> insert(1, "a")
        |> insert(2, "b")
    keys = walk(map, [], |acc, k, _v| List.append(acc, k))
    sorted_keys = List.sort_asc(keys)
    sorted_keys == [1, 2, 3]

expect # from_list with duplicates (last wins)
    list = [(1, "first"), (2, "b"), (1, "second")]
    map = from_list(list)
    get(map, 1) == Ok("second") &&
    len(map) == 2

expect # Chained operations
    map = empty({})
        |> insert(1, 10)
        |> insert(2, 20)
        |> insert(3, 30)
        |> remove(2)
        |> insert(4, 40)
    len(map) == 3 &&
    get(map, 2) == Err {} &&
    get(map, 4) == Ok(40)

expect # Stress test
    map = List.range({ start: At 1, end: At 500 })
        |> List.walk(empty({}), |acc, i| insert(acc, i, Num.to_str(i)))
    get(map, 1) == Ok("1") &&
    get(map, 250) == Ok("250") &&
    get(map, 500) == Ok("500") &&
    len(map) == 500
