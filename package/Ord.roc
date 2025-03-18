module [
    Ordering,
    Ord,
    compare,
]

Ordering : [EQ, LT, GT]

Ord implements
    compare : a, a -> Ordering where a implements Ord
