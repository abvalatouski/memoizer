# An alternative to representable functors

The library provides a `Memoizer` type class similar to `Representable`
(from [`adjunctions`](https://hackage.haskell.org/package/adjunctions))
that is suitable for containers with size known only at runtime. The only difference between
those type classes is that `Memoizer` passes helper information to the `tabulate` function.

```haskell
{-# LANGUAGE TypeFamilies #-}

class Memoizer t where
    -- Rep t
    type Arg t

    -- Helps memoizer to know all possible function inputs.
    type DomainHint t

    -- tabulate
    remember :: (Arg t -> a) -> DomainHint t -> t a

    -- index
    recall      :: t a -> Arg t ->       a
    recallMaybe :: t a -> Arg t -> Maybe a

    {-# MINIMAL remember, (recall | recallMaybe) #-}
```

## Instances

| Data type                       | `Arg`                      | `DomainHint`                                          | Does support unsafe indexing? |
| ------------------------------- | -------------------------- | ----------------------------------------------------- | ----------------------------- |
| any `Representable` `Functor`   | `Representable f => Rep f` | `()` (none)                                           |                               |
| `Array`, `UArray`               | `Ix i => i`                | `Ix i => (i, i)` (range of the array)                 | Yes                           |
| boxed/unboxed/storable `Vector` | `Int`                      | `Int` (length of the array)                           | Yes                           |
| `HashMap`                       | `(Hashable k, Eq k) => k`  | `(Hashable k, Eq k) => [k]` (all the function inputs) |                               |
| `Map`                           | `Ord k => k`               | `Ord k => [k]` (all the function inputs)              |                               |
| `IntMap`                        | `Int`                      | `[Int]` (all the function inputs)                     |                               |

## `Store` comonad

The library also provides `StoreT` comonad transformer that uses `Memoizer` under the hood.

```haskell
data StoreT memoizer w a
    = StoreT (w (memoizer a)) (DomainHint memoizer) (Arg memoizer)
```

## Is there a documentation?

The source code has a few Haddock documentation, so you can run `stack haddock` to render it.
