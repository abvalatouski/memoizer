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

    -- index
    recall :: t a -> Arg t -> a

    -- tabulate
    memoize :: (Arg t -> a) -> DomainHint t -> t a
```

Currently there are instances for:
- all `Representable` functors (wrapped into a `newtype`);
- `Array` (from [`array`](https://hackage.haskell.org/package/array));
- `UArray` (from [`array`](https://hackage.haskell.org/package/array));
- boxed `Vector` (from [`vector`](https://hackage.haskell.org/package/vector));
- unboxed `Vector` (from [`vector`](https://hackage.haskell.org/package/vector));
- storable `Vector` (from [`vector`](https://hackage.haskell.org/package/vector));
- boxed `Vector` (from [`vector`](https://hackage.haskell.org/package/vector))
  with unsafe indexing;
- unboxed `Vector` (from [`vector`](https://hackage.haskell.org/package/vector))
  with unsafe indexing;
- storable `Vector` (from [`vector`](https://hackage.haskell.org/package/vector))
  with unsafe indexing;
- `HashMap`
  (from [`unordered-containers`](https://hackage.haskell.org/package/unordered-containers));
- `Map` (from [`containers`](https://hackage.haskell.org/package/containers));
- `IntMap` (from [`containers`](https://hackage.haskell.org/package/containers)).

## `Store` comonad

The library also provides `StoreT` comonad transformer that uses `Memoizer` under the hood.

```haskell
data StoreT memoizer w a
    = StoreT (w (memoizer a)) (DomainHint memoizer) (Arg memoizer)
```

## Is there a documentation?

The source code has a few Haddock-documentation, so you can run `stack haddock` to render it.
