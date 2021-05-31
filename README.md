# An alternative to representable functors

The library provides a `Memoizer` type class similar to `Representable`
from [`adjunctions`](https://hackage.haskell.org/package/adjunctions) package
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
    apply :: t a -> Arg t -> a

    -- tabulate
    memoize :: (Arg t -> a) -> DomainHint t -> t a
```

Currently there are instances for `Vector`, `HashMap`
(from [`unordered-containers`](https://hackage.haskell.org/package/unordered-containers) package),
and all `Representable` functors (wrapped into a `newtype`).

## `Store` comonad

The library also provides `Store` comonad that uses `Memoizer` under the hood.

```haskell
data Store memoizer a
    = Store (memoizer a) (DomainHint memoizer) (Arg memoizer)
```

Currently there is no transformer.
