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

    -- Helps memoizer to know all possible function results.
    type DomainHint t

    -- index
    apply :: t a -> Arg t -> t a

    -- tabulate
    memoize :: (Arg t -> a) -> DomainHint t -> t a
```

Currently there are instances for `Vector` and `HashMap`
(from [`unordered-containers`](https://hackage.haskell.org/package/unordered-containers) package).

## `Store` comonad

The library also provides `Store` comonad that uses `Memoizer` under the hood.

```haskell
data Store memoizer a
    = Store (memoizer a) (DomainHint a) (Arg a)
```

Currently there is no transformer.
