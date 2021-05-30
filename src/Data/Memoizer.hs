{-# LANGUAGE TypeFamilies #-}

-- |
-- Module : Data.Memoizer
--
-- Provides an alternative to @Representable@ 'Functor'
-- from [@adjunctions@](https://hackage.haskell.org/package/adjunctions) package, that is suitable
-- for containers with size known only at runtime. The only difference between those type classes
-- is that `Memoizer` passes helper information to the `tabulate` function.
module Data.Memoizer
    ( Memoizer
        ( Arg
        , DomainHint
        , apply
        , memoize
        )
    , Memoized
    )
  where

import           Data.Hashable       (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Vector         (Vector)
import qualified Data.Vector         as Vector

-- | Memoizes a particular function.
--
--   Similar to @Representable@.
class Memoizer t where
    -- | Argument of the function.
    --
    --   Similar to @Rep t@.
    type Arg t

    -- Helps memoizer to know all possible function inputs.
    type DomainHint t

    -- | Applies the function to the argument.
    --
    --   Similar to @index@.
    apply :: t a -> Arg t -> a

    -- | Memoizes the function.
    --
    --   Similar to @tabulate@.
    memoize :: (Arg t -> a) -> DomainHint t -> t a

-- | Uses length of the 'Vector' as its 'DomainHint'.
instance Memoizer Vector where
    type Arg        Vector = Int
    type DomainHint Vector = Int

    apply   = (Vector.!)
    memoize = flip Vector.generate

instance (Eq k, Hashable k) => Memoizer (HashMap k) where
    type Arg        (HashMap k) = k
    type DomainHint (HashMap k) = [k]

    apply        = (HashMap.!)
    memoize f ks = let vs = map f ks in HashMap.fromList $ zip ks vs

-- | Memoized functor.
--
--   Used to shorten some constraints.
class (Functor f, Memoizer f) => Memoized f
