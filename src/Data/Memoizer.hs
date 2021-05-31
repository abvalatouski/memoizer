{-# LANGUAGE TypeFamilies #-}

-- |
-- Module : Data.Memoizer
--
-- Provides an alternative to 'Representable' 'Functor's
-- (from [@adjunctions@](https://hackage.haskell.org/package/adjunctions)), that is suitable
-- for containers with size known only at runtime. The only difference between those type classes
-- is that `Memoizer` passes helper information to the `tabulate` function.
module Data.Memoizer
    ( -- * Type class
      Memoizer
        ( Arg
        , DomainHint
        , apply
        , memoize
        )
    , Memoizing

      -- * Wrapping 'Representable' 'Functor's
    , WrappedRepresentable
        ( WrapRepresentable
        , getWrappedRepresentable
        )
    )
  where

import           Data.Functor.Rep    (Representable (..))
import           Data.Hashable       (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.IntMap         (IntMap)
import qualified Data.IntMap         as IntMap
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Vector         (Vector)
import qualified Data.Vector         as Vector

-- | Memoizes a particular function.
--
--   Similar to 'Representable'.
class Memoizer t where
    -- | Argument of the function.
    --
    --   Similar to 'Rep'.
    type Arg t

    -- | Helps memoizer to know all possible function inputs (i.e. /domain/).
    --
    --   Generally speaking, it should be represented as a list of values. However, in some cases,
    --   the list can be stored /implicitly/. For example, an array of values, as a function
    --   from an integer to something, can figure out its inputs, knowing only the amount of them.
    --   Also all the 'Representable' 'Functor's already know their inputs, so they define
    --   'DomainHint' as @()@.
    type DomainHint t

    -- | Applies the function to the argument.
    --
    --   Similar to 'index'.
    apply :: t a -> Arg t -> a

    -- | Memoizes the function.
    --
    --   Similar to 'tabulate'.
    memoize :: (Arg t -> a) -> DomainHint t -> t a

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

instance Ord k => Memoizer (Map k) where
    type Arg        (Map k) = k
    type DomainHint (Map k) = [k]

    apply        = (Map.!)
    memoize f ks = let vs = map f ks in Map.fromList $ zip ks vs

instance Memoizer IntMap where
    type Arg        IntMap = Int
    type DomainHint IntMap = [Int]

    apply        = (IntMap.!)
    memoize f ks = let vs = map f ks in IntMap.fromList $ zip ks vs

-- | Used to derive 'Memoizer' instances for all 'Representable' 'Functor's.
newtype WrappedRepresentable f a = WrapRepresentable
    { getWrappedRepresentable :: f a
    }

instance Representable f => Memoizer (WrappedRepresentable f) where
    type Arg        (WrappedRepresentable f) = Rep f
    type DomainHint (WrappedRepresentable f) = ()

    apply   = index . getWrappedRepresentable
    memoize = const . WrapRepresentable . tabulate

-- | Memoizing functor.
--
--   Used when is too boring to write two constraints instead of one.
class (Functor f, Memoizer f) => Memoizing f
