{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
-- Module : Data.Memoizer
--
-- Provides an alternative to 'Representable' 'Functor's
-- (from [@adjunctions@](https://hackage.haskell.org/package/adjunctions)), that is suitable
-- for containers with size known only at runtime. The only difference between those type classes
-- is that `Memoizer` passes helper information to the `tabulate` function.
module Data.Memoizer
    ( -- * Type classes
      Memoizer
        ( Arg
        , DomainHint
        , apply
        , memoize
        )
    , Memoizing

      -- * Helper functions
    , memoize'

      -- * Type wrappers
    , WrappedRepresentable
        ( WrapRepresentable
        , getWrappedRepresentable
        )
    , Unsafe
        ( Unsafe
        , getUnsafe
        )
    )
  where

import           GHC.Exts             (IsList (Item, fromList))

import           Control.DeepSeq      (NFData, force)
import           Data.Array           (Array)
import           Data.Array.IArray    (IArray, Ix, array)
import qualified Data.Array.IArray    as IArray
import           Data.Array.Unboxed   (UArray)
import           Data.Functor.Rep     (Representable (..))
import           Data.Hashable        (Hashable)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HashMap
import           Data.IntMap          (IntMap)
import qualified Data.IntMap          as IntMap
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Vector          (Vector)
import qualified Data.Vector.Generic  as Generic (Vector)
import qualified Data.Vector.Generic  as Generic.Vector
import qualified Data.Vector.Storable as Storable (Vector)
import qualified Data.Vector.Unboxed  as Unboxed (Vector)

-- Type classes.

-- | Memoizes a particular function.
--
--   Similar to 'Representable'.
--
--   NOTE:
--   Almost all instances of that type class does not satisfy the laws of 'Representable'.
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

-- | Defines 'DomainHint' as pair of indices.
instance (forall e. IArray Array e, Ix i) => Memoizer (Array i) where
    type Arg        (Array i) = i
    type DomainHint (Array i) = (i, i)

    apply   = (IArray.!)
    memoize = memoizeArray

-- | Defines 'DomainHint' as pair of indices.
instance (forall e. IArray UArray e, Ix i) => Memoizer (UArray i) where
    type Arg        (UArray i) = i
    type DomainHint (UArray i) = (i, i)

    apply   = (IArray.!)
    memoize = memoizeArray

-- | Defines 'DomainHint' as 'Int' (length of the 'Vector').
instance Memoizer Vector where
    type Arg        Vector = Int
    type DomainHint Vector = Int

    apply   = (Generic.Vector.!)
    memoize = flip Generic.Vector.generate

-- | Defines 'DomainHint' as 'Int' (length of the 'Unboxed.Vector').
instance (forall a. Generic.Vector Unboxed.Vector a) => Memoizer Unboxed.Vector where
    type Arg        Unboxed.Vector = Int
    type DomainHint Unboxed.Vector = Int

    apply   = (Generic.Vector.!)
    memoize = flip Generic.Vector.generate

-- | Defines 'DomainHint' as 'Int' (length of the 'Storable.Vector').
instance (forall a. Generic.Vector Storable.Vector a) => Memoizer Storable.Vector where
    type Arg        Storable.Vector = Int
    type DomainHint Storable.Vector = Int

    apply   = (Generic.Vector.!)
    memoize = flip Generic.Vector.generate

-- | Defines 'DomainHint' as list of keys.
instance (Eq k, Hashable k) => Memoizer (HashMap k) where
    type Arg        (HashMap k) = k
    type DomainHint (HashMap k) = [k]

    apply   = (HashMap.!)
    memoize = memoizeKeyValuePairs

-- | Defines 'DomainHint' as list of keys.
instance Ord k => Memoizer (Map k) where
    type Arg        (Map k) = k
    type DomainHint (Map k) = [k]

    apply   = (Map.!)
    memoize = memoizeKeyValuePairs

-- | Defines 'DomainHint' as list of keys.
instance Memoizer IntMap where
    type Arg        IntMap = Int
    type DomainHint IntMap = [Int]

    apply   = (IntMap.!)
    memoize = memoizeKeyValuePairs

-- | Memoizing functor.
--
--   Used when is too boring to write two constraints instead of one.
class (Functor f, Memoizer f) => Memoizing f

-- Type wrappers.

-- | Used to derive 'Memoizer' instances for all 'Representable' 'Functor's.
newtype WrappedRepresentable f a = WrapRepresentable
    { getWrappedRepresentable :: f a
    }

-- | Defines 'DomainHint' as @()@.
instance Representable f => Memoizer (WrappedRepresentable f) where
    type Arg        (WrappedRepresentable f) = Rep f
    type DomainHint (WrappedRepresentable f) = ()

    apply   = index . getWrappedRepresentable
    memoize = const . WrapRepresentable . tabulate

-- | Used to derive 'Memoizer' instances with unsafe indexing.
newtype Unsafe f a = Unsafe
    { getUnsafe :: f a
    }

-- | Defines 'DomainHint' as 'Int' (length of the 'Vector').
instance Memoizer (Unsafe Vector) where
    type Arg        (Unsafe Vector) = Int
    type DomainHint (Unsafe Vector) = Int

    apply   = Generic.Vector.unsafeIndex . getUnsafe
    memoize = (Unsafe .) . memoize

-- | Defines 'DomainHint' as 'Int' (length of the 'Unboxed.Vector').
instance (forall a. Generic.Vector Unboxed.Vector a) => Memoizer (Unsafe Unboxed.Vector) where
    type Arg        (Unsafe Unboxed.Vector) = Int
    type DomainHint (Unsafe Unboxed.Vector) = Int

    apply   = Generic.Vector.unsafeIndex . getUnsafe
    memoize = (Unsafe .) . memoize

-- | Defines 'DomainHint' as 'Int' (length of the 'Storable.Vector').
instance (forall a. Generic.Vector Storable.Vector a) => Memoizer (Unsafe Storable.Vector) where
    type Arg        (Unsafe Storable.Vector) = Int
    type DomainHint (Unsafe Storable.Vector) = Int

    apply   = Generic.Vector.unsafeIndex . getUnsafe
    memoize = (Unsafe .) . memoize

-- | Memoizes a particular function
--   and stores its outputs in /normal form/.
memoize' :: (Memoizer t, NFData (t a)) => (Arg t -> a) -> DomainHint t -> t a
memoize' = (force .) . memoize

-- Utils.

memoizeKeyValuePairs :: (IsList t, Item t ~ (k, v)) => (k -> v) -> [k] -> t
memoizeKeyValuePairs f keys =
    let values = fmap f keys
     in fromList $ zip keys values

memoizeArray :: (IArray a e, Ix i) => (i -> e) -> (i, i) -> a i e
memoizeArray f bounds =
    let indices = IArray.range bounds
        elems   = fmap f indices
     in array bounds $ zip indices elems
