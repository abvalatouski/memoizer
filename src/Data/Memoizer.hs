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
    ( -- * Type class
      Memoizer
        ( Arg
        , DomainHint
        , remember
        , recall
        , recallMaybe
        )

      -- * Helper functions
    , remember'
    , recallOrDefault

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

import           Data.Maybe
import           GHC.Exts             (IsList (Item, fromList))

import           Control.DeepSeq      (NFData, force)
import           Data.Array           (Array)
import qualified Data.Array.Base      as Array
import           Data.Array.IArray    (IArray, Ix)
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

-- Type class.

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

    -- | Memoizes the function.
    --
    --   Similar to 'tabulate'.
    remember :: (Arg t -> a) -> DomainHint t -> t a

    -- | Applies the function to the argument.
    --
    --   Similar to 'index'.
    --
    --   NOTE:
    --   Usually implementations of that function are /not total/, thus the isomorphism between
    --   functions and data structures does not exist, and thus the laws of 'Representable'
    --   are /not satisfied/.
    recall :: t a -> Arg t -> a
    recall = (fromMaybe bomb .) . recallMaybe
      where
        bomb = error "recall: Memoized function does not provide a value for the given input."

    -- | Applies the function to the argument.
    --
    --   Similar to 'index'.
    --
    --   NOTE:
    --   'Maybe' breaks isomorphism between functions and data structures, and thus the laws
    --   of 'Representable' are /not satisfied/.
    recallMaybe :: t a -> Arg t -> Maybe a
    recallMaybe = (Just .) . recall

    {-# MINIMAL remember, (recall | recallMaybe) #-}

-- | Defines 'DomainHint' as pair of indices.
instance (forall e. IArray Array e, Ix i) => Memoizer (Array i) where
    type Arg        (Array i) = i
    type DomainHint (Array i) = (i, i)

    remember    = memoizeArray
    recallMaybe = safeArrayIndex

-- | Defines 'DomainHint' as pair of indices.
instance (forall e. IArray UArray e, Ix i) => Memoizer (UArray i) where
    type Arg        (UArray i) = i
    type DomainHint (UArray i) = (i, i)

    remember    = memoizeArray
    recallMaybe = safeArrayIndex

-- | Defines 'DomainHint' as 'Int' (length of the 'Vector').
instance Memoizer Vector where
    type Arg        Vector = Int
    type DomainHint Vector = Int

    remember    = flip Generic.Vector.generate
    recallMaybe = (Generic.Vector.!?)

-- | Defines 'DomainHint' as 'Int' (length of the 'Unboxed.Vector').
instance (forall a. Generic.Vector Unboxed.Vector a) => Memoizer Unboxed.Vector where
    type Arg        Unboxed.Vector = Int
    type DomainHint Unboxed.Vector = Int

    remember     = flip Generic.Vector.generate
    recallMaybe = (Generic.Vector.!?)

-- | Defines 'DomainHint' as 'Int' (length of the 'Storable.Vector').
instance (forall a. Generic.Vector Storable.Vector a) => Memoizer Storable.Vector where
    type Arg        Storable.Vector = Int
    type DomainHint Storable.Vector = Int

    remember    = flip Generic.Vector.generate
    recallMaybe = (Generic.Vector.!?)

-- | Defines 'DomainHint' as list of keys.
instance (Eq k, Hashable k) => Memoizer (HashMap k) where
    type Arg        (HashMap k) = k
    type DomainHint (HashMap k) = [k]

    remember    = memoizeKeyValuePairs
    recallMaybe = flip HashMap.lookup

-- | Defines 'DomainHint' as list of keys.
instance Ord k => Memoizer (Map k) where
    type Arg        (Map k) = k
    type DomainHint (Map k) = [k]

    remember    = memoizeKeyValuePairs
    recallMaybe = flip Map.lookup

-- | Defines 'DomainHint' as list of keys.
instance Memoizer IntMap where
    type Arg        IntMap = Int
    type DomainHint IntMap = [Int]

    remember    = memoizeKeyValuePairs
    recallMaybe = flip IntMap.lookup

-- Helper functions.

-- | Memoizes a particular function
--   and stores its outputs in /normal form/.
remember' :: (Memoizer t, NFData (t a)) => (Arg t -> a) -> DomainHint t -> t a
remember' = (force .) . remember

-- | Applies the argument to the memoized function.
--   When the function does provide a value for the argument, evaluates the default value.
recallOrDefault :: Memoizer t => a -> t a -> Arg t -> a
recallOrDefault def = (fromMaybe def .) . recallMaybe

-- Type wrappers.

-- | Used to derive 'Memoizer' instances for all 'Representable' 'Functor's.
newtype WrappedRepresentable f a = WrapRepresentable
    { getWrappedRepresentable :: f a
    }

-- | Defines 'DomainHint' as @()@.
instance Representable f => Memoizer (WrappedRepresentable f) where
    type Arg        (WrappedRepresentable f) = Rep f
    type DomainHint (WrappedRepresentable f) = ()

    remember = const . WrapRepresentable . tabulate
    recall   = index . getWrappedRepresentable

-- | Used to derive 'Memoizer' instances with unsafe indexing.
newtype Unsafe f a = Unsafe
    { getUnsafe :: f a
    }

-- | Defines 'DomainHint' as pair of indices.
instance (forall e. IArray Array e, Ix i) => Memoizer (Unsafe (Array i)) where
    type Arg        (Unsafe (Array i)) = i
    type DomainHint (Unsafe (Array i)) = (i, i)

    remember = (Unsafe .) . memoizeArray
    recall   = unsafeArrayIndex . getUnsafe

-- | Defines 'DomainHint' as pair of indices.
instance (forall e. IArray UArray e, Ix i) => Memoizer (Unsafe (UArray i)) where
    type Arg        (Unsafe (UArray i)) = i
    type DomainHint (Unsafe (UArray i)) = (i, i)

    remember = (Unsafe .) . memoizeArray
    recall   = unsafeArrayIndex . getUnsafe

-- | Defines 'DomainHint' as 'Int' (length of the 'Vector').
instance Memoizer (Unsafe Vector) where
    type Arg        (Unsafe Vector) = Int
    type DomainHint (Unsafe Vector) = Int

    remember = (Unsafe .) . remember
    recall   = Generic.Vector.unsafeIndex . getUnsafe

-- | Defines 'DomainHint' as 'Int' (length of the 'Unboxed.Vector').
instance (forall a. Generic.Vector Unboxed.Vector a) => Memoizer (Unsafe Unboxed.Vector) where
    type Arg        (Unsafe Unboxed.Vector) = Int
    type DomainHint (Unsafe Unboxed.Vector) = Int

    remember = (Unsafe .) . remember
    recall   = Generic.Vector.unsafeIndex . getUnsafe

-- | Defines 'DomainHint' as 'Int' (length of the 'Storable.Vector').
instance (forall a. Generic.Vector Storable.Vector a) => Memoizer (Unsafe Storable.Vector) where
    type Arg        (Unsafe Storable.Vector) = Int
    type DomainHint (Unsafe Storable.Vector) = Int

    remember = (Unsafe .) . remember
    recall   = Generic.Vector.unsafeIndex . getUnsafe

-- Utils.

memoizeKeyValuePairs :: (IsList t, Item t ~ (k, v)) => (k -> v) -> [k] -> t
memoizeKeyValuePairs f keys =
    let values = fmap f keys
     in fromList $ zip keys values

memoizeArray :: (IArray a e, Ix i) => (i -> e) -> (i, i) -> a i e
memoizeArray f bounds =
    let indices = IArray.range bounds
        elems   = fmap f indices
     in IArray.array bounds $ zip indices elems

unsafeArrayIndex :: (IArray a e, Ix i) => a i e -> i -> e
unsafeArrayIndex array index =
    let i = IArray.index (IArray.bounds array) index
     in array `Array.unsafeAt` i

safeArrayIndex :: (IArray a e, Ix i) => a i e -> i -> Maybe e
safeArrayIndex array index
    | i >= 0 && i < n = Just $ array `Array.unsafeAt` i
    | otherwise       = Nothing
  where
    i = IArray.index (IArray.bounds array) index
    n = Array.numElements array
