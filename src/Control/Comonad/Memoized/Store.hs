{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
-- Module : Control.Comonad.Memoized.Store
--
-- Provides 'Store' 'Comonad' parametrized by a 'Memoizing' 'Functor'.
module Control.Comonad.Memoized.Store
    ( -- * Store comonad
      Store
    , store
    , runStore
    , StoreT
        ( StoreT
        )
    , storeT
    , runStoreT

    -- * Re-exports
    , Comonad (..)
    , ComonadStore (..)
    )
  where

import           Control.Comonad
import           Control.Comonad.Store.Class
import           Control.Comonad.Trans.Class
import           Data.Functor.Identity

import           Data.Memoizer

-- | A memoized store comonad.
type Store memoizer = StoreT memoizer Identity

-- | Constructs an action.
store ::
    Memoizer g
 => (Arg g -> a)
 -> DomainHint g
 -> Arg g
 -> Store g a
store =
    storeT . Identity

-- | Runs an action.
runStore ::
    Memoizer g
 => Store g a
 -> (Arg g -> a, DomainHint g, Arg g)
runStore (StoreT (Identity memo) dh arg) =
    (recall memo, dh, arg)

-- | A memoized store comonad transformer.
data StoreT memoizer w a 
    = StoreT (w (memoizer a)) (DomainHint memoizer) (Arg memoizer)

instance (Functor g, Functor w) => Functor (StoreT g w) where
    fmap f (StoreT wmemo dh arg) =
        StoreT (fmap f <$> wmemo) dh arg

instance (Functor g, Memoizer g, Comonad w) => Comonad (StoreT g w) where
    extract (StoreT wmemo _dh arg) =
        extract wmemo `recall` arg
    
    duplicate (StoreT wmemo dh arg) =
        let f wmemo' = memoize (StoreT wmemo' dh) dh
         in StoreT (extend f wmemo) dh arg

instance (Functor g, Memoizer g, Comonad w, Arg g ~ s) => ComonadStore s (StoreT g w) where
    pos     (StoreT _wmemo _dh  arg) = arg
    peek  x (StoreT  wmemo _dh _arg) = extract wmemo `recall` x
    peeks f (StoreT  wmemo _dh  arg) = extract wmemo `recall` f arg
    seek  x (StoreT  wmemo  dh _arg) = StoreT wmemo dh x
    seeks f (StoreT  wmemo  dh  arg) = StoreT wmemo dh (f arg)

instance (Functor g, Memoizer g) => ComonadTrans (StoreT g) where
    lower (StoreT wmemo _dh arg) =
        flip recall arg <$> wmemo

-- | Constructs an action.
storeT ::
    (Memoizer g, Functor w)
 => w (Arg g -> a)
 -> DomainHint g
 -> Arg g
 -> StoreT g w a
storeT wf dh arg =
    let wmemo = flip memoize dh <$> wf
     in StoreT wmemo dh arg

-- | Runs an action.
runStoreT ::
    (Memoizer g, Functor w)
 => StoreT g w a
 -> (w (Arg g -> a), DomainHint g, Arg g)
runStoreT (StoreT wmemo dh arg) =
    (recall <$> wmemo, dh, arg)
