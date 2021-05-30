{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
-- Module : Control.Comonad.Memoized.Store
--
-- Provides 'Store' 'Comonad' parametrized by a 'Memoized' 'Functor'.
module Control.Comonad.Memoized.Store
    ( Store (Store)

    -- * Re-exports
    , ComonadStore (..)
    )
  where

import           Control.Comonad
import           Control.Comonad.Store.Class

import           Data.Memoizer

-- | A memoized store comonad.
--
--   Currently has no transformer.
data Store memoizer a
    = Store (memoizer a) (DomainHint memoizer) (Arg memoizer)

instance Functor g => Functor (Store g) where
    fmap f (Store memoizer domainHint arg) =
        Store (f <$> memoizer) domainHint arg

instance Memoized g => Comonad (Store g) where
    extract (Store memoizer _domainHint arg) =
        memoizer `apply` arg
    
    duplicate (Store memoizer domainHint arg) =
        let memoizer' = (memoize' . Store) memoizer
         in Store memoizer' domainHint arg
      where
        memoize' f = memoize (f domainHint) domainHint

instance (Memoized g, Arg g ~ s) => ComonadStore s (Store g) where
    pos     (Store _memoizer _domainHint  arg) = arg
    peek  x (Store  memoizer _domainHint _arg) = memoizer `apply` x
    peeks f (Store  memoizer _domainHint  arg) = memoizer `apply` f arg
    seek  x (Store  memoizer  domainHint _arg) = Store memoizer domainHint x
    seeks f (Store  memoizer  domainHint  arg) = Store memoizer domainHint (f arg)
