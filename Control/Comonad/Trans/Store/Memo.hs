{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Trans.Store.Memo
-- Copyright   :  (C) 2008-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- The memoizing store (state-in-context/costate) comonad transformer is 
-- subject to the laws:
-- 
-- > x = seek (pos x) x
-- > y = pos (seek y x)
-- > seek y x = seek y (seek z x)
--
-- This version of the transformer lazily memoizes the result of applying the 
-- comonad to the current state. This can be useful for avoiding redundant 
-- computation if you reuse the same StoreT object multiple times.
----------------------------------------------------------------------------
module Control.Comonad.Trans.Store.Memo
  ( 
  -- * The Store comonad
    Store, store, runStore
  -- * The Store comonad transformer
  , StoreT, storeT, runStoreT
  , lowerStoreT
  -- * Operations
  , pos
  , seek, seeks
  , peek, peeks
  ) where

import Control.Applicative
import Control.Comonad
import Control.Comonad.Hoist.Class
import Control.Comonad.Trans.Class
import Data.Functor.Identity
import Data.Functor.Apply
import Data.Semigroup

#ifdef __GLASGOW_HASKELL__
import Data.Typeable
instance (Typeable s, Typeable1 w) => Typeable1 (StoreT s w) where
  typeOf1 dswa = mkTyConApp storeTTyCon [typeOf (s dswa), typeOf1 (w dswa)]
    where
      s :: StoreT s w a -> s
      s = undefined
      w :: StoreT s w a -> w a
      w = undefined

instance (Typeable s, Typeable1 w, Typeable a) => Typeable (StoreT s w a) where
  typeOf = typeOfDefault

storeTTyCon :: TyCon
storeTTyCon = mkTyCon "Control.Comonad.Trans.Store.Memo.StoreT"
{-# NOINLINE storeTTyCon #-}
#endif

type Store s = StoreT s Identity

store :: (s -> a) -> s -> Store s a 
store f s = StoreT (Identity f) s (Identity (f s))

runStore :: Store s a -> (s -> a, s)
runStore (StoreT (Identity f) s _) = (f, s)

-- inhabitants of @StoreT wf s w@ ensure that
--
-- > w = ($s) <$> wf
data StoreT s w a = StoreT (w (s -> a)) s (w a)

runStoreT :: StoreT s w a -> (w (s -> a), s)
runStoreT (StoreT wf s _) = (wf, s)

storeT :: Functor w => w (s -> a) -> s -> StoreT s w a
storeT wf s = StoreT wf s (fmap ($s) wf)

instance Functor w => Functor (StoreT s w) where
  fmap f (StoreT wf s w) = StoreT (fmap (f .) wf) s (fmap f w)

instance (Apply w, Semigroup s) => Apply (StoreT s w) where
  StoreT ff m _ <.> StoreT fa n _ = storeT ((<*>) <$> ff <.> fa) (m <> n)

instance (Applicative w, Semigroup s, Monoid s) => Applicative (StoreT s w) where
  pure a = storeT (pure (const a)) mempty
  StoreT ff m _ <*> StoreT fa n _ = storeT ((<*>) <$> ff <*> fa) (m `mappend` n)

instance Extend w => Extend (StoreT s w) where
  duplicate (StoreT wf s _) = storeT (extend storeT wf) s 

instance Comonad w => Comonad (StoreT s w) where
  extract (StoreT _ _ w) = extract w

instance ComonadTrans (StoreT s) where
  lower (StoreT _ _ w) = w

lowerStoreT :: StoreT s w a -> w a
lowerStoreT (StoreT _ _ w) = w

instance ComonadHoist (StoreT s) where
  cohoist (StoreT f s w) = StoreT (Identity (extract f)) s (Identity (extract w))

-- | Read the current position
pos :: StoreT s w a -> s
pos (StoreT _ s _) = s

-- | Seek to an absolute location
--
-- > seek s = peek s . duplicate
seek :: Comonad w => s -> StoreT s w a -> StoreT s w a
seek s (StoreT f _ _) = storeT f s

-- | Seek to a relative location
--
-- > seeks f = peeks f . duplicate
seeks :: Comonad w => (s -> s) -> StoreT s w a -> StoreT s w a
seeks f (StoreT g s _) = storeT g (f s)

-- | Peek at a value at a given absolute location
--
-- > peek x . extend (peek y) = peek y
peek :: Comonad w => s -> StoreT s w a -> a
peek s (StoreT g _ _) = extract g s

-- | Peek at a value at a given relative location
peeks :: Comonad w => (s -> s) -> StoreT s w a -> a
peeks f (StoreT g s _) = extract g (f s)

