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
-- > x = put (get x) x
-- > y = get (put y x)
-- > put y x = put y (put z x)
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
  -- * Operations
  , get
  , put
  , modify
  , experiment
  ) where

import Control.Comonad
import Control.Comonad.Hoist.Class
import Control.Comonad.Trans.Class
import Data.Functor.Identity

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

instance Extend w => Extend (StoreT s w) where
  duplicate (StoreT wf s _) = storeT (extend storeT wf) s 

instance Comonad w => Comonad (StoreT s w) where
  extract (StoreT _ _ w) = extract w

instance ComonadTrans (StoreT s) where
  lower (StoreT _ _ w) = w

instance ComonadHoist (StoreT s) where
  cohoist (StoreT f s w) = StoreT (Identity (extract f)) s (Identity (extract w))

get :: StoreT s w a -> s
get (StoreT _ s _) = s

put :: Comonad w => s -> StoreT s w a -> a 
put s (StoreT f _ _) = extract f s

modify :: Comonad w => (s -> s) -> StoreT s w a -> a
modify f (StoreT g s _) = extract g (f s)

experiment :: (Comonad w, Functor f) => f (s -> s) -> StoreT s w a -> f a
experiment fs (StoreT g s _) = fmap (\f -> extract g (f s)) fs
