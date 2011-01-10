-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Trans.Store.Lazy
-- Copyright   :  (C) 2008-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- The lazy store (state-in-context/costate) comonad transformer is subject to the laws:
-- 
-- > x = put (get x) x
-- > y = get (put y x)
-- > put y x = put y (put z x)
--
-- Thanks go to Russell O'Connor and Daniel Peebles for their help formulating 
-- and proving the laws for this comonad transformer.
----------------------------------------------------------------------------
module Control.Comonad.Trans.Store.Lazy
  ( 
  -- * The Store comonad
    Store, store, runStore
  -- * The Store comonad transformer
  , StoreT(..), runStoreT
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

#ifdef GHC_TYPEABLE
import Data.Typeable
instance (Typeable s, Typeable1 w) => Typeable1 (StoreT s w) where
  typeOf1 dswa = mkTyConApp storeTTyCon [typeOf (s dswa), typeOf1 (w dswa)]
    where
      s :: StoreT s w a -> s
      s = undefined
      w :: StoreT s w a -> w a
      w = undefined

storeTTyCon :: TyCon
storeTTyCon = mkTyCon "Control.Comonad.Trans.Store.Lazy.StoreT"
{-# NOINLINE storeTTyCon #-}
#endif

type Store s = StoreT s Identity

store :: (s -> a) -> s -> Store s a 
store f s = StoreT (Identity f) s

runStore :: Store s a -> (s -> a, s)
runStore ~(StoreT (Identity f) s) = (f, s)

data StoreT s w a = StoreT (w (s -> a)) s

runStoreT :: StoreT s w a -> (w (s -> a), s)
runStoreT ~(StoreT wf s) = (wf, s)

instance Functor w => Functor (StoreT s w) where
  fmap f ~(StoreT wf s) = StoreT (fmap (f .) wf) s

instance Comonad w => Comonad (StoreT s w) where
  extract ~(StoreT wf s) = extract wf s
  duplicate ~(StoreT wf s) = StoreT (extend StoreT wf) s
  extend f ~(StoreT wf s) = StoreT (extend (\wf' s' -> f (StoreT wf' s')) wf) s

instance ComonadTrans (StoreT s) where
  lower ~(StoreT f s) = fmap ($s) f

instance ComonadHoist (StoreT s) where
  cohoist ~(StoreT f s) = StoreT (Identity (extract f)) s

get :: StoreT s w a -> s
get ~(StoreT _ s) = s

put :: Comonad w => s -> StoreT s w a -> a 
put s ~(StoreT f _) = extract f s

modify :: Comonad w => (s -> s) -> StoreT s w a -> a
modify f ~(StoreT g s) = extract g (f s)

experiment :: (Comonad w, Functor f) => f (s -> s) -> StoreT s w a -> f a
experiment fs ~(StoreT g s) = fmap (\f -> extract g (f s)) fs
