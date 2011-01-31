{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Trans.Traced.Memo
-- Copyright   :  (C) 2008-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- The memoized traced comonad transformer (aka the cowriter or 
-- exponential comonad transformer).
--
----------------------------------------------------------------------------
module Control.Comonad.Trans.Traced.Memo
  ( 
  -- * Traced comonad
    Traced
  , traced
  , runTraced
  -- * Traced comonad transformer
  , TracedT
  , tracedT
  , runTracedT
  -- * Operations
  , trace
  , listen
  , listens
  , censor
  ) where

import Control.Applicative
import Control.Monad.Instances
import Control.Monad (ap)
import Control.Comonad
import Control.Comonad.Hoist.Class
import Control.Comonad.Trans.Class
import Data.Functor
import Data.Functor.Apply
import Data.Functor.Identity
import Data.Monoid
import Data.Typeable

type Traced m = TracedT m Identity

traced :: Monoid m => (m -> a) -> Traced m a
traced f = TracedT (Identity f) (Identity (f mempty))

runTraced :: Traced m a -> m -> a
runTraced (TracedT (Identity f) _) = f

data TracedT m w a = TracedT (w (m -> a)) (w a)

runTracedT :: TracedT m w a -> w (m -> a)
runTracedT (TracedT wf _) = wf

tracedT :: (Functor w, Monoid m) => w (m -> a) -> TracedT m w a 
tracedT wf = TracedT wf (fmap ($ mempty) wf)

instance Functor w => Functor (TracedT m w) where
  fmap g (TracedT wf wa) = TracedT ((g .) <$> wf) (g <$> wa)

instance Apply w => Apply (TracedT m w) where
  TracedT wf wf' <.> TracedT wa wa' = TracedT (ap <$> wf <.> wa) (wf' <.> wa')

instance Applicative w => Applicative (TracedT m w) where
  pure a = TracedT (pure (const a)) (pure a)
  TracedT wf wf' <*> TracedT wa wa' = TracedT (ap <$> wf <*> wa) (wf' <*> wa')

instance (Extend w, Monoid m) => Extend (TracedT m w) where
  extend f = tracedT . extend (\wf m -> f (tracedT (fmap (. mappend m) wf))) . runTracedT

instance (Comonad w, Monoid m) => Comonad (TracedT m w) where
  extract (TracedT _ wa) = extract wa

instance ComonadTrans (TracedT m) where
  lower (TracedT _ wa) = wa

instance ComonadHoist (TracedT m) where
  cohoist (TracedT wf wa) = TracedT (Identity (extract wf)) (Identity (extract wa))

trace :: (Comonad w, Monoid m) => m -> TracedT m w a -> a
trace m (TracedT wf _) = extract wf m

listen :: (Functor w, Monoid m) => TracedT m w a -> TracedT m w (a, m)
listen (TracedT wf wa) = TracedT (fmap (\f m -> (f m, m)) wf) (fmap (\a -> (a,mempty)) wa)

listens :: (Functor w, Monoid m) => (m -> b) -> TracedT m w a -> TracedT m w (a, b)
listens g (TracedT wf wa) = TracedT (fmap (\f m -> (f m, g m)) wf) (fmap (\a -> (a, g mempty)) wa)

censor :: (Functor w, Monoid m) => (m -> m) -> TracedT m w a -> TracedT m w a
censor g = tracedT . fmap (. g) . runTracedT

#ifdef __GLASGOW_HASKELL__

instance (Typeable s, Typeable1 w) => Typeable1 (TracedT s w) where
  typeOf1 dswa = mkTyConApp tracedTTyCon [typeOf (s dswa), typeOf1 (w dswa)]
    where
      s :: TracedT s w a -> s
      s = undefined
      w :: TracedT s w a -> w a
      w = undefined

tracedTTyCon :: TyCon
tracedTTyCon = mkTyCon "Control.Comonad.Trans.Traced.Memo.TracedT"
{-# NOINLINE tracedTTyCon #-}

#endif
