-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Trans.Traced
-- Copyright   :  (C) 2008-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- The trace comonad transformer (aka the cowriter or exponential comonad transformer).
--
----------------------------------------------------------------------------
module Control.Comonad.Trans.Traced
  ( 
  -- * Traced comonad
    Traced
  , traced
  , runTraced
  -- * Traced comonad transformer
  , TracedT(..)
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
import Data.Semigroup

import Data.Typeable

type Traced m = TracedT m Identity

traced :: (m -> a) -> Traced m a
traced f = TracedT (Identity f)

runTraced :: Traced m a -> m -> a
runTraced (TracedT (Identity f)) = f

newtype TracedT m w a = TracedT { runTracedT :: w (m -> a) }

instance Functor w => Functor (TracedT m w) where
  fmap g = TracedT . fmap (g .) . runTracedT

instance Apply w => Apply (TracedT m w) where
  TracedT wf <.> TracedT wa = TracedT (ap <$> wf <.> wa)

instance Applicative w => Applicative (TracedT m w) where
  pure = TracedT . pure . const 
  TracedT wf <*> TracedT wa = TracedT (ap <$> wf <*> wa)

instance (Extend w, Semigroup m) => Extend (TracedT m w) where
  extend f = TracedT . extend (\wf m -> f (TracedT (fmap (. (<>) m) wf))) . runTracedT

instance (Comonad w, Semigroup m, Monoid m) => Comonad (TracedT m w) where
  extract (TracedT wf) = extract wf mempty

instance (Semigroup m, Monoid m) => ComonadTrans (TracedT m) where
  lower = fmap ($mempty) . runTracedT

instance (Semigroup m, Monoid m) => ComonadHoist (TracedT m) where
  cohoist = traced . extract . runTracedT

trace :: (Comonad w, Monoid m) => m -> TracedT m w a -> a
trace m (TracedT wf) = extract wf m

listen :: Functor w => TracedT m w a -> TracedT m w (a, m)
listen = TracedT . fmap (\f m -> (f m, m)) . runTracedT

listens :: Functor w => (m -> b) -> TracedT m w a -> TracedT m w (a, b)
listens g = TracedT . fmap (\f m -> (f m, g m)) . runTracedT 

censor :: Functor w => (m -> m) -> TracedT m w a -> TracedT m w a
censor g = TracedT . fmap (. g) . runTracedT

#ifdef __GLASGOW_HASKELL__

instance (Typeable s, Typeable1 w) => Typeable1 (TracedT s w) where
  typeOf1 dswa = mkTyConApp tracedTTyCon [typeOf (s dswa), typeOf1 (w dswa)]
    where
      s :: TracedT s w a -> s
      s = undefined
      w :: TracedT s w a -> w a
      w = undefined

tracedTTyCon :: TyCon
tracedTTyCon = mkTyCon "Control.Comonad.Trans.Traced.TracedT"
{-# NOINLINE tracedTTyCon #-}

#endif
