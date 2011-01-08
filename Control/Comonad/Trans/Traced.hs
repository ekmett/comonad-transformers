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

import Control.Comonad
import Control.Comonad.Apply
import Control.Comonad.Hoist.Class
import Control.Comonad.Trans.Class
import Data.Functor
import Data.Functor.Apply
import Data.Functor.Identity
import Data.Monoid

type Traced m = TracedT m Identity

traced :: (m -> a) -> Traced m a
traced f = TracedT (Identity f)

runTraced :: Monoid m => Traced m a -> m -> a
runTraced (TracedT (Identity f)) = f

newtype TracedT m w a = TracedT { runTracedT :: w (m -> a) }

instance Functor w => Functor (TracedT m w) where
  fmap g = TracedT . fmap (g .) . runTracedT

instance (Comonad w, Monoid m) => Comonad (TracedT m w) where
  extract (TracedT wf) = extract wf mempty
  extend f = TracedT . extend (\wf m -> f (TracedT (fmap (. mappend m) wf))) . runTracedT

instance Monoid m => ComonadTrans (TracedT m) where
  lower = fmap ($mempty) . runTracedT

instance Monoid m => ComonadHoist (TracedT m) where
  cohoist = traced . extract . runTracedT

instance (Monoid m, FunctorApply w) => FunctorApply (TracedT m w) where
  TracedT wf <.> TracedT wa = TracedT ((\mf ma m -> (mf m) (ma m)) <$> wf <.> wa)

instance (Monoid m, ComonadApply w) => ComonadApply (TracedT m w)

trace :: (Comonad w, Monoid m) => m -> TracedT m w a -> a
trace m (TracedT wf) = extract wf m

listen :: Functor w => TracedT m w a -> TracedT m w (a, m)
listen = TracedT . fmap (\f m -> (f m, m)) . runTracedT

listens :: Functor w => (m -> b) -> TracedT m w a -> TracedT m w (a, b)
listens g = TracedT . fmap (\f m -> (f m, g m)) . runTracedT 

censor :: Functor w => (m -> m) -> TracedT m w a -> TracedT m w a
censor g = TracedT . fmap (. g) . runTracedT
