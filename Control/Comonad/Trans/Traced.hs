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
  ) where

import Control.Comonad
import Control.Comonad.Trans.Class
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

trace :: (Comonad w, Monoid m) => m -> TracedT m w a -> a
trace m (TracedT wf) = extract wf m
