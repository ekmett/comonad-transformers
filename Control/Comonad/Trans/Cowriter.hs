-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Trans.Cowriter
-- Copyright   :  (C) 2008-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- The cowriter comonad transformer (aka the exponential comonad transformer).
--
----------------------------------------------------------------------------
module Control.Comonad.Trans.Cowriter
  ( 
  -- * Cowriter comonad
    Cowriter
  , cowriter
  , runCowriter
  -- * Cowriter comonad transformer
  , CowriterT(..)
  -- * Operations
  , tell
  ) where

import Control.Comonad
import Control.Comonad.Trans.Class
import Data.Functor.Identity
import Data.Monoid

type Cowriter m = CowriterT m Identity

cowriter :: (m -> a) -> Cowriter m a
cowriter f = CowriterT (Identity f)

runCowriter :: Monoid m => Cowriter m a -> m -> a
runCowriter (CowriterT (Identity f)) = f

newtype CowriterT m w a = CowriterT { runCowriterT :: w (m -> a) }

instance Functor w => Functor (CowriterT m w) where
  fmap g = CowriterT . fmap (g .) . runCowriterT

instance (Comonad w, Monoid m) => Comonad (CowriterT m w) where
  extract (CowriterT wf) = extract wf mempty
  extend f = CowriterT . extend (\wf m -> f (CowriterT (fmap (. mappend m) wf))) . runCowriterT

instance Monoid m => ComonadTrans (CowriterT m) where
  lower = fmap ($mempty) . runCowriterT

tell :: (Comonad w, Monoid m) => m -> CowriterT m w a -> a
tell m (CowriterT wf) = extract wf m
