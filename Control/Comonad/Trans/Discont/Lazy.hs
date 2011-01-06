{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Trans.Discont.Lazy
-- Copyright   :  (C) 2008-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
-- 
-- Discont is the Density comonad of a constant functor, just as Cont is a 
-- Codensity monad of a constant functor.
--
-- Note that Discont and Context are isomorphic, but DiscontT and ContextT are
-- not.
--
----------------------------------------------------------------------------
module Control.Comonad.Trans.Discont.Lazy
  ( 
  -- * The 'discontinuation' comonad
    Discont
  , discont
  -- * The discontinuation comonad transformer
  , runDiscont
  , DiscontT(..)
  , runDiscontT
  -- * Combinators
  , callCV
  ) where

import Data.Functor.Identity
import Control.Comonad
import Control.Comonad.Trans.Class

type Discont s = DiscontT s Identity

data DiscontT s w a = DiscontT (w s -> a) (w s)

discont :: (s -> a) -> s -> Discont s a 
discont f s = DiscontT (f . runIdentity) (Identity s)

runDiscont :: Discont s a -> (s -> a, s) 
runDiscont ~(DiscontT f (Identity s)) = (f . Identity,  s)

runDiscontT :: DiscontT s w a -> (w s -> a, w s)
runDiscontT ~(DiscontT f s) = (f, s)

instance Functor w => Functor (DiscontT s w) where
  fmap g ~(DiscontT f ws) = DiscontT (g . f) ws

instance Comonad w => Comonad (DiscontT s w) where
  extract ~(DiscontT f ws) = f ws
  duplicate ~(DiscontT f ws) = DiscontT (DiscontT f) ws

instance ComonadTrans (DiscontT s) where
  lower ~(DiscontT f s) = extend f s

callCV :: DiscontT s w (DiscontT s w (DiscontT s w a -> a) -> b) -> b
callCV ~(DiscontT k s) = k s (DiscontT (\s' ~(DiscontT k' _) -> k' s') s)
