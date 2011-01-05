-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Trans.Context
-- Copyright   :  (C) 2008-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- The state-in-context comonad transformer is subject to the laws:
-- 
-- x = put (get x) x
-- y = get (put y x)
-- put y x = put y (put z x)
--
----------------------------------------------------------------------------
module Control.Comonad.Trans.Context 
  ( Context, context, runContext
  , ContextT(..), runContextT
  , get, put, modify, experiment
  ) where

import Control.Comonad
import Control.Comonad.Trans.Class
import Data.Functor.Identity

type Context s = ContextT s Identity

data ContextT s w a = ContextT (w (s -> a)) s

context :: (s -> a) -> s -> Context s a 
context f s = ContextT (Identity f) s

runContext :: Context s a -> (s -> a, s)
runContext (ContextT (Identity f) s) = (f, s)

runContextT :: ContextT s w a -> (w (s -> a), s)
runContextT (ContextT f s) = (f, s)

instance Functor w => Functor (ContextT s w) where
  fmap g (ContextT f s) = ContextT (fmap (g .) f) s

instance Comonad w => Comonad (ContextT s w) where
  extract (ContextT f s) = extract f s
  duplicate (ContextT f s) = ContextT (fmap (\_ -> ContextT f) f) s
  --    dup (contextT f s) = contextT (map (λ x → contextT f) f) s

instance ComonadTrans (ContextT s) where
  lower (ContextT f s) = fmap ($s) f

get :: ContextT s w a -> s
get (ContextT _ s) = s

put :: Comonad w => s -> ContextT s w a -> a 
put s (ContextT f _) = extract f s

modify :: Comonad w => (s -> s) -> ContextT s w a -> a
modify f (ContextT g s) = extract g (f s)

experiment :: (Comonad w, Functor f) => f (s -> s) -> ContextT s w a -> f a
experiment fs (ContextT g s) = fmap (\f -> extract g (f s)) fs
