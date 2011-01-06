-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Trans.Stream
-- Copyright   :  (C) 2008-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- The f-branching stream comonad, aka the cofree comonad for a Functor f.
-- 
-- Provided here as a comonad-transformer version of the 'ListT done right' 
-- monad transformer.
----------------------------------------------------------------------------
module Control.Comonad.Trans.Stream 
  ( 
  -- * The Stream comonad
    Stream, stream, runStream
  -- * The Stream comonad transformer
  , StreamT(..)
  -- * Operations
  , tails
  , unfolds
  , unfoldsW
  ) where

import Control.Applicative
import Control.Comonad
import Control.Comonad.Trans.Class
import Data.Functor.Identity
import Data.Foldable
import Data.Monoid
import Data.Traversable

type Stream f = StreamT f Identity

stream :: a -> f (Stream f a) -> Stream f a 
stream a as = StreamT (Identity (a, as))

runStream :: Stream f a -> (a, f (Stream f a))
runStream = runIdentity . runStreamT

data StreamT f w a = StreamT { runStreamT :: w (a, f (StreamT f w a)) }

instance (Functor w, Functor f) => Functor (StreamT f w) where
  fmap f = StreamT . fmap (\(a, as) -> (f a, fmap f <$> as)) . runStreamT

instance (Comonad w, Functor f) => Comonad (StreamT f w) where
  extract = fst . extract . runStreamT
  duplicate = StreamT . extend (\w -> (StreamT w, duplicate <$> snd (extract w))) . runStreamT
  extend f = StreamT . extend (\w -> (f (StreamT w), extend f <$> snd (extract w))) . runStreamT

instance Functor f => ComonadTrans (StreamT f) where
  lower = fmap fst . runStreamT

instance (Foldable w, Foldable f) => Foldable (StreamT f w) where
  foldMap f = foldMap (\(a, as) -> f a `mappend` foldMap (foldMap f) as) . runStreamT

instance (Traversable w, Traversable f) => Traversable (StreamT f w) where
  traverse f (StreamT w) = StreamT <$> traverse (\(a, as) -> (,) <$> f a <*> traverse (traverse f) as) w

tails :: Comonad w => StreamT f w a -> f (StreamT f w a)
tails = snd . extract . runStreamT

unfolds :: Functor f => (a -> (b, f a)) -> a -> Stream f b
unfolds f a = let (h, t) = f a in stream h (unfolds f <$> t)

unfoldsW :: (Comonad w, Functor f) => (w a -> (b, f a)) -> w a -> StreamT f w b
unfoldsW f = StreamT . extend (\s -> let (h, t) = f s in (h, fmap (\a -> unfoldsW f (a <$ s)) t))
