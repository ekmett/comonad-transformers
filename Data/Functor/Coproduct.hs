-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Coproduct
-- Copyright   :  (C) 2008-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
----------------------------------------------------------------------------
module Data.Functor.Coproduct
  ( Coproduct(..) 
  , left
  , right
  , coproduct
  ) where

import Control.Comonad
import Data.Functor.Apply
import Data.Functor.Alt
import Data.Functor.Extend
import Data.Foldable
import Data.Traversable

newtype Coproduct f g a = Coproduct { getCoproduct :: Either (f a) (g a) }

left :: f a -> Coproduct f g a
left = Coproduct . Left

right :: g a -> Coproduct f g a
right = Coproduct . Right

coproduct :: (f a -> b) -> (g a -> b) -> Coproduct f g a -> b
coproduct f g = either f g . getCoproduct

instance (Functor f, Functor g) => Functor (Coproduct f g) where
  fmap f = Coproduct . coproduct (Left . fmap f) (Right . fmap f)

instance (Foldable f, Foldable g) => Foldable (Coproduct f g) where
  foldMap f = coproduct (foldMap f) (foldMap f)

instance (Traversable f, Traversable g) => Traversable (Coproduct f g) where
  traverse f = coproduct
    (fmap (Coproduct . Left) . traverse f)  
    (fmap (Coproduct . Right) . traverse f)
  
instance (Extend f, Extend g) => Extend (Coproduct f g) where
  extend f = Coproduct . coproduct
    (Left . extend (f . Coproduct . Left))
    (Right . extend (f . Coproduct . Right))

instance (Comonad f, Comonad g) => Comonad (Coproduct f g) where
  extract = coproduct extract extract
