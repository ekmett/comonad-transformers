{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
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
    Stream
  , stream
  , runStream
  , unfolds
  -- * The Stream comonad transformer
  , StreamT(..)
  -- * Operations
  , tails
  , unfoldsW
  ) where

import Control.Applicative
import Control.Comonad
import Control.Comonad.Apply
import Control.Comonad.Hoist.Class
import Control.Comonad.Trans.Class
import Data.Functor.Apply
import Data.Functor.Identity
import Data.Foldable
import Data.Traversable
import Data.Monoid

#ifdef GHC_TYPEABLE
import Data.Data
#endif

-- | Isomorphic to the definition:
--
-- > data Stream f a = a :< f (Stream f a)
type Stream f = StreamT f Identity

-- | cons onto an f-branching stream
stream :: a -> f (Stream f a) -> Stream f a 
stream a as = StreamT (Identity (a, as))

-- | uncons from an f-branching stream
runStream :: Stream f a -> (a, f (Stream f a))
runStream = runIdentity . runStreamT

-- | unfold a stream from a seed.
unfolds :: Functor f => (a -> (b, f a)) -> a -> Stream f b
unfolds f a = let (h, t) = f a in stream h (unfolds f <$> t)

-- | The f-branching stream comonad transformer is a comonadic version of
-- the \"ListT done Right\" monad transformer. You can extract the underlying comonadic 
-- value by using 'lower' or runStream
data StreamT f w a = StreamT { runStreamT :: w (a, f (StreamT f w a)) }

instance (Functor w, Functor f) => Functor (StreamT f w) where
  fmap f = StreamT . fmap (\(a, as) -> (f a, fmap f <$> as)) . runStreamT

instance (Comonad w, Functor f) => Comonad (StreamT f w) where
  extract = fst . extract . runStreamT
  duplicate = StreamT . extend (\w -> (StreamT w, duplicate <$> snd (extract w))) . runStreamT
  extend f = StreamT . extend (\w -> (f (StreamT w), extend f <$> snd (extract w))) . runStreamT

instance (ComonadApply w, FunctorApply f) => FunctorApply (StreamT f w) where
  StreamT ffs <.> StreamT aas = StreamT (liftW2 wfa ffs aas) where
    wfa (f,fs) (a,as) = (f a, (<.>) <$> fs <.> as)

instance (ComonadApply w, FunctorApply f) => ComonadApply (StreamT f w)

instance Functor f => ComonadTrans (StreamT f) where
  lower = fmap fst . runStreamT

instance Functor f => ComonadHoist (StreamT f) where
  cohoist (StreamT wa) = stream a (cohoist <$> as) where
    (a,as) = extract wa 

instance (Foldable w, Foldable f) => Foldable (StreamT f w) where
  foldMap f = foldMap (\(a, as) -> f a `mappend` foldMap (foldMap f) as) . runStreamT

instance (Traversable w, Traversable f) => Traversable (StreamT f w) where
  traverse f (StreamT w) = StreamT <$> traverse (\(a, as) -> (,) <$> f a <*> traverse (traverse f) as) w

{-
instance Show a => Show (Identity a) where
  showsPrec d (Identity a) = showParen (d > 10) $
      showString "Identity " . showsPrec 11 a

instance (Show (w (a, f (StreamT f w a)))) => Show (StreamT f w a) where
  showsPrec d (StreamT w) = showParen (d > 10) $ 
      showString "StreamT " . showsPrec 11 w
-}

tails :: Comonad w => StreamT f w a -> f (StreamT f w a)
tails = snd . extract . runStreamT

unfoldsW :: (Comonad w, Functor f) => (w a -> (b, f a)) -> w a -> StreamT f w b
unfoldsW f = StreamT . extend (\s -> let (h, t) = f s in (h, fmap (\a -> unfoldsW f (a <$ s)) t))

#ifdef GHC_TYPEABLE

instance (Typeable1 f, Typeable1 w) => Typeable1 (StreamT f w) where
  typeOf1 dfwa = mkTyConApp streamTTyCon [typeOf1 (f dfwa), typeOf1 (w dfwa)]
    where
      f :: StreamT f w a -> f a 
      f = undefined
      w :: StreamT f w a -> w a
      w = undefined

instance (Typeable1 f, Typeable1 w, Typeable a) => Typeable (StreamT f w a) where
  typeOf = typeOfDefault

streamTTyCon :: TyCon
streamTTyCon = mkTyCon "Control.Comonad.Trans.Stream.StreamT"
{-# NOINLINE streamTTyCon #-}

-- if any structure ever cried out for generic programming, this is it
instance 
  ( Typeable1 f
  , Typeable1 w
  , Data (w (a, f (StreamT f w a)))
  , Data (a, f (StreamT f w a))
  , Data (f (StreamT f w a))
  , Data a
  ) => Data (StreamT f w a) where
    gfoldl f z (StreamT a) = z StreamT `f` a
    toConstr _ = streamTConstr
    gunfold k z c = case constrIndex c of
        1 -> k (z StreamT)
        _ -> error "gunfold"
    dataTypeOf _ = streamTDataType
    dataCast1 f = gcast1 f

streamTConstr :: Constr
streamTConstr = mkConstr streamTDataType "StreamT" [] Prefix
{-# NOINLINE streamTConstr #-}

streamTDataType :: DataType
streamTDataType = mkDataType "Control.Comonad.Trans.Stream.StreamT" [streamTConstr]
{-# NOINLINE streamTDataType #-}

#endif
