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
  -- * StreamT nodes
  , Node(..)
  ) where

import Control.Applicative
import Control.Comonad
import Control.Comonad.Hoist.Class
import Data.Functor.Apply
import Control.Comonad.Trans.Class
import Data.Functor.Identity
import Data.Foldable
import Data.Traversable
import Data.Monoid

#ifdef __GLASGOW_HASKELL__
import Data.Data
#endif

-- | Isomorphic to the definition:
--
-- > data Stream f a = a :< f (Stream f a)
type Stream f = StreamT f Identity

-- | cons onto an f-branching stream
stream :: a -> f (Stream f a) -> Stream f a 
stream a as = StreamT (Identity (a :< as))

-- | uncons from an f-branching stream
runStream :: Stream f a -> (a, f (Stream f a))
runStream (StreamT (Identity (a :< as))) = (a, as)

-- | unfold a stream from a seed.
unfolds :: Functor f => (a -> (b, f a)) -> a -> Stream f b
unfolds f a = let (h, t) = f a in stream h (unfolds f <$> t)

data Node f w a = a :< f (StreamT f w a)


infixr 5 :<

fstN :: Node f w a -> a
fstN (a :< _) = a

sndN :: Node f w a -> f (StreamT f w a)
sndN (_ :< as) = as

instance (Functor w, Functor f)  => Functor (Node f w) where
  fmap f (a :< as) = f a :< fmap (fmap f) as

  
-- | The f-branching stream comonad transformer is a comonadic version of
-- the \"ListT done Right\" monad transformer. You can extract the underlying comonadic 
-- value by using 'lower' or runStream
data StreamT f w a = StreamT { runStreamT :: w (Node f w a) }

instance (Show a, Show (StreamT f w a), Show (f (StreamT f w a)), Show (w (Node f w a))) => Show (Node f w a) where
  showsPrec d (a :< as) = showParen (d > 5) $
    showsPrec 6 a . showString " :< " . showsPrec 5 as

instance (Show (w (Node f w a)), Show (Node f w a), Show a, Show (f (StreamT f w a))) => Show (StreamT f w a) where
  showsPrec d (StreamT wa) = showParen (d > 10) $ 
    showsPrec 11 wa

instance (Functor w, Functor f) => Functor (StreamT f w) where
  fmap f = StreamT . fmap (fmap f) . runStreamT

-- TODO: relax requirement to just Extend
instance (Comonad w, Functor f) => Extend (StreamT f w) where
  duplicate = StreamT . extend (\w -> StreamT w :< (duplicate <$> sndN (extract w))) . runStreamT
  extend f = StreamT . extend (\w -> f (StreamT w) :< (extend f <$> sndN (extract w))) . runStreamT

instance (Comonad w, Functor f) => Comonad (StreamT f w) where
  extract = fstN . extract . runStreamT

instance (Comonad w, Apply w, Apply f) => Apply (StreamT f w) where
  StreamT ffs <.> StreamT aas = StreamT (liftF2 wfa ffs aas) where
    wfa (f :< fs) (a :< as) = f a :< ((<.>) <$> fs <.> as)

instance Functor f => ComonadTrans (StreamT f) where
  lower = fmap fstN . runStreamT 

instance Functor f => ComonadHoist (StreamT f) where
  cohoist (StreamT wa) = stream a (cohoist <$> as) where
    a :< as = extract wa 

instance (Foldable w, Foldable f) => Foldable (StreamT f w) where
  foldMap f = foldMap (\(a :< as) -> f a `mappend` foldMap (foldMap f) as) . runStreamT

instance (Traversable w, Traversable f) => Traversable (StreamT f w) where
  traverse f (StreamT w) = StreamT <$> traverse (\(a :< as) -> (:<) <$> f a <*> traverse (traverse f) as) w

-- TODO
-- instance (Distributive w, Distributive f) => Distributive (StreamT f w) where

{-
instance Show a => Show (Identity a) where
  showsPrec d (Identity a) = showParen (d > 10) $
      showString "Identity " . showsPrec 11 a

instance (Show (w (a, f (StreamT f w a)))) => Show (StreamT f w a) where
  showsPrec d (StreamT w) = showParen (d > 10) $ 
      showString "StreamT " . showsPrec 11 w
-}

tails :: Comonad w => StreamT f w a -> f (StreamT f w a)
tails = sndN . extract . runStreamT

unfoldsW :: (Comonad w, Functor f) => (w a -> (b, f a)) -> w a -> StreamT f w b
unfoldsW f = StreamT . extend (\s -> let (h, t) = f s in h :< fmap (\a -> unfoldsW f (a <$ s)) t)

#ifdef __GLASGOW_HASKELL__

typeF :: t f w a -> w a -> f a 
typeF = undefined
typeW :: t f w a -> f a -> w a
typeW = undefined

instance (Typeable1 f, Typeable1 w) => Typeable1 (Node f w) where
  typeOf1 d = mkTyConApp nodeTyCon [typeOf1 (typeF d undefined), typeOf1 (typeW d undefined)]

instance (Typeable1 f, Typeable1 w, Typeable a) => Typeable (Node f w a) where
  typeOf = typeOfDefault

instance (Typeable1 f, Typeable1 w) => Typeable1 (StreamT f w) where
  typeOf1 d = mkTyConApp streamTTyCon [typeOf1 (typeF d undefined), typeOf1 (typeW d undefined)]

instance (Typeable1 f, Typeable1 w, Typeable a) => Typeable (StreamT f w a) where
  typeOf = typeOfDefault

nodeTyCon :: TyCon
nodeTyCon = mkTyCon "Control.Comonad.Trans.Stream.Node"
{-# NOINLINE nodeTyCon #-}

streamTTyCon :: TyCon
streamTTyCon = mkTyCon "Control.Comonad.Trans.Stream.StreamT"
{-# NOINLINE streamTTyCon #-}

instance (Typeable1 f, Typeable1 w, Data a, Data (f (StreamT f w a)), Data (StreamT f w a)) => Data (Node f w a) where
  gfoldl k z (a :< as) = z (:<) `k` a `k` as
  toConstr _ = nodeConstr
  gunfold f z c = case constrIndex c of
    1 -> f (f (z (:<)))
    _ -> error "gunfold"
  dataTypeOf _ = nodeDataType
  dataCast1 f = gcast1 f

-- if any structure ever cried out for generic programming, this is it
instance 
  ( Typeable1 f
  , Typeable1 w
  , Data (w (Node f w a))
  , Data (Node f w a)
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

nodeConstr :: Constr
nodeConstr = mkConstr streamTDataType ":<" [] Infix
{-# NOINLINE nodeConstr #-}

nodeDataType :: DataType
nodeDataType = mkDataType "Control.Comonad.Trans.Stream.Node" [nodeConstr]
{-# NOINLINE nodeDataType #-}

#endif
