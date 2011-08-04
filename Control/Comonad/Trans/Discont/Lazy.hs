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
-- 'Discont' is the density comonad of a constant functor, just as 'Cont' is a 
-- Codensity monad of a constant functor. (For the definition of Density and
-- Codensity, see the non-Haskell 98 'adjunctions' package)
--
-- Note that while 'Discont' and 'Store' are isomorphic, 'DiscontT' and 'StoreT' 
-- are not.
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
  ) where

import Data.Functor.Identity
import Control.Comonad
import Control.Comonad.Trans.Class

#ifdef __GLASGOW_HASKELL__
import Data.Typeable

instance (Typeable s, Typeable1 w) => Typeable1 (DiscontT s w) where
  typeOf1 dswa = mkTyConApp discontTTyCon [typeOf (s dswa), typeOf1 (w dswa)]
    where 
      s :: DiscontT s w a -> s
      s = undefined
      w :: DiscontT s w a -> w a
      w = undefined

discontTTyCon :: TyCon
discontTTyCon = mkTyCon "Control.Comonad.Trans.Discont.Lazy.DiscontT" 
{-# NOINLINE discontTTyCon #-}

#endif

type Discont s = DiscontT s Identity

data DiscontT s w a = DiscontT (w s -> a) (w s)

discont :: (s -> a) -> s -> Discont s a 
discont f s = DiscontT (f . runIdentity) (Identity s)

runDiscont :: Discont s a -> (s -> a, s) 
runDiscont ~(DiscontT f (Identity s)) = (f . Identity,  s)

runDiscontT :: DiscontT s w a -> (w s -> a, w s)
runDiscontT ~(DiscontT f s) = (f, s)

instance Functor (DiscontT s w) where
  fmap g ~(DiscontT f ws) = DiscontT (g . f) ws

instance Extend (DiscontT s w) where
  duplicate ~(DiscontT f ws) = DiscontT (DiscontT f) ws

instance Comonad (DiscontT s w) where
  extract ~(DiscontT f ws) = f ws

instance ComonadTrans (DiscontT s) where
  lower ~(DiscontT f s) = extend f s

-- instance Apply w => Apply (DiscontT s w) where

