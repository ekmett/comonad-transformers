{-# LANGUAGE CPP, RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Trans.Discont.Memo
-- Copyright   :  (C) 2008-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
-- 
--
-- 'Discont' is the density comonad of a constant functor, just as 'Cont' is a 
-- Codensity monad of a constant functor. (For the definition of Density and
-- Codensity, see the non-Haskell 98 'adjunctions' package)
--
-- Note that while 'Discont' and 'Store' are isomorphic, 'DiscontT' and 'StoreT' 
-- are not.
--
-- Like the memoizing store comonad, version memoizes the result of applying 
-- the continuation to the current context. 
--
----------------------------------------------------------------------------
module Control.Comonad.Trans.Discont.Memo
  ( 
  -- * The 'discontinuation' comonad
    Discont
  , discont
  -- * The discontinuation comonad transformer
  , runDiscont
  , DiscontT
  , discontT 
  , runDiscontT
  -- * Combinators
  , callCV
  , label
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
discontTTyCon = mkTyCon "Control.Comonad.Trans.Discont.Memo.DiscontT" 
{-# NOINLINE discontTTyCon #-}

#endif

type Discont s = DiscontT s Identity

data DiscontT s w a = DiscontT (w s -> a) (w s) a

discont :: (s -> a) -> s -> Discont s a 
discont f s = DiscontT (f . runIdentity) (Identity s) (f s)

discontT :: (w s -> a) -> w s -> DiscontT s w a 
discontT f s = DiscontT f s (f s)

runDiscont :: Discont s a -> (s -> a, s) 
runDiscont (DiscontT f (Identity s) _) = (f . Identity,  s)

runDiscontT :: DiscontT s w a -> (w s -> a, w s)
runDiscontT (DiscontT f s _) = (f, s)

instance Functor (DiscontT s w) where
  fmap g (DiscontT f ws a) = DiscontT (g . f) ws (g a)

instance Extend (DiscontT s w) where
  duplicate (DiscontT f ws _) = discontT (discontT f) ws

instance Comonad (DiscontT s w) where
  extract (DiscontT _ _ a) = a

instance ComonadTrans (DiscontT s) where
  lower (DiscontT f s _) = extend f s

-- instance Apply w => Apply (DiscontT s w) where

label :: Comonad w => DiscontT s w a -> s 
label (DiscontT _ ws _) = extract ws

callCV :: DiscontT s w (DiscontT s w (DiscontT s w a -> a) -> b) -> b
callCV (DiscontT k s _) = k s (discontT (\s' (DiscontT k' _ _) -> k' s') s)
