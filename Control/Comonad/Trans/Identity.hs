-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Trans.Identity
-- Copyright   :  (C) 2008-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
-- 
----------------------------------------------------------------------------
module Control.Comonad.Trans.Identity
  ( IdentityT(..)
  ) where

import Control.Monad.Trans.Identity

-- Provided by Control.Comonad to avoid an orphan
{-
instance Comonad w => Comonad (IdentityT w) where
  extract = extract . runIdentityT
  extend f (IdentityT m) = IdentityT (extend (f . IdentityT) m)
-}

-- Provided by Control.Comonad.Trans.Class to avoid an orphan
{-
instance ComonadTrans IdentityT where
  colift = IdentityT
-}
