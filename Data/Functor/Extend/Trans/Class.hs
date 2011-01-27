-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Extend.Trans.Class
-- Copyright   :  (C) 2008-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
----------------------------------------------------------------------------
module Data.Functor.Extend.Trans.Class
  ( ExtendTrans(..) ) where

import Control.Comonad
import Control.Monad.Trans.Identity

class ExtendTrans t where
  lower :: Extend w => t w a -> w a 

-- avoiding orphans
instance ExtendTrans IdentityT where
  lower = runIdentityT
