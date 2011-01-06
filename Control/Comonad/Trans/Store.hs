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
-- The store comonad transformer (aka costate).
-- This version is lazy; for a strict version, see
-- "Control.Comonad.Trans.Store.Strict", which has the same interface.
--
----------------------------------------------------------------------------
module Control.Comonad.Trans.Store
  ( module Control.Comonad.Trans.Store.Lazy
  ) where

import Control.Comonad.Trans.Store.Lazy
