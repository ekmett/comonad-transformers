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
-- The environment comonad transformer (aka coreader).
-- This version is lazy; for a strict version, see
-- "Control.Comonad.Trans.Env.Strict", which has the same interface.
--
----------------------------------------------------------------------------
module Control.Comonad.Trans.Env
  ( module Control.Comonad.Trans.Env.Lazy
  ) where

import Control.Comonad.Trans.Env.Lazy
