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
-- The memoizing discontinuation comonad transformer.
-- This version is lazy; for a strict version, see
-- "Control.Comonad.Trans.Discont.Strict", which has the same interface.
--
----------------------------------------------------------------------------
module Control.Comonad.Trans.Discont.Memo
  ( module Control.Comonad.Trans.Discont.Memo.Lazy
  ) where

import Control.Comonad.Trans.Discont.Memo.Lazy
