--------------------------------------------------------------------
-- |
-- Module    : Control.Comonad.Supply
-- Copyright : (c) Edward Kmett 2008-2011
--             (c) Iavor S. Diatchki, 2007
-- License   : BSD3
--
-- Maintainer: Edward Kmett <ekmett@gmail.com>
-- Stability : provisional
-- Portability: portable
--
-- The technique for generating new values is based on the paper
-- ''On Generating Unique Names''
-- by Lennart Augustsson, Mikael Rittri, and Dan Synek.
-- 
--------------------------------------------------------------------

module Control.Comonad.Trans.Supply
  ( module Control.Comonad

  -- * Creating supplies
  , SupplyT
  , newSupply
  , newEnumSupply
  , newNumSupply

  -- * Generating new supplies from old
  , supplyLeft
  , supplyRight
  , split
  , split2
  , split3
  , split4
  ) where

import Control.Comonad
import Control.Concurrent.MVar
import Control.Functor.Extras
import System.IO.Unsafe (unsafePerformIO)

-- Basics ----------------------------------------------------------------------

-- | A type that can be used to generate values on demand.
-- A supply may be turned into two different supplies by using
-- the functions 'supplyLeft' and 'supplyRight'.

data Split a = Split a a
type SupplyT = StreamT Fork
type Supply = SupplyT Identity

newSupply    :: a -> (a -> a) -> IO (Supply a)
newSupply a f = newSupplyT (Identity a) (f . runIdentity)


newSupplyT   :: w a -> (w a -> a) -> IO (SupplyT w a)
newSupply x f = fmap (gen True) (newMVar (iterate f x))

  -- The extra argument to ``gen'' is passed because without
  -- it Hugs spots that the recursive calls are the same but does
  -- not know that unsafePerformIO is unsafe.
  where gen _ r = Node { supplyValue  = unsafePerformIO (genSym r),
                         supplyLeft   = gen False r,
                         supplyRight  = gen True r }

        genSym       :: MVar [a] -> IO a
        genSym r      = do a : as <- takeMVar r
                           putMVar r as
                           return a

newEnumSupply  :: (Enum a) => IO (Supply a)
newEnumSupply   = newSupply (toEnum 0) succ

newNumSupply   :: (Num a) => IO (Supply a)
newNumSupply    = newSupply 0 (1+)

-- | Generate an infinite list of supplies by using 'supplyLeft' and
-- 'supplyRight' repeatedly.
split          :: Supply a -> [Supply a]
split s         = supplyLeft s : split (supplyRight s)

-- | Split a supply into two different supplies.
-- The resulting supplies are different from the input supply.
split2         :: Supply a -> (Supply a, Supply a)
split2 s        = (supplyLeft s, supplyRight s)

-- | Split a supply into three different supplies.
split3         :: Supply a -> (Supply a, Supply a, Supply a)
split3 s        = let s1 : s2 : s3 : _ = split s
                  in (s1,s2,s3)

-- | Split a supply into four different supplies.
split4         :: Supply a -> (Supply a, Supply a, Supply a, Supply a)
split4 s        = let s1 : s2 : s3 : s4 : _ = split s
                  in (s1,s2,s3,s4)

instance Copointed Supply where
    extract = supplyValue

instance Comonad Supply where
    extend f s = Node { supplyValue = f s
                      , supplyLeft  = extend f (supplyLeft s)
                      , supplyRight = extend f (supplyRight s)
                      }

instance FunctorSplit Supply where
    fsplit = split2
