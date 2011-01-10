-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Trans.Env.Strict
-- Copyright   :  (C) 2008-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- The environment comonad transformer (aka coreader).
-- This adds an extra value that can be accessed in the environment.
--
-- Left adjoint to the reader comonad.
----------------------------------------------------------------------------
module Control.Comonad.Trans.Env.Strict
  ( 
  -- * The strict environment comonad
    Env
  , env
  , runEnv
  -- * The strict environment comonad transformer
  , EnvT(..)
  , runEnvT
  -- * Combinators
  , ask
  , asks
  , local
  ) where

import Control.Comonad
import Control.Comonad.Apply
import Control.Comonad.Trans.Class
import Control.Comonad.Hoist.Class
import Data.Functor.Apply
import Data.Functor.Identity
import Data.Monoid

#ifdef GHC_TYPEABLE
import Data.Data

instance (Typeable s, Typeable1 w) => Typeable1 (EnvT s w) where
  typeOf1 dswa = mkTyConApp envTTyCon [typeOf (s dswa), typeOf1 (w dswa)]
    where
      s :: EnvT s w a -> s
      s = undefined
      w :: EnvT s w a -> w a
      w = undefined

envTTyCon :: TyCon
envTTyCon = mkTyCon "Control.Comonad.Trans.Env.Strict.EnvT"
{-# NOINLINE envTTyCon #-}

instance (Typeable s, Typeable1 w, Typeable a) => Typeable (EnvT s w a) where
  typeOf = typeOfDefault

instance 
  ( Typeable e
  , Typeable1 w
  , Data e
  , Data (w a)
  , Data a
  ) => Data (EnvT e w a) where
    gfoldl f z (EnvT e wa) = z EnvT `f` e `f` wa
    toConstr _ = envTConstr
    gunfold k z c = case constrIndex c of
        1 -> k (k (z EnvT))
        _ -> error "gunfold"
    dataTypeOf _ = envTDataType
    dataCast1 f = gcast1 f

envTConstr :: Constr
envTConstr = mkConstr envTDataType "EnvT" [] Prefix
{-# NOINLINE envTConstr #-}

envTDataType :: DataType
envTDataType = mkDataType "Control.Comonad.Trans.Env.Strict.EnvT" [envTConstr]
{-# NOINLINE envTDataType #-}

#endif

type Env e = EnvT e Identity
data EnvT e w a = EnvT e (w a)

env :: e -> a -> Env e a
env e a = EnvT e (Identity a)

runEnv :: Env e a -> (e, a)
runEnv (EnvT e (Identity a)) = (e, a)

runEnvT :: EnvT e w a -> (e, w a)
runEnvT (EnvT e wa) = (e, wa)

instance Functor w => Functor (EnvT e w) where
  fmap g (EnvT e wa) = EnvT e (fmap g wa)

instance Comonad w => Comonad (EnvT e w) where
  extract (EnvT _ wa) = extract wa
  duplicate p@(EnvT e wa) = EnvT e (p <$ wa)

instance ComonadTrans (EnvT e) where
  lower (EnvT _ wa) = wa

instance ComonadHoist (EnvT e) where
  cohoist (EnvT e wa) = EnvT e (Identity (extract wa))

instance (Monoid e, FunctorApply w) => FunctorApply (EnvT e w) where
  EnvT ef wf <.> EnvT ea wa = EnvT (ef `mappend` ea) (wf <.> wa)

instance (Monoid e, ComonadApply w) => ComonadApply (EnvT e w)

ask :: EnvT e w a -> e
ask (EnvT e _) = e

asks :: (e -> f) -> EnvT e w a -> f
asks f (EnvT e _) = f e

local :: (e -> e) -> EnvT e w a -> EnvT e w a
local f (EnvT e wa) = EnvT (f e) wa

