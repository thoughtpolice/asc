--------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE NoStarIsType          #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances  #-}

--------------------------------------------------------------------------------

-- |
-- Module      : Data.Fix
-- Copyright   : (c) Austin Seipp 2019
-- License     : BSD3
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- Type-level Y combinator: the @'Fix'@ datatype. (Reader note: defined here to
-- avoid a dependency on @recursion-schemes@, and because this hasn't been
-- adopted into @base@ yet, for some reason.)

module Data.Fix
  ( Fix(..)
  ) where

--------------------------------------------------------------------------------

-- base
import           Data.Kind ( Type )

-- avc
import           Data.Ap ( Ap(..) )

--------------------------------------------------------------------------------

-- | Recursive fix-point datatype. (Also known as @Mu@.)
newtype Fix (f :: Type -> Type) = Roll { unroll :: f (Fix f) }

-- | @-XQuantifiedConstraints@
deriving via (Ap f (Fix f)) instance (forall b. Eq b   => Eq (f b))   => Eq (Fix f)

-- | @-XQuantifiedConstraints@
deriving via (Ap f (Fix f)) instance (forall b. Show b => Show (f b)) => Show (Fix f)
