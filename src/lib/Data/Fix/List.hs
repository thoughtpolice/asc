--------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}

--------------------------------------------------------------------------------

-- |
-- Module      : Data.Fix.List
-- Copyright   : (c) Austin Seipp 2019
-- License     : BSD3
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- @'List'@ datatype.
--
module Data.Fix.List
  ( ListF(..) -- :: Type -> Type -> Type
  , List      -- :: Type -> Type
  , fromListF -- :: [a] -> List a
  ) where

--------------------------------------------------------------------------------

-- avc
import           Data.Fix

--------------------------------------------------------------------------------

data ListF a r = NilF | ConsF a r
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Lists, encoded as an open data type via @'Fix'@.
type List a = Fix (ListF a)

fromListF :: [a] -> List a
fromListF []     = Roll NilF
fromListF (y:ys) = Roll (ConsF y (fromListF ys))
