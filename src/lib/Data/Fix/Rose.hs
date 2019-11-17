--------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}

--------------------------------------------------------------------------------

-- |
-- Module      : Data.Fix.Rose
-- Copyright   : (c) Austin Seipp 2019
-- License     : BSD3
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- @'Rose'@ datatype.
--
module Data.Fix.Rose
  ( TreeF(..) -- :: Type -> Type -> Type
  , Tree      -- :: Type -> Type
  , toTree    -- :: a -> [Tree a] -> Tree a
  ) where

--------------------------------------------------------------------------------

-- avc
import           Data.Fix

--------------------------------------------------------------------------------

-- | Rose @'Tree'@ functor, which encodes nodes in the tree with their leafs.
data TreeF a r = NodeF a [r]
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Rose trees, encoded as an open datatype using @'Fix'@.
type Tree a = Fix (TreeF a)

-- | Create a Rose @'Tree'@ from a node and its leaves.
toTree :: a -> [Tree a] -> Tree a
toTree x ys = Roll (NodeF x ys)
