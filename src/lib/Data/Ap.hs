--------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE NoStarIsType          #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE QuantifiedConstraints #-}

--------------------------------------------------------------------------------

-- |
-- Module      : Data.Ap
-- Copyright   : (c) Austin Seipp 2019
-- License     : BSD3
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- A type for type-level \"application\", named @'Ap'@. This type effectively
-- allows you to talk about applying a higher-kind type @f@ to some other type
-- @a@.
--
-- This type is conceptually similar to the @'Control.Applicative.Alt'@ type
-- for @'Control.Applicative.Alternative'@ instances, though @Alt@ is primarily
-- intended to give a different monoidal structure to @'Alternative'@ things.
-- @'Ap'@, instead, is merely a simple utility meant for derivation.
--
-- The intended use of this module is to provide quantified instances for the
-- @'Ap'@ type based on @-XQuantifiedConstraints@ -- that is, instances which
-- are predicated on some constraint containing quantifiers (e.g.  @forall@).
-- This allows us, for instance, to write an instance of @'Show'@ for /all/
-- higher-kinded types @f@, provided that they are applied to a type @a@ which
-- is an instance of @'Show'@ as well. Users can then re-use this instance for
-- their own types through @-XDerivingVia@.

module Data.Ap
  ( Ap(..)
  ) where

--------------------------------------------------------------------------------

-- base
import           Data.Function ( on )
import           Data.Kind     ( Type )

--------------------------------------------------------------------------------

-- | Wrapper for higher-kinded type application. Intended to be used with
-- @-XDerivingVia@ in order to derive instances for higher-kinded types.
newtype Ap (f :: k -> Type) (a :: k) = Ap { unAp :: (f a) }

-- instance bonanza follows

instance (Eq a, forall b. Eq b => Eq (f b)) => Eq (Ap f a) where
  (==) = (==) `on` unAp

instance (Show a, forall b. Show b => Show (f b)) => Show (Ap f a) where
  show = show . unAp
