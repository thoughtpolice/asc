--------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------------------------------------

-- |
-- Module      : Data.TagBag
-- Copyright   : (c) Austin Seipp 2019
--               (c) Max Bolingbroke 2011-2019
--               (c) Neil Mitchell 2010-2019
-- License     : BSD3
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- \"Tag-bags\" are an ordering relation for multi-sets described by
-- <https://ndmitchell.com/downloads/paper-rethinking_supercompilation-29_sep_2010.pdf "Rethinking Supercompilation">,
-- 2010, by Mitchell. These @'TagBag'@s give a well-quasi-order which is
-- useful for syntax trees that have @'Tag'@s associated with their
-- subnodes. Attach a @'Tag'@ to your syntax tree described by a
-- recursive-functor @f@, then implement @'explore'@ to recursively analyze
-- your trees, building @'TagBag'@s from the nodes.

module Data.TagBag
  ( -- * Tag bags
    Tag     -- :: Type
  , TagBag  -- :: Type

  , addBag   -- :: Tag -> TagBag
  , toSet    -- :: TagBat -> IntSet
  , toMap    -- :: TagBat -> IntMap Int
  , bagSize  -- :: TagBag -> Int
  , bagOrder -- :: TagBag -> TagBag -> Bool
  ) where

--------------------------------------------------------------------------------
-- Imports

-- containers
import           Data.IntSet       ( IntSet )
import           Data.IntMap       ( IntMap )
import qualified Data.IntMap                  as IM

--------------------------------------------------------------------------------
-- Mitchell's Tag-bags, reloaded

-- | A unique tag for a node in a syntax tree.
newtype Tag = Tag { unTag :: Int }
  deriving stock (Eq, Show, Ord)
  deriving newtype Num

-- | Mitchell's \"Tag-bags\". These are simply multi-sets, with a special
-- ordering relation between @'Tag'@s inside.
--
-- Recall that a multi-set is simply a set where each element has a cardinality.
-- Then a multi-set is simply a mapping from some element @k@ to a value @'Int'@,
-- tracking occurrences (cardinality) of the element. We might think of this
-- data structure as a simple @'Map'@, with the respective keys and values.
--
-- The following well-quasi-order \( (\trianglelefteq) \) then relates these
-- multi-sets, creating @'TagBag'@s:
--
-- \( b_1 \trianglelefteq b_2 \iff set(b_1) = set(b_2) \land \lvert b_1 \rvert \leq  \lvert b_2 \rvert \)
--
-- where \( set \) is given by @'toSet'@ and cardinality by @'bagSize'@.
-- This operator is defined by @'bagOrder'@.
--
newtype TagBag = TagBag { unTagBag :: IntMap Int }
  deriving stock (Eq, Show, Ord)
  deriving newtype Monoid

instance Semigroup TagBag where
  TagBag t1 <> TagBag t2 = TagBag (IM.unionWith (+) t1 t2)

-- | Create a @'TagBag'@ from a single @'Tag'@.
addBag :: Tag -> TagBag
addBag (Tag tg) = TagBag (IM.singleton tg 1)

-- | Convert a @'TagBag'@ to an @'IntSet'@.
toSet :: TagBag -> IntSet
toSet (TagBag tb) = IM.keysSet tb

-- | Convert a @'TagBag'@ to an @'IntMap'@ of @'Int'@s.
toMap :: TagBag -> IntMap Int
toMap (TagBag tb) = tb

-- | Cardinality of a @'TagBag'@.
bagSize :: TagBag -> Int
bagSize (TagBag tb) = IM.foldr add 0 tb where add !c !a = c + a -- sum (IM.elems tb)

-- | Tag-bag ordering.
bagOrder :: TagBag -> TagBag -> Bool
bagOrder ta tb = toSet ta == toSet tb && bagSize ta <= bagSize tb
