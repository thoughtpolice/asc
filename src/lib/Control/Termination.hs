--------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE NoStarIsType               #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- Enable default Generic instances for user-defined types
-- #define ENABLE_GENERICS 1

-- Enable the 'optimized' history implementation which prunes stale entries.
-- Described in section 6.1 of "Termination Combinators Forever": 'Pruning
-- histories using transitivity'
#define ENABLE_OPTIMIZED_HISTORY 1

--------------------------------------------------------------------------------

-- |
-- Module      : Control.Termination
-- Copyright   : (c) Austin Seipp 2019
--               (c) Max Bolingbroke 2011-2019
-- License     : BSD3
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This file contains a generalized implementation of /termination combinators/
-- as specified in <https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/termination-combinators.pdf "Termination Combinators Forever">
-- by Peyton Jones & Bolingbroke. The original code (particularly for
-- internal well-quasi-orders) was written by Max Bolingbroke and taken from the
-- @<https://hackage.haskell.org/package/termination-combinators termination-combinators>@
-- package, but was extended and generalized some.


module Control.Termination
  ( -- * Intro
    -- $intro

    -- * Basic usage
    -- $usage

    -- * User interface

    -- | The interface for users to execute and manage termination tests is
    -- relatively simple; it consists only of two steps: creating a @'History'@
    -- to track progress, and submitting tests with the @'check'@ routine.
    --
    -- However, /creating/ termination tests is described in the "Termination
    -- Combinators" section that follows later on.

    -- ** Testing for termination

    -- | When executing a termination test against an incoming sequence of
    -- values, we run a @'check'@ function against each value. The termination
    -- test can then return two results, characterized by @'TestResult'@: it
    -- can tell us to @'Stop'@ if we think the sequence will begin to diverge,
    -- or @'Continue'@ if it's OK to continue with more values in the future.
    --
    -- This API is all that's necessary to run a termination test against some
    -- sequence of values of type @a@, once you have an initial @'History'@
    -- constructed at the start (see below).

    TestResult(..) -- :: Type -> Type
  , check          -- :: History a -> a -> TestResult a

    -- ** Sequence history

    -- | In order to record the list of sequences submitted so far, you need to
    -- keep a log of them. The @'History' a@ type does this, for some type @a@
    --
    -- Given some type @a@ with a termination @'Test'@ available (e.g. the
    -- @'test'@ value iff an instance of @'Terminable'@), you can create an
    -- initial history using the @'history'@ function, and then submit values
    -- using the @'check'@ function. This allows you to compute new values
    -- incrementally.

  , History -- :: Type -> Type
  , history -- :: Test a -> History a

    -- * Termination Combinators

    -- | /Termination combinators/ are used to construct build and compose
    -- sound __termination tests__ for various types, including your own. A
    -- termination test or combinator is a value or function of type @'Test'
    -- a@, allowing you to ask if a sequence of values of type @a@ will diverge
    -- or not. Create an instance of @'Terminable'@ once you've constructed a
    -- @'Test' a@ for your type, and reuse the @'test'@ value when you need it.
    --
    -- A fundamental design element of termination combinators is that they are
    -- /compositional/ and /modular/: you build termination @'Test'@s for a
    -- types of your own out of simpler termination tests. A core set of
    -- combinators and instances for the @'Terminable'@ class form your
    -- building blocks for your own types. Several core combinators as well as
    -- some auxiliary combinators are also included.

  , Test -- :: Type -> Type

    -- ** Core combinators

    -- | In order to build termination tests for your own types, you can start
    -- with a set of fundamental combinators, @'eqT'@ and @'ordT'@, as well as
    -- all instances of the @'Terminable'@ class. These building blocks allow
    -- you to make instances of your own types with some work, but many useful
    -- types can simply use default @'Eq'@ or @'Ord'@ based tests.
    --
    -- A requirement for termination tests is that they are, in some sense,
    -- defined over types which are fundamentally /finite/ in the number of
    -- inhabitants. Essential combinators such as @'eqT'@, @'ordT'@ are defined
    -- under this assumption: that the instances their respective classes are,
    -- by definition, finite. (While certain infinite recursive structures,
    -- such as lists, are also instances of these types,
    -- equality/ordering/comparison over such an infinite structure is
    -- necessarily undefined and therefore we \"quotient\" it out, for our
    -- considerations.)
    --
    -- While most concrete types will be easier to use by simply conjuring a
    -- @'test'@ value and letting polymorphism do the rest, tests based on
    -- class constraints (such as the fundamental ones below) /must/ be
    -- provided by value: there would be no way to provide (non-overlapping)
    -- instances for these types simultaneously. Furthermore, certain types
    -- which may satisfy multiple constraints will certainly want one
    -- /particular/ implementation that must be chosen (e.g. the @'Terminable'
    -- 'Char'@ instance uses @'ordT'@ while @'Terminable' 'Bool'@ uses @'eqT'@,
    -- even though @'Bool'@s are in some sense \"ordered\" and @'Char'@s can be
    -- compared for equality.)

  , Terminable(..) -- :: Type -> Constraint
  , eqT  -- :: Eq a  => Test a
  , ordT -- :: Ord a => Test a

    -- ** Recursive type combinators

    -- | Aside from simple algebraic sum and product types, it is also possible
    -- to define termination tests for /recursive/ data types, including types
    -- like lists (@'listT'@) or rose trees (@'treeT'@). Two fundamental
    -- combinators are provided for those cases: @'fixT'@ allows you to define
    -- a @'Test'@ for any open recursive type, defined in terms of an
    -- underlying @'Fix'@ @'Functor'@. @'gfixT'@ does the same, but uses the
    -- tools of @'Traversable'@ in order to walk the structure and gather
    -- elements automatically.
    --
    -- Most open recursive data types that will be used with this library can
    -- be written with lawful instances of @'Traversable'@, and furthermore,
    -- those instances can often be automatically dervied. In such a case,
    -- usage of @'gfixT'@ to derive a combinator will prove to be relatively
    -- easy:
    --
    -- @
    -- type F  a   = 'Fix' (F' a)
    -- data F' a r = ...
    --   deriving ('Functor', 'Foldable', 'Traversable')
    --
    -- fT :: 'Test' a -> 'Test' (F a)
    -- fT tt = 'gfixT' fix'
    --   where
    --     fix' :: 'Test' r -> 'Test' (F' a r)
    --     fix' = ...
    -- @

  , fixT  -- :: ... -> Test (Fix f)
  , gfixT -- :: ... -> Test (Fix f)

    -- ** Extra combinators

    -- | On top of the core combinators, some extra combinators are defined for
    -- convenience, including common recursive types, making it easier to
    -- define termination tests for client types.
    --
    -- While many cases for creating termination tests will use the
    -- @'Terminable'@ class to get a @'test' :: 'Test' a@ value via
    -- polymorphism, providing actual /value/s for these @'Test'@s and not just
    -- instances is done for a reason: for instance, when a combinator such as
    -- @'maybeT'@ is used, it requires an explicit argument. The problem is
    -- that instance selection alone (via @'test'@) cannot distinguish when you
    -- may want to choose combinators like @'ordT'@ vs @'eqT'@, which would
    -- have overlapping instance criteria. While most @'Terminable'@ instances
    -- will not overlap in this way, the @'Test'@s themselves are also exported
    -- for ease of use, and stricter type-correctness (in case you consciously
    -- do not want instance selection to pick an instance automatically, and
    -- prefer a concrete type instead.)

  , intT    -- :: Test Int
  , mapT    -- :: Test v -> Test (Map k v)
  , intMapT -- :: Test v -> Test (IntMap v)
  , maybeT  -- :: Test v -> Test (Maybe v)
  , listT   -- :: Test a -> Test [a]
  , treeT   -- :: Test a -> Test (Tree a)

    -- *** Tagged syntax tree combinators

    -- | An API for testing syntax trees marked with @'Tag'@ values, using the
    -- /tag-bag well-quasi-order/ defined by Mitchell and Bolingbroke, is
    -- included below. (See the "Data.TagBag" module for more information.)
    --
    -- The essential idea is that any implementation of a type which has
    -- @'Tag'@ values hanging off it can be made an instance of the @'Accrue'@
    -- class: @'Accrue'@ \"explores\" a structure and collects the @'Tag'@
    -- values, and then merges them into a @'TagBag'@ with its child nodes.
    -- With that implemented, it's possible to create a termination test out of
    -- a tree's tags: the @'taggedT'@ combinator will do so for you.

  , Accrue(..) -- :: (Type -> Type) -> Constraint
  , explore    -- :: Accrue t => t Tag -> Test (t Tag)

    -- * Deriving termination tests

    -- | For types which are instances of the @'Generic'@ class, it's possible
    -- to derive an instance for @'Terminable'@ automatically, provided a few
    -- simple rules are met.
    --
    -- Next, you can simply use @deriving@ in order to derive the needed @'Generic'@ classes,
    -- as well as the @'Terminable'@ class (assuming @-XDeriveAnyClass@):
    --
    -- @
    -- data Exp
    --   = ...
    --   deriving ('Eq', 'Show', 'GHC.Generic', 'Generic', 'HasDatatypeInfo', 'Terminable')
    -- @

  , gtest -- :: (Generic, ...) => Test a

    -- * Utilities

    -- | These are some extra utilities, mainly designed to help users write
    -- iterative function pipelines that safely and automatically handle
    -- divergence.
    --
    -- The most general utility here is @'enchant'@ combined with a @'Spell'@
    -- -- all other combinators are derived from this one. @'enchant'@ is
    -- effectively a generalized version of @'enhance'@ that curries the
    -- underlying mealy machine structure

  , overhaul  -- :: Test a -> (a -> a)       -> (a -> a)
  , improve   -- :: Test a -> (a -> Maybe a) -> (a -> a)
  , enhance   -- :: Test a -> ...            -> (a -> a)

  , Spell(..) -- :: Type -> Type -> Type
  , enchant   -- :: Test a -> (b -> a -> Spell b a) -> b -> (a -> a)

    -- * Re-exports

  , module Data.Fix
  , module Data.Fix.Rose
  , module Data.Fix.List
  , module Data.TagBag

  ) where

--------------------------------------------------------------------------------
-- Imports

-- base
import           Control.Exception
import           Data.Bool
import           Data.Bifunctor                       ( bimap )
import           Data.Function                        ( on )
import           Data.Functor.Contravariant
import           Data.Kind                            ( Type )
import           Data.Proxy
import           Data.Void                            ( Void, absurd )
import           Unsafe.Coerce                        ( unsafeCoerce )

-- containers
import           Data.Map          ( Map )
import           Data.IntMap       ( IntMap )
import qualified Data.IntMap                  as IM
import qualified Data.Map                     as M

-- contravariant
import           Data.Functor.Contravariant.Divisible

-- generics-sop
import qualified Generics.SOP as G

-- self: avc
import           Data.Fix
import           Data.Fix.List
import           Data.Fix.Rose
import           Data.TagBag

-- FIXME: remove these
import Data.Coerce
import Control.Applicative

--------------------------------------------------------------------------------
-- Documentation

{- $intro

A question that commonly arises in tools like compilers or theorem provers
asks: /"will this process or activity terminate?"/ For example, if a compiler
inlines a recursive function, it cannot do so forever, but may do so a limited
number of times. This question of whether the inlining process /terminates/ can
be stated in a more direct way:

* __The online termination problem__: given a possibly-infinite sequence of
values \( x_0, x_1, x_2, ... \), with the elements provided one-by-one, shout
"'Stop'!" if the sequence looks like it is /diverging/: if the process
producing these values will looks like it will continue forever. Try not to
shout "@'Stop'@" for any finite sequence, but always guarantee to shout
"@'Stop'@" at some point in every infinite sequence.

The test is \"online\" in the sense that terms are presented one-by-one to an
oracle that tells us whether to @'Stop'@ or not.

We might think of /divergence/ the same way we think of the Haskell infinte
loop @let x = x in (x :: a)@, commonly known as "bottom" or simply
@'undefined'@. Attempting to evaluate this term results in an infinte loop, and
we might like to know if some process or system would do the same.

Termination is a particular problem for compilers or program transformation
tools. /Supercompilation/, which is a simple implementation of partial
evaluation, is such a problem: how do we know when tasks like inlining
functions will terminate? In this scenario, the values \( p_0, p_1, p_2, ... \)
might be successive transformations of the input program \( p \), defined by
some language semantics.

This module provides an an API for describing /termination tests/ which
implement the algorithm described above: they let us ask, given a @'Test' a@
for some type @a@, when a sequence of @a@ values will diverge. These tests are
/sound/, so all divergent and infinite sequences will eventually halt.

-}

{- $usage

The core idea behind the API is to begin with a termination test:

@
tt :: 'Test' a  
@

This test describes, in some sense, how to see if a sequence of values @a@ is
eventually diverging, or if the process producing @a@ values will continue
forever. We might intuitively think of a value of type @'Test' a@ as
\"consuming values of type @a@\ over time" -- every \( p_n \) value in the
sequence is consumed by the @'Test'@. This gives rise to the notion that
@'Test'@ is a @'Contravariant'@ functor:

@
'contramap' :: (a -> b) -> 'Test' b -> 'Test' a
@

And so, for any @'Test'@ which consumes values of type @b@, you may create a
@'Test'@ consuming values of type @a@ -- providing you have a method for
turning @a@s into @b@s.

In order to keep track of the sequence of values we've submitted so far (the
\"online\" problem), we use a 'History' data type in order to preserve that
information. Every time we test if a new value @a@ is beginning to diverge, we
need a @'History'@, including the first value. We can create an initial
@'History'@ from a @'Test'@ defined for a type:

@
initialHistory = 'history' tt :: 'History' a
@

All types which have a @'Test'@ defined implement the @'Terminable'@ type class
with the polymorphic @'test'@ method, so the above could have also been
written:

@
initialHistory = 'history' 'test' :: 'Terminable' a => 'History' a
@

Once we have the initial history, we can use the @'check'@ function to begin
testing values for divergence:

@
'check' :: 'History' a -> a -> 'TestResult' a

func :: Terminable a => a -> a
func a = case ('history' 'test' \`check` a) of
  'Stop' -> a
  'Continue' newHistory -> ...
@

If @'check'@ returns @'Stop'@, then the sequence will diverge, so we should
halt the process and return the given value. But if you can @'Continue'@, then
you may create a new @a@ and submit it and run the above test again. The
@'Stop'@ value will always be returned, even if the set of possible values we
try to submit is infinite.

As a concrete example, consider a program optimizer. You may @'check'@ a value,
and then if you can @'Continue'@, optimize it, and @'check'@ the result.
Inevitably you will be told to @'Stop'@, and should exit. By providing this
guarantee, it's possible, for instance, to inline recursive functions without
any specified threshold -- the library will inevitably @'Stop'@ for you.

This can be encapsulated using the @'foldTerminable'@ function, which is a
simple function that manages a @'History'@ for you and \"folds\" over a
termination test and value, stopping when told to @'Stop'@, or when the
user-specified function returns @'Nothing'@. For instance, given a language
small-step function, and a termination test:

@
data Exp

step :: Exp -> 'Maybe' Exp
expT :: 'Test' Exp
@

The following function acts like a superoptimizer:

@
supercompile :: Exp -> Exp
supercompile = 'foldTerminable' expT step
@

-}

--------------------------------------------------------------------------------
-- Types

-- | The result of a single termination test. A single test may result in a
-- value of @'Stop'@, which means that the sequence will begin to diverge. A
-- result of @'Continue' h@ returns a new history h, and indicates it is safe
-- to continue.
data TestResult a where
  -- | __Stop__: this sequence of terms has begun to diverge, and therefore we
  -- can halt now.
  Stop
    :: TestResult a

  -- | __Continue__: it's safe to continue moving forward with new sequence
  -- entires. The returned @'History'@ value is to be used in all future
  -- calls to @'check'@.
  Continue
    :: History a
    -> TestResult a

-- | A recorded history of values of type @a@. Given a @'History' a@, you can
-- use the function @'check' :: 'History' a -> a -> 'TestResult' a@ to see if
-- the the sequence is diverging.
data History a where
  H :: (a -> TestResult a) -> History a

-- | Test a value @a@ with the given @'History'@, and return a @'TestResult'@
-- telling you whether to @'Continue'@ course or not. If this is the first
-- value to be @'check'@ed, then you can use the @'history'@ function along
-- with @'test'@ or some @'Test' a@ value in order to construct the initial
-- @'History'@. Otherwise, you __MUST__ use a @'History'@ returned to you by
-- @'Continue'@, indicating it is safe to keep submitting values.
check
  :: History a
  -- ^ @'History'@ of values present in the sequence.
  -> a
  -- ^ Value to @'Test'@.
  -> TestResult a
  -- ^ To be, or not to be?
check (H k) a = k a

-- | A \"termination test\". A @'Test'@ encapsulates a test routine that, given
-- a possibly infinite sequence of terms, may shout \"stop!\" if it looks like
-- the sequence will diverge.
--
-- Termination tests are @'Contravariant'@ functors: they are \"fed\" values of
-- type @a@, which is added to the @'History'@ when running @'check'@.
-- Furthermore, termination tests are @'Divisible'@ and @'Decidable'@: they
-- admit both product and sum compositions, so you can use @'divided'@ and
-- @'chosen'@ to create new @'Test'@s from compositions of old ones. Hence a
-- termination test is /modular/: in the same way that you can build arbitrary
-- data types from simple product @(,)@, sum (@'Either'@) and recursive
-- @('Fix')@ types, you can build arbitrary termination tests the same way.
--
-- Three combinators from the original paper are more generally expressed
-- in terms of @'Contravariant'@:
--
-- @
-- eitherT = 'choose' 'id' = 'chosen'  :: 'Test' a -> 'Test' b -> 'Test' ('Either' a b)
-- pairT   = 'divide' 'id' = 'divided' :: 'Test' a -> 'Test' b -> 'Test' (a, b)
-- alwaysT = 'conquer' :: 'Test' a
-- @

#ifndef ENABLE_OPTIMIZED_HISTORY 
newtype Test (a :: Type) = Test (Coyoneda Equivalence a)
  -- DerivingVia is amazing.
  deriving (Contravariant, Divisible, Decidable) via (Coyoneda Equivalence)
#else
newtype Test (a :: Type) = Test (Coyoneda Equiv a)
  deriving (Contravariant, Divisible, Decidable) via (Coyoneda Equiv)
#endif

-- 'Test' is a well-quasi-order: a non-symmetric equality relation. In
-- "Termination Combinators Forever", the original specification is equivalent
-- the Predicate functor available in base, while an optimized version is
-- actually the contravariant CoYoneda transformation of Predicate. we use this
-- exact representation, and derive the implementation of 'Test' as well as its
-- fundamental combinators from downstream instances, using DerivingVia.
--
-- theory: Coyoneda Equivalence a ~ (a -> a -> Bool)
-- proof:
--
--     data T0   a =           T0          (a -> a -> Bool)    -- defn of 'Equivalence'
--     data T1 r a =           T1          (a -> a -> r)       -- generalize to binary relations with type 'r'
--     data T2 r a = forall b. T2 (a -> b) (b -> b -> r)       -- introduce lambda, with quantifier
--     data T3   a = forall b. T3 (a -> b) (b -> b -> Bool)    -- r ~ Bool
--     data T4   a = forall b. T4 (a -> b) (Equivalence b)     -- defn of Equivalence
--     data T5 f a = forall b. T5 (a -> b) (f b)               -- generalize: Equivalence ~ f
--     type T6 f a = Coyoneda f a                              -- defn of contravariant Coyoneda
--     type T7   a = Coyoneda Equivalence a                    -- f ~ Equivalence
--
-- This implementation allows us to automatically derive contramap, eitherT,
-- and pairT from their upstream instances automatically.
--
-- The above uses the simple case invoving 'Equivalence', but the same general
-- proof holds for any result type r, including r ~ ( Bool, Bool ) for the
-- optimized implementation that prunes histories (using 'Equiv')
--


-- The bi-directional 'WQO' pattern allows the implementation to use the exact
-- same code from the paper -- it maps the given pattern to the different
-- underlying representation we've chosen.
#ifndef ENABLE_OPTIMIZED_HISTORY 
pattern WQO :: forall a. forall b. (a -> b) -> (b -> b -> Bool) -> Test a
pattern WQO co eq = Test (Coyoneda co (Equivalence eq))
#else
pattern WQO :: forall a. forall b. (a -> b) -> (b -> b -> (Bool, Bool)) -> Test a
pattern WQO co eq = Test (Coyoneda co (Equiv eq))
#endif

{-# COMPLETE WQO #-}

{-- Original implementation of 'Test', from the paper

data Test (a :: Type) where
  -- | A termination test is encoded as a \"well-quasi-ordering\" of the
  -- underlying data type. The type variable @b@ is existentially quantified:
  -- any two WQOs cannot unify these types, only the input types @a@.
  --
  -- WQOs are binary relations of the form @R :: a -> a -> 'Bool'@. They are
  -- functorial: the type variable @'a'@ is in negative position, making them
  -- contravariant, not covariant, functors.
  --
  -- In this implementation we split a WQO into two functions: a /map/ and a
  -- relation @R@. The map is purely an optimization: it allows us to compose
  -- type-changing transformations while avoiding the recomputation of the
  -- relation @R@ on every composition. (This is effectively an application
  -- of the coyoneda transformation on an @'Equivalence'@ relation.)

  WQO
    :: (a -> b)
    -- ^ A \"map\" function. This parameter exists as an optimization for
    -- @'Contravariant'@: it allows us to compose type-changing transformations
    -- on input values rather than computing them immediately. See the
    -- @'contramap'@ implementation for more details.

    -> (b -> b -> Bool)
    -- ^ A relation @R@ between two values expressing a well-quasi-order (WQO).

    -> Test a

instance Contravariant Test where
  contramap :: (b -> a) -> Test a -> Test b
  contramap g (WQO f k) = WQO (f . g) k

-- | Contravariant @'Control.Applicative.Applicative'@
instance Divisible Test where
  conquer :: Test a
  conquer = WQO id (\_ _ -> True)

  divide :: (a -> (b, c)) -> Test b -> Test c -> Test a
  divide cleave (WQO fa ta) (WQO fb tb) = 
    WQO (bimap fa fb . cleave) $ \b1 b2 ->
      (ta `on` fst) b1 b2 && (tb `on` snd) b1 b2

-- | Contravariant @'Control.Applicative.Alternative'@
instance Decidable Test where
  lose :: (a -> Void) -> Test a
  lose f = WQO f (\a b -> absurd (a <> b))

  choose :: (a -> Either b c) -> Test b -> Test c -> Test a
  choose fork (WQO fa ta) (WQO fb tb) = 
    WQO (bimap fa fb . fork) $ \l r -> case (l, r) of
      (Left a1, Left a2)   -> a1 `ta` a2
      (Right b1, Right b2) -> b1 `tb` b2
      (_, _)               -> False

alwaysT :: Test a
alwaysT = conquer

pairT :: Test a -> Test b -> Test (a, b)
pairT = divide id

eitherT :: Test a -> Test b -> Test (Either a b)
eitherT = choose id
--}

--------------------------------------------------------------------------------
-- History

-- | Given a @'Test'@, create an empty @'History'@ for it. This must be the
-- initial @'History'@ used when you begin to @'check'@ values.
--
-- Generally, if the type you're testing is already an instance of
-- @'Terminable'@ (see below), then you can simply use the @'test'@ combinator
-- in order to get a proper @'Test'@:
--
-- @
-- anyHistory :: 'Terminable' a => 'History' a
-- anyHistory = 'history' 'test'
-- @
history
  :: Test a
  -- ^ Termination test.
  -> History a
  -- ^ A @'History'@ which can be used to begin running @'check'@s. 
history (WQO f (<|)) = H (go []) where
#ifndef ENABLE_OPTIMIZED_HISTORY 
  go ys v = case f v of
    y | any (\x -> x <| y) ys -> Stop
      | otherwise             -> Continue $ H $ go (y:ys)
#else
  go ys x = case investigate ys (f x) of
    Nothing  -> Stop
    Just yss -> Continue $ H (go yss)

  investigate ys y
    | or gts    = Nothing
    | otherwise = Just (y : [ v | (False, v) <- lts `zip` ys ])
    where (gts, lts) = unzip (map (<| y) ys)
#endif

--------------------------------------------------------------------------------
-- Fundamental termination combinators

-- | A termination test for any set of /finite/ values with an equality
-- relation defined.
eqT :: Eq a => Test a

-- | A termination test for any set of /finite/ values with an ordering
-- relation defined.
ordT :: Ord a => Test a

#ifndef ENABLE_OPTIMIZED_HISTORY 
eqT  = WQO id (==)
ordT = WQO id (<=)
#else
eqT  = WQO id (symm (==))
ordT = WQO id (symm (<=))

-- lift any equivalence relation into its "symmetric duplicate"
symm :: (a -> a -> Bool) -> (a -> a -> (Bool, Bool))
symm (<|) = \a b -> (a <| b, b <| a)
#endif

-- | A class for types that have a termination test defined. Note that a type
-- @t@ must be /finite/: it must have a countable, non-infinite number of
-- distinct inhabitants.
class Terminable a where
  {-# MINIMAL test #-}

  -- | A termination @'Test'@ for some type @a@.
  test :: Test a
#ifdef ENABLE_GENERICS
  test = gtest

  default test
    :: ( G.Generic a
       , G.HasDatatypeInfo a
       , G.Code a ~ '[xs]
       , G.All Terminable xs
       ) => Test a
#endif

instance Terminable ()   where test = eqT
instance Terminable Bool where test = eqT
instance Terminable Int  where test = ordT
instance Terminable Char where test = ordT

instance (Terminable a, Terminable b) => Terminable (Either a b) where
  test = chosen test test

instance (Terminable a, Terminable b) => Terminable (a, b) where
  test = divided test test

instance (Ord k, Eq k, Terminable v) => Terminable (Map k v) where
  test = mapT test

instance Terminable a => Terminable (Maybe a) where
  test = maybeT test

instance Terminable v => Terminable (IntMap v) where
  test = intMapT test

instance Terminable a => Terminable [a] where
  test = listT test

instance Terminable a => Terminable (Tree a) where
  test = treeT test

--------------------------------------------------------------------------------

-- data Identity a = Id a

class Fooish a where
  fooish :: a

-- | Termination tests for @'Generic'@ datatypes, defined using
-- @<https://hackage.haskell.org/package/generics-sop generics-sop>@.
gtest
  :: forall a xs.
     ( G.Generic a
     , G.HasDatatypeInfo a
     , G.Code a ~ '[xs]
     , G.All Fooish xs
     ) => a
gtest = G.to $ gtest' (G.datatypeInfo (Proxy :: Proxy a))

gtest' :: G.All Fooish xs => G.DatatypeInfo '[xs] -> G.SOP G.I '[xs]
gtest' d = gtestFor (G.hd (G.constructorInfo d))

gtestFor :: G.All Fooish xs => G.ConstructorInfo xs -> G.SOP G.I '[xs]
gtestFor (G.Constructor _) = G.SOP $ G.Z $ spineWithNames (G.hpure (G.K ""))
gtestFor (G.Infix _ _ _)   = G.SOP $ G.Z $ spineWithNames (G.hpure (G.K ""))
gtestFor (G.Record _ fs)   = G.SOP $ G.Z $ spineWithNames (G.hliftA sfieldName fs)
  where
    sfieldName :: G.FieldInfo a -> G.K String a
    sfieldName (G.FieldInfo n) = G.K n

spineWithNames :: (G.All Fooish xs, G.SListI xs) => G.NP (G.K String) xs -> G.NP G.I xs
spineWithNames = G.hcliftA __ps aux
  where
    aux (G.K "") = G.I $ fooish
    aux (G.K n)  = G.I $ mapException (addFieldName n) fooish

addFieldName :: G.FieldName -> ErrorCall -> ErrorCall
addFieldName n (ErrorCallWithLocation str loc) =
  ErrorCallWithLocation (n ++ ": " ++ str) loc

__ps :: Proxy Fooish
__ps = Proxy

--------------------------------------------------------------------------------
-- Extra termination combinators

-- | Construct a termination test for @'Map'@s.
mapT :: (Ord k, Eq k) => Test v -> Test (Map k v)
mapT (WQO fv tv) = WQO (M.map fv) t where
  ok m1 (k2, v2) = case M.lookup k2 m1 of
    Just v1 -> v1 `tv` v2
    Nothing -> error "mapT"
#ifndef ENABLE_OPTIMIZED_HISTORY
  t m1 m2 = M.keysSet m1 == M.keysSet m2 &&
            all (ok m1) (M.assocs m2)
#else
  t m1 m2 =
    let kb   = M.keysSet m1 == M.keysSet m2
        ab f = all (f . ok m1) (M.assocs m2)
    in (kb && ab fst, kb && ab snd)
#endif

-- | Construct a termination test for @'IntMap'@s.
intMapT :: Test v -> Test (IntMap v)
intMapT (WQO fv tv) = WQO (IM.map fv) t where
  ok m1 (k2, v2) = case IM.lookup k2 m1 of
    Just v1 -> v1 `tv` v2
    Nothing -> error "intMapT"
#ifndef ENABLE_OPTIMIZED_HISTORY 
  t m1 m2 = IM.keysSet m1 == IM.keysSet m2 &&
            all (ok m1) (IM.assocs m2)
#else
  t m1 m2 =
    let kb   = IM.keysSet m1 == IM.keysSet m2
        ab f = all (f . ok m1) (IM.assocs m2)
    in (kb && ab fst, kb && ab snd)
#endif

-- | Construct a termination @'Test'@ for @'Maybe'@s.
maybeT :: Test a -> Test (Maybe a)
maybeT wqo = inject `contramap` (chosen conquer wqo)
  where
    inject Nothing = Left ()
    inject (Just x) = Right x

-- | Construct a termination @'Test'@ for @'Int'@s.
intT :: Test Int
intT = ordT

-- | Termination test for rose @'Tree'@s of type @a@.
treeT :: forall a. Test a -> Test (Tree a)
treeT eltT = gfixT fix'
  where
    inject (NodeF x ts) = (x, ts)

    fix' :: Test r -> Test (TreeF a r)
    fix' subT = inject `contramap` divided eltT (listT subT)

-- | Convert a termination tests for values of type @a@ into tests of values
-- of type @[a]@.
listT :: forall a. Test a -> Test [a]
listT eltT = fromListF `contramap` (gfixT fix')
  where
    inject NilF         = Left ()
    inject (ConsF y ys) = Right (y, ys)

    fix' :: Test r -> Test (ListF a r)
    fix' tailT = inject `contramap` chosen eqT (divided eltT tailT)

--------------------------------------------------------------------------------
-- Recursive termination combinators

-- | @'fixT'@ can be used to construct a termination test out of any
-- @'Functor'@ which has its recursive components explicitly defined in terms
-- of @'Fix'@.
--
-- Note that @'fixT'@ only supports /equi-recursive/ types, that is, recursive
-- types which refer to themselves -- mutually recursive types are not
-- supported. (note the presence of types that highlight this, such as the
-- gatherer, which has type @t rec -> t [rec]@.)
fixT
  :: Functor t
  => (forall rec. t rec -> [rec])
  -- ^ The \"gatherer\", which collects the \"children\" of a @'Functor' t@
  -- term given by @t rec@.

  -> (forall rec. t rec -> t rec)
  -- ^ The \"calibrator\".

  -> (forall rec. Test rec -> Test (t rec))
  -- ^ The \"injector\", which injects terms of type @'Test' a@ into terms of
  -- type @'Test' (t a)@ for your @'Functor'@.

  -> Test (Fix t)
  -- ^ A test for an open, equi-recursive @'Fix'@ type.

fixT kids p f = wqo
  where
    eta :: Test a -> Test a
    eta tt = WQO g h where
      g = case tt of WQO f' _  -> unsafeCoerce f'
      h = case tt of WQO _  t' -> unsafeCoerce t'

    wqo = case f (eta wqo) of
      WQO inj k -> WQO wrap mtest
        where
          wrap (Roll t) = RollMem { memIt = inj (p t)
                                  , memKids = map wrap (kids t)
                                  }

#ifndef ENABLE_OPTIMIZED_HISTORY 
          mtest a b = memIt a `k` memIt b || mtest a `any` memKids b
#else
          mtest a b =
            let ( r0, r1 ) = memIt a `k` memIt b
            in ( r0 || (fst . mtest a) `any` memKids b
               , r1 || (snd . mtest a) `any` memKids b
               )
#endif
            

-- | Generalized @'fixT'@ for any @'Traversable'@, @'Fix'@point datatype, built
-- on top of @'fixT'@. This simply traverses the structure and gathers the
-- resulting values immediately, and provides correct-by-construction
-- implementations of both the gatherer and calibrator.
--
-- In the common case, you'll simply describe your functor and use
-- @DeriveFunctor@, @DeriveFoldable@, and @DeriveTraversable@ in order to
-- derive the needed instances automatically. Then you simply need to provide a
-- proper injector function and you'll be done.
gfixT
  :: Traversable t
  => (forall rec. Test rec -> Test (t rec))
  -- ^ The \"injector\", which injects terms of type @'Test' a@ into terms of
  -- type @'Test' (t a)@ for your @'Functor'@.

  -> Test (Fix t)
  -- ^ A test for an open, equi-recursive @'Traversable'@ @'Fix'@ type.

gfixT = fixT (runGather . traverse (\x -> Gather (x:))) id

--
-- extra details
--

-- used to memoise per-element work of datatype fixed points
data FixMem b = RollMem { memIt :: b, memKids :: [FixMem b] }

newtype Gather a b = Gather ([a] -> [a])

runGather :: Gather a b -> [a]
runGather (Gather g) = g []

instance Functor (Gather a) where
  fmap _ (Gather xs) = Gather xs

instance Applicative (Gather a) where
  pure _ = Gather id
  Gather xs <*> Gather ys = Gather (xs . ys)

--------------------------------------------------------------------------------
-- Traversing and collecting TagBags

-- | Accrue @'Tag'@s across some type @t@ into a @'TagBag'@, which can be used
-- to build termination criteria for a @'Test'@ automatically.
class Accrue (t :: Type -> Type) where
  {-# MINIMAL gather #-}

  -- | Explore a type @t@ with @'Tag'@s attached, and produce @'TagBag'@s from
  -- it. This operation alone allows us to define generic termination tests for
  -- any type which has an instance. This function is intended to
  gather :: t Tag -> TagBag

-- | A termination test for any type @t@ which can be defined in terms of
-- a @'TagBag'@, described by its @'Accrue'@ instance.
explore :: Accrue t => Test (t Tag)
explore = gather `contramap` (toMap `contramap` intMapT intT)

--------------------------------------------------------------------------------
-- Utilities

-- | @'Spell'@ captures the common functor which appears in the argument of the
-- @step@ and @skip@ functions used for @'enhance'@.
--
-- @
-- step  :: a -> 'Maybe' a
-- skip  :: a -> 'Bool'
-- spell :: a -> 'Spell' a
--
-- 'Nothing' ~ 'Cast'
-- 'Just' a  ~ 'Focus' a, iff skip a
-- 'Just' a  ~ 'Power' a
--
-- spell ~ (skip, step)
-- @
data Spell (a :: Type)
  = Cast
  -- ^ Cast the spell: tells the enhancement process to stop, and return the
  -- latest, final value
  | Focus a
  -- ^ Focus the spell.
  | Power a
  -- ^ Power the spell.

-- | A powerful variant of @'enhance'@ -- @'enchant'@ acts like a spell that
-- allows you to magically improve a function. @'Spell'@ is effectively a way
-- of currying the @skip@ and @step@ functions for @'enhance'@, which is a
-- mealy machine. In other words, it captures the common @(b -> a -> _)@
-- functor which appears in argument position.
enchant
  :: Test a
  -- ^ Termination test for values of type @a@
  -> (a -> Spell a)
  -- ^ Spell logic
  -> (a -> a)
  -- ^ Enhanced function
enchant tt spell = go (history tt) where
  go hist = \state -> case spell state of
    Cast -> state
    Focus state' -> go hist state'
    Power state' -> case check hist state' of
      Stop           -> state'
      Continue hist' -> go hist' state

-- | A generalization of @'improve'@, which we can use to enhance a function,
-- like a powerful fold, but with more precise control over the termination
-- criteria, and always guaranteed to terminate at some point even for infinite
-- sequences. It is similar to a mealy machine, for termination @'Test'@s.
enhance
  :: Test a
  -- ^ Termination test for values of type @a@
  -> (a -> Maybe a)
  -- ^ \"Improvement\" function.
  -> (a -> Bool)
  -- ^ \"Skip\" predicate.
  -> (a -> a)
  -- ^ Enhanced function.

#if 1
enhance tt step skip = enchant tt $ \a -> case step a of
  Nothing -> Cast
  Just a' -> bool (Power a') (Focus a') (skip a')
#else
-- the following function is essentially the higher order version of the
-- reducer in "Supercompilation by Evaluation", section 3.3
enhance tt step skip = go (history tt) where
  go hist = \state -> case step state of
    Nothing -> state
    Just state'
      | skip state' -> go hist state'
      | otherwise      -> case check hist state' of
          Stop           -> state'
          Continue hist' -> go hist' state'
#endif

-- | A generalization of @'overhaul'@, which allows the improvement function to
-- possibly return @'Nothing'@, indicating it should stop immediately.
improve
  :: Test a
  -- ^ Termination test for values of type @a@
  -> (a -> Maybe a)
  -- ^ Transformation function. If the sequence has yet to diverge, this can
  -- produce a new value to be tested, or end when it chooses.
  -> (a -> a)
  -- ^ Transformed function.
improve tt step = enhance tt step (const False)

-- | \"Overhaul\" any function into a super-powered version that will be
-- applied repeatedly until divergence is detected. The function @'overhaul'
-- tt@ for some @tt :: 'Test' a@ is operationally similar to the following
-- function:
--
-- @
-- bottom :: (a -> a) -> (a -> a)
-- bottom k = \a -> 'last' ('iterate' k a)
-- @
--
-- except, rather than looping forever, @'overhaul'@ will end when it looks like
-- the iterated sequence is beginning to diverge.
--
-- @
-- 'overhaul' k = 'improve' ('Just' . k)
-- @
overhaul
  :: Test a
  -- ^ Termination test for values of type @a@
  -> (a -> a)
  -- ^ Input function
  -> (a -> a)
  -- ^ Overhauled function
overhaul tt step = enhance tt (Just . step) (const False)

--------------------------------------------------------------------------------
-- Bonus nonsense

--
-- the power of DerivingVia
--

{--

newtype Test0 (a :: Type) = Test0 (a -> a -> Bool)
  deriving (Contravariant, Divisible, Decidable) via Equivalence

newtype Test1 (a :: Type) = Test1 (Equivalence a)
  deriving newtype (Contravariant, Divisible, Decidable)

newtype Test2 a = Test2 (a -> a -> Bool)
  -- this is ok
  deriving (Contravariant, Divisible, Decidable) via Equivalence

data Testing2 a where
  Testing2 :: (a -> b) -> (b -> b -> Bool) -> Testing2 a
  -- these don't work
--deriving (Contravariant, Divisible, Decidable) via Equivalence

--}

--
-- coyoneda bullshit
--

-- Coyoneda transform of a contravariant functor
-- defined here to avoid orphans for Divisible/Decidable
data Coyoneda (f :: Type -> Type) (a :: Type) where
  Coyoneda :: (a -> b) -> f b -> Coyoneda f a

instance Contravariant (Coyoneda f) where
  contramap g (Coyoneda f b) = Coyoneda (f . g) b
  {-# INLINE contramap #-}

-- needs to be in kan-extensions...
instance Divisible f => Divisible (Coyoneda f) where
  conquer :: Coyoneda f a
  conquer = Coyoneda id conquer
  {-# INLINE conquer #-}

  divide :: (a -> (b, c)) -> Coyoneda f b -> Coyoneda f c -> Coyoneda f a
  divide cleave (Coyoneda fb b) (Coyoneda fc c)
    = Coyoneda (bimap fb fc . cleave) (divided b c)
  {-# INLINE divide #-}

-- needs to be in kan-extensions...
instance Decidable f => Decidable (Coyoneda f) where
  lose :: (a -> Void) -> Coyoneda f a
  lose f = Coyoneda id (lose f)
  {-# INLINE lose #-}

  choose :: (a -> Either b c) -> Coyoneda f b -> Coyoneda f c -> Coyoneda f a
  choose fork (Coyoneda fb b) (Coyoneda fc c)
    = Coyoneda (bimap fb fc . fork) (chosen b c)
  {-# INLINE choose #-}

--
-- a better equivalence relation: section 6.1
--

-- 'Equivalence' but with "symmetric duplication": the result of both R(a,b)
-- and R(b,a) is recorded
newtype Equiv a = Equiv (a -> a -> (Bool, Bool))

instance Contravariant Equiv where
  contramap f (Equiv g) = Equiv (\x y -> f x `g` f y)

instance Divisible Equiv where
  conquer = Equiv (\_ _ -> (True, True))
  divide f (Equiv g) (Equiv h) = Equiv $ \a b -> case f a of
    (a', a'') -> case f b of
      (b', b'') -> 
        let (r0,r2) = g a'  b'
            (r1,r3) = h a'' b''
        in (r0 && r1, r2 && r3)

instance Decidable Equiv where
  lose f = Equiv (\a b -> absurd (f a <> f b))
  choose f (Equiv g) (Equiv h) = Equiv $ \a b -> case f a of
    Left c -> case f b of
      Left d  -> g c d
      Right{} -> (False, False)
    Right c -> case f b of
      Left{}  -> (False, False)
      Right d -> h c d

{-- -- DEBUG

--
-- other nonsense
--

-- Day convolution of two functors: covariant product
data Day (f :: Type -> Type) (g :: Type -> Type) (a :: Type) where
  Day
    :: f b
    -> g c
    -> (b -> c -> a)
    -> Day f g a

-- Night convolution of two functors: contravariant product
data Night (f :: Type -> Type) (g :: Type -> Type) (a :: Type) where
  Night
    :: f b
    -> g c
    -> (a -> (b, c))
    -> Night f g a

-- Sun storm of two functors: covariant sum
data Sun (f :: Type -> Type) (g :: Type -> Type) (a :: Type) where
  Sun
    :: f b
    -> g c
    -> (b -> a)
    -> (c -> a)
    -> Sun f g a

-- Moon storm of two contravariant functors: contravariant sum
data Moon (f :: Type -> Type) (g :: Type -> Type) (a :: Type) where
  Moon
    :: f b
    -> g c
    -> (a -> Either b c)
    -> Moon f g a

instance Functor (Day f g) where
  fmap f (Day fb gc bca) = Day fb gc (\b c -> f (bca b c))
  {-# INLINE fmap #-}

instance Contravariant (Night f g) where
  contramap f (Night fb gc abc) = Night fb gc (abc . f)
  {-# INLINE contramap #-}

instance Functor (Sun f g) where
  fmap f (Sun fb gc ba ca) = Sun fb gc (f . ba) (f . ca)
  {-# INLINE fmap #-}

instance Contravariant (Moon f g) where
  contramap f (Moon fb gc abc) = Moon fb gc (abc . f)
  {-# INLINE contramap #-}

diag :: f a -> Night f f a
diag fa = Night fa fa (\a -> (a, a))

contract :: Divisible f => Night f f a -> f a
contract (Night ca cb k) = divide k ca cb

instance Divisible f => Divisible (Night f f) where
  conquer :: Night f f a
  conquer = diag conquer

  divide :: (a -> (b, c)) -> Night f f b -> Night f f c -> Night f f a
  divide cleave fb fc = Night (contract fb) (contract fc) cleave
--}
