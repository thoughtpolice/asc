--------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-missing-methods  #-}

{-# LANGUAGE BangPatterns               #-}
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
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE NoStarIsType               #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE UndecidableInstances       #-}

--------------------------------------------------------------------------------

-- |
-- Module      : MoreCore
-- Copyright   : (c) Austin Seipp 2019
--               (c) Max Bolingbroke 2011-2019
-- License     : BSD3
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
--
-- Sketch of the core language described in Max's thesis,
-- <https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/bolingbroke-thesis.pdf
-- "Call-by-need Supercompilation">. It is described more briefly in
-- <https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/supercomp-by-eval.pdf
-- "Supercompilation by Evaluation">, 2010, by Bolingbroke, and Peyton Jones.
--
module MoreCore
  ( main -- :: IO ()
  ) where

--------------------------------------------------------------------------------
-- Imports

-- base

-- generics-sop
import           Generics.SOP
import qualified GHC.Generics as GHC

-- self: supercomp
import           Control.Termination

--------------------------------------------------------------------------------
-- Types

type Var = String

data Lit = LitInt Int | LitChar Char
  deriving (GHC.Generic, Generic, HasDatatypeInfo)

data Prim = PrimMul | PrimAdd
  deriving (GHC.Generic, Generic, HasDatatypeInfo)

data DCon
  deriving (GHC.Generic, Generic, HasDatatypeInfo)

data Val
  = Lam Var Term
  | Lit Lit
  | DCon [Var]
  deriving (GHC.Generic, Generic, HasDatatypeInfo)

data Alt
  = LitAlt Lit
  | ConAlt [Var]
  deriving (GHC.Generic, Generic, HasDatatypeInfo)

data Term
  = Var Var
  | Val Val
  | TApp Term Var
  | Primop2 Prim Term Term
  | Let [(Var, Term)] Term
  | Case Term [(Alt, Term)]
  deriving (GHC.Generic, Generic, HasDatatypeInfo)

data StackFrame
  = Update Var
  | Apply Var
  | Scrutinize [(Alt, Term)]
  | AppPrim1 Prim Var
  | AppPrim2 Prim Var
  deriving (GHC.Generic, Generic, HasDatatypeInfo)

type Heap = [ (Var, Term) ]
type Stack = [ StackFrame ]
type State = ( Heap, Term, Stack )

instance Terminable Lit
instance Terminable Prim
instance Terminable DCon
instance Terminable Val
instance Terminable Alt
instance Terminable Term
instance Terminable StackFrame
instance Terminable State

--------------------------------------------------------------------------------
-- Driver

-- | @'MoreCore'@ compiler entry point.
main :: IO ()
main = putStrLn "MoreCore: NIH"
