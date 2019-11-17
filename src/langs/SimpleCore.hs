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
-- Module      : SimpleCore
-- Copyright   : (c) Austin Seipp 2019
--               (c) Max Bolingbroke 2011-2019
-- License     : BSD3
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This file contains a sketch of the untyped @Exp@ lambda calculus described
-- by <https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/termination-combinators.pdf
-- "Termination Combinators Forever">, by Peyton Jones, and Bolingbroke,
-- section 7.

module SimpleCore
  ( main -- :: IO ()
  ) where

--------------------------------------------------------------------------------
-- Imports

-- base
import           Data.Functor.Contravariant           ( Contravariant(..) )
import           Data.List

-- generics-sop
import           Generics.SOP
import qualified GHC.Generics as GHC

-- self: supercomp
import           Control.Termination

--------------------------------------------------------------------------------
-- Example: small-step lambda calculus

data Name = Map | Foldr | Even
  deriving (Eq, Show, GHC.Generic, Generic, HasDatatypeInfo)

----------------------------------------

data Exp where
  EVar   :: String -> Exp
  EFnVar :: Name -> Exp
  EApp   :: Exp -> Exp -> Exp
  ELam   :: String -> Exp -> Exp
  deriving (Eq, Show, GHC.Generic, Generic, HasDatatypeInfo)

data Node = VarN | FnVarN Name | AppN | LamN
  deriving (Eq, Show, GHC.Generic, Generic, HasDatatypeInfo)

-- | A termination test for a lambda calculus @'Exp'@.
expT :: Test Exp
expT = contramap inject (treeT eqT) where
  inject (EFnVar x) = toTree (FnVarN x) []
  inject (EVar _)   = toTree VarN []
  inject (EApp f e) = toTree AppN [ inject f, inject e ]
  inject (ELam _ e) = toTree LamN [ inject e ]

instance Terminable Exp where test = expT

----------------------------------------
-- Tagged Syntax Trees

type TExp = (TExp', Tag)
data TExp' where
  TEVar   :: String -> TExp'
  TEFnVar :: Name -> TExp'
  TEApp   :: TExp -> TExp -> TExp'
  TELam   :: String -> TExp -> TExp'
  deriving (Eq, Show, GHC.Generic, Generic, HasDatatypeInfo)

instance Accrue ((,) TExp') where
  gather = go where
    go (e, tg) = addBag tg <> go' e
    
    go' (TEVar _)     = mempty
    go' (TEFnVar _)   = mempty
    go' (TELam _ _)   = mempty
    go' (TEApp e1 e2) = go e1 <> go e2

instance Terminable TExp where
  -- | Tagged syntax tree termination test. Generated completely automatically
  -- from the @'Accrue'@ instance.
  test = explore 

----------------------------------------

freeVarsT :: TExp -> [String]
freeVarsT (TEFnVar _, _)   = []
freeVarsT (TEVar x, _)     = [x]
freeVarsT (TEApp e1 e2, _) = freeVarsT e1 ++ freeVarsT e2
freeVarsT (TELam x e, _)   = freeVarsT e \\ [x]

substT :: String -> TExp -> TExp -> TExp
substT _ (TEFnVar v, t) _ = (TEFnVar v, t)
substT x (TEVar v, t) new
  | x == v    = new
  | otherwise = (TEVar v, t)
substT x (TELam y e, t) new
  | x == y = (TELam y e, t)
  | x /= y && (y `notElem` freeVarsT new) = (TELam y (substT x e new), t)
  | otherwise = error ("cannot subst " <> show x)
substT x (TEApp t1 t2, t) new = (TEApp (substT x t1 new) (substT x t2 new), t)

compileT :: [ (String, TExp) ] -> TExp -> TExp
compileT ctx = improve explore (eval ctx)
  where
    -- (\x.e) v ~~> e[x/v]
    eval _ (TEApp (TELam x e, _) v, _) = pure (substT x e v)

    -- iff (f ~~> f') then (f n ~~> f' n)
    eval m (TEApp f v, t) = do
      f' <- eval m f
      pure (TEApp f' v, t)

    -- trivial cases
    eval m (TEVar x,   _)  = lookup x m
    eval _ (TEFnVar _, _)  = Nothing
    eval _ (TELam _ _, _)  = Nothing

----------------------------------------

freeVarsE :: Exp -> [String]
freeVarsE (EFnVar _)   = []
freeVarsE (EVar x)     = [x]
freeVarsE (EApp e1 e2) = freeVarsE e1 ++ freeVarsE e2
freeVarsE (ELam x e)   = freeVarsE e \\ [x]

substE :: String -> Exp -> Exp -> Exp
substE _ (EFnVar v) _ = EFnVar v
substE x (EVar v) new
  | x == v    = new
  | otherwise = EVar v
substE x (ELam y e) new
  | x == y = ELam y e
  | x /= y && (y `notElem` freeVarsE new) = ELam y (substE x e new)
  | otherwise = error ("cannot subst " <> show x)
substE x (EApp t1 t2) new = EApp (substE x t1 new) (substE x t2 new)

-- | \"Supercompile\" an expression to normal form, unless it diverges.
compileE :: [ (String, Exp) ] -> Exp -> Exp
compileE ctx = improve test (eval ctx)
  where
    -- (\x.e) v ~~> e[x/v]
    eval _ (EApp (ELam x e) v) = pure (substE x e v)

    -- iff (f ~~> f') then (f n ~~> f' n)
    eval m (EApp f v) = EApp <$> eval m f <*> pure v

    -- trivial cases
    eval m (EVar x)   = lookup x m
    eval _ (EFnVar _) = Nothing
    eval _ (ELam _ _) = Nothing

--------------------------------------------------------------------------------
-- Driver

-- | @'SimpleCore'@ compiler entry point.
main :: IO ()
main = print (compileE mempty exampleE1)
    >> print (compileT mempty exampleT1)

exampleE0, exampleE1 :: Exp
exampleT0, exampleT1 :: TExp

exampleE0 = EVar "one"
exampleE1 = EApp (ELam "x" (EVar "x")) exampleE0

exampleT0 = (TEVar "one", 1)
exampleT1 = (TEApp (TELam "x" (TEVar "x", 4), 3) exampleT0, 2)
