{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Fpc.Ast
  ( Var
  , Bind, unbind, bind
  , TTyp (..), Typ, nat, (-:>)
  , TExp (..), Exp
  , expvar, zero, succ
  , ifz, lam, fixp, ap, subst
  )
  where

import Prelude hiding (succ)

import Control.Arrow
import Data.Functor.Foldable
import Data.Functor.Classes

import Text.Show.Deriving
import Data.Eq.Deriving

{- TODO: change to use De Bruijin indices -}
type Var = String

type Bind = (,)

unbind :: (Bind Var a) -> (Var, a)
unbind = id

bind :: Var -> a -> Bind Var a
bind v x = (v,x)

data TTyp a = Nat
            | a :-> a
            deriving (Eq, Show, Functor)

$(deriveShow1 ''TTyp)
$(deriveEq1 ''TTyp)

type Typ = Fix TTyp

nat :: Typ
nat = Fix Nat

(-:>) :: Typ -> Typ -> Typ
(-:>) = curry $ Fix . uncurry (:->)

data TExp a = EVar Var
            | Z
            | Succ a
            | Ifz a a (Bind Var a)
            | Lam Typ (Bind Var a)
            | Ap a a
            | FixP Typ (Bind Var a)
            deriving (Show, Functor)

$(deriveShow1 ''TExp)

type Exp = Fix TExp

expvar :: Var -> Exp
expvar = Fix . EVar

zero :: Exp
zero = Fix Z

succ :: Exp -> Exp
succ = Fix . Succ

ifz :: Exp -> Exp -> Bind Var Exp -> Exp
ifz e e0 xe1 = Fix (Ifz e e0 xe1)

lam :: Typ -> Bind Var Exp -> Exp
lam = curry $ Fix . uncurry Lam

ap :: Exp -> Exp -> Exp
ap = curry $ Fix . uncurry Ap

fixp :: Typ -> Bind Var Exp -> Exp
fixp = curry $ Fix . uncurry FixP

-- subst e' x e = [e'/x]e
subst :: Exp -> Var -> Exp -> Exp
subst e' x = cata (go e' x)
  where
    go e' x (EVar v) = if x == v then e' else expvar v
    go e' x t = Fix t

