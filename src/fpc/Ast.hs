{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}

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
type Var = Int

data Bind a b = Abs b deriving (Eq, Show, Functor, Show1)

unbind :: (Bind Var a) -> (Var, a)
unbind (Abs a) = (0, a)

bind :: Var -> a -> Bind Var a
bind v x = Abs x

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
subst e' _ e = cata (go e') e 0
  where
    go :: Exp -> TExp (Int -> Exp) -> Int -> Exp
    go e' (EVar v) i = if i == v then e' else expvar v
    go e' (Ifz f f0 xf1) i =
      let (v, f1) = unbind xf1
       in ifz (f i) (f0 i) $ bind v (f1 $ i+1)
    go e' (Lam ty xf) i =
      let (v, f) = unbind xf
       in lam ty $ bind v (f $ i+1)
    go e' (FixP ty xf) i =
      let (v, f) = unbind xf
       in fixp ty $ bind v (f $ i+1)
    go e' Z _ = zero
    go e' (Succ f) i = succ (f i)
    go e' (Ap f1 f2) i = ap (f1 i) (f2 i)

