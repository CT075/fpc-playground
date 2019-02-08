
module Fpc.Statics
  ( synthtype
  )
  where

import Prelude hiding (lookup)

import Control.Arrow
import Control.Monad
import Data.Functor.Foldable
import Data.Map.Strict

import Fpc.Ast

type Env = Map Var Typ

lookupTy :: Var -> Env -> Maybe Typ
lookupTy = lookup

insertTy :: Var -> Typ -> Env -> Env
insertTy _ ty env = insert 0 ty (mapKeys (+1) env)

-- I think this can be written much more concisely using monadic cata
synthtype' :: TExp (Env -> Maybe Typ) -> Env -> Maybe Typ
synthtype' (EVar v) env = lookupTy v env
synthtype' Z env = Just nat
synthtype' (Succ f) env = do
  t <- f env
  guard $ t == nat
  return nat
synthtype' (Ifz f f0 xf1) env = do
  t <- f env
  guard $ t == nat
  t0 <- f0 env
  let (v, f1) = unbind xf1
  t1 <- f1 $ insertTy v nat env
  guard $ t0 == t1
  return t0
synthtype' (Lam t xf) env = do
  let (v, f) = unbind xf
  t2 <- f $ insertTy v t env
  return (t -:> t2)
synthtype' (Ap f0 f1) env = do
  tfunc <- f0 env
  (t1 :-> t2) <- return $ unfix tfunc
  tinput <- f1 env
  guard $ tinput == t1
  return t2
synthtype' (FixP t xf) env = do
  let (v, f) = unbind xf
  t' <- f env
  guard $ t == t'
  return t

synthtype :: Exp -> Maybe Typ
synthtype = flip (cata synthtype') empty

