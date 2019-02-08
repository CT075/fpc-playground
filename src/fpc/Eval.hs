
import Prelude hiding (succ)

import Control.Arrow
import Control.Monad hiding (ap)
import Data.Functor.Foldable
import Data.Map.Strict

import Fpc.Ast

data Result = Step Exp | Val

trystep' :: TExp (Exp, Result) -> Result
trystep' (EVar _) = error "malformed input!"
trystep' Z = Val
trystep' (Succ (_, Step e)) = Step (succ e)
trystep' (Succ (_, Val)) = Val
trystep' (Ifz (_, Step e) (e0,_) xe1) =
  let (v, (e1, _)) = unbind xe1 in
  Step $ ifz e e0 $ bind v e1
trystep' (Ifz (e, Val) (e0,_) xe1) =
  case unfix e of
       Z -> Step e0
       Succ n -> let (v, (e1,_)) = unbind xe1 in Step $ subst n v e1
trystep' (Lam _ xe) = Val
trystep' (Ap (_, Step e1) (e2, _)) = Step $ ap e1 e2
trystep' (Ap (e1, Val) (_, Step e2)) = Step $ ap e1 e2
trystep' (Ap (e1, Val) (e2, Val)) =
  case unfix e1 of
       Lam _ xe ->
         let (v, e) = unbind xe
          in Step $ subst e2 v e
       _ -> error "malformed input!"
trystep' (FixP t xe) =
  let (v, (e, _)) = unbind xe
   in Step $ subst (fixp t $ bind v e) v e

trystep :: Exp -> Result
trystep = para trystep'

eval :: Exp -> Exp
eval e =
  case trystep e of
       Val -> e
       Step e' -> eval e'

