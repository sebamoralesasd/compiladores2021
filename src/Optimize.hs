module Optimize (optimizeTerm) where
import Lang (Term, Tm (BinaryOp, Const, IfZ, Let), Const (CNat), BinaryOp (Add, Sub))
import Eval (semOp)
import Subst (subst)

optimizeTerm :: Term -> Term
{- Constant Folding -}
optimizeTerm (BinaryOp i binaryOp (Const _ (CNat x)) (Const _ (CNat y))) =
  Const i (CNat (semOp binaryOp x y))
-- x +- 0 = x
optimizeTerm (BinaryOp i binaryOp term (Const _ (CNat 0))) = term
-- 0 + x = x
optimizeTerm (BinaryOp i Add (Const _ (CNat 0)) x) = x
-- 0 - x = 0. TODO: acomodar printing y revisar divergencia
optimizeTerm (BinaryOp i Sub (Const _ (CNat 0)) x) = Const i (CNat 0)

optimizeTerm (IfZ _ (Const _ (CNat 0)) thenTerm _) = thenTerm
optimizeTerm (IfZ _ (Const _ (CNat _)) _ elseTerm) = elseTerm

{- Constant Propagation -}
optimizeTerm (Let i name ty x@(Const _ (CNat _)) term) =
    subst x term

{- Not optimized -}
optimizeTerm t = t