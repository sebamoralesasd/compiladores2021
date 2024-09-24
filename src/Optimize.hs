{-# LANGUAGE PatternSynonyms #-}

module Optimize (optimizeTerm) where
import Lang
import Subst (subst)

pattern CONST :: Int -> Term
pattern CONST n <- Const i (CNat n)

solveBinaryOp :: Term -> Term
solveBinaryOp t = case t of
  BinaryOp i Add t1 (CONST 0) -> t1
  BinaryOp i Add (CONST 0) t2 -> t2
  BinaryOp i Sub t1 (CONST 0) -> t1
  BinaryOp i Sub (CONST 0) t2 -> Const i (CNat 0)
  tt -> tt

optimizeTerm :: Term -> Term
{- Constant Propagation -}
optimizeTerm (Let i name ty x@(CONST _) term) = subst x term
{- Other -}
optimizeTerm (IfZ _ (CONST 0) thenTerm _) = thenTerm
optimizeTerm (IfZ _ (CONST _) _ elseTerm) = elseTerm
optimizeTerm (Lam i name ty t1) = Lam i name ty (optimizeTerm t1)
optimizeTerm (BinaryOp i bop t1 t2) = solveBinaryOp $ BinaryOp i bop (optimizeTerm t1) (optimizeTerm t2)
optimizeTerm (Fix i funName fTy name ty t1) = Fix i funName fTy name ty (optimizeTerm t1)
optimizeTerm (App i t1 t2) = App i (optimizeTerm t1) (optimizeTerm t2)
-- TODO: ver caso efecto del print
--optimizeTerm (Print i str t1)

{- Not optimized -}
optimizeTerm t = t

optimize :: [Decl Term] -> [Decl Term]
optimize = map $ fmap optimizeTerm