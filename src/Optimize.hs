module Optimize (optimizeTerm) where
import Lang (Term, Tm (BinaryOp, Const), Const (CNat))
import Eval (semOp)

optimizeTerm :: Term -> Term
optimizeTerm (BinaryOp info binaryOp (Const _ (CNat x)) (Const _ (CNat y))) =
  Const info (CNat (semOp binaryOp x y))
optimizeTerm t = t