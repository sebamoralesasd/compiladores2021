{-|
Module      : Elab
Description : Elabora un término fully named a uno locally closed.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Este módulo permite elaborar términos y declaraciones para convertirlas desde
fully named (@NTerm) a locally closed (@Term@) 
-}

module Elab ( elab, elab_decl ) where

import Lang
import Subst
import MonadFD4
import Common

desugar :: MonadFD4 m => SNTerm -> m NTerm
desugar (SV info var) = return (V info var)
desugar (SConst info con) = return (Const info con)
desugar (SLam info [] _) = failPosFD4 info $ "Lista de binders vacia"
desugar (SLam info [(name, ty)] sterm) =
  do 
    term <- desugar sterm
    return (Lam info name ty term)
desugar (SLam info ((name, ty) : bindersTail) sterm) =
  do 
    term <- desugar (SLam info bindersTail sterm)
    return (Lam info name ty term)
desugar (SApp info sterm1 sterm2) = 
  do
    term1 <- desugar sterm1
    term2 <- desugar sterm2
    return (App info term1 term2)
desugar (SBinaryOp info binOp sterm1 sterm2) = 
  do
    term1 <- desugar sterm1
    term2 <- desugar sterm2
    return (BinaryOp info binOp term1 term2)
desugar (SFix info name ty name2 ty2 sterm) = 
  do 
    term <- desugar sterm
    return (Fix info name ty name2 ty2 term)
desugar (SIfZ info sterm1 sterm2 sterm3) = 
  do
    term1 <- desugar sterm1
    term2 <- desugar sterm2
    term3 <- desugar sterm3
    return (IfZ info term1 term2 term3)
desugar (SLetFunction info _ _ _ [] _ _) = failPosFD4 info $ "Lista de binders vacia"
-- caso con fix
desugar (SLetFunction info True fname fType [(name, ty)] sterm1 sterm2) = undefined
-- caso con paso intermedio
desugar (SLetFunction info True fname fType binders sterm1 sterm2) = undefined
desugar (SLetFunction info False fname fType binders sterm1 sterm2) = 
  desugar (SLet info fname (letFunType fType binders) sterm1 sterm2)
desugar (SLet info name ty sterm1 sterm2) = 
  do
    term1 <- desugar sterm1
    term2 <- desugar sterm2
    return (Let info name ty term1 term2)
desugar (SPrint info str sterm) = undefined
-- TODO: revisar
-- desugar (SApp info sterm1 sterm2) = 
--   do
--     term2 <- desugar sterm2
--     case sterm1 of 
--       (SPrint info str) -> return (Print info str term2)
--       otherwise -> do
--                      term1 <- desugar sterm1
--                      return (App info term1 term2)
-- desugar (SPrint info str) = return Lam info x Nat (Print info str x)

letFunType :: Ty -> [Binder] -> Ty
letFunType b [] = undefined
letFunType b [(name, ty)] = ty
letFunType b ((name, ty) : binders) = FunTy ty (letFunType b binders)

-- | 'elab' transforma variables ligadas en índices de de Bruijn
-- en un término dado. 
elab :: NTerm -> Term
elab = elab' []

elab' :: [Name] -> NTerm -> Term
elab' env (V p v) =
  -- Tenemos que hver si la variable es Global o es un nombre local
  -- En env llevamos la lista de nombres locales.
  if v `elem` env 
    then  V p (Free v)
    else V p (Global v)

elab' _ (Const p c) = Const p c
elab' env (Lam p v ty t) = Lam p v ty (close v (elab' (v:env) t))
elab' env (Fix p f fty x xty t) = Fix p f fty x xty (closeN [f, x] (elab' (x:f:env) t))
elab' env (IfZ p c t e)         = IfZ p (elab' env c) (elab' env t) (elab' env e)
-- Operador Print
elab' env (Print i str t) = Print i str (elab' env t)
-- Operadores binarios
elab' env (BinaryOp i o t u) = BinaryOp i o (elab' env t) (elab' env u)
-- Aplicaciones generales
elab' env (App p h a) = App p (elab' env h) (elab' env a)
elab' env (Let p v vty def body) = Let p v vty (elab' env def) (close v (elab' (v:env) body))

-- TODO: esto para que esta?
elab_decl :: Decl NTerm -> Decl Term
elab_decl = fmap elab
