-- |
-- Module      : Elab
-- Description : Elabora un término fully named a uno locally closed.
-- Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
-- License     : GPL-3
-- Maintainer  : mauro@fceia.unr.edu.ar
-- Stability   : experimental
--
-- Este módulo permite elaborar términos y declaraciones para convertirlas desde
-- fully named (@NTerm) a locally closed (@Term@)
module Elab (elab, elab_decl) where

import Common
import Lang
import MonadFD4
import Subst


-- | Transforma una lista de tipos [t1 t2 ... tn] en el tipo de funcion t1 -> t2 -> ... -> tn
createFunType :: [STy] -> STy
createFunType [] = undefined
createFunType [ty] = ty
createFunType (ty : binders) = SFunTy ty (createFunType binders)

-- | Transforma tipos azucarados en su notación core.
desugarTy :: MonadFD4 m => STy -> m Ty
desugarTy SNatTy = return NatTy
desugarTy (SFunTy stau stau') = do
  tau <- desugarTy stau
  tau' <- desugarTy stau'
  return (FunTy tau tau')
desugarTy (STypeSinonym name) = do
  t <- lookupSTy name
  case t of
    Nothing -> failPosFD4 NoPos $ "Tipo no declarado."
    Just ty -> return ty

-- | Transforma términos azucarados en su notación core.
desugar :: MonadFD4 m => SNTerm -> m NTerm
desugar (SV info var) = return (V info var)
desugar (SConst info con) = return (Const info con)
desugar (SLam info [] _) = failPosFD4 info $ "Lista de binders vacia"
desugar (SLam info [(name, sty)] sterm) =
  do
    term <- desugar sterm
    ty <- desugarTy sty
    return (Lam info name ty term)
desugar (SLam info ((name, sty) : bindersTail) sterm) =
  do
    term <- desugar (SLam info bindersTail sterm)
    ty <- desugarTy sty
    return (Lam info name ty term)
desugar (SApp info sterm1 sterm2) =
  do
    term2 <- desugar sterm2
    case sterm1 of
      (SPrintUnary infoPrint str) -> return (Print infoPrint str term2)
      _ -> do
        term1 <- desugar sterm1
        return (App info term1 term2)
desugar (SPrintUnary info str) = return (Lam info "x" NatTy (Print info str (V info "x")))
desugar (SBinaryOp info binOp sterm1 sterm2) =
  do
    term1 <- desugar sterm1
    term2 <- desugar sterm2
    return (BinaryOp info binOp term1 term2)
desugar (SFix info name sty name2 sty2 sterm) =
  do
    term <- desugar sterm
    ty <- desugarTy sty
    ty2 <- desugarTy sty2
    return (Fix info name ty name2 ty2 term)
desugar (SIfZ info sterm1 sterm2 sterm3) =
  do
    term1 <- desugar sterm1
    term2 <- desugar sterm2
    term3 <- desugar sterm3
    return (IfZ info term1 term2 term3)
desugar (SLet info name sty [] sterm1 sterm2) =
  do
    term1 <- desugar sterm1
    term2 <- desugar sterm2
    ty <- desugarTy sty
    return (Let info name ty term1 term2)
desugar (SLet info fName fReturnType binders sterm1 sterm2) =
  desugar (SLet info fName fType [] funToBody sterm2)
  where
    types = map snd binders ++ [fReturnType]
    fType = createFunType types
    funToBody = SLam info (tail binders) sterm1
-- caso con fix
desugar (SLetRec info _ _ [] _ _) = failPosFD4 info $ "Lista de binders vacia"
desugar (SLetRec info fName fSReturnType [(name, sty)] sterm1 sterm2) =
  do
    term1 <- desugar sterm1
    term2 <- desugar sterm2
    ty <- desugarTy sty
    fReturnType <- desugarTy fSReturnType
    let fixTerm = Fix info fName fType name ty term1
        fType = FunTy ty fReturnType
     in return (Let info fName fType fixTerm term2)
-- caso con paso intermedio
desugar (SLetRec info fName fReturnType binders sterm1 sterm2) =
  desugar (SLetRec info fName fType [head binders] funToBody sterm2)
  where
    types = map snd binders ++ [fReturnType]
    fType = createFunType (tail types)
    funToBody = SLam info (tail binders) sterm1
desugar (SinTy info name sty) = undefined

-- | 'elab' transforma variables ligadas en índices de de Bruijn
-- en un término dado.
elab :: MonadFD4 m => SNTerm -> m Term
elab n = do
  nterm <- desugar n
  return $ elab' [] nterm

elab' :: [Name] -> NTerm -> Term
elab' env (V p v) =
  -- Tenemos que ver si la variable es Global o es un nombre local
  -- En env llevamos la lista de nombres locales.
  if v `elem` env
    then V p (Free v)
    else V p (Global v)
elab' _ (Const p c) = Const p c
elab' env (Lam p v ty t) = Lam p v ty (close v (elab' (v : env) t))
elab' env (Fix p f fty x xty t) = Fix p f fty x xty (closeN [f, x] (elab' (x : f : env) t))
elab' env (IfZ p c t e) = IfZ p (elab' env c) (elab' env t) (elab' env e)
-- Operador Print
elab' env (Print i str t) = Print i str (elab' env t)
-- Operadores binarios
elab' env (BinaryOp i o t u) = BinaryOp i o (elab' env t) (elab' env u)
-- Aplicaciones generales
elab' env (App p h a) = App p (elab' env h) (elab' env a)
elab' env (Let p v vty def body) = Let p v vty (elab' env def) (close v (elab' (v : env) body))

-- TODO: esto para que esta?
elab_decl :: Decl SNTerm -> Decl Term
elab_decl = undefined --fmap elab
