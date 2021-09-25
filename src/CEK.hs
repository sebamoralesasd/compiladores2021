module CEK () where

import GHC.Natural (Natural)
import Lang
import Lang (Tm (BinaryOp))
import MonadFD4 (MonadFD4, printFD4)

-- TODO: ver si puede quedar más legible
data Closure = ClosureFun Enviroment Name Term | ClosureFix Enviroment Name Name Term

data Value = Natural Int | ClosureValue Closure

-- El valor enésimo corresponde al índice de De Bruijn
type Enviroment = [Natural]

-- TODO: Agregar caso para Let
data Frame
  = ApplicationLeftEmtpy Enviroment Term -- p . _ t
  | FrameClosure Closure -- clos t
  | FrameIfZ Enviroment Term Term -- p . ifz _ then t else e
  | OplusLeftEmpty Enviroment BinaryOp Term -- p. _ ⊕ t
  | OplusRightEmpty Value BinaryOp -- v ⊕ _
  | FramePrint String -- print str _

-- data Kontinuation = None | Some Frame Kontinuation
type Kontinuation = [Frame]

search :: MonadFD4 m => Term -> Enviroment -> Kontinuation -> m Value
search (Print info string term) enviroment kontinuation =
  search term enviroment (FramePrint string : kontinuation)
search (BinaryOp info binaryOp left right) enviroment kontinuation =
  search left enviroment (OplusLeftEmpty enviroment binaryOp right : kontinuation)
search (IfZ info condition thenTerm elseTerm) enviroment kontinuation =
  search condition enviroment (FrameIfZ enviroment thenTerm elseTerm : kontinuation)
search (App info left right) enviroment kontinuation =
  search left enviroment (ApplicationLeftEmtpy enviroment right : kontinuation)
search (V info var) enviroment kontinuation = undefined -- TODO: implementar
-- find var
-- where
--     find :: MonadFD4 m => Var -> m Natural
--     find (Bound n) = return n
--     -- find (Global name) = lookup name
search (Const info (CNat constant)) enviroment kontinuation =
  destroy (Natural constant) kontinuation
search (Lam info name ty body) enviroment kontinuation =
  destroy (ClosureValue (ClosureFun enviroment name body)) kontinuation
search (Fix info functionName functionType argumentName argumentType term) enviroment kontinuation =
  destroy (ClosureValue (ClosureFix enviroment functionName argumentName term)) kontinuation
search (Let info name ty replacement term) enviroment kontinuation = undefined

-- TODO: como tipamos esto?
destroy :: MonadFD4 m => Value -> Kontinuation -> m Value
destroy value ((FramePrint string) : kontinuation) =
  do
    printFD4 string
    destroy value kontinuation
destroy (Natural n) ((OplusLeftEmpty enviroment binaryOp term) : kontinuation) =
  search term enviroment (OplusRightEmpty (Natural n) binaryOp : kontinuation)
destroy (Natural n') ((OplusRightEmpty (Natural n) binaryOp) : kontinuation) =
  destroy (Natural n_oplus_n') kontinuation
  where
    n_oplus_n' =
      case binaryOp of
        Add -> n + n'
        Sub -> n - n'
destroy (Natural n) ((FrameIfZ enviroment thenTerm elseTerm) : kontinuation) =
  let term = if n == 0 then thenTerm else elseTerm 
  in search term enviroment kontinuation
destroy (ClosureValue clousure) (ApplicationLeftEmtpy enviroment term : kontinuation) =
  search term enviroment (FrameClosure clousure : kontinuation)
destroy value (FrameClosure (ClosureFun enviroment name term) : kontinuation) =
  search term substitutedEnviroment kontinuation
  where
    -- TODO: reemplazar el entorno acordemente con name->value
    substitutedEnviroment = enviroment
destroy value (FrameClosure (ClosureFix enviroment functionName argumentName term) : kontinuation) =
  search term substitutedEnviroment kontinuation
  where
    -- TODO: reemplazar acordemente con : argumentName->value y functionName->clos_fix(enviroment, functionName, argumentName, term)
    substitutedEnviroment = enviroment
destroy value kontinuation = undefined

-- TODO: ojo que falta caso base