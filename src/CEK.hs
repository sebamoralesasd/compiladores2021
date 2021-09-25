module CEK () where

import GHC.Natural (Natural)
import Lang
import MonadFD4 (MonadFD4)
import Lang (Tm(BinaryOp))

data Closure = ClosureFun Enviroment Name Term | ClosureFix Enviroment Name Name Term

data Value = Natural Natural | Closure

-- El valor enésimo corresponde al índice de De Bruijn
type Enviroment = [Natural]

-- TODO: Agregar caso para Let
data Frame
  = ApplicationLeftEmtpy Enviroment Term -- p . _ t
  | FrameClosure -- clos t
  | Ifz Term Term -- p . ifz _ then t else e
  | OplusLeftEmpty Enviroment BinaryOp Term -- p. _ ⊕ t
  | OplusRightEmpty Value BinaryOp Term -- v ⊕ _
  | FramePrint String -- print str _

-- data Kontinuation = None | Some Frame Kontinuation
type Kontinuation = [Frame]

search :: MonadFD4 m => Term -> Enviroment -> Kontinuation -> m Value
search (Print info string term) enviroment kontinuation = 
    search term enviroment (FramePrint string:kontinuation)
search (BinaryOp info binaryOp left right) enviroment kontinuation = 
    search left enviroment (OplusLeftEmpty enviroment binaryOp right :kontinuation)
search (IfZ info condition thenTerm elseTerm) enviroment kontinuation = undefined
search (App info left right) enviroment kontinuation = undefined
search (V info var) enviroment kontinuation = undefined
search (Const info constant) enviroment kontinuation = undefined
search (Lam info functionName functionType body) enviroment kontinuation = undefined
search (Fix info functionName functionType argumentName argumentType term) enviroment kontinuation = undefined
search (Let info name ty replacement term) enviroment kontinuation = undefined

destroy :: MonadFD4 m => Value -> Kontinuation -> m Value
destroy value ((FramePrint string) : kontinuation) = undefined
destroy value ((OplusLeftEmpty enviroment oplus term) : kontinuation) = undefined
destroy valueRight ((OplusRightEmpty valueLeft oplus term) : kontinuation) = undefined
destroy (Natural 0) ((Ifz thenTerm elseTerm) : kontinuation) = undefined
destroy (Natural n) ((Ifz thenTerm elseTerm) : kontinuation) = undefined
-- destroy (Closure) ((Ifz thenTerm elseTerm) : kontinuation) = undefined
destroy (ClosureFun enviroment name term) (ApplicationLeftEmtpy enviroment2 term2 : kontinuation) = undefined
destroy (ClosureFix enviroment functionName argumentName term) (ApplicationLeftEmtpy enviroment2 term2 : kontinuation) = undefined
destroy value kontinuation = undefined
