module CEK
  ()
 where

import GHC.Natural (Natural)
import MonadFD4 (MonadFD4)
import Lang


data Value = Nat Natural | ClossureFun Enviroment 

-- El valor enésimo corresponde al índice de De Bruijn
type Enviroment = [Natural]

data Oplus = Plus | Minus

-- TODO: Agregar caso para Let
data Frame = 
      ApplicationLeftEmtpy Enviroment Term      -- p . _ t
    | Closure                                   -- clos t
    | Ifz Term Term                             -- p . ifz _ then t else e
    | OplusLeftEmpty Enviroment Oplus Term      -- p. _ ⊕ t
    | OplusRightEmpty Value Oplus Term          -- v ⊕ _
    | FramePrint String                         -- print str _

-- data Kontinuation = None | Some Frame Kontinuation
type Kontinuation = [Frame]

search :: MonadFD4 m => Term -> Enviroment -> Kontinuation -> m Value
search (V info var) = undefined
search (Const info const) = undefined
search (Lam info functionName functionType body) = undefined
search (App info left right) = undefined
search (Print info string term) = undefined
search (BinaryOp info binaryOp left right) = undefined
search (Fix info functionName functionType argumentName argumentType term) = undefined
search (IfZ info condition thenTerm elseTerm) = undefined
search (Let info name ty replacement term) = undefined

destroy :: MonadFD4 m => Value -> Kontinuation -> m Value
destroy = undefined
