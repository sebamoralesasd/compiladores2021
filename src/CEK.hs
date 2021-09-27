module CEK () where

import Lang
import MonadFD4 (MonadFD4, failPosFD4, lookupDecl, printFD4)
import PPrint (pp)

-- TODO: ver si puede quedar más legible
data Closure = ClosureFun Enviroment Name Term | ClosureFix Enviroment Name Name Term

data Value = Natural Int | ClosureValue Closure

-- El valor enésimo corresponde al índice de De Bruijn
type Enviroment = [Value]

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
search (Print pos string term) enviroment kontinuation =
  search term enviroment (FramePrint string : kontinuation)
search (BinaryOp pos binaryOp left right) enviroment kontinuation =
  search left enviroment (OplusLeftEmpty enviroment binaryOp right : kontinuation)
search (IfZ pos condition thenTerm elseTerm) enviroment kontinuation =
  search condition enviroment (FrameIfZ enviroment thenTerm elseTerm : kontinuation)
search (App pos left right) enviroment kontinuation =
  search left enviroment (ApplicationLeftEmtpy enviroment right : kontinuation)
search (V pos var) enviroment kontinuation =
  case var of
    (Bound n) -> destroy (enviroment !! n) kontinuation
    (Free name) -> undefined
    (Global name) -> do
      r <- lookupDecl name
      case r of
        Just term -> search term enviroment kontinuation
        Nothing -> failPosFD4 pos ("No se encontró la declaración asociada al nombre " ++ name)
search (Const pos (CNat constant)) enviroment kontinuation =
  destroy (Natural constant) kontinuation
search (Lam pos name ty body) enviroment kontinuation =
  destroy (ClosureValue (ClosureFun enviroment name body)) kontinuation
search (Fix pos functionName functionType argumentName argumentType term) enviroment kontinuation =
  destroy (ClosureValue (ClosureFix enviroment functionName argumentName term)) kontinuation
search (Let pos name ty replacement term) enviroment kontinuation = undefined

destroy :: MonadFD4 m => Value -> Kontinuation -> m Value
destroy value [] = return (value)
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
    substitutedEnviroment = value : enviroment
destroy value (FrameClosure (ClosureFix enviroment functionName argumentName term) : kontinuation) =
  search term substitutedEnviroment kontinuation
  where
    substitutedEnviroment = value : ClosureValue (ClosureFix enviroment functionName argumentName term) : enviroment
destroy _ _ = undefined

-- TODO: ojo que falta caso base

interactive :: MonadFD4 m => State -> m Value
interactive (TermEnviromentKontinuation term enviroment kontinuation) =
  do
    nextValue <- searchStep term enviroment kontinuation
    -- print nextValue
    interactive nextValue
interactive (ValueKontinuation value []) = return value
interactive (ValueKontinuation value kontinuation) =
  do
    nextValue <- destroyStep value kontinuation
    -- print nextValue
    interactive nextValue

data State = TermEnviromentKontinuation Term Enviroment Kontinuation | ValueKontinuation Value Kontinuation

searchStep :: MonadFD4 m => Term -> Enviroment -> Kontinuation -> m State
searchStep (Print pos string term) enviroment kontinuation =
  return (TermEnviromentKontinuation term enviroment (FramePrint string : kontinuation))
searchStep (BinaryOp pos binaryOp left right) enviroment kontinuation =
  return (TermEnviromentKontinuation left enviroment (OplusLeftEmpty enviroment binaryOp right : kontinuation))
searchStep (IfZ pos condition thenTerm elseTerm) enviroment kontinuation =
  return (TermEnviromentKontinuation condition enviroment (FrameIfZ enviroment thenTerm elseTerm : kontinuation))
searchStep (App pos left right) enviroment kontinuation =
  return (TermEnviromentKontinuation left enviroment (ApplicationLeftEmtpy enviroment right : kontinuation))
searchStep (V pos var) enviroment kontinuation =
  case var of
    (Bound n) -> return (ValueKontinuation (enviroment !! n) kontinuation)
    (Free name) -> undefined
    (Global name) -> do
      r <- lookupDecl name
      case r of
        Just term -> return (TermEnviromentKontinuation term enviroment kontinuation)
        Nothing -> failPosFD4 pos ("No se encontró la declaración asociada al nombre " ++ name)
searchStep (Const pos (CNat constant)) enviroment kontinuation =
  return (ValueKontinuation (Natural constant) kontinuation)
searchStep (Lam pos name ty body) enviroment kontinuation =
  return (ValueKontinuation (ClosureValue (ClosureFun enviroment name body)) kontinuation)
searchStep (Fix pos functionName functionType argumentName argumentType term) enviroment kontinuation =
  return (ValueKontinuation (ClosureValue (ClosureFix enviroment functionName argumentName term)) kontinuation)
searchStep (Let pos name ty replacement term) enviroment kontinuation = undefined

destroyStep :: MonadFD4 m => Value -> Kontinuation -> m State
destroyStep value ((FramePrint string) : kontinuation) =
  do
    printFD4 string
    destroyStep value kontinuation
destroyStep (Natural n) ((OplusLeftEmpty enviroment binaryOp term) : kontinuation) =
  return (TermEnviromentKontinuation term enviroment (OplusRightEmpty (Natural n) binaryOp : kontinuation))
destroyStep (Natural n') ((OplusRightEmpty (Natural n) binaryOp) : kontinuation) =
  return (ValueKontinuation (Natural n_oplus_n') kontinuation)
  where
    n_oplus_n' =
      case binaryOp of
        Add -> n + n'
        Sub -> n - n'
destroyStep (Natural n) ((FrameIfZ enviroment thenTerm elseTerm) : kontinuation) =
  let term = if n == 0 then thenTerm else elseTerm
   in return (TermEnviromentKontinuation term enviroment kontinuation)
destroyStep (ClosureValue clousure) (ApplicationLeftEmtpy enviroment term : kontinuation) =
  return (TermEnviromentKontinuation term enviroment (FrameClosure clousure : kontinuation))
destroyStep value (FrameClosure (ClosureFun enviroment name term) : kontinuation) =
  return (TermEnviromentKontinuation term substitutedEnviroment kontinuation)
  where
    substitutedEnviroment = value : enviroment
destroyStep value (FrameClosure (ClosureFix enviroment functionName argumentName term) : kontinuation) =
  return (TermEnviromentKontinuation term substitutedEnviroment kontinuation)
  where
    substitutedEnviroment = value : ClosureValue (ClosureFix enviroment functionName argumentName term) : enviroment
destroyStep _ _ = undefined
