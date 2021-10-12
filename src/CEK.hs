module CEK where

import Control.Monad (liftM2)
import Lang
import MonadFD4 (MonadFD4, failPosFD4, lookupDecl, printFD4)
import PPrint (pp)
import Eval (semOp)
import Common

-- TODO: ver si puede quedar más legible
data Closure = ClosureFun Enviroment Name Term | ClosureFix Enviroment Name Name Term

data Value = Natural Int | ClosureValue Closure

-- El valor enésimo corresponde al índice de De Bruijn
type Enviroment = [Value]

-- TODO: Agregar caso para Let
data Frame
  = ApplicationLeftEmpty Enviroment Term -- p . _ t
  | FrameClosure Closure -- clos t
  | FrameIfZ Enviroment Term Term -- p . ifz _ then t else e
  | OplusLeftEmpty Enviroment BinaryOp Term -- p. _ ⊕ t
  | OplusRightEmpty Value BinaryOp -- v ⊕ _
  | FramePrint String -- print str _
  | FrameLetIn Enviroment Name Term -- let x = _ in t

-- data Kontinuation = None | Some Frame Kontinuation
type Kontinuation = [Frame]

search :: MonadFD4 m => Term -> Enviroment -> Kontinuation -> m Value
search (Print _ string term) env kont =
  search term env (FramePrint string : kont)
search (BinaryOp _ binaryOp left right) env kont =
  search left env (OplusLeftEmpty env binaryOp right : kont)
search (IfZ _ condition thenTerm elseTerm) env kont =
  search condition env (FrameIfZ env thenTerm elseTerm : kont)
search (App _ left right) env kont =
  search left env (ApplicationLeftEmpty env right : kont)
search (V pos var) env kont =
  case var of
    (Bound n) -> destroy (env !! n) kont
    (Free _) -> undefined
    (Global name) -> do
      r <- lookupDecl name
      case r of
        Just term -> search term env kont
        Nothing -> failPosFD4 pos ("No se encontró la declaración asociada al nombre " ++ name)
search (Const _ (CNat constant)) env kont =
  destroy (Natural constant) kont
search (Lam _ name ty body) env kont =
  destroy (ClosureValue (ClosureFun env name body)) kont
search (Fix _ functionName functionType argumentName argumentType term) env kont =
  destroy (ClosureValue (ClosureFix env functionName argumentName term)) kont
search (Let _ name ty replacement term) env kont =
  search replacement env (FrameLetIn env name term : kont)

destroy :: MonadFD4 m => Value -> Kontinuation -> m Value
destroy value [] = return value
destroy value ((FramePrint string) : kont) =
  do
    printFD4 string
    destroy value kont
destroy (Natural n) ((OplusLeftEmpty env binaryOp term) : kont) =
  search term env (OplusRightEmpty (Natural n) binaryOp : kont)
destroy (Natural n') ((OplusRightEmpty (Natural n) binaryOp) : kont) =
  destroy (Natural n_oplus_n') kont
  where
    n_oplus_n' = semOp binaryOp n n'
destroy (Natural n) ((FrameIfZ env thenTerm elseTerm) : kont) =
  let term = if n == 0 then thenTerm else elseTerm
   in search term env kont
destroy (ClosureValue clousure) (ApplicationLeftEmpty env term : kont) =
  search term env (FrameClosure clousure : kont)
destroy value (FrameClosure (ClosureFun env name term) : kont) =
  search term substitutedEnviroment kont
  where
    substitutedEnviroment = value : env
destroy value (FrameClosure (ClosureFix env functionName argumentName term) : kont) =
  search term substitutedEnviroment kont
  where
    substitutedEnviroment = value : ClosureValue (ClosureFix env functionName argumentName term) : env
destroy value ((FrameLetIn env name term) : kont) = search term (value : env) kont
destroy _ _ = undefined

-- TODO: ojo que falta caso base

-- PRINTING
stateToString :: MonadFD4 m => State -> m String
stateToString (TermEnviromentKontinuation term env kont) =
  do
    ppterm <- pp term
    return ("⟨" ++ ppterm ++ "⟩")
-- TODO: arreglar printing para value.
stateToString (ValueKontinuation value kont) = return ("⟪" ++ "⟫")

frameToString :: MonadFD4 m => Frame -> m String
frameToString (ApplicationLeftEmpty env term) =
  do
    enviromentString <- enviromentToString env
    ppterm <- pp term
    return (enviromentString ++ " . " ++ "_ ⊕ " ++ ppterm)
frameToString (FrameClosure closure) = closureToString closure
frameToString (FrameIfZ env thenTerm elseTerm) =
  do
    enviromentString <- enviromentToString env
    ppthenTerm <- pp thenTerm
    ppelseTerm <- pp elseTerm
    return (enviromentString ++ " . ifz _ then " ++ ppthenTerm ++ " else " ++ ppelseTerm)
frameToString (OplusLeftEmpty env binaryOp term) =
  do
    enviromentString <- enviromentToString env
    ppterm <- pp term
    return (enviromentString ++ " . _ " ++ show binaryOp ++ " " ++ ppterm)
frameToString (OplusRightEmpty value binaryOp) =
  do
    valueString <- valueToString value
    return (valueString ++ " " ++ show binaryOp ++ " _")
frameToString (FramePrint string) = return ("print " ++ string ++ " _")
frameToString (FrameLetIn env name term) =
  do
    enviromentString <- enviromentToString env
    ppterm <- pp term
    return (enviromentString ++ " . _ let " ++ name ++ " in " ++ ppterm)

valueToString :: MonadFD4 m => Value -> m String
valueToString (Natural n) = return (show n)
valueToString (ClosureValue closure) = closureToString closure

closureToString :: MonadFD4 m => Closure -> m String
closureToString (ClosureFun env name term) =
  do
    ppterm <- pp term
    enviromentString <- enviromentToString env
    return ("clos_fun(" ++ enviromentString ++ ", " ++ name ++ ", " ++ ppterm)
closureToString (ClosureFix env fname name term) =
  do
    ppterm <- pp term
    enviromentString <- enviromentToString env
    return ("clos_fix(" ++ enviromentString ++ ", " ++ fname ++ ", " ++ name ++ ", " ++ ppterm)

enviromentToString :: MonadFD4 m => Enviroment -> m String
enviromentToString [] = return ""
enviromentToString (env : tl) =
  liftM2 (++) (enviromentToString tl) (valueToString env)

data State = TermEnviromentKontinuation Term Enviroment Kontinuation | ValueKontinuation Value Kontinuation

-- STEP IMPLEMENTATION
interactive :: MonadFD4 m => State -> m Value
interactive (TermEnviromentKontinuation term env kont) =
  do
    nextValue <- searchStep term env kont
    str <- stateToString nextValue
    printFD4 str
    interactive nextValue
interactive (ValueKontinuation value []) = return value
interactive (ValueKontinuation value kont) =
  do
    nextValue <- destroyStep value kont
    str <- stateToString nextValue
    printFD4 str
    interactive nextValue

searchStep :: MonadFD4 m => Term -> Enviroment -> Kontinuation -> m State
searchStep (Print pos string term) env kont =
  return (TermEnviromentKontinuation term env (FramePrint string : kont))
searchStep (BinaryOp pos binaryOp left right) env kont =
  return (TermEnviromentKontinuation left env (OplusLeftEmpty env binaryOp right : kont))
searchStep (IfZ pos condition thenTerm elseTerm) env kont =
  return (TermEnviromentKontinuation condition env (FrameIfZ env thenTerm elseTerm : kont))
searchStep (App pos left right) env kont =
  return (TermEnviromentKontinuation left env (ApplicationLeftEmpty env right : kont))
searchStep (V pos var) env kont =
  case var of
    (Bound n) -> return (ValueKontinuation (env !! n) kont)
    (Free name) -> undefined
    (Global name) -> do
      r <- lookupDecl name
      case r of
        Just term -> return (TermEnviromentKontinuation term env kont)
        Nothing -> failPosFD4 pos ("No se encontró la declaración asociada al nombre " ++ name)
searchStep (Const pos (CNat constant)) env kont =
  return (ValueKontinuation (Natural constant) kont)
searchStep (Lam pos name ty body) env kont =
  return (ValueKontinuation (ClosureValue (ClosureFun env name body)) kont)
searchStep (Fix pos functionName functionType argumentName argumentType term) env kont =
  return (ValueKontinuation (ClosureValue (ClosureFix env functionName argumentName term)) kont)
searchStep (Let pos name ty replacement term) env kont = 
  return (TermEnviromentKontinuation replacement env (FrameLetIn env name term : kont))

destroyStep :: MonadFD4 m => Value -> Kontinuation -> m State
destroyStep value ((FramePrint string) : kont) =
  do
    printFD4 string
    destroyStep value kont
destroyStep (Natural n) ((OplusLeftEmpty env binaryOp term) : kont) =
  return (TermEnviromentKontinuation term env (OplusRightEmpty (Natural n) binaryOp : kont))
destroyStep (Natural n') ((OplusRightEmpty (Natural n) binaryOp) : kont) =
  return (ValueKontinuation (Natural n_oplus_n') kont)
  where
    n_oplus_n' = semOp binaryOp n n'
destroyStep (Natural n) ((FrameIfZ env thenTerm elseTerm) : kont) =
  let term = if n == 0 then thenTerm else elseTerm
   in return (TermEnviromentKontinuation term env kont)
destroyStep (ClosureValue clousure) (ApplicationLeftEmpty env term : kont) =
  return (TermEnviromentKontinuation term env (FrameClosure clousure : kont))
destroyStep value (FrameClosure (ClosureFun env name term) : kont) =
  return (TermEnviromentKontinuation term substitutedEnviroment kont)
  where
    substitutedEnviroment = value : env
destroyStep value (FrameClosure (ClosureFix env functionName argumentName term) : kont) =
  return (TermEnviromentKontinuation term substitutedEnviroment kont)
  where
    substitutedEnviroment = value : ClosureValue (ClosureFix env functionName argumentName term) : env
destroyStep value ((FrameLetIn env name term) : kont) = return (TermEnviromentKontinuation term (value : env) kont)
destroyStep _ _ = undefined
