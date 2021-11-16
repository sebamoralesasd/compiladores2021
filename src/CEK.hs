module CEK where

import Data.List (intercalate)
import Eval (semOp)
import Lang
import MonadFD4 (MonadFD4, failPosFD4, lookupDecl, printFD4)
import PPrint (pp)

-- Sólo admite Lam y Fix
data Closure = Closure Enviroment Term

data Value = Natural Int | ClosureValue Closure

-- El valor enésimo corresponde al índice de De Bruijn
type Enviroment = [Value]

data Frame
  = ApplicationLeftEmpty Enviroment Term -- p . _ t
  | FrameClosure Closure -- clos t
  | FrameIfZ Enviroment Term Term -- p . ifz _ then t else e
  | OplusLeftEmpty Enviroment BinaryOp Term -- p. _ ⊕ t
  | OplusRightEmpty Value BinaryOp -- v ⊕ _
  | FramePrint String -- print str _
  | FrameLetIn Enviroment Name Term -- let x = _ in t (se le agrega el Name para el printing)

type Kontinuation = [Frame]

search :: MonadFD4 m => Term -> Enviroment -> Kontinuation -> m Value
search term env kont = innerSearchDestroy (TermEnviromentKontinuation term env kont)

destroy :: MonadFD4 m => Value -> Kontinuation -> m Value
destroy value kont = innerSearchDestroy $ ValueKontinuation value kont

innerSearchDestroy :: MonadFD4 m => State -> m Value
innerSearchDestroy state@(TermEnviromentKontinuation term env kont) =
  do
    nextValue <- searchStep term env kont
    innerSearchDestroy nextValue
innerSearchDestroy state@(ValueKontinuation value kont) =
  do
    nextValue <- destroyStep value kont
    innerSearchDestroy nextValue

------------------------------------------------------------------------
-- PRINTING
stateToString :: MonadFD4 m => State -> m String
stateToString (TermEnviromentKontinuation term env kont) =
  do
    ppterm <- pp term
    envString <- enviromentToString env
    kontString <- kontinuationToString kont
    return ("<" ++ ppterm ++ ", " ++ envString ++ ", " ++ kontString ++ ">")
stateToString (ValueKontinuation value kont) =
  do
    valueString <- valueToString value
    kontString <- kontinuationToString kont
    return ("<<" ++ valueString ++ ", " ++ kontString ++ ">>")

frameToString :: MonadFD4 m => Frame -> m String
frameToString (ApplicationLeftEmpty env term) =
  do
    enviromentString <- enviromentToString env
    ppterm <- pp term
    return (enviromentString ++ " . _ " ++ ppterm)
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
    return (enviromentString ++ ". let " ++ name ++ " = _ in " ++ ppterm)

valueToString :: MonadFD4 m => Value -> m String
valueToString (Natural n) = return (show n)
valueToString (ClosureValue closure) = closureToString closure

closureToString :: MonadFD4 m => Closure -> m String
closureToString (Closure env (Lam _ name _ term)) =
  do
    ppterm <- pp term
    enviromentString <- enviromentToString env
    return ("clos_fun(" ++ enviromentString ++ ", " ++ name ++ ", " ++ ppterm)
closureToString (Closure env (Fix _ fname _ name _ term)) =
  do
    ppterm <- pp term
    enviromentString <- enviromentToString env
    return ("clos_fix(" ++ enviromentString ++ ", " ++ fname ++ ", " ++ name ++ ", " ++ ppterm)
closureToString _ = undefined

enviromentToString :: MonadFD4 m => Enviroment -> m String
enviromentToString env =
  do
    valueStrings <- mapM valueToString env
    return ("{" ++ intercalate ", " valueStrings ++ "}")

kontinuationToString :: MonadFD4 m => Kontinuation -> m String
kontinuationToString kont =
  do
    valueStrings <- mapM frameToString kont
    return (intercalate " > " (valueStrings ++ ["e"]))

data State = TermEnviromentKontinuation Term Enviroment Kontinuation | ValueKontinuation Value Kontinuation

------------------------------------------------------------------------
-- STEP IMPLEMENTATION
interactive :: MonadFD4 m => State -> m Value
interactive state@(TermEnviromentKontinuation term env kont) =
  do
    str <- stateToString state
    printFD4 str
    nextValue <- searchStep term env kont
    interactive nextValue
interactive state@(ValueKontinuation value []) =
  do
    str <- stateToString state
    printFD4 str
    return value
interactive state@(ValueKontinuation value kont) =
  do
    str <- stateToString state
    printFD4 str
    nextValue <- destroyStep value kont
    interactive nextValue

-- TODO: Analizar: Es más correcto que sea de tipo State->State, pero sería más feo con tantos wrappers.
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
searchStep t@Lam {} env kont =
  return (ValueKontinuation (ClosureValue (Closure env t)) kont)
searchStep t@Fix {} env kont =
  return (ValueKontinuation (ClosureValue (Closure env t)) kont)
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
destroyStep value (FrameClosure (Closure env (Lam _ _ _ term)) : kont) =
  return (TermEnviromentKontinuation term substitutedEnviroment kont)
  where
    substitutedEnviroment = value : env
destroyStep value (FrameClosure (Closure env fixTerm@(Fix _ _ _ _ _ term)) : kont) =
  return (TermEnviromentKontinuation term substitutedEnviroment kont)
  where
    substitutedEnviroment = value : ClosureValue (Closure env fixTerm) : env
destroyStep value ((FrameLetIn env name term) : kont) = return (TermEnviromentKontinuation term (value : env) kont)
destroyStep _ _ = undefined
