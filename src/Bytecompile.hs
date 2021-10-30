{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : Byecompile
-- Description : Compila a bytecode. Ejecuta bytecode.
-- Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
-- License     : GPL-3
-- Maintainer  : mauro@fceia.unr.edu.ar
-- Stability   : experimental
--
-- Este módulo permite compilar módulos a la BVM. También provee una implementación de la BVM
-- para ejecutar bytecode.
module Bytecompile (Bytecode, runBC, bcWrite, bcRead, bytecompileModule, humanReadableBC) where

import Data.Binary (Binary (get, put), Word32, decode, encode)
import Data.Binary.Get (getWord32le, isEmpty)
import Data.Binary.Put (putWord32le)
import qualified Data.ByteString.Lazy as BS
import Data.Char (ord)
import Lang
import MonadFD4
import PPrint (pp)
import Subst (close)

type Opcode = Int

type Bytecode = [Int]

newtype Bytecode32 = BC {un32 :: [Word32]}

type Stack = [Val]

type Env = [Val]

data Val = I Int | Fun Env Bytecode | RA Env Bytecode
  deriving (Show)

{- Esta instancia explica como codificar y decodificar Bytecode de 32 bits -}
instance Binary Bytecode32 where
  put (BC bs) = mapM_ putWord32le bs
  get = go
    where
      go =
        do
          empty <- isEmpty
          if empty
            then return $ BC []
            else do
              x <- getWord32le
              BC xs <- go
              return $ BC (x : xs)

{- Estos sinónimos de patrón nos permiten escribir y hacer
pattern-matching sobre el nombre de la operación en lugar del código
entero, por ejemplo:

   f (CALL : cs) = ...

 Notar que si hubieramos escrito algo como
   call = 5
 no podríamos hacer pattern-matching con `call`.

 En lo posible, usar estos códigos exactos para poder ejectutar un
 mismo bytecode compilado en distintas implementaciones de la máquina.
-}
pattern NULL = 0

pattern RETURN = 1

pattern CONST = 2

pattern ACCESS = 3

pattern FUNCTION = 4

pattern CALL = 5

pattern ADD = 6

pattern SUB = 7

pattern IFZ = 8

pattern FIX = 9

pattern STOP = 10

pattern SHIFT = 11

pattern DROP = 12

pattern PRINT = 13

pattern PRINTN = 14

-- Salta n pasos hacia delante
pattern JUMP = 15

bc :: MonadFD4 m => Term -> m Bytecode
bc (V i var) =
  case var of
    Bound index -> return [ACCESS, index]
    Free name -> undefined
    Global _ -> undefined -- Son terminos sin nombre
bc (Const _ (CNat n)) = return [CONST, n]
bc (Lam i name ty term) =
  do
    termBC <- bc term
    let innerFunction = termBC ++ [RETURN]
    return $ [FUNCTION, length innerFunction] ++ innerFunction
bc (App i t1 t2) =
  do
    bc1 <- bc t1
    bc2 <- bc t2
    return $ bc1 ++ bc2 ++ [CALL]
bc (Print i str term) =
  do
    termBC <- bc term
    return $ [PRINT] ++ map ord str ++ [NULL] ++ termBC ++ [PRINTN]
bc (BinaryOp i binOp t1 t2) =
  do
    bc1 <- bc t1
    bc2 <- bc t2
    return $ bc1 ++ bc2 ++ [binaryOpOpcode]
  where
    binaryOpOpcode =
      case binOp of
        Add -> ADD
        Sub -> SUB
bc (Fix i funName fTy name ty term) =
  do
    fnBC <- bc (Lam i funName ty term)
    return $ fnBC ++ [FIX]
bc (IfZ i cond thenTerm elseTerm) =
  do
    condBc <- bc cond
    thenBc <- bc thenTerm
    elseBc <- bc elseTerm
    return $ condBc ++ [IFZ, length thenBc + 1] ++ thenBc ++ [JUMP, length elseBc] ++ elseBc
bc (Let i name ty term1 term2) =
  do
    bc1 <- bc term1
    bc2 <- bc term2
    return $ bc1 ++ [SHIFT] ++ bc2 ++ [DROP]

type Module = [Decl Term]

setFreeVar :: Var -> Var
setFreeVar (Global name) = Free name
setFreeVar v = v

bytecompileModule :: MonadFD4 m => Module -> m Bytecode
bytecompileModule mdl =
  do
    -- fmap interno sobre Decl, fmap externo sobre Term.
    -- Tanto Term como Decl son funtores.
    term <- declsToLetIn $ map (fmap (fmap setFreeVar)) mdl
    ppTerm <- pp term
    printFD4 ppTerm
    termByteCode <- bc term
    return $ termByteCode ++ [PRINTN] ++ [STOP]
  where
    declsToLetIn :: MonadFD4 m => [Decl Term] -> m Term
    declsToLetIn [] = undefined
    declsToLetIn [Decl pos name t] = return $ Let pos name NatTy t (V pos (Bound 0))
    declsToLetIn ((Decl pos name t) : k) =
      do
        kLetInRecursive <- declsToLetIn k
        return $ Let pos name NatTy t $ close name kLetInRecursive

-- | Toma un bytecode, lo codifica y lo escribe un archivo
bcWrite :: Bytecode -> FilePath -> IO ()
bcWrite bs filename = BS.writeFile filename (encode $ BC $ fromIntegral <$> bs)

---------------------------

-- * Ejecución de bytecode

---------------------------

-- | Lee de un archivo y lo decodifica a bytecode
bcRead :: FilePath -> IO Bytecode
bcRead filename = (map fromIntegral <$> un32) . decode <$> BS.readFile filename

runBC :: MonadFD4 m => Bytecode -> m ()
runBC byteCode = runBC' byteCode [] []

-- TODO: Ver si se puede ser más expressivos
runBC' :: MonadFD4 m => Bytecode -> Env -> Stack -> m ()
runBC' c e s =
  do
    printFD4 $ show (humanReadableBC c, e, s)
    nextStep <- runBCstep (c, e, s)
    case nextStep of
      ([], _, _) ->
        do
          printFD4 "Se terminó el código"
          return ()
      (c', e', s') -> runBC' c' e' s'

-- Aux
humanReadableOpcode :: Opcode -> String
humanReadableOpcode RETURN = "RETURN"
humanReadableOpcode NULL = "NULL"
humanReadableOpcode CONST = "CONST"
humanReadableOpcode ACCESS = "ACCESS"
humanReadableOpcode FUNCTION = "FUNCTION"
humanReadableOpcode CALL = "CALL"
humanReadableOpcode ADD = "ADD"
humanReadableOpcode SUB = "SUB"
humanReadableOpcode IFZ = "IFZ"
humanReadableOpcode FIX = "FIX"
humanReadableOpcode PRINT = "PRINT"
humanReadableOpcode PRINTN = "PRINTN"
humanReadableOpcode JUMP = "JUMP"
humanReadableOpcode SHIFT = "SHIFT"
humanReadableOpcode DROP = "DROP"
humanReadableOpcode STOP = "STOP"
humanReadableOpcode n = "Literal " ++ show n

humanReadableBC :: Bytecode -> [String]
humanReadableBC = map humanReadableOpcode

-- La MonadFD4 es solo para printear
runBCstep :: MonadFD4 m => (Bytecode, Env, Stack) -> m (Bytecode, Env, Stack)
runBCstep (NULL : c, e, s) = undefined
runBCstep (RETURN : _, _, v : RA e c : s) =
  return (c, e, v : s)
runBCstep (CONST : n : c, e, s) =
  return (c, e, I n : s)
runBCstep (ACCESS : i : c, e, s) =
  return (c, e, e !! i : s)
runBCstep (FUNCTION : len : c_raw, e, s) =
  let (c_f, c) = splitAt len c_raw in return (c, e, Fun e c_f : s)
runBCstep (CALL : c, e, v : Fun e_f c_f : s) =
  return (c_f, v : e_f, RA e c : s)
runBCstep (ADD : c, e, I n : I m : s) =
  return (c, e, I (m + n) : s)
runBCstep (SUB : c, e, I n : I m : s) =
  return (c, e, I (max 0 (m - n)) : s)
runBCstep (IFZ : len : c, e, I n : s) =
  if n == 0
    then return (c, e, s)
    else return (drop len c, e, s)
runBCstep (FIX : c, e, Fun e_fix c_f : s) =
  return (c, e, s)
  where
    -- TODO: revisar si efectivamente genera el nudo o el shadowing interfiere
    e_fix = Fun e_fix c_f : e
runBCstep ([STOP], e, s) = return ([], e, s)
runBCstep (SHIFT : c, e, v : s) =
  return (c, v : e, s)
runBCstep (DROP : c, v : e, s) =
  return (c, e, s)
runBCstep (PRINT : c', e, s) =
  do
    c <- consume c'
    return (c, e, s)
  where
    consume :: MonadFD4 m => Bytecode -> m Bytecode
    consume (NULL : c) = return c
    consume (x_i : c) =
      do
        printFD4 $ show x_i
        consume c
    consume _ = undefined
runBCstep (PRINTN : c, e, I n : s) =
  do
    printFD4 $ show n
    return (c, e, I n : s)
runBCstep (JUMP : n : c, e, s) =
  return (drop n c, e, s)
runBCstep ([], _, _) = failFD4 "Interrupción inesperada: no hay más bytecode pero no se consumió STOP"
runBCstep (command : c, e, s) = failFD4 "ERROR: Comando inesperado."
