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
import Data.Char (ord)
import qualified Data.ByteString.Lazy as BS
import Lang
import MonadFD4
import Subst (close)

type Opcode = Int

type Bytecode = [Int]

newtype Bytecode32 = BC {un32 :: [Word32]}

type Stack = [Val]

type Env = [Val]

data Val = I Int | Fun Env Bytecode | RA Env Bytecode

instance Num (Val) where
  (I x) + (I y) = I $ x + y
  (I x) - (I y) = I $ max 0 (x - y)

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

pattern POP_JUMP_IF_NOT_0 = 8

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
    (Bound index) -> return [ACCESS, index]
    (Free name) ->
      do
        x <- lookupDecl name
        case x of
          Just dec -> bc dec
          Nothing -> failPosFD4 i $ "Variable " ++ name ++ " no declarada"
    Global _ -> undefined -- Son terminos sin nombre
bc (Const _ (CNat n)) = return [CONST, n]
bc (Lam i name ty term) =
  do
    termBC <- bc term
    return $ [FUNCTION, length termBC] ++ termBC ++ [CALL]
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
    return $ condBc ++ [POP_JUMP_IF_NOT_0, length thenBc + 1] ++ thenBc ++ [JUMP, length elseBc] ++ elseBc
bc (Let i name ty term1 term2) =
  do
    bc1 <- bc term1
    bc2 <- bc term2
    return $ bc1 ++ [SHIFT] ++ bc2 ++ [DROP]

type Module = [Decl Term]

bytecompileModule :: MonadFD4 m => Module -> m Bytecode
bytecompileModule mdl = 
  do 
    term <- declsToTerm mdl
    termByteCode <- bc term
    return $ termByteCode ++ [PRINTN] ++ [STOP]
    where
      declsToTerm :: MonadFD4 m => [Decl Term] -> m Term
      -- TODO: Cambiar el tipo Nat acordemente
      declsToTerm [] = undefined 
      -- TODO: recorrer t para los Global
      declsToTerm [Decl pos name t] = return $ close name $ Let pos name NatTy t (V pos (Free name))
      declsToTerm ((Decl pos name t): k) =
        do 
          kt <- declsToTerm k
          return $ Let pos name NatTy t kt

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



runBC' :: MonadFD4 m => Bytecode -> Env -> Stack -> m ()
runBC' (NULL : c) e s = undefined
runBC' (RETURN : _) _ (v : RA e c : s) =
  runBC' c e (v : s)
runBC' (CONST : n : c) e s =
  runBC' c e (I n : s)
runBC' (ACCESS : i : c) e s =
  runBC' c e (e !! i : s)
runBC' (FUNCTION : len : c) e s =
  let c_f = take len c in runBC' c e (Fun e c_f : s)  
runBC' (CALL : c) e (v : Fun e_f c_f : s) =
  runBC' c_f (v : e_f) (RA e c : s)
runBC' (ADD : c) e (n : m : s) =
  runBC' c e (m + n : s)
runBC' (SUB : c) e (n : m : s) =
  runBC' c e (m - n : s)
runBC' (POP_JUMP_IF_NOT_0 : len: c) e (I n: s) = 
  if n == 0 
    then runBC' c e s
    else runBC' (drop len c) e s
runBC' (FIX : c) e (Fun e_fix c_f: s) = 
  runBC' c e s
  where
    -- TODO: revisar si efectivamente genera el nudo o el shadowing interfiere
    e_fix = Fun e_fix c_f : e
runBC' (STOP : c) e s = return ()
runBC' (SHIFT : c) e (v : s) =
  runBC' c (v : e) s
runBC' (DROP : c) (v : e) s =
  runBC' c e s
runBC' (PRINT : c') e s =
  do
    c <- consume c'
    runBC' c e s
    where
      consume :: MonadFD4 m => Bytecode -> m Bytecode
      consume (NULL: c) = return c
      consume (x_i: c) = 
        do 
          printFD4 $ show x_i
          consume c
      consume _ = undefined 

runBC' (PRINTN : c) e (I n : s) =
  do
    printFD4 $ show n
    runBC' c e (I n : s)
runBC' (JUMP : n : c) e s =
  runBC' (drop n c) e s
runBC' (command : c) e s = undefined
runBC' [] _ _ = failFD4 "Interrupción inesperada: no hay más bytecode pero no se consumió STOP"


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
humanReadableOpcode POP_JUMP_IF_NOT_0 = "POP_JUMP_IF_NOT_0"
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



runBCstep' :: MonadFD4 m => (Bytecode, Env, Stack) -> m (Bytecode, Env, Stack)
runBCstep' ((NULL : c), e, s) = undefined
runBCstep' ((RETURN : _), _, (v : RA e c : s)) =
  return (c, e, (v : s))
-- runBCstep' (CONST : n : c) e s =
--   runBCstep' c e (I n : s)
-- runBCstep' (ACCESS : i : c) e s =
--   runBCstep' c e (e !! i : s)
-- runBCstep' (FUNCTION : len : c) e s =
--   let c_f = take len c in runBCstep' c e (Fun e c_f : s)  
-- runBCstep' (CALL : c) e (v : Fun e_f c_f : s) =
--   runBCstep' c_f (v : e_f) (RA e c : s)
-- runBCstep' (ADD : c) e (n : m : s) =
--   runBCstep' c e (m + n : s)
-- runBCstep' (SUB : c) e (n : m : s) =
--   runBCstep' c e (m - n : s)
-- runBCstep' (POP_JUMP_IF_NOT_0 : len: c) e (I n: s) = 
--   if n == 0 
--     then runBCstep' c e s
--     else runBCstep' (drop len c) e s
-- runBCstep' (FIX : c) e (Fun e_fix c_f: s) = 
--   runBCstep' c e s
--   where
--     -- TODO: revisar si efectivamente genera el nudo o el shadowing interfiere
--     e_fix = Fun e_fix c_f : e
-- runBCstep' (STOP : c) e s = return ()
-- runBCstep' (SHIFT : c) e (v : s) =
--   runBCstep' c (v : e) s
-- runBCstep' (DROP : c) (v : e) s =
--   runBCstep' c e s
-- runBCstep' (PRINT : c') e s =
--   do
--     c <- consume c'
--     runBCstep' c e s
--     where
--       consume :: MonadFD4 m => Bytecode -> m Bytecode
--       consume (NULL: c) = return c
--       consume (x_i: c) = 
--         do 
--           printFD4 $ show x_i
--           consume c
--       consume _ = undefined 

-- runBCstep' (PRINTN : c) e (I n : s) =
--   do
--     printFD4 $ show n
--     runBCstep' c e (I n : s)
-- runBCstep' (JUMP : n : c) e s =
--   runBCstep' (drop n c) e s
-- runBCstep' (command : c) e s = undefined
-- runBCstep' [] _ _ = failFD4 "Interrupción inesperada: no hay más bytecode pero no se consumió STOP"

runBCstep' x = undefined