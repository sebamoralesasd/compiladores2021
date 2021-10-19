{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : Byecompile
Description : Compila a bytecode. Ejecuta bytecode.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Este módulo permite compilar módulos a la BVM. También provee una implementación de la BVM 
para ejecutar bytecode.
-}
module Bytecompile
  (Bytecode, runBC, bcWrite, bcRead,bytecompileModule)
 where

import Lang
import MonadFD4

import qualified Data.ByteString.Lazy as BS
import Data.Binary ( Word32, Binary(put, get), decode, encode )
import Data.Binary.Put ( putWord32le )
import Data.Binary.Get ( getWord32le, isEmpty )
import Data.List

type Opcode = Int
type Bytecode = [Int]

newtype Bytecode32 = BC { un32 :: [Word32] }

{- Esta instancia explica como codificar y decodificar Bytecode de 32 bits -}
instance Binary Bytecode32 where
  put (BC bs) = mapM_ putWord32le bs
  get = go
    where go =
           do
            empty <- isEmpty
            if empty
              then return $ BC []
              else do x <- getWord32le
                      BC xs <- go
                      return $ BC (x:xs)

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
pattern NULL     = 0
pattern RETURN   = 1
pattern CONST    = 2
pattern ACCESS   = 3
pattern FUNCTION = 4
pattern CALL     = 5
pattern ADD      = 6
pattern SUB      = 7
pattern POP_JUMP_IF_NOT_0      = 8
pattern FIX      = 9
pattern STOP     = 10
pattern SHIFT    = 11
pattern DROP     = 12
pattern PRINT    = 13
pattern PRINTN   = 14
pattern JUMP   = 15

bc :: MonadFD4 m => Term -> m Bytecode
bc (V i var) =
  case var of
    (Bound index) -> return [ACCESS, index]
    (Free name) ->
      do
        x <- lookupDecl name
        case x of
          Just dec -> bc dec
          Nothing -> failPosFD4 i $ "Variable " ++ name ++ "No declarada"
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
--bc (Print i str term)
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
--bc (Fix i funName fTy name ty term)
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
bc t = undefined

type Module = [Decl Term]

bytecompileModule :: MonadFD4 m => Module -> m Bytecode
bytecompileModule = error "implementame"

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
runBC c = error "implementame"
