{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : MonadFD4
Description : Mónada con soporte para estado, errores, e IO.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Definimos la clase de mónadas 'MonadFD4' que abstrae las mónadas con soporte para estado, errores e IO,
y la mónada 'FD4' que provee una instancia de esta clase.
-}

module MonadFD4 (
  FD4,
  runFD4,
  lookupDecl,
  lookupTy,
  lookupSTy,
  printFD4,
  setLastFile,
  getLastFile,
  eraseLastFileDecls,
  failPosFD4,
  failFD4,
  addDecl,
  addTy,
  addsupTy,
  catchErrors,
  MonadFD4,
  module Control.Monad.Except,
  module Control.Monad.State)
 where

import Common
import Lang
import Global
import Errors ( Error(..) )
import Control.Monad.State
import Control.Monad.Except
import System.IO
import Data.List (deleteFirstsBy)

-- * La clase 'MonadFD4m'

{-| La clase de mónadas 'MonadFD4' clasifica a las mónadas con soporte para operaciones @IO@, estado de tipo 'Global.GlEnv', y errores de tipo 'Errors.Error'.

Las mónadas @m@ de esta clase cuentan con las operaciones:
   - @get :: m GlEnv@
   - @put :: GlEnv -> m ()@
   - @throwError :: Error -> m a@
   - @catchError :: m a -> (Error -> m a) -> m a@
   - @liftIO :: IO a -> m a@

y otras operaciones derivadas de ellas, como por ejemplo
   - @modify :: (GlEnv -> GlEnv) -> m ()

-}
class (MonadIO m, MonadState GlEnv m, MonadError Error m) => MonadFD4 m where

printFD4 :: MonadFD4 m => String -> m ()
printFD4 = liftIO . putStrLn

setLastFile :: MonadFD4 m => FilePath -> m ()
setLastFile filename = modify (\s -> s {lfile = filename})

getLastFile :: MonadFD4 m => m FilePath
getLastFile = gets lfile

addDecl :: MonadFD4 m => Decl Term -> m ()
addDecl d = modify (\s -> s { glb = d : glb s, cantDecl = cantDecl s + 1 })
  
addTy :: MonadFD4 m => Name -> Ty -> m ()
addTy n ty = modify (\s -> s { tyEnv = (n,ty) : tyEnv s })

addsupTy :: MonadFD4 m => Name -> Ty -> m ()
addsupTy n supTy = modify (\s -> s { supTyEnv = (n,supTy) : supTyEnv s })

eraseLastFileDecls :: MonadFD4 m => m ()
eraseLastFileDecls = do 
      s <- get
      let n = cantDecl s
          (era,rem) = splitAt n (glb s)
          tyEnv' = deleteTy (map declName era) (tyEnv s)
      modify (\s -> s {glb = rem, cantDecl = 0, tyEnv = tyEnv'})
   where deleteTy xs ps = deleteFirstsBy (\x y -> fst x == fst y) ps (map (flip (,) NatTy) xs)
hasName :: Name -> Decl a -> Bool
hasName nm (Decl { declName = nm' }) = nm == nm'

lookupDecl :: MonadFD4 m => Name -> m (Maybe Term)
lookupDecl nm = do
     s <- get
     case filter (hasName nm) (glb s) of
       (Decl { declBody=e }):_ -> return (Just e)
       [] -> return Nothing

lookupTy :: MonadFD4 m => Name -> m (Maybe Ty)
lookupTy nm = do
      s <- get
      return $ lookup nm (tyEnv s)

lookupSTy :: MonadFD4 m => Name -> m (Maybe Ty)
lookupSTy nm = do
      s <- get
      return $ lookup nm (supTyEnv s)

failPosFD4 :: MonadFD4 m => Pos -> String -> m a
failPosFD4 p s = throwError (ErrPos p s)

failFD4 :: MonadFD4 m => String -> m a
failFD4 = failPosFD4 NoPos

catchErrors  :: MonadFD4 m => m a -> m (Maybe a)
catchErrors c = catchError (Just <$> c) 
                           (\e -> liftIO $ hPutStrLn stderr (show e) 
                              >> return Nothing)

----
-- Importante, no eta-expandir porque GHC no hace una
-- eta-contracción de sinónimos de tipos
-- y Main no va a compilar al escribir `InputT FD4()`

-- | El tipo @FD4@ es un sinónimo de tipo para una mónada construida usando dos transformadores de mónada sobre la mónada @IO@.
-- El transformador de mónad @ExcepT Error@ agrega a la mónada IO la posibilidad de manejar errores de tipo 'Errors.Error'.
-- El transformador de mónadas @StateT GlEnv@ agrega la mónada @ExcepT Error IO@ la posibilidad de manejar un estado de tipo 'Global.GlEnv'.
type FD4 = StateT GlEnv (ExceptT Error IO)

-- | Esta es una instancia vacía, ya que 'MonadFD4' no tiene funciones miembro.
instance MonadFD4 FD4

-- 'runFD4\'' corre una computación de la mónad 'FD4' en el estado inicial 'Global.initialEnv' 
runFD4' :: FD4 a -> IO (Either Error (a, GlEnv))
runFD4' c =  runExceptT $ runStateT c initialEnv

runFD4:: FD4 a -> IO (Either Error a)
runFD4 c = fmap fst <$> runFD4' c
