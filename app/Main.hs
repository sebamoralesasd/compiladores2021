{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Main
Description : Compilador de FD4.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}

module Main where

import System.Console.Haskeline ( defaultSettings, getInputLine, runInputT, InputT )
import Control.Monad.Catch (MonadMask)

--import Control.Monad
import Control.Monad.Trans
import Data.List (nub,  intersperse, isPrefixOf )
import Data.Char ( isSpace )
import Control.Exception ( catch , IOException )
import System.IO ( hPrint, stderr, hPutStrLn )

import System.Exit
--import System.Process ( system )
import Options.Applicative
--import Data.Text.Lazy (unpack)

import Global ( GlEnv(..) )
import Errors
import Lang
import Parse ( P, tm, program, declOrTm, runP )
import Elab ( elab )
import Eval ( eval )
import EvalCEK ( evalCEK )
import PPrint ( pp , ppTy, ppDecl )
import MonadFD4
import TypeChecker ( tc, tcDecl )

prompt :: String
prompt = "FD4> "

{- 
 Tipo para representar las banderas disponibles en línea de comando.
-}
data Mode =
    Interactive
  | Typecheck
  | InteractiveCEK
  deriving Show
-- | Bytecompile 
  -- | Bytecompile 
-- | Bytecompile 
-- | RunVM
-- | CC
-- | Canon
-- | LLVM
-- | Build

-- | Parser de banderas
parseMode :: Parser (Mode,Bool)
parseMode = (,) <$> 
      (flag' Typecheck ( long "typecheck" <> short 't' <> help "Chequear tipos e imprimir el término")
     <|> flag' InteractiveCEK (long "interactiveCEK" <> short 'k' <> help "Ejecutar interactivamente en la CEK")
  -- <|> flag' Bytecompile (long "bytecompile" <> short 'm' <> help "Compilar a la BVM")
  -- <|> flag' RunVM (long "runVM" <> short 'r' <> help "Ejecutar bytecode en la BVM")
      <|> flag Interactive Interactive ( long "interactive" <> short 'i' <> help "Ejecutar en forma interactiva")
  -- <|> flag' CC ( long "cc" <> short 'c' <> help "Compilar a código C")
  -- <|> flag' Canon ( long "canon" <> short 'n' <> help "Imprimir canonicalización")
  -- <|> flag' LLVM ( long "llvm" <> short 'l' <> help "Imprimir LLVM resultante")
  -- <|> flag' Build ( long "build" <> short 'b' <> help "Compilar")
      )
   <*> pure False
   -- reemplazar por la siguiente línea para habilitar opción
   -- <*> flag False True (long "optimize" <> short 'o' <> help "Optimizar código")
  
-- | Parser de opciones general, consiste de un modo y una lista de archivos a procesar
parseArgs :: Parser (Mode,Bool, [FilePath])
parseArgs = (\(a,b) c -> (a,b,c)) <$> parseMode <*> many (argument str (metavar "FILES..."))

main :: IO ()
main = execParser opts >>= go
  where
    opts = info (parseArgs <**> helper)
      ( fullDesc
     <> progDesc "Compilador de FD4"
     <> header "Compilador de FD4 de la materia Compiladores 2021" )

    go :: (Mode,Bool,[FilePath]) -> IO ()
    go (Interactive,_,files) = 
              do runFD4 (runInputT defaultSettings (repl files Interactive))
                 return ()
    go (Typecheck,opt, files) =
              runOrFail $ mapM_ (typecheckFile opt) files
    go (InteractiveCEK,_, files) = 
      do 
        runFD4 (runInputT defaultSettings (repl files InteractiveCEK))
        return ()
    -- go (Bytecompile,_, files) =
    --           runOrFail $ mapM_ bytecompileFile files
    -- go (RunVM,_,files) =
    --           runOrFail $ mapM_ bytecodeRun files
    -- go (CC,_, files) =
    --           runOrFail $ mapM_ ccFile files
    -- go (Canon,_, files) =
    --           runOrFail $ mapM_ canonFile files 
    -- go (LLVM,_, files) =
    --           runOrFail $ mapM_ llvmFile files
    -- go (Build,_, files) =
    --           runOrFail $ mapM_ buildFile files

runOrFail :: FD4 a -> IO a
runOrFail m = do
  r <- runFD4 m
  case r of
    Left err -> do
      liftIO $ hPrint stderr err
      exitWith (ExitFailure 1)
    Right v -> return v

repl :: (MonadFD4 m, MonadMask m) => [FilePath] -> Mode -> InputT m ()
repl args mode = do
       lift $ catchErrors $ compileFiles args mode
       s <- lift get
       when (inter s) $ liftIO $ putStrLn
         (  "Entorno interactivo para LD4.\n"
         ++ "Escriba :? para recibir ayuda.")
       loop
  where loop = do
           minput <- getInputLine prompt
           case minput of
               Nothing -> return ()
               Just "" -> loop
               Just x -> do
                       c <- liftIO $ interpretCommand x
                       b <- lift $ catchErrors $ handleCommand c mode
                       maybe loop (`when` loop) b

compileFiles ::  MonadFD4 m => [FilePath] -> Mode -> m ()
compileFiles [] _   = return ()
compileFiles (x:xs) mode = do
        modify (\s -> s { lfile = x, inter = False })
        compileFile mode x
        compileFiles xs mode

loadFile ::  MonadFD4 m => FilePath -> m [Decl NTerm]
loadFile f = do
    let filename = reverse(dropWhile isSpace (reverse f))
    x <- liftIO $ catch (readFile filename)
               (\e -> do let err = show (e :: IOException)
                         hPutStrLn stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err)
                         return "")
    setLastFile filename
    parseIO filename program x

compileFile ::  MonadFD4 m => Mode -> FilePath -> m ()
compileFile mode f = do
    printFD4 ("Abriendo "++f++"...")
    let filename = reverse(dropWhile isSpace (reverse f))
    x <- liftIO $ catch (readFile filename)
               (\e -> do let err = show (e :: IOException)
                         hPutStrLn stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err)
                         return "")
    decls <- parseIO filename program x
    mapM_ (handleDecl mode) decls

typecheckFile ::  MonadFD4 m => Bool -> FilePath -> m ()
typecheckFile opt f = do
    printFD4  ("Chequeando "++f)
    decls <- loadFile f
    ppterms <- mapM (typecheckDecl >=> ppDecl) decls
    mapM_ printFD4 ppterms

parseIO ::  MonadFD4 m => String -> P a -> String -> m a
parseIO filename p x = case runP p x filename of
                  Left e  -> throwError (ParseErr e)
                  Right r -> return r

typecheckDecl :: MonadFD4 m => Decl NTerm -> m (Decl Term)
typecheckDecl (Decl p x t) = do
        let dd = (Decl p x (elab t))
        tcDecl dd
        return dd

handleDecl ::  MonadFD4 m => Mode -> Decl NTerm -> m ()
handleDecl mode d = do
        (Decl p x tt) <- typecheckDecl d
        printFD4 $ show mode
        te <- case mode of
          Interactive -> eval tt
          InteractiveCEK -> evalCEK tt
          _ -> undefined
        addDecl (Decl p x te)


-- TODO: agregar comando para evaluación con CEK
data Command = Compile CompileForm
             | PPrint String
             | Type String
             | Reload
             | Browse
             | Quit
             | Help
             | Noop

data CompileForm = CompileInteractive     String
                 | CompileInteractiveCEK  String
                 | CompileFile            String

data InteractiveCommand = Cmd [String] String (String -> Command) String

-- | Parser simple de comando interactivos
interpretCommand :: String -> IO Command
interpretCommand x
  =  if isPrefixOf ":" x then
       do  let  (cmd,t')  =  break isSpace x
                t         =  dropWhile isSpace t'
           --  find matching commands
           let  matching  =  filter (\ (Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
           case matching of
             []  ->  do  putStrLn ("Comando desconocido `" ++ cmd ++ "'. Escriba :? para recibir ayuda.")
                         return Noop
             [Cmd _ _ f _]
                 ->  do  return (f t)
             _   ->  do  putStrLn ("Comando ambigüo, podría ser " ++
                                   concat (intersperse ", " [ head cs | Cmd cs _ _ _ <- matching ]) ++ ".")
                         return Noop

     else
       return (Compile (CompileInteractiveCEK x))

commands :: [InteractiveCommand]
commands
  =  [ Cmd [":browse"]      ""        (const Browse) "Ver los nombres en scope",
       Cmd [":load"]        "<file>"  (Compile . CompileFile)
                                                     "Cargar un programa desde un archivo",
       Cmd [":print"]       "<exp>"   PPrint          "Imprime un término y sus ASTs sin evaluarlo",
       Cmd [":reload"]      ""        (const Reload)         "Vuelve a cargar el último archivo cargado",
       Cmd [":type"]        "<exp>"   Type           "Chequea el tipo de una expresión",
       Cmd [":quit",":Q"]        ""        (const Quit)   "Salir del intérprete",
       Cmd [":help",":?"]   ""        (const Help)   "Mostrar esta lista de comandos",
       Cmd [":cek",":k"]   "<exp>"         (Compile . CompileInteractiveCEK)   "Evaluar usando la máquina CEK" ]


helpTxt :: [InteractiveCommand] -> String
helpTxt cs
  =  "Lista de comandos:  Cualquier comando puede ser abreviado a :c donde\n" ++
     "c es el primer caracter del nombre completo.\n\n" ++
     "<expr>                  evaluar la expresión\n" ++
     "let <var> = <expr>      definir una variable\n" ++
     unlines (map (\ (Cmd c a _ d) ->
                   let  ct = concat (intersperse ", " (map (++ if null a then "" else " " ++ a) c))
                   in   ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d) cs)

-- | 'handleCommand' interpreta un comando y devuelve un booleano
-- indicando si se debe salir del programa o no.
handleCommand ::  MonadFD4 m => Command -> Mode -> m Bool
handleCommand cmd mode = do
   printFD4 $ show mode
   s@GlEnv {..} <- get
   case cmd of
       Quit   ->  return False
       Noop   ->  return True
       Help   ->  printFD4 (helpTxt commands) >> return True
       Browse ->  do  printFD4 (unlines [ name | name <- reverse (nub (map declName glb)) ])
                      return True
       Compile c ->
                  do  case c of
                          CompileInteractive e -> compilePhrase e mode
                          CompileInteractiveCEK e -> compilePhrase e mode
                          CompileFile f        -> put (s {lfile=f, cantDecl=0}) >> compileFile mode f
                      return True
       Reload ->  eraseLastFileDecls >> (getLastFile >>= (compileFile mode)) >> return True
       PPrint e   -> printPhrase e >> return True
       Type e    -> typeCheckPhrase e >> return True

compilePhrase ::  MonadFD4 m => String -> Mode -> m ()
compilePhrase x mode =
  do
    dot <- parseIO "<interactive>" declOrTm x
    case dot of 
      Left d  -> handleDecl mode d
      Right t -> handleTerm t mode

handleTerm ::  MonadFD4 m => NTerm -> Mode -> m ()
handleTerm t mode = do
         let tt = elab t
         s <- get
         ty <- tc tt (tyEnv s)
         te <- case mode of
           Interactive -> eval tt
           InteractiveCEK -> evalCEK tt
           _ -> undefined
         ppte <- pp te
         printFD4 (ppte ++ " : " ++ ppTy ty)

printPhrase   :: MonadFD4 m => String -> m ()
printPhrase x =
  do
    x' <- parseIO "<interactive>" tm x
    let ex = elab x'
    t  <- case x' of 
           (V p f) -> maybe ex id <$> lookupDecl f
           _       -> return ex  
    printFD4 "NTerm:"
    printFD4 (show x')
    printFD4 "\nTerm:"
    printFD4 (show t)

typeCheckPhrase :: MonadFD4 m => String -> m ()
typeCheckPhrase x = do
         t <- parseIO "<interactive>" tm x
         let tt = elab t
         s <- get
         ty <- tc tt (tyEnv s)
         printFD4 (ppTy ty)