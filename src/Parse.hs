{-|
Module      : Parse
Description : Define un parser de términos FD40 a términos fully named.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}

module Parse (tm, Parse.parse, decl, runP, P, program, declOrTm) where

import Prelude hiding ( const )
import Lang
import Common
import Text.Parsec hiding (runP,parse)
import Data.Char ( isNumber, ord )
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language --( GenLanguageDef(..), emptyDef )
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Expr (Operator, Assoc)
import Control.Monad.Identity (Identity)

type P = Parsec String ()

-----------------------
-- Lexer
-----------------------
-- | Analizador de Tokens
lexer :: Tok.TokenParser u
lexer = Tok.makeTokenParser $
        emptyDef {
         commentLine    = "#",
         reservedNames = ["let", "fun", "fix", "then", "else","in", 
                           "ifz", "print", "Nat", "rec", "type"],
         reservedOpNames = ["->",":","=","+","-"]
        }

whiteSpace :: P ()
whiteSpace = Tok.whiteSpace lexer

natural :: P Integer 
natural = Tok.natural lexer

stringLiteral :: P String
stringLiteral = Tok.stringLiteral lexer

parens :: P a -> P a
parens = Tok.parens lexer

identifier :: P String
identifier = Tok.identifier lexer

reserved :: String -> P ()
reserved = Tok.reserved lexer

reservedOp :: String -> P ()
reservedOp = Tok.reservedOp lexer

-----------------------
-- Parsers
-----------------------

num :: P Int
num = fromInteger <$> natural

var :: P Name
var = identifier 

getPos :: P Pos
getPos = do pos <- getPosition
            return $ Pos (sourceLine pos) (sourceColumn pos)

tyatom :: P STy
tyatom = (reserved "Nat" >> return SNatTy)
         <|> parens typeP

typeP :: P STy
typeP = try (do 
          x <- tyatom
          reservedOp "->"
          y <- typeP
          return (SFunTy x y))
      <|> tyatom
          
const :: P Const
const = CNat <$> num

printOp :: P SNTerm
printOp = do
  i <- getPos
  reserved "print"
  str <- option "" stringLiteral
  return (SPrintUnary i str)

binary :: String -> BinaryOp -> Assoc -> Operator String () Identity SNTerm
binary s f = Ex.Infix (reservedOp s >> return (SBinaryOp NoPos f))

table :: [[Operator String () Identity SNTerm]]
table = [[binary "+" Add Ex.AssocLeft,
          binary "-" Sub Ex.AssocLeft]]

expr :: P SNTerm
expr = Ex.buildExpressionParser table tm

atom :: P SNTerm
atom =     (flip SConst <$> const <*> getPos)
       <|> flip SV <$> var <*> getPos
       <|> parens expr
       <|> printOp

-- parsea un par (variable : tipo)
binding :: P [(Name, STy)]
binding = do v <- var
             reservedOp ":"
             ty <- typeP
             return [(v, ty)]

-- TODO: Revisar, me parece que multibinding abarca a binding, lo cual haría que la definición de binders sea más plana
-- x y z ... : \tau
multibinding :: P [(Name, STy)]
multibinding = 
  do
    variables <- many1 var
    reservedOp ":"
    ty <- typeP
    return (map (\name -> (name, ty)) variables)

binders :: P [(Name, STy)]
binders = 
  do 
    b <- many (parens (multibinding <|> binding))
    return (concat b)


lam :: P SNTerm
lam = do i <- getPos
         reserved "fun"
         vty <- binders
         reservedOp "->"
         t <- expr
         return (SLam i vty t)

-- Nota el parser app también parsea un solo atom.
app :: P SNTerm
app = (do i <- getPos
          f <- atom
          args <- many atom
          return (foldl (SApp i) f args))

ifz :: P SNTerm
ifz = do i <- getPos
         reserved "ifz"
         c <- expr
         reserved "then"
         t <- expr
         reserved "else"
         e <- expr
         return (SIfZ i c t e)

fix :: P SNTerm
fix = do i <- getPos
         reserved "fix"
         [(f, fty)] <- parens binding
         [(x, xty)] <- parens binding
         reservedOp "->"
         t <- expr
         return (SFix i f fty x xty t)

letexp :: P SNTerm
letexp = do
  i <- getPos
  reserved "let"
  v <- var
  bind <- binders
  reservedOp ":"
  ty <- typeP
  reservedOp "="  
  def <- expr
  reserved "in"
  body <- expr
  return (SLet i v ty bind def body)
  
letfunexp :: P SNTerm
letfunexp = do
  i <- getPos
  reserved "let"
  (do 
    reserved "rec"
    letrecexpAux i True
      <|>
      letrecexpAux i False)
  where letrecexpAux i isRec = 
          do
            functionName <- var
            bind <- binders
            reservedOp ":"
            functionReturnType <- typeP
            reservedOp "="  
            def <- expr
            reserved "in"
            body <- expr
            if isRec then return (SLetRec i functionName functionReturnType bind def body)
                    else return (SLet i functionName functionReturnType bind def body)

-- | Parser de términos
tm :: P SNTerm
-- TODO chequear si debería ser así: 
tm = app <|> lam <|> ifz <|> printOp <|> fix <|> try letexp <|> letfunexp
--tm = app <|> lam <|> ifz <|> printOp <|> fix <|> letexp <|> letfunexp

-- | Parser de declaraciones
decl :: P (Decl SNTerm)
decl = try letdecl <|> try letrecdecl <|> letfundecl-- <|> vardecl <|> typedecl

typedecl :: P (Decl SNTerm)
typedecl = do
  i <- getPos
  reserved "type"
  v <- var
  reservedOp "="
  ty <- typeP
  return (Decl i v (SinTy i v ty))

-- TODO: Revisar qué hacemos con variableType
letdecl :: P (Decl SNTerm)
letdecl = do
  i <- getPos
  reserved "let"
  [(variable, variableType)] <- binding <|> parens binding
  reservedOp "="  
  def <- expr
  return (Decl i variable def)


letfundecl :: P (Decl SNTerm)
letfundecl = do
  i <- getPos
  reserved "let"
  v <- var
  bind <- binders
  reservedOp ":"
  ty <- typeP
  reservedOp "="  
  def <- expr
  return (Decl i v (SLam i bind def))

letrecdecl :: P (Decl SNTerm)
letrecdecl = do
  i <- getPos
  reserved "let"
  reserved "rec"
  v <- var
  bind <- binders
  reservedOp ":"
  fty <- typeP
  reservedOp "="  
  def <- expr
  case bind of
    [(n, ty)] -> 
      let sfixDecl = SFix i v (SFunTy ty fty) n ty def
      in return (Decl i v sfixDecl)
    _ -> 
      let sLetRec = SLetRec i v fty bind def (SLam i bind def)
      in return (Decl i v sLetRec)

-- | Parser de programas (listas de declaraciones) 
program :: P [Decl SNTerm]
program = many decl

-- | Parsea una declaración a un término
-- Útil para las sesiones interactivas
declOrTm :: P (Either (Decl SNTerm) SNTerm)
declOrTm =  try (Right <$> expr) <|> (Left <$> decl)

-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

--para debugging en uso interactivo (ghci)
parse :: String -> SNTerm
parse s = case runP expr s "" of
            Right t -> t
            Left e -> error ("no parse: " ++ show s)
