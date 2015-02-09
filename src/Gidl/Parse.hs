
module Gidl.Parse where

import Data.Functor.Identity
import Data.Monoid
import Control.Monad
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Token
import Text.Parsec.Combinator
import Text.Parsec.Language
import Text.Parsec.Error

import Gidl.Types

type Parser u a = ParsecT String u Identity a

lexer :: GenTokenParser String u Identity
lexer = makeTokenParser haskellDef

tWhiteSpace :: Parser u ()
tWhiteSpace = whiteSpace lexer

tInteger :: Parser u Integer
tInteger = (integer lexer) <?> "integer"

tNatural :: Parser u Integer
tNatural = do
  i <- tInteger
  case i < 0 of
    True -> fail "expected positive integer"
    False -> return i

tFloat :: Parser u Double
tFloat = (float lexer) <?> "floating point number"

tString :: Parser u String
tString = (stringLiteral lexer) <?> "string"

tSymbol :: Parser u String
tSymbol = (many1 $ noneOf "()\" \t\n\r") <?> "symbol"

tIdentifier :: String -> Parser u ()
tIdentifier i = do
  s <- tSymbol
  case s == i of
    True -> return ()
    False -> fail ("expected identifier " ++ i)

tList :: Parser u a -> Parser u a
tList c = do
  tWhiteSpace
  void $ char '('
  tWhiteSpace
  r <- c
  tWhiteSpace
  void $ char ')'
  return r
  <?> "list"


tPair :: Parser u a
      -> Parser u b
      -> Parser u (a, b)
tPair a b = tList $ do
  ra <- a
  tWhiteSpace
  rb <- b
  return (ra, rb)

tTypeName :: Parser TypeEnv TypeName
tTypeName = do
  s <- tSymbol
  te <- getState
  case lookupTypeName s te of
    Just _ -> return s
    Nothing -> fail ("expected a known type name, got " ++ s)

tStructRow :: Parser TypeEnv (Identifier, TypeName)
tStructRow = tPair tSymbol tTypeName
  <?> "struct row"

tStructBody :: Parser TypeEnv [(Identifier, TypeName)]
tStructBody = tList (many1 (tWhiteSpace >> tStructRow))
  <?> "struct body"

tStructDecl :: Parser TypeEnv (TypeName, Type)
tStructDecl = tList $ do
  tIdentifier "def-struct"
  tWhiteSpace
  n <- tSymbol
  b <- tStructBody
  return (n, StructType (Struct b))

defineType :: (TypeName, Type) -> Parser TypeEnv ()
defineType (tn, t) = do
  te <- getState
  case lookupTypeName tn te of
    Just _ -> fail ("type named '" ++ tn ++ "' already exists")
    Nothing -> setState (insertType tn t te)

tNewtypeDecl :: Parser TypeEnv (TypeName, Type)
tNewtypeDecl = tList $ do
  tIdentifier "def-newtype"
  tWhiteSpace
  n <- tSymbol
  tWhiteSpace
  c <- tTypeName
  return (n, NewtypeType (Newtype c))

tEnumDecl :: Parser TypeEnv (TypeName, Type)
tEnumDecl = tList $ do
  tIdentifier "def-enum"
  tWhiteSpace
  n <- tSymbol
  -- XXX specify bit width, optionally
  vs <- tList $ many1 $ tPair tSymbol tNatural
  -- XXX check that symbols are unique, numbers are unique, numbers are
  -- ascending
  -- XXX make it possible to implicitly assign numbers
  return (n, EnumType (EnumT Bits32 vs))

tDecls :: Parser TypeEnv TypeEnv
tDecls = do
  _ <- many ((choice [try tStructDecl, try tNewtypeDecl, try tEnumDecl]) >>= defineType)
  getState

parseDecls :: String -> Either ParseError TypeEnv
parseDecls s = runP tDecls mempty "" s

