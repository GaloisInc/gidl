{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Gidl.Parse (parseDecls, ppDecl) where

import Prelude ()
import Prelude.Compat

import Control.Monad ((>=>))
import Control.Monad.Reader (ask, lift, local, runReaderT)
import Data.List (partition, group, intercalate)
import Data.SCargot.Comments (withHaskellComments)
import Data.SCargot
import Data.SCargot.Language.HaskLike
import Data.SCargot.Repr.WellFormed
import Data.Text (unpack, pack)

import Gidl.Types
import Gidl.Interface

-- We parse into this abstract structure before converting it to the
-- structures Gidl uses elsewhere. That way, we can separate our
-- parsing and our checking.
data Decl
  = NewtypeDecl Identifier Identifier
  | EnumDecl (Identifier, Bits) [(Identifier, Integer)]
  | StructDecl Identifier [(Identifier, Identifier)]
  | InterfaceDecl Identifier [Identifier] [(Identifier, MethodDecl)]
    deriving (Eq, Show)

data MethodDecl
  = AttrDecl Perm Identifier
  | StreamDecl Identifier
    deriving (Eq, Show)

unlessEmpty :: [a] -> (a -> String) -> Either String ()
unlessEmpty [] _ = return ()
unlessEmpty as msg  = throw (intercalate ";\n" (map msg as))

duplicated :: (Eq a) => [a] -> [a]
duplicated as = map (\a -> a !! 0) $ filter (\a -> length a > 1) $ group as

-- Here's a function to convert those decls.
toEnv :: [Decl] -> Either String (TypeEnv, InterfaceEnv)
toEnv decls = do
  unlessEmpty (duplicated typeNames)
      (\n -> "Type named '" ++ n ++ "' declared multiple times")
  unlessEmpty (filter (\t -> elem t (map fst builtins)) typeNames)
      (\n -> "Builtin type named '" ++ n ++ "' cannot be redeclared")
  unlessEmpty (duplicated interfaceNames)
      (\n -> "Interface named '" ++ n ++ "' declared multiple times")

  typs <- mapM (flip runReaderT [] . getTypePair . getName) typDs
  ifcs <- mapM getIfacePair interfaceNames
  return (TypeEnv typs, InterfaceEnv ifcs)
  where (typDs, ifcDs) = partition isTypeDecl decls

        builtins = let TypeEnv bs = baseTypeEnv in bs

        typeNames = map getName typDs
        interfaceNames = map getName ifcDs

        typMap = [(getName d, toType d) | d <- typDs] ++
                 [(n, return (n, t)) | (n, t) <- builtins ]
        ifcMap = [(getName i, toInterface i) | i <- ifcDs]

        -- this is gross because I'm trying to make sure declarations
        -- can happen in any order.
        getType n = snd `fmap` getTypePair n
        getTypePair n = do
          env <- ask
          if n `elem` env
            then lift $ throw ("Types cannot be recursive.\n" ++
                               showCycle env)
            else case lookup n typMap of
              Just rs -> rs
              Nothing -> lift $ throw ("Unknown primitive type: " ++ n)

        getIface n = snd `fmap` getIfacePair n
        getIfacePair n = case lookup n ifcMap of
          Just (Right i) -> return i
          Just (Left l)  -> Left l
          Nothing        -> Left ("Unknown interface: " ++ n)

        getPrimType n = do
          t <- getType n
          case t of
            PrimType t' -> return t'
            _ -> lift $ throw ("Expected primitive type but got " ++ show t)

        -- converts a decl to an actual type
        toType (NewtypeDecl n t) = local (n:) $ do
          t' <- getPrimType t
          return (n, PrimType (Newtype n t'))
        toType (EnumDecl (n, s) ts) = local (n:) $ do
          lift $ unlessEmpty (duplicated (map fst ts))
              (\i -> "Enum identifier '" ++ i
                  ++ "' repeated in declaration of 'Enum " ++ n ++ "'")
          lift $ unlessEmpty (duplicated (map snd ts))
              (\i -> "Enum value '" ++ (show i)
                  ++ "' repeated in declaration of 'Enum " ++ n ++ "'")
          return (n, PrimType (EnumType n s ts))
        toType (StructDecl n ss) = local (n:) $ do
          ps <- mapM (getType . snd) ss
          return (n, StructType n (zip (map fst ss) ps))
        toType _ = error "[unreachable]"

        toMethod (n, AttrDecl perm t) = do
          t' <- runReaderT (getType t) []
          return (n, AttrMethod perm t')

        toMethod (n, StreamDecl t) = do
          t' <- runReaderT (getType t) []
          return (n, StreamMethod 0 t')

        toInterface (InterfaceDecl n is ms) = do
          ms' <- mapM toMethod ms
          is' <- mapM getIface is
          return (n, Interface n is' ms')
        toInterface _ = error "[unreachable]"

        getName (NewtypeDecl n _)     = n
        getName (EnumDecl (n, _) _)   = n
        getName (StructDecl n _)      = n
        getName (InterfaceDecl n _ _) = n

        isTypeDecl InterfaceDecl {} = False
        isTypeDecl _                = True

        showCycle []        = error "[unreachable]"
        showCycle [x]       = "  In recursive type `" ++ x ++ "`"
        showCycle ls@(x:_)  = "  In mutually recursive cycle " ++ go ls
          where go (y:ys) = "`" ++ y ++ "` => " ++ go ys
                go []     = "`" ++ x ++ "`"

parseDecls :: String -> Either String (TypeEnv, InterfaceEnv)
parseDecls = return . pack >=> decode gidlSpec >=> toEnv

gidlSpec :: SExprParser HaskLikeAtom Decl
gidlSpec
  = withHaskellComments
  $ setCarrier tDecl
  $ asWellFormed haskLikeParser

-- utility aliases and helpers
type Parse a = WellFormedSExpr HaskLikeAtom -> Either String a

throw :: String -> Either String a
throw = Left

infix 9 /?/
(/?/) :: Either String a -> String -> Either String a
Left msg /?/ ctx = throw (msg ++ "\n  in parsing " ++ ctx)
r        /?/ _   = r

infix 9 `asErr`
asErr :: Either String a -> String -> Either String a
Left _ `asErr` msg = throw msg
r      `asErr` _   = r

seShow :: WellFormedSExpr HaskLikeAtom -> String
seShow sx = "`" ++ unpack (encodeOne wf sx) ++ "`"
  where wf = setFromCarrier fromWellFormed haskLikePrinter

atShow :: WellFormedSExpr HaskLikeAtom -> String
atShow e = go e ++ " " ++ seShow e
  where
    go (A (HSInt _))    = "int"
    go (A (HSString _)) = "string"
    go (A (HSIdent _))  = "identifier"
    go (A (HSFloat _))  = "float"
    go (L _)            = "list"
    go _                = "??"

-- basic parsing of things (these might show up in s-cargot
--  proper eventually?)

tSymbol :: Parse String
tSymbol e = asAtom go e `asErr`
              ("Expected identifier; got " ++ atShow e)
  where go (HSIdent i) = return (unpack i)
        go sx          = throw ("Expected identifier; got " ++ atShow (A sx))

tType :: Parse String
tType = tSymbol

tInteger :: Parse Integer
tInteger e = asAtom go e `asErr` ("Expected integer; got " ++ atShow e)
  where go (HSInt n) = return n
        go sx        = throw ("Expected integer; got " ++ atShow (A sx))

-- some parsing of gidl-specific types
tBits :: Parse Bits
tBits = tInteger >=> tWidth

tWidth :: Integer -> Either String Bits
tWidth 8  = return Bits8
tWidth 16 = return Bits16
tWidth 32 = return Bits32
tWidth 64 = return Bits64
tWidth _  = throw "Expected enum bit size to be 8, 16, 32, or 64"

tPermission :: Parse Perm
tPermission e = asAtom go e `asErr` ("unknown permission: " ++ seShow e)
  where go token
          | token == "read"      || token == "r"  = return Read
          | token == "write"     || token == "w"  = return Write
          | token == "readwrite" || token == "rw" = return ReadWrite
          | otherwise = throw "error"

-- newtypes
tNewtypeDecl :: Parse Decl
tNewtypeDecl = asList go
 where go ["def-newtype",name,typ] =
         NewtypeDecl <$> tSymbol name /?/ "newtype name"
                     <*> tSymbol typ  /?/ "newtype type"
       go _ = throw "wrong number of elements"

-- structs
tStructDecl :: Parse Decl
tStructDecl = asList go
  where go ("def-struct":name:body) =
            StructDecl
              <$> (tSymbol name         /?/ "struct name")
              <*> (mapM tStructRow body /?/
                     ("struct body " ++ seShow (L (A "..." : body))))
        go _ = throw "invalid struct decl"

tStructRow :: Parse (Identifier, Identifier)
tStructRow sx =
  fromPair tSymbol tType sx /?/ ("struct row " ++ seShow sx)

-- enums
tEnumDecl :: Parse Decl
tEnumDecl = asList go
  where go ("def-enum" : name : body) =
          EnumDecl <$> tEnumName name      /?/ "enum name"
                   <*> mapM tEnumBody body
        go _ = throw "invalid enum decl"

tEnumName :: Parse (Identifier, Bits)
tEnumName (L [name, size]) = (,) <$> tSymbol name <*> tBits size
tEnumName name             = (,) <$> tSymbol name <*> return Bits8

tEnumBody :: Parse (Identifier, Integer)
tEnumBody e =
  fromPair tSymbol tInteger e /?/ ("enum constructor " ++ seShow e)

-- interface declarations
tInterfaceDecl :: Parse Decl
tInterfaceDecl = asList go
  where go ("def-interface":name:parents:body) =
          InterfaceDecl
            <$> tSymbol name                  /?/ "interface name"
            <*> asList (mapM tSymbol) parents
                  /?/ ("interface parents " ++ seShow parents)
            <*> mapM tInterfaceMethod body
        go _ = throw "invalid interface decl"

tInterfaceMethod :: Parse (Identifier, MethodDecl)
tInterfaceMethod e =
  fromPair tSymbol (asList go) e /?/ ("interface method " ++ seShow e)
  where go ["attr",   p, t] = AttrDecl   <$> tPermission p <*> tType t
        go ["stream", t]    = StreamDecl <$> tType t
        go (x:_) = throw ("unknown interface type: " ++ seShow x)
        go []    = throw "empty interface decl"

-- declarations in general
tDecl :: Parse Decl
tDecl ls@(L ("def-struct" : _)) =
  tStructDecl ls /?/ ("struct " ++ showHead ls)
tDecl ls@(L ("def-newtype" : _)) =
  tNewtypeDecl ls /?/ ("newtype " ++ showHead ls)
tDecl ls@(L ("def-enum" : _)) =
  tEnumDecl ls /?/ ("enum " ++ showHead ls)
tDecl ls@(L ("def-interface" : _)) =
  tInterfaceDecl ls /?/ ("interface " ++ showHead ls)
tDecl (L (A name : _)) =
  throw ("unknown declaration type: " ++ seShow (A name))
tDecl item = do
  throw ("invalid top-level item " ++ seShow item)

showHead :: WellFormedSExpr HaskLikeAtom -> String
showHead (L (a:b:_)) = seShow (L [a,b,"..."])
showHead sx          = seShow sx

-- For now, no pretty-printing (but it will come soon!)
ident :: Identifier -> WellFormedSExpr HaskLikeAtom
ident = A . HSIdent . pack

int :: Integer -> WellFormedSExpr HaskLikeAtom
int = A . HSInt

ppBits :: Bits -> WellFormedSExpr HaskLikeAtom
ppBits Bits8  = A (HSInt 8)
ppBits Bits16 = A (HSInt 16)
ppBits Bits32 = A (HSInt 32)
ppBits Bits64 = A (HSInt 64)

ppDecl :: Decl -> WellFormedSExpr HaskLikeAtom
ppDecl (NewtypeDecl name typ) =
  L ["def-newtype", ident name, ident typ ]
ppDecl (EnumDecl (name, Bits32) fields) =
  L ( "def-enum"
    : ident name
    : [ L [ ident a, int b ]
       | (a, b) <- fields
       ]
    )
ppDecl (EnumDecl (name, bits) fields) =
  L ( "def-enum"
    : L [ ident name, ppBits bits ]
    : [ L [ ident a, int b ]
      | (a, b) <- fields
      ]
    )
ppDecl (StructDecl name fields) =
  L ( "def-struct"
    : ident name
    : [ L [ident a, ident b ]
      | (a, b) <- fields
      ]
    )
ppDecl (InterfaceDecl name parents methods) =
  L ( "def-interface"
    : ident name
    : L (map ident parents)
    : map go methods
    ) where go (n, m) = L [ ident n, ppMethod m ]

ppMethod :: MethodDecl -> WellFormedSExpr HaskLikeAtom
ppMethod (StreamDecl name)    = L [ "stream", ident name ]
ppMethod (AttrDecl perm name) = L [ "attr", ppPerm perm, ident name ]
  where ppPerm Read      = "r"
        ppPerm Write     = "w"
        ppPerm ReadWrite = "rw"
