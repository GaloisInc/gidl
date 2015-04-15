{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Gidl.Parse (parseDecls) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad ((>=>), guard)
import           Data.List (nub, partition)
import           Data.SCargot.Comments (withHaskellComments)
import           Data.SCargot.General ( SExprSpec
                                      , convertSpec
                                      , decode
                                      , asWellFormed
                                      )
import           Data.SCargot.HaskLike (HaskLikeAtom(..), haskLikeSpec)
import           Data.SCargot.Repr.WellFormed
import           Data.Text (unpack, pack)

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
  | StreamDecl Integer Identifier
    deriving (Eq, Show)

check :: Bool -> String -> Either String ()
check True _ = return ()
check _ msg  = throw msg

-- Here's a function to convert those decls.
toEnv :: [Decl] -> Either String (TypeEnv, InterfaceEnv)
toEnv decls = do
  check (unique typeNames) "duplicate type names"
  check (unique interfaceNames) "duplicate interface names"
  typs <- mapM getTypePair typeNames
  ifcs <- mapM getIfacePair interfaceNames
  return (TypeEnv typs, InterfaceEnv ifcs)
  where (typDs, ifcDs) = partition isTypeDecl decls

        builtins = let TypeEnv bs = baseTypeEnv in bs

        typeNames = map fst builtins ++ map getName typDs
        interfaceNames = map getName ifcDs

        typMap = [(getName d, toType d) | d <- typDs] ++
                 [(n, return (n, t)) | (n, t) <- builtins ]
        ifcMap = [(getName i, toInterface i) | i <- ifcDs]

        -- this is gross because I'm trying to make sure declarations
        -- can happen in any order. XXX: prevent recursion!
        getType n = snd `fmap` getTypePair n
        getTypePair n = case lookup n typMap of
            Just (Right t) -> return t
            Just (Left l)  -> Left l
            Nothing        -> throw ("Unknown primitive type: " ++ n)

        getIface n = snd `fmap` getIfacePair n
        getIfacePair n = case lookup n ifcMap of
          Just (Right i) -> return i
          Just (Left l)  -> Left l
          Nothing        -> throw ("Unknown interface: " ++ n)

        getPrimType n = do
          t <- getType n
          case t of
            PrimType t' -> return t'
            _ -> throw ("Expected primitive type but got " ++ show t)

        -- converts a decl to an actual type
        toType (NewtypeDecl n t) = do
          t' <- getPrimType t
          return (n, PrimType (Newtype n t'))
        toType (EnumDecl (n, s) ts) = do
          guard (unique (map fst ts)) /?/ "baz"
          return (n, PrimType (EnumType n s ts))
        toType (StructDecl n ss) = do
          ps <- mapM (getPrimType . snd) ss
          return (n, StructType n (zip (map fst ss) ps))
        toType _ = error "[unreachable]"

        toMethod (n, AttrDecl perm t) = do
          t' <- getType t
          return (n, AttrMethod perm t')
        toMethod (n, StreamDecl rate t) = do
          t' <- getType t
          return (n, StreamMethod rate t')

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

        unique l = nub l == l


parseDecls :: String -> Either String (TypeEnv, InterfaceEnv)
parseDecls = return . pack >=> decode gidlSpec >=> toEnv

gidlSpec :: SExprSpec HaskLikeAtom Decl
gidlSpec
  = withHaskellComments
  $ convertSpec tDecl ppDecl
  $ asWellFormed haskLikeSpec

-- utility aliases and helpers
type Parse a = WellFormedSExpr HaskLikeAtom -> Either String a

throw :: String -> Either String a
throw = Left

infix 9 /?/
(/?/) :: Either String a -> String -> Either String a
Left msg /?/ ctx = throw (msg ++ "\n  in parsing " ++ ctx)
r        /?/ _   = r

-- basic parsing of things (these might show up in s-cargot
--  proper eventually?)
tSymbol :: Parse String
tSymbol = asAtom go
  where go (HSIdent i) = return (unpack i)
        go sx          = throw ("Expected identifier; got " ++ show sx)

tType :: Parse String
tType = tSymbol

tInteger :: Parse Integer
tInteger = asAtom go
  where go (HSInt n) = return n
        go sx        = throw ("Expected integer; got " ++ show sx)

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
tPermission = asAtom go
  where go token
          | token == "read"      || token == "r"  = return Read
          | token == "write"     || token == "w"  = return Write
          | token == "readwrite" || token == "rw" = return ReadWrite
          | otherwise = throw ("unknown permission: " ++ show token)

-- newtypes
tNewtypeDecl :: Parse Decl
tNewtypeDecl = asList $ \ls -> do
  car (isAtom "def-newtype") ls
  name <- cdr (car tSymbol) ls       /?/ "newtype name"
  typ  <- cdr (cdr (car tSymbol)) ls /?/ "newtype type"
  return (NewtypeDecl name typ)

-- structs
tStructDecl :: Parse Decl
tStructDecl = asList go
  where go ("def-struct":name:body) =
            StructDecl <$> (tSymbol name         /?/ "structure name")
                       <*> (mapM tStructRow body /?/ "structure body")
        go _ = throw "invalid struct decl"

tStructRow :: Parse (Identifier, Identifier)
tStructRow = fromPair tSymbol tType

-- enums
tEnumDecl :: Parse Decl
tEnumDecl = asList go
  where go ("def-enum" : name : body) =
          EnumDecl <$> tEnumName name      /?/ "enum name"
                   <*> mapM tEnumBody body /?/ "enum body"
        go _ = throw "invalid enum decl"

tEnumName :: Parse (Identifier, Bits)
tEnumName (L [name, size]) = (,) <$> tSymbol name <*> tBits size
tEnumName name             = (,) <$> tSymbol name <*> return Bits32

tEnumBody :: Parse (Identifier, Integer)
tEnumBody = fromPair tSymbol tInteger

-- interface declarations
tInterfaceDecl :: Parse Decl
tInterfaceDecl = asList go
  where go ("def-interface":name:parents:body) =
          InterfaceDecl
            <$> tSymbol name                  /?/ "interface name"
            <*> asList (mapM tSymbol) parents /?/ "interface supers"
            <*> mapM tInterfaceMethod body    /?/ "interface methods"
        go _ = throw "invalid interface decl"

tInterfaceMethod :: Parse (Identifier, MethodDecl)
tInterfaceMethod = fromPair tSymbol (asList go)
  where go ["attr",   p, t] = AttrDecl   <$> tPermission p <*> tType t
        go ["stream", n, t] = StreamDecl <$> tInteger n    <*> tType t
        go (x:_) = throw ("unknown interface type: " ++ show x)
        go []    = throw "empty interface decl"

-- declarations in general
tDecl :: Parse Decl
tDecl ls@(L ("def-struct"    : _)) = tStructDecl ls    /?/ "struct"
tDecl ls@(L ("def-newtype"   : _)) = tNewtypeDecl ls   /?/ "newtype"
tDecl ls@(L ("def-enum"      : _)) = tEnumDecl ls      /?/ "enum"
tDecl ls@(L ("def-interface" : _)) = tInterfaceDecl ls /?/ "interface"
tDecl (L (A name : _)) =
  throw ("unknown declaration type: " ++ show name)
tDecl item = do
  throw ("invalid top-level item " ++ show item)

-- For now, no pretty-printing (but it will come soon!)
ppDecl :: Decl -> WellFormedSExpr HaskLikeAtom
ppDecl = const "<unimplemented>"
