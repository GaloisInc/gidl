{-# LANGUAGE OverloadedStrings #-}
module Gidl.Backend.Elm.Common where

import Data.Char (isSpace,toUpper)

import Gidl.Types
    (Atom(..),Type(..),PrimType(..))

import Prelude ()
import Prelude.Compat

import Text.PrettyPrint.Mainland

type Namespace = [String]
type ModulePath = [String]

-- | Convert @some_name@ to @someName@, dropping any trailing @_t@
toCamel :: String -> String
toCamel ('_':'t':[]) = []
toCamel ('_':[]) = []
toCamel ('_':a:as) = (toUpper a) : toCamel as
toCamel (a:as) = a : toCamel as
toCamel [] = []

-- | Convert @some_name@ to @SomeName@, dropping any trailing @_t@
toCapped :: String -> String
toCapped = first_cap . toCamel
  where
  first_cap (s:ss) = (toUpper s) : ss
  first_cap []     = []

-- | Get the 'toCapped' name of a 'Type'
cappedName :: Type -> String
cappedName (StructType tn _) = toCapped tn
cappedName (PrimType (Newtype tn _)) = toCapped tn
cappedName (PrimType (EnumType tn _ _)) = toCapped tn
cappedName (PrimType (AtomType _)) =
  error "do not take cappedName of an AtomType"

-- | Given a 'ModulePath' and a final component, format a qualified
-- name. Also useful for building module names themselves.
mkQName :: ModulePath -> String -> Doc
mkQName modulepath mname =
  mconcat $ punctuate dot $ map text (modulepath ++ [mname])

-- | Given a 'ModulePath' for the @Types@ module and a 'Type', give
-- the qualified name that refers to that type. For built-in types
-- like @Bool@, the name might not be qualified at all.
elmTypeQName :: ModulePath -> Type -> Doc
elmTypeQName mp (StructType tn _) =
  mkQName mp (toCapped tn)
  <> dot <> text (toCapped tn)
elmTypeQName mp (PrimType (Newtype tn _)) =
  mkQName mp (toCapped tn)
  <> dot <> text (toCapped tn)
elmTypeQName _  (PrimType (EnumType "bool_t" _ _)) = "Bool"
elmTypeQName mp (PrimType (EnumType tn _ _)) =
  mkQName mp (toCapped tn)
  <> dot <> text (toCapped tn)
elmTypeQName _  (PrimType (AtomType a)) = case a of
  AtomInt _  -> "Int"
  AtomWord _ -> "Int"
  AtomFloat  -> "Float"
  AtomDouble -> "Float"

data ImportType = UserType String
                | NoImport
                deriving (Eq, Show)

importType :: Type -> ImportType
importType (StructType n _) = UserType n
importType (PrimType (EnumType "bool_t" _ _)) = NoImport
importType (PrimType (EnumType n _ _)) = UserType n
importType (PrimType (Newtype n _)) = UserType n
importType (PrimType (AtomType _)) = NoImport

importDecl :: (String -> Doc) -> ImportType -> Doc
importDecl mkpath (UserType t) =
  text "import" <+> mkpath (toCapped t)
importDecl _ NoImport = empty

isUserDefined :: Type -> Bool
isUserDefined tr = case importType tr of
  UserType _ -> True
  _ -> False

-- | Get the name of the @encode@ function for a type
typeEncoder :: ModulePath -> Type -> Doc
typeEncoder mp (PrimType p) = primTypeEncoder mp p
typeEncoder mp t =
  mkQName mp (cappedName t) <> dot <> "encode"

primTypeEncoder :: ModulePath -> PrimType -> Doc
primTypeEncoder mp (Newtype tn _) =
  mkQName mp (toCapped tn) <> dot <> "encode"
primTypeEncoder _  (EnumType "bool_t" _ _) = "Json.Encode.bool"
primTypeEncoder mp (EnumType tn _ _) =
  mkQName mp (toCapped tn) <> dot <> "encode"
primTypeEncoder _  (AtomType (AtomInt _)) = "Json.Encode.int"
primTypeEncoder _  (AtomType (AtomWord _)) = "Json.Encode.int"
primTypeEncoder _  (AtomType AtomFloat) = "Json.Encode.float"
primTypeEncoder _  (AtomType AtomDouble) = "Json.Encode.float"

-- | Get the name of the @decode@ function for a type
typeDecoder :: ModulePath -> Type -> Doc
typeDecoder mp (PrimType p) = primTypeDecoder mp p
typeDecoder mp t =
  mkQName mp (cappedName t) <> dot <> "decode"

primTypeDecoder :: ModulePath -> PrimType -> Doc
primTypeDecoder mp (Newtype tn _) =
  mkQName mp (toCapped tn) <> dot <> "decode"
primTypeDecoder _  (EnumType "bool_t" _ _) = "Json.Decode.bool"
primTypeDecoder mp (EnumType tn _ _) =
  mkQName mp (toCapped tn) <> dot <> "decode"
primTypeDecoder _  (AtomType (AtomInt _)) = "Json.Decode.int"
primTypeDecoder _  (AtomType (AtomWord _)) = "Json.Decode.int"
primTypeDecoder _  (AtomType AtomFloat) = "Json.Decode.float"
primTypeDecoder _  (AtomType AtomDouble) = "Json.Decode.float"

-- | Look up the @init@ name for a 'Type'
typeInit :: ModulePath -> Type -> Doc
typeInit mp (PrimType p) = primTypeInit mp p
typeInit mp t =
  mkQName mp (cappedName t) <> dot <> "init"

primTypeInit :: ModulePath -> PrimType -> Doc
primTypeInit mp (Newtype tn _) =
  mkQName mp (toCapped tn) <> dot <> "init"
primTypeInit _  (EnumType "bool_t" _ _) = "False"
primTypeInit mp (EnumType tn _ _) =
  mkQName mp (toCapped tn) <> dot <> "init"
primTypeInit _  (AtomType (AtomInt _)) = "0"
primTypeInit _  (AtomType (AtomWord _)) = "0"
primTypeInit _  (AtomType AtomFloat) = "0"
primTypeInit _  (AtomType AtomDouble) = "0"

-- | Splits up a dot-separated string into components
strToNs :: String -> [String]
strToNs str =
  case break (== '.') (dropWhile isSpace str) of

    (a,'.' : b) | null a    ->          strToNs b
                | otherwise -> trim a : strToNs b

    (a,_)       | null a    -> []
                | otherwise -> [trim a]

  where
  trim = takeWhile (not . isSpace)

-- | Stack some 'Doc's between @l@ and @r@, beginning each line after
-- the first with @p@
encloseStack :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseStack l r p ds = case ds of
  [] -> empty
  [d] -> l <+> d </> r
  _ -> align (l <+> (folddoc (\a b -> a </> p <+> b) ds) </> r)
