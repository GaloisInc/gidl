{-# LANGUAGE OverloadedStrings #-}
module Gidl.Backend.Elm (
    elmBackend
  ) where

import qualified Paths_gidl as P

import Data.Char (isSpace,toUpper)
import Data.List (intercalate,nub)

import Gidl.Interface
    (Interface(..))
import Gidl.Schema (interfaceTypes)
import Gidl.Types (Atom(..),Type(..), PrimType(..),typeLeaves)

import Ivory.Artifact
    (Artifact,artifactPath,artifactText)
import Ivory.Artifact.Template (artifactCabalFileTemplate)

import Text.PrettyPrint.Mainland
--    ((<+>),dot,empty,prettyLazyText,punctuate,stack,text)

type Namespace = [String]

elmBackend :: [Interface] -> String -> String -> [Artifact]
elmBackend iis _pkgName nsStr = map (artifactPath "src") sourceMods
  where
    ns = strToNs nsStr
    sourceMods = tmods ++ {-imods ++-} [elmUtilsModule ns]
    types = nub [ t | i <- iis, t <- interfaceTypes i ]
    tmods = [ typeModule ns t
            | t <- types
            , isUserDefined t
            ]
    imods = concat [ [ interfaceModule (ns ++ ["Interface"]) i
                     , elmModule ns i ]
                   | i <- iis
                   ]

elmUtilsModule :: Namespace -> Artifact
elmUtilsModule ns =
  artifactPath (foldr1 (\ p rest -> p ++ "/" ++ rest) ns) $
  artifactCabalFileTemplate P.getDataDir "support/elm/Utils.elm.template" env
  where
  env = [ ("module_path", foldr1 (\p rest -> p ++ "." ++ rest) ns) ]

-- Types -----------------------------------------------------------------------

type ModulePath = [String]

-- invariant: only make a typeModule from a StructType, NewtypeType, or EnumType
-- i.e. when isUserDefined is true.
typeModule :: Namespace -> Type -> Artifact
typeModule ns t =
  artifactPath (intercalate "/" mp) $
  artifactText ((typeModuleName t) ++ ".elm") $
  prettyLazyText 1000 $
  stack $
    [ "module" <+> tm (typeModuleName t) <+> "exposing (..)"
    , empty
    , stack (imports ++ [
                 "import"
                 <+> text (foldr (\p rest -> p ++ "." ++ rest) "Utils" ns)
                 <+> "as Utils"
               , "import Json.Decode exposing ((:=))"
               , "import Json.Encode"
               ])
    , empty
    , typeDecl mp t
    ]
  where
    imports = map (importDecl tm) $ nub $ map importType $ typeLeaves t
    tm = typeModuleFQName mp
    mp = ns ++ ["Types"]

typeModuleFQName :: ModulePath -> String -> Doc
typeModuleFQName modulepath mname =
  mconcat $ punctuate dot $ map text (modulepath ++ [mname])

importDecl :: (String -> Doc) -> ImportType -> Doc
importDecl _ (LibraryType p) =
  text "import" <+> text p
importDecl mkpath (UserType t) =
  text "import" <+> mkpath (userTypeModuleName t)
importDecl _ NoImport = empty

typeElmType :: Type -> String
typeElmType (StructType tn _) = userTypeModuleName tn
typeElmType (PrimType (Newtype tn _)) = userTypeModuleName tn
typeElmType (PrimType (EnumType tn _ _)) = userTypeModuleName tn
typeElmType (PrimType (AtomType a)) = case a of
  AtomInt _  -> "Int"
  AtomWord _ -> "Int"
  AtomFloat  -> "Float"
  AtomDouble -> "Float"

typeFQElmType :: ModulePath -> Type -> Doc
typeFQElmType mp (StructType tn _) =
  typeModuleFQName mp (userTypeModuleName tn)
  <> dot <> text (userTypeModuleName tn)
typeFQElmType mp (PrimType (Newtype tn _)) =
  typeModuleFQName mp (userTypeModuleName tn)
  <> dot <> text (userTypeModuleName tn)
typeFQElmType mp (PrimType (EnumType "bool_t" _ _)) = "Bool"
typeFQElmType mp (PrimType (EnumType tn _ _)) =
  typeModuleFQName mp (userTypeModuleName tn)
  <> dot <> text (userTypeModuleName tn)
typeFQElmType _  (PrimType (AtomType a)) = case a of
  AtomInt _  -> "Int"
  AtomWord _ -> "Int"
  AtomFloat  -> "Float"
  AtomDouble -> "Float"

typeModuleName :: Type -> String
typeModuleName (StructType tn _) = userTypeModuleName tn
typeModuleName (PrimType (Newtype tn _)) = userTypeModuleName tn
typeModuleName (PrimType (EnumType tn _ _)) = userTypeModuleName tn
typeModuleName (PrimType (AtomType _)) = error "do not take typeModuleName of an AtomType"

userTypeModuleName :: String -> String
userTypeModuleName = first_cap . u_to_camel
  where
  first_cap (s:ss) = (toUpper s) : ss
  first_cap []     = []
  u_to_camel ('_':'t':[]) = []
  u_to_camel ('_':[]) = []
  u_to_camel ('_':a:as) = (toUpper a) : u_to_camel as
  u_to_camel (a:as) = a : u_to_camel as
  u_to_camel [] = []

typeDecl :: ModulePath -> Type -> Doc
typeDecl mp t@(StructType _ ss) = stack
  [ "type alias" <+> text tname <+> equals
  , indent 4 $ encloseStack lbrace rbrace comma
      [ text i <+> colon <+> typeFQElmType mp st
      | (i,st) <- ss ]
  , empty
  , "encode" <+> colon <+> text tname <+> "->" <+> "Json.Encode.Value"
  , "encode" <+> text "x" <+> equals
  , indent 2 $ stack
      [ "Json.Encode.object"
      , indent 2 $ encloseStack lbracket rbracket comma
        [ parens (dquotes (text i)
            <> comma <+> typeEncoder mp st <+> "x" <> dot <> (text i))
        | (i,st) <- ss ]
      ]
  , empty
  , "decode" <+> colon <+> "Json.Decode.Decoder" <+> text tname
  , "decode" <+> equals <+> text tname
  , case ss of
      [] -> empty
      (i0, st0) : ss' -> indent 2 $ stack $
        [ backquotes "Json.Decode.map" <+> parens
            (dquotes (text i0) <+> ":=" <+> typeDecoder mp st0) ] ++
        [ backquotes "Utils.thenMap" <+> parens
            (dquotes (text i) <+> ":=" <+> typeDecoder mp st)
        | (i,st) <- ss' ]
  ]
  where
  tname = typeModuleName t

typeDecl mp t@(PrimType (Newtype _ n)) = stack
  [ "type alias" <+> text tname <+> equals <+> text (typeElmType (PrimType n))
  , empty
  , "encode" <+> colon <+> text tname <+> "->" <+> "Json.Encode.Value"
  , "encode" <+> equals <+> primTypeEncoder mp n
  , empty
  , "decode" <+> colon <+> "Json.Decode.Decoder" <+> text tname
  , "decode" <+> equals <+> primTypeDecoder mp n
  ]
  where
  tname = typeModuleName t

typeDecl _  t@(PrimType (EnumType _ _ es)) = stack
  [ text "type" <+> text tname
  , indent 2 $ encloseStack equals empty (text "|")
      [ text (userTypeModuleName i)
      | (i, _) <- es ]
  , "encode" <+> colon <+> text tname <+> "->" <+> "Json.Encode.Value"
  , "encode" <+> "x" <+> equals
  , indent 2 $ stack [
        "case x of"
      , indent 2 $ stack
          [ text (userTypeModuleName i) <+> "->"
            <+> "Json.Encode.string" <+> dquotes (text (userTypeModuleName i))
          | (i, _) <- es ]
      ]
  , empty
  , "decode" <+> colon <+> "Json.Decode.Decoder" <+> text tname
  , "decode" <+> equals
  , indent 2 $ stack [
        "Json.Decode.customDecoder" <+> "Json.Decode.string"
        <+> lparen <> "\\tag ->"
      , indent 2 $ stack [
            "case tag of"
          , indent 2 $ stack $
              [ dquotes (text (userTypeModuleName i)) <+> "->"
                <+> "Ok" <+> text (userTypeModuleName i)
              | (i, _) <- es ] ++
              [ "str -> Err (\"unrecognized" <+> text tname
                <+> "tag: \" ++ str)" <> rparen
              ]
          ]
      ]
  ]
  where
  tname = typeModuleName t

typeDecl _ t = error ("typeDecl: cannot create Elm decl for type " ++ show t)

typeEncoder :: ModulePath -> Type -> Doc
typeEncoder mp (PrimType p) = primTypeEncoder mp p
typeEncoder mp t =
  typeModuleFQName mp (typeModuleName t) <> dot <> "encode"

primTypeEncoder :: ModulePath -> PrimType -> Doc
primTypeEncoder mp (Newtype tn _) =
  typeModuleFQName mp (userTypeModuleName tn) <> dot <> "encode"
primTypeEncoder _  (EnumType "bool_t" _ _) = "Json.Encode.bool"
primTypeEncoder mp (EnumType tn _ _) =
  typeModuleFQName mp (userTypeModuleName tn) <> dot <> "encode"
primTypeEncoder _  (AtomType (AtomInt _)) = "Json.Encode.int"
primTypeEncoder _  (AtomType (AtomWord _)) = "Json.Encode.int"
primTypeEncoder _  (AtomType AtomFloat) = "Json.Encode.float"
primTypeEncoder _  (AtomType AtomDouble) = "Json.Encode.float"

typeDecoder :: ModulePath -> Type -> Doc
typeDecoder mp (PrimType p) = primTypeDecoder mp p
typeDecoder mp t =
  typeModuleFQName mp (typeModuleName t) <> dot <> "decode"

primTypeDecoder :: ModulePath -> PrimType -> Doc
primTypeDecoder mp (Newtype tn _) =
  typeModuleFQName mp (userTypeModuleName tn) <> dot <> "decode"
primTypeDecoder _  (EnumType "bool_t" _ _) = "Json.Decode.bool"
primTypeDecoder mp (EnumType tn _ _) =
  typeModuleFQName mp (userTypeModuleName tn) <> dot <> "decode"
primTypeDecoder _  (AtomType (AtomInt _)) = "Json.Decode.int"
primTypeDecoder _  (AtomType (AtomWord _)) = "Json.Decode.int"
primTypeDecoder _  (AtomType AtomFloat) = "Json.Decode.float"
primTypeDecoder _  (AtomType AtomDouble) = "Json.Decode.float"

-- Interface -------------------------------------------------------------------
interfaceModule :: [String] -> Interface -> Artifact
interfaceModule = undefined

elmModule :: [String] -> Interface -> Artifact
elmModule = undefined

data ImportType = LibraryType String
                | UserType String
                | NoImport
                deriving (Eq, Show)

importType :: Type -> ImportType
importType (StructType n _) = UserType n
importType (PrimType (EnumType "bool_t" _ _)) = NoImport
importType (PrimType (EnumType n _ _)) = UserType n
importType (PrimType (Newtype n _)) = UserType n
importType (PrimType (AtomType _)) = NoImport

isUserDefined :: Type -> Bool
isUserDefined tr = case importType tr of
  UserType _ -> True
  _ -> False

-- Utilities -------------------------------------------------------------------

strToNs :: String -> [String]
strToNs str =
  case break (== '.') (dropWhile isSpace str) of

    (a,'.' : b) | null a    ->          strToNs b
                | otherwise -> trim a : strToNs b

    (a,_)       | null a    -> []
                | otherwise -> [trim a]

  where
  trim = takeWhile (not . isSpace)

encloseStack :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseStack l r p ds = case ds of
  [] -> empty -- l </> r
  [d] -> l <+> d </> r
  _ -> align (l <+> (folddoc (\a b -> a </> p <+> b) ds) </> r)

