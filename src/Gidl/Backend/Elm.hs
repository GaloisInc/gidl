{-# LANGUAGE OverloadedStrings #-}
module Gidl.Backend.Elm (
    elmBackend
  ) where

import qualified Paths_gidl as P

import Data.Char (isSpace,toUpper)
import Data.List (intercalate,nub)

import Gidl.Interface
    (Interface(..),Method(..),MethodName,interfaceMethods,readable,writable)
import Gidl.Types
    (Atom(..),Type(..),PrimType(..),childTypes,typeLeaves)

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
    sourceMods = tmods ++ imods ++ [elmUtilsModule ns]
    types = nub $ concat [ childTypes t
                         | i <- iis
                         , t <- clientMessageTypes i
                         ]
    tmods = [ typeModule ns t
            | t <- types
            , isUserDefined t
            ]
    imods = [ interfaceModule (ns ++ ["Interface"]) i
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
typeFQElmType _  (PrimType (EnumType "bool_t" _ _)) = "Bool"
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
  [ "type alias" <+> tname <+> equals
  , indent 2 $ encloseStack lbrace rbrace comma
      [ text i <+> colon <+> typeFQElmType mp st
      | (i,st) <- ss ]
  , empty
  , "encode" <+> colon <+> tname <+> "->" <+> "Json.Encode.Value"
  , "encode" <+> text "x" <+> equals
  , indent 2 $ stack
      [ "Json.Encode.object"
      , indent 2 $ encloseStack lbracket rbracket comma
        [ parens (dquotes (text i)
            <> comma <+> typeEncoder mp st <+> "x" <> dot <> (text i))
        | (i,st) <- ss ]
      ]
  , empty
  , "decode" <+> colon <+> "Json.Decode.Decoder" <+> tname
  , "decode" <+> equals <+> tname
  , case ss of
      [] -> empty
      (i0, st0) : ss' -> indent 2 $ stack $
        [ backquotes "Json.Decode.map" <+> parens
            (dquotes (text i0) <+> ":=" <+> typeDecoder mp st0) ] ++
        [ backquotes "Utils.thenMap" <+> parens
            (dquotes (text i) <+> ":=" <+> typeDecoder mp st)
        | (i,st) <- ss' ]
  , empty
  , "init" <+> colon <+> tname
  , "init" <+> equals
  , indent 2 $ encloseStack lbrace rbrace comma
      [ text i <+> equals <+> typeInit mp st
      | (i,st) <- ss ]
  ]
  where
  tname = text (typeModuleName t)

typeDecl mp t@(PrimType (Newtype _ n)) = stack
  [ "type alias" <+> tname <+> equals <+> text (typeElmType (PrimType n))
  , empty
  , "encode" <+> colon <+> tname <+> "->" <+> "Json.Encode.Value"
  , "encode" <+> equals <+> primTypeEncoder mp n
  , empty
  , "decode" <+> colon <+> "Json.Decode.Decoder" <+> tname
  , "decode" <+> equals <+> primTypeDecoder mp n
  , empty
  , "init" <+> colon <+> tname
  , "init" <+> equals <+> primTypeInit mp n
  ]
  where
  tname = text (typeModuleName t)

typeDecl _  t@(PrimType (EnumType _ _ es)) = stack
  [ text "type" <+> tname
  , indent 2 $ encloseStack equals empty "|"
      [ text (userTypeModuleName i)
      | (i, _) <- es ]
  , "encode" <+> colon <+> tname <+> "->" <+> "Json.Encode.Value"
  , "encode" <+> "x" <+> equals
  , indent 2 $ stack [
        "case x of"
      , indent 2 $ stack
          [ text (userTypeModuleName i) <+> "->"
            <+> "Json.Encode.string" <+> dquotes (text (userTypeModuleName i))
          | (i, _) <- es ]
      ]
  , empty
  , "decode" <+> colon <+> "Json.Decode.Decoder" <+> tname
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
              [ "str -> Err (\"unrecognized" <+> tname
                <+> "tag: \" ++ str)" <> rparen
              ]
          ]
      ]
  , empty
  , "init" <+> colon <+> tname
  , "init" <+> equals <+> text (userTypeModuleName firstConstr)
  ]
  where
  tname = text (typeModuleName t)
  firstConstr =
    case es of
      [] -> error ("empty enumeration " ++ typeModuleName t)
      (i, _) : _ -> i

typeDecl _ t = error ("typeDecl: cannot create Elm decl for type " ++ show t)

typeInit :: ModulePath -> Type -> Doc
typeInit mp (PrimType p) = primTypeInit mp p
typeInit mp t =
  typeModuleFQName mp (typeModuleName t) <> dot <> "init"

primTypeInit :: ModulePath -> PrimType -> Doc
primTypeInit mp (Newtype tn _) =
  typeModuleFQName mp (userTypeModuleName tn) <> dot <> "init"
primTypeInit _  (EnumType "bool_t" _ _) = "False"
primTypeInit mp (EnumType tn _ _) =
  typeModuleFQName mp (userTypeModuleName tn) <> dot <> "init"
primTypeInit _  (AtomType (AtomInt _)) = "0"
primTypeInit _  (AtomType (AtomWord _)) = "0"
primTypeInit _  (AtomType AtomFloat) = "0"
primTypeInit _  (AtomType AtomDouble) = "0"

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

interfaceModule :: ModulePath -> Interface -> Artifact
interfaceModule mp i@(Interface rawName _ _) =
  artifactPath (intercalate "/" mp) $
  artifactText ((ifModuleName i) ++ ".elm") $
  prettyLazyText 1000 $
  stack $
    [ "module" <+> im (ifModuleName i) <+> "exposing (..)"
    , empty
    , stack $ typeimports ++ extraimports
    , empty
    , clientIfDecl tmp rawName (ifModuleName i) (clientMessages i)
    ]
  where
  im mname = mconcat $ punctuate dot
                     $ map text (mp ++ [mname])
  tm mname = mconcat $ punctuate dot
                     $ map text (tmp ++ [mname])
  tmp = typepath mp ++ ["Types"]
    where typepath = reverse . drop 1 . reverse

  typeimports = map (\a -> importDecl tm a)
              $ nub
              $ map importType
              $ clientMessageTypes i

  extraimports = [ "import Http"
                 , "import Json.Decode"
                 , "import Json.Encode"
                 , "import Task" ]

data ClientMessage
  = GetMessage String Type
  | SetMessage String Type
  deriving (Eq, Show)

clientMessages :: Interface -> [ClientMessage]
clientMessages i = concatMap f (interfaceMethods i)
  where
  f :: (MethodName, Method) -> [ClientMessage]
  f (_, (StreamMethod _    _)) = [] -- unimplemented
  f (n, (AttrMethod   perm t)) =
    [ SetMessage n t | writable perm ] ++
    [ GetMessage n t | readable perm ]

clientMessageTypes :: Interface -> [Type]
clientMessageTypes i =
     [ t | (GetMessage _ t) <- clientMessages i]
  ++ [ t | (SetMessage _ t) <- clientMessages i]

clientIfDecl :: ModulePath -> String -> String -> [ClientMessage] -> Doc
clientIfDecl _ _ interfaceName [] =
    "-- Cannot define client interface for"
    <+> text interfaceName <+> ": schema is empty"
clientIfDecl tmp rawName interfaceName ms = stack $
    [ "-- Define client interface for" <+> text interfaceName
    , "type alias" <+> "Client" <+> "msg" <+> equals
    , indent 2 $ encloseStack lbrace rbrace comma $
        [ getFieldName n <+> colon <+> "Cmd msg"
        | (GetMessage n _) <- ms
        ] ++
        [ setFieldName n <+> colon
          <+> typeFQElmType tmp t <+> "->" <+> "Cmd msg"
        | (SetMessage n t) <- ms
        ]
    , empty
    , "type" <+> "Response"
    , indent 2 $ encloseStack equals empty (text "|") $
        [ getResponseConstr n <+> typeFQElmType tmp t
        | (GetMessage n t) <- ms ] ++
        [ setResponseConstr n
        | (SetMessage n _) <- ms ]
    , "client" <+> colon
      <+> "(Http.Error -> msg)" <+> "->"
      <+> parens ("Response" <+> "->" <+> "msg") <+> "->"
      <+> "String" <+> "->"
      <+> "Client" <+> "msg"
    , "client" <+> "err" <+> "ok" <+> "url" <+> equals
    , indent 2 $ encloseStack lbrace rbrace comma $
        [ getFieldName n <+> equals
          <+> "Task.perform" <+> "err"
          <+> parens ("ok" <+> "<<" <+> getResponseConstr n)
          <+> parens ("Http.get" <+> typeDecoder tmp t
            <+> parens ("url" <+> "++"
              <+> dquotes ("/" <> text rawName <> "/" <> text n)))
        | (GetMessage n t) <- ms ] ++
        [ setFieldName n <+> equals <+> "\\x ->"
          <+> "Task.perform" <+> "err"
          <+> parens ("ok" <+> "<<" <+> "always" <+> setResponseConstr n)
          <+> parens ("Http.post" <+> parens ("Json.Decode.succeed" <+> "()")
            <+> parens ("url" <+> "++"
              <+> dquotes ("/" <> text rawName <> "/" <> text n))
            <+> parens ("Http.string"
              <+> parens ("Json.Encode.encode 1000"
                <+> parens (typeEncoder tmp t <+> "x"))))
        | (SetMessage n t) <- ms
        ]
    , empty
    , "type alias" <+> "Handler"
      <+> "model" <+> "msg" <+> equals
    , indent 2 $ encloseStack lbrace rbrace comma $
        [ handleGet n <+> colon
          <+> typeFQElmType tmp t <+> "-> model -> (model, Cmd msg)"
        | (GetMessage n t) <- ms ] ++
        [ handleSet n <+> colon <+> "model -> (model, Cmd msg)"
        | (SetMessage n _) <- ms ]
    , empty
    , "defaultHandler" <+> colon <+> "Handler" <+> "model" <+> "msg"
    , "defaultHandler" <+> equals
    , indent 2 $ encloseStack lbrace rbrace comma $
        [ handleGet n <+> equals <+> "\\_ m -> (m, Cmd.none)"
        | (GetMessage n _) <- ms ] ++
        [ handleSet n <+> equals <+> "\\m -> (m, Cmd.none)"
        | (SetMessage n _) <- ms ]
    , empty
    , "handle : Handler model msg -> Response -> model -> (model, Cmd msg)"
    , "handle" <+> "h" <+> "r" <+> "m" <+> equals
    , indent 2 $ stack $
        [ "case r of"
        , indent 2 $ stack $
            [ getResponseConstr n <+> "x" <+> "->"
              <+> "h" <> dot <> handleGet n <+> "x" <+> "m"
            | (GetMessage n _) <- ms ] ++
            [ setResponseConstr n <+> "->"
              <+> "h" <> dot <> handleSet n <+> "m"
            | (SetMessage n _) <- ms
            ]
        ]
    ]
  where
  handleGet n = "handleGot" <> text (userTypeModuleName n)
  handleSet n = "handleSet" <> text (userTypeModuleName n)
  getResponseConstr n = "Got" <> text (userTypeModuleName n)
  setResponseConstr n = "Set" <> text (userTypeModuleName n)
  getFieldName n = "get" <> text (userTypeModuleName n)
  setFieldName n = "set" <> text (userTypeModuleName n)

ifModuleName :: Interface -> String
ifModuleName (Interface iname _ _) = aux iname
  where
  aux :: String -> String
  aux = first_cap . u_to_camel
  first_cap (s:ss) = (toUpper s) : ss
  first_cap []     = []
  u_to_camel ('_':'i':[]) = []
  u_to_camel ('_':[]) = []
  u_to_camel ('_':a:as) = (toUpper a) : u_to_camel as
  u_to_camel (a:as) = a : u_to_camel as
  u_to_camel [] = []

-- Utilities -------------------------------------------------------------------

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
