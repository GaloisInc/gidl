
module Gidl.Backend.Ivory.Interface where


import Data.Monoid
import Data.List (intercalate, nub)
import Data.Char (toUpper)

import Gidl.Types
import Gidl.Interface
import Gidl.Schema
import Gidl.Backend.Ivory.Types
import Ivory.Artifact
import Text.PrettyPrint.Mainland

interfaceModule :: [String] -> Interface -> Artifact
interfaceModule modulepath ir =
  artifactPath (intercalate "/" modulepath) $
  artifactText ((ifModuleName ir) ++ ".hs") $
  prettyLazyText 80 $
  stack
    [ text "{-# LANGUAGE DataKinds #-}"
    , text "{-# LANGUAGE RankNTypes #-}"
    , empty
    , text "module"
      <+> im (ifModuleName ir)
      <+> text "where"
    , empty
    , stack $ typeimports ++ extraimports
    , empty
    , schemaDoc (ifModuleName ir) (producerSchema ir)
    , empty
    , schemaDoc (ifModuleName ir) (consumerSchema ir)
    ]
  where
  im mname = mconcat $ punctuate dot
                     $ map text (modulepath ++ [mname])
  tm mname = mconcat $ punctuate dot
                     $ map text (typepath modulepath ++ ["Types", mname])
    where typepath = reverse . drop 1 . reverse

  typeimports = map (importDecl tm)
              $ nub
              $ map importType
              $ interfaceTypes ir
  extraimports = [ text "import Ivory.Language"
                 , text "import Ivory.Serialize"
                 ]

schemaDoc :: String -> Schema -> Doc
schemaDoc interfaceName (Schema schemaName [])     =
    text "-- Cannot define" <+> text schemaName  <+> text "schema for"
        <+> text interfaceName <+> text "interface: schema is empty"
schemaDoc interfaceName (Schema schemaName schema) = stack
    [ text "-- Define" <+> text schemaName  <+> text "schema for"
        <+> text interfaceName <+> text "interface"
    , text "data" <+> text typeName <> text "Handler"
    , indent 2 $ encloseStack equals empty (text "|")
        [ case t of
            PrimType VoidType -> text (handlerName n)
                <+> text "(forall eff . Ivory eff ())"
            _ -> text (handlerName n)
                  <+> parens (text "forall s eff . ConstRef s" 
                    <+> parens (text (typeIvoryType t))
                    <+> text "-> Ivory eff ()")
        | (_, (Message n t)) <- schema
        ]
    , text "data" <+> senderConstructor <+> equals <+> senderConstructor
    , indent 2 $ encloseStack lbrace rbrace comma
        [ case t of
            PrimType VoidType -> text (senderName n) <+> colon <> colon
                <+> text "(forall eff . Ivory eff ())"
            _ -> text (senderName n) <+> colon <> colon
                  <+> parens (text "forall s eff . ConstRef s" 
                    <+> parens (text (typeIvoryType t))
                    <+> text "-> Ivory eff ()")
        | (_, (Message n t)) <- schema
        ]
    ]
  where
  senderConstructor = text typeName <> text "Sender" 
  handlerName n = userTypeModuleName n ++ schemaName ++ "Handler"
  senderName n = userEnumValueName n ++ schemaName ++ "Sender"
  typeName = interfaceName ++ schemaName

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


