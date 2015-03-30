
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

interfaceModule :: [String] -> InterfaceRepr -> Artifact
interfaceModule modulepath ir =
  artifactPath (intercalate "/" modulepath) $
  artifactText ((ifModuleName ir) ++ ".hs") $
  prettyLazyText 80 $
  stack
    [ text "{-# LANGUAGE DeriveDataTypeable #-}"
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
  extraimports = [ text "import Data.Serialize"
                 , text "import Data.Typeable"
                 , text "import Data.Data"
                 , text "import qualified Test.QuickCheck as Q" ]

schemaDoc :: String -> Schema -> Doc
schemaDoc interfaceName (Schema schemaName [])     =
    text "-- Cannot define" <+> text schemaName  <+> text "schema for"
        <+> text interfaceName <+> text "interface: schema is empty"
schemaDoc interfaceName (Schema schemaName schema) = stack
    [ text "-- Define" <+> text schemaName  <+> text "schema for"
        <+> text interfaceName <+> text "interface"
    , text "data" <+> text typeName
    , indent 2 $ encloseStack equals deriv (text "|")
        [ text (constructorName n) <+> text (typeIvoryType t)
        | (_, (Message n t)) <- schema
        ]
    ]
  where
  constructorName n = userTypeModuleName n ++ schemaName
  deriv = text "deriving (Eq, Show, Data, Typeable)"
  typeName = interfaceName ++ schemaName

ifModuleName :: InterfaceRepr -> String
ifModuleName (InterfaceRepr iname _) = aux iname
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


