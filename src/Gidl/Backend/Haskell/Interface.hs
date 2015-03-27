
module Gidl.Backend.Haskell.Interface where


import Data.Monoid
import Data.List (intercalate, nub)
import Data.Char (toUpper)

import Gidl.Types
import Gidl.Interface
import Gidl.Schema
import Gidl.Backend.Haskell.Types
import Ivory.Artifact
import Text.PrettyPrint.Mainland

interfaceModule :: [String] -> InterfaceRepr -> Artifact
interfaceModule modulepath ir@(InterfaceRepr _ i) =
  artifactPath (intercalate "/" modulepath) $
  artifactText ((ifModuleName ir) ++ ".hs") $
  prettyLazyText 80 $
  stack
    [ text "module"
      <+> im (ifModuleName ir)
      <+> text "where"
    , empty
    , stack $ typeimports ++ extraimports
    , empty
    , schemaDoc (ifModuleName ir) "Producer" (producerSchema ir)
    , empty
    , schemaDoc (ifModuleName ir) "Consumer" (consumerSchema ir)
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
  extraimports = [ text "import Data.Serialize" ]

schemaDoc :: String -> String -> Schema -> Doc
schemaDoc interfaceName schemaName (Schema [])     =
    text "-- Cannot define" <+> text schemaName  <+> text "schema for"
        <+> text interfaceName <+> text "interface: schema is empty"
schemaDoc interfaceName schemaName (Schema schema) = stack
    [ text "-- Define" <+> text schemaName  <+> text "schema for"
        <+> text interfaceName <+> text "interface"
    , text "data" <+> text typeName
    , indent 2 $ encloseStack equals deriv (text "|")
        [ text (constructorName n) <+> text (typeHaskellType t)
        | (_, (Message n t)) <- schema
        ]
    , empty
    , text ("put" ++ typeName) <+> colon <> colon <+> text "Putter" <+> text typeName
    , stack
        [ text ("put" ++ typeName)
            <+> parens (text (constructorName n) <+> text "m")
            <+> equals
            <+> text "put" <> text (cerealSize Bits32) <+> ppr h <+> text ">>"
            <+> text "put" <+> text "m"
        | (h, Message n _) <- schema ]

    , text ("get" ++ typeName) <+> colon <> colon <+> text "Get" <+> text typeName
    , text ("get" ++ typeName) <+> equals <+> text "do"
    , indent 2 $ stack
        [ text "a" <+> text "<- get" <> text (cerealSize Bits32)
        , text "case a of"
        , indent 2 $ stack $
            [ ppr h <+> text "-> do" </> (indent 2 (stack
                [ text "m <- get"
                , text "return" <+> parens (text (constructorName n) <+> text "m")
                ]))
            | (h,Message n _) <- schema
            ] ++
            [ text "_ -> fail"
              <+> dquotes (text "encountered unknown tag in get" <> text typeName)
            ]
        ]
    , empty
    , serializeInstance typeName
    ]
  where
  constructorName n = userTypeModuleName n ++ schemaName
  deriv = text "deriving (Eq, Show)"
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


