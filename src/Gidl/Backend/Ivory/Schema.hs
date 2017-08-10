{-# LANGUAGE CPP #-}

module Gidl.Backend.Ivory.Schema where

import Data.Monoid
import Data.List (intercalate, nub)
import Data.Char (toUpper)

import Gidl.Interface
import Gidl.Schema
import Gidl.Backend.Ivory.Types
import Ivory.Artifact
#if MIN_VERSION_mainland_pretty(0,6,0)
import Text.PrettyPrint.Mainland.Class
#endif
import Text.PrettyPrint.Mainland

schemaModule :: [String] -> Interface -> Schema -> Artifact
schemaModule modulepath ir schema =
  artifactPath (intercalate "/" (modulepath ++ [ifModuleName ir])) $
  artifactText (schemaName ++ ".hs") $
  prettyLazyText 1000 $
  stack
    [ text "{-# LANGUAGE DataKinds #-}"
    , text "{-# LANGUAGE RankNTypes #-}"
    , text "{-# LANGUAGE ScopedTypeVariables #-}"
    , text "{-# OPTIONS_GHC -fno-warn-unused-imports #-}"
    , empty
    , text "module"
      <+> im (ifModuleName ir) <> dot <> text schemaName
      <+> text "where"
    , empty
    , stack $ typeimports ++ extraimports
    , empty
    , schemaDoc (ifModuleName ir) schema
    ]
  where
  (Schema schemaName _) = schema
  rootpath = reverse . drop 1 . reverse
  modAt path = mconcat (punctuate dot (map text path))
  im mname = modAt (modulepath ++ [mname])
  tm mname = modAt (rootpath modulepath ++ ["Types", mname])
  unpackMod = modAt (rootpath modulepath ++ ["Unpack"])

  typeimports = map (importDecl tm)
              $ nub
              $ map importType
              $ interfaceTypes ir

  extraimports = [ text "import" <+> unpackMod
                 , text "import Ivory.Language"
                 , text "import Ivory.Serialize"
                 , text "import Ivory.Stdlib"
                 ]

schemaDoc :: String -> Schema -> Doc
schemaDoc interfaceName (Schema schemaName [])     =
    text "-- Cannot define" <+> text schemaName  <+> text "schema for"
        <+> text interfaceName <+> text "interface: schema is empty"
schemaDoc interfaceName (Schema schemaName schema) = stack
    [ text "-- Define" <+> text schemaName  <+> text "schema for"
        <+> text interfaceName <+> text "interface"
    , empty
    , text "data" <+> constructor <+> equals <+> constructor
    , indent 2 $ encloseStack lbrace rbrace comma
        [ text (accessorName n) <+> colon <> colon
           <+> parens (text "forall s r b s' . ConstRef s'"
             <+> typeIvoryArea Embedded t
             <+> text "-> Ivory ('Effects r b ('Scope s)) IBool")
        | (_, (Message n t)) <- schema
        ]
    , empty
    , text (parserName typeName) <+> align
        (stack [ text ":: forall s0 r b s2 s3 n"
               , text " . (ANat n)"
               , text "=> ConstRef s2 ('Array n ('Stored Uint8))"
               , text "-> Ref s3 ('Stored Uint32)"
               , text "->" <+> text typeName
               , text "-> Ivory ('Effects r b ('Scope s0)) IBool"
               ])
    , text (parserName typeName) <+> text "arr offs iface = do"
    , indent 2 $ stack
        [ text "unpackWithCallback arr offs $ \\tag_ref -> do"
        , indent 2 $ text "(tag :: Uint32) <- deref tag_ref"
        , indent 2 $ text "cond" <+> encloseStack lbracket rbracket comma
           [ parens (text "tag ==?" <+> ppr h) <+> text "==>"
            <+> text "unpackWithCallback arr offs"
            <+> parens (text (accessorName n) <+> text "iface")
           | (h, Message n _) <- schema
           ]
        ]
    , empty
    , text (senderName typeName) <+> align
        (stack [ text ":: forall n s1 s2"
               , text " . (ANat n)"
               , text "=> Ref s1 ('Array n ('Stored Uint8))"
               , text "-> Ref s2 ('Stored Uint32)"
               , text "->" <+> constructor
               ])
    , text (senderName typeName) <+> text "arr offs" <+> equals
        <+> constructor
    , indent 2 $ encloseStack lbrace rbrace comma
        [ text (accessorName n) <+> equals <+> text "\\m -> do" </> indent 4
            (stack [ text "o <- deref offs"
                   , text "let required_size = fromInteger (packSize (packRep :: PackRep"
                       <+> typeIvoryArea Embedded t <+> text ")"
                       <+> text "+ packSize (packRep :: PackRep ('Stored Uint32)))"
                   , text "    sufficient_space = (o + required_size) <? arrayLen arr"
                   , text "when sufficient_space $ do"
                   , indent 2 $ stack
                       [ text "ident <- local (ival (" <+> ppr h <+> text ":: Uint32))"
                       , text "packInto arr o (constRef ident)"
                       , text "packInto arr (o + fromInteger (packSize (packRep :: PackRep ('Stored Uint32)))) m"
                       , text "offs += required_size"
                       ]
                   , text "return sufficient_space"
                   ])
        | (h, (Message n t)) <- schema
        ]
    ]
  where
  constructor = text typeName
  accessorName n = userEnumValueName n ++ schemaName
  typeName = interfaceName ++ schemaName

parserName :: String -> String
parserName tn = userEnumValueName tn ++ "Parser"
senderName :: String -> String
senderName tn = userEnumValueName tn ++ "Sender"

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


