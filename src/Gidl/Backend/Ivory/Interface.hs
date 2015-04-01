
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
    , text "{-# LANGUAGE ScopedTypeVariables #-}"
    , text "{-# OPTIONS_GHC -fno-warn-unused-imports #-}"
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
    , empty
    , text (parserName typeName) <+> align
        (stack [ text ":: forall s0 r b s2 s3 n"
               , text " . (ANat n)"
               , text "=>" <+> brackets (text typeName <> text "Handler")
               , text "-> ConstRef s2 (Array n (Stored Uint8))"
               , text "-> Ref s3 (Stored Uint32)"
               , text "-> Ivory ('Effects r b (Scope s0)) ()"
               ])
    , text (parserName typeName) <+> text "hs arr offs = do"
    , indent 2 $ stack
        [ text "unpackWithCallback arr offs $ \\tag_ref -> do"
        , indent 2 $ text "tag <- deref tag_ref"
        , indent 2 $ text "cond_ (map (aux tag) hs)"
        , text "where"
        , text "aux :: Uint32"
        , text "    ->" <+> text typeName <> text "Handler"
        , text "    -> Cond ('Effects r b (Scope s0)) ()"
        , stack 
           [ text "aux ident" <+> parens (text (handlerName n) <+> text "k")
              <+> equals
              <+> parens (text "ident ==?" <+> ppr h) <+> text "==>"
              </> indent 2 callback
           | (h, Message n t) <- schema
           , let callback = case t of
                   PrimType VoidType -> text "k"
                   _ -> text "unpackWithCallback arr offs k"
           ]
        ]
    , empty
    , text "data" <+> senderConstructor <+> equals <+> senderConstructor
    , indent 2 $ encloseStack lbrace rbrace comma
        [ case t of
            PrimType VoidType -> text (senderName n) <+> colon <> colon
                <+> text "(forall s r b . Ivory ('Effects r b (Scope s)) IBool)"
            _ -> text (senderName n) <+> colon <> colon
                  <+> parens (text "forall s r b s' . ConstRef s'" 
                    <+> parens (text (typeIvoryType t))
                    <+> text "-> Ivory ('Effects r b (Scope s)) IBool")
        | (_, (Message n t)) <- schema
        ]
    , empty
    , text (userEnumValueName typeName) <> text "Sender" <+> align
        (stack [ text ":: forall n s1 s2"
               , text " . (ANat n)"
               , text "=> Ref s1 (Array n (Stored Uint8))"
               , text "-> Ref s2 (Stored Uint32)"
               , text "->" <+> senderConstructor
               ])
    , text (userEnumValueName typeName) <> text "Sender arr offs" <+> equals
        <+> senderConstructor
    , indent 2 $ encloseStack lbrace rbrace comma
        [ case t of
            PrimType VoidType -> text (senderName n) <+> equals <+> text "do" </> indent 4
                  (stack [ text "o <- deref offs"
                         , text "let required_size = fromInteger (packSize (packRep :: PackRep (Stored Uint32)))"
                         , text "    sufficient_space = (o + required_size) <? arrayLen arr"
                         , text "when sufficient_space $ do"
                         , indent 2 $ stack
                             [ text "ident <- local (ival (" <+> ppr h <+> text ":: Uint32))"
                             , text "packInto arr o (constRef ident)"
                             , text "offs += required_size"
                             ]
                         , text "return sufficient_space"
                         ])

            _ -> text (senderName n) <+> equals <+> text "\\m -> do" </> indent 4
                  (stack [ text "o <- deref offs"
                         , text "let required_size = fromInteger (packSize (packRep :: PackRep"
                             <+> parens (text (typeIvoryType t)) <+> text ")"
                             <+> text "+ packSize (packRep :: PackRep (Stored Uint32)))"
                         , text "    sufficient_space = (o + required_size) <? arrayLen arr"
                         , text "when sufficient_space $ do"
                         , indent 2 $ stack
                             [ text "ident <- local (ival (" <+> ppr h <+> text ":: Uint32))"
                             , text "packInto arr o (constRef ident)"
                             , text "packInto arr (o + sizeOf (Proxy :: Proxy (Stored Uint32))) m"
                             , text "offs += required_size"
                             ]
                         , text "return sufficient_space"
                         ])
        | (h, (Message n t)) <- schema
        ]
    ]
  where
  senderConstructor = text typeName <> text "Sender"
  handlerName n = userTypeModuleName n ++ schemaName ++ "Handler"
  senderName n = userEnumValueName n ++ schemaName ++ "Sender"
  typeName = interfaceName ++ schemaName
  parserName tn = userEnumValueName tn ++ "Parser"

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


