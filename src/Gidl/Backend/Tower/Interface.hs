
module Gidl.Backend.Tower.Interface where


import Data.Monoid
import Data.List (intercalate, nub)

import Gidl.Types
import Gidl.Interface
import Gidl.Schema
import Gidl.Backend.Ivory.Types
import Gidl.Backend.Ivory.Interface (ifModuleName, parserName)
import Ivory.Artifact
import Text.PrettyPrint.Mainland

interfaceModule :: [String] -> Interface -> Schema -> Artifact
interfaceModule modulepath ir schema =
  artifactPath (intercalate "/" (modulepath ++ [ifModuleName ir])) $
  artifactText (schemaName ++ ".hs") $
  prettyLazyText 80 $
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
  rootpath = reverse . drop 3 . reverse
  modAt path = mconcat (punctuate dot (map text path))
  im mname = modAt (modulepath ++ [mname])
  tm mname = modAt (rootpath modulepath ++ ["Ivory","Types", mname])
  ivoryIFMod = modAt (rootpath modulepath
                      ++ ["Ivory","Interface", ifModuleName ir, schemaName])

  typeimports = map (importDecl tm)
              $ nub
              $ map importType
              $ interfaceTypes ir

  extraimports = [ text "import qualified" <+> ivoryIFMod <+> text "as I"
                 , text "import Ivory.Language"
                 , text "import Ivory.Stdlib"
                 , text "import Ivory.Tower"
                 , text "import Ivory.Serialize"
                 ]

schemaDoc :: String -> Schema -> Doc
schemaDoc interfaceName (Schema schemaName [])     =
    text "-- Cannot define" <+> text schemaName  <+> text "schema for"
        <+> text interfaceName <+> text "interface: schema is empty"
schemaDoc interfaceName (Schema schemaName schema) = stack
    [ text "-- Define" <+> text schemaName  <+> text "schema for"
        <+> text interfaceName <+> text "interface"
    , empty
    , text "data" <+> constructor <+> text "c" <+> equals <+> constructor
    , indent 2 $ encloseStack lbrace rbrace comma
        [ case t of
            PrimType VoidType -> text (accessorName n) <+> colon <> colon
                <+> text "c (Stored IBool)"
            _ -> text (accessorName n) <+> colon <> colon
                    <+> text "c"
                    <+> parens (text (typeIvoryType t))
        | (_, (Message n t)) <- schema
        ]
    , empty
    , text (inputFuncName typeName) <+> align
        (stack [ text ":: (ANat n)"
               , text "=> ChanOutput (Array n (Stored Uint8))"
               , text "-> Tower e" <+> parens (constructor <+> text "ChanOutput")
               ])
    , text (inputFuncName typeName) <+> text "frame_ch" <+> equals <+> text "do"
    , indent 2 $ stack
        [ stack [ chanName n <+> text "<- channel"
                | (_, Message n _) <- schema ]
        , empty
        , text "monitor" <+> dquotes (text (outputFuncName typeName))
            <+> text "$ do"
        , indent 2 $ stack
            [ text "handler frame_ch \"parse_frame\" $ do" 
            , indent 2 $ stack
                [ stack [ emitterName n <+> text "<- emitter"
                           <+> parens (text "fst" <+> chanName n)
                           <+> text "1"
                        | (_, Message n _) <- schema
                        ]
                , text "callback $ \\f -> do"
                , indent 2 $ stack
                    [ text "offs <- local izero"
                    , text "_ <- I." <> text (parserName typeName)
                        <+> text "f offs $ I." <> constructor
                    , indent 2 $ encloseStack lbrace rbrace comma
                        [ case t of
                            PrimType VoidType ->
                                 text "I." <> text (accessorName n) <+> equals
                                  <+> text "emitV" <+> emitterName n
                                  <+> text "true >> return true"
                            _ -> text "I." <> text (accessorName n) <+> equals
                                  <+> text "\\v -> emit" <+> emitterName n
                                  <+> text "v >> return true"
                        | (_, Message n t) <- schema
                        ]
                    , text "return ()"
                    ]
                ]

            ]
        , empty
        , text "return" <+> constructor <+> encloseStack lbrace rbrace comma
            [ text (accessorName n) <+> equals
              <+> parens (text "snd" <+> chanName n)
            | (_, Message n _) <- schema
            ]
        ]
    , empty
    , text (outputFuncName typeName) <> align
        (stack [ text ":: (ANat n)"
               , text "=>" <+> constructor <+> text "ChanOutput"
               , text "-> Tower e (ChanOutput (Array n (Stored Uint8)))"
               ])
    , text (outputFuncName typeName) <+> text "a" <+> equals <+> text "do"
    , indent 2 $ stack
        [ text "frame_ch <- channel"
        , text "monitor" <+> dquotes (text (outputFuncName typeName))
            <+> text "$ do"
        , indent 2 $ stack
            [ text "return ()"
            ]
        , text "return (snd frame_ch)"
        ]
    ]
  where
  constructor = text typeName
  accessorName n = userEnumValueName n ++ schemaName
  typeName = interfaceName ++ schemaName
  inputFuncName tn = userEnumValueName tn ++ "Input"
  outputFuncName tn = userEnumValueName tn ++ "Output"

  chanName s = text "ch_" <> text s
  emitterName s = text "emitter_" <> text s

