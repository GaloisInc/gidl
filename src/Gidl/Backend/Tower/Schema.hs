
module Gidl.Backend.Tower.Schema where


import Data.Monoid
import Data.List (intercalate, nub)

import Gidl.Interface
import Gidl.Schema
import Gidl.Backend.Ivory.Types
import Gidl.Backend.Ivory.Schema (ifModuleName, parserName, senderName)
import Ivory.Artifact
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
  rootpath = reverse . drop 2 . reverse
  modAt path = mconcat (punctuate dot (map text path))
  im mname = modAt (modulepath ++ [mname])
  tm mname = modAt (rootpath modulepath ++ ["Ivory","Types", mname])
  ivoryIFMod = modAt (rootpath modulepath
                      ++ ["Ivory","Interface", ifModuleName ir, schemaName])

  typeimports = map (importDecl tm)
              $ nub
              $ map importType
              $ interfaceTypes ir

  extraimports = [ text "import" <+> modAt (rootpath modulepath ++ ["Ivory", "Types"])
                 , text "import qualified" <+> ivoryIFMod <+> text "as I"
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
    , text "data" <+> constructor<+> equals <+> constructor
    , indent 2 $ encloseStack lbrace rbrace comma
        [ accessorName n <+> colon <> colon
           <+> text "ChanOutput" <+> typeIvoryArea Embedded t
        | (_, (Message n t)) <- schema
        ]
    , empty
    , text (inputFuncName typeName) <+> align
        (stack [ text ":: (ANat n)"
               , text "=> ChanOutput ('Array n ('Stored Uint8))"
               , text "-> Tower e" <+> constructor
               ])
    , text (inputFuncName typeName) <+> text "frame_ch" <+> equals <+> text "do"
    , indent 2 $ stack
        [ towerMonadDependencies
        , stack [ chanName n <+> text "<- channel"
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
                        [ text "I." <> accessorName n <+> equals
                            <+> text "\\v -> emit" <+> emitterName n
                            <+> text "v >> return true"
                        | (_, Message n _) <- schema
                        ]
                    , text "return ()"
                    ]
                ]

            ]
        , empty
        , text "return" <+> constructor <+> encloseStack lbrace rbrace comma
            [ accessorName n <+> equals
              <+> parens (text "snd" <+> chanName n)
            | (_, Message n _) <- schema
            ]
        ]
    , empty
    , text (outputFuncName typeName) <> align
        (stack [ text ":: (ANat n)"
               , text "=>" <+> constructor
               , text "-> Tower e (ChanOutput ('Array n ('Stored Uint8)))"
               ])
    , text (outputFuncName typeName) <+> text "a" <+> equals <+> text "do"
    , indent 2 $ stack
        [ towerMonadDependencies
        , text "frame_ch <- channel"
        , text "monitor" <+> dquotes (text (inputFuncName typeName))
            <+> text "$ do"
        , indent 2 $ stack
            [ text "handler" <+> parens (accessorName n <+> text "a")
                <+> dquotes (accessorName n) <+> text "$ do"
                </> indent 2 (parseEmitBody n)
                </> empty
            | (_, Message n _) <- schema
            ]
        , text "return (snd frame_ch)"
        ]
    ]
  where
  constructor = text typeName
  accessorName n = text (userEnumValueName n ++ schemaName)
  typeName = interfaceName ++ schemaName

  chanName s = text "ch_" <> text s
  emitterName s = text "emitter_" <> text s

  parseEmitBody n = stack
    [ text "e <- emitter (fst frame_ch) 1"
    , text "callback $ \\w -> do"
    , indent 2 $ stack
        [ text "f <- local izero"
        , text "o <- local izero"
        , text "ok <-" <+> text "I." <> accessorName n
            <+> parens (text "I." <> text (senderName typeName)
                        <+> text "f o")
            <+> text "w"
        , text "ifte_ ok (emit e (constRef f)) (return ())"
        ]
    ]

  towerMonadDependencies = stack
    [ text "towerModule serializeModule"
    , text "mapM_ towerArtifact serializeArtifacts"
    , text "mapM_ towerModule typeModules"
    , text "mapM_ towerDepends typeModules"
    , empty
    ]

inputFuncName :: String -> String
inputFuncName tn = userEnumValueName tn ++ "Input"

outputFuncName :: String -> String
outputFuncName tn = userEnumValueName tn ++ "Output"
