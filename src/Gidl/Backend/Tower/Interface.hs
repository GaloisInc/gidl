
module Gidl.Backend.Tower.Interface where


import Data.Monoid
import Data.List (intercalate)

import Gidl.Types
import Gidl.Interface
import Gidl.Schema
import Gidl.Backend.Ivory.Types
import Gidl.Backend.Ivory.Schema (ifModuleName)
import Ivory.Artifact
import Text.PrettyPrint.Mainland

interfaceModule :: [String] -> Interface -> Artifact
interfaceModule modulepath i =
  artifactPath (intercalate "/" modulepath) $
  artifactText (ifModuleName i ++ ".hs") $
  prettyLazyText 80 $
  stack
    [ text "{-# LANGUAGE DataKinds #-}"
    , text "{-# LANGUAGE RankNTypes #-}"
    , text "{-# LANGUAGE ScopedTypeVariables #-}"
    , text "{-# LANGUAGE KindSignatures #-}"
    , text "{-# LANGUAGE RecordWildCards #-}"
    , text "{-# OPTIONS_GHC -fno-warn-unused-imports #-}"
    , text "{-# OPTIONS_GHC -fno-warn-unused-matches #-}"
    , empty
    , text "module"
      <+> im (ifModuleName i)
      <+> text "where"
    , empty
    , stack imports
    , empty
    , attrsDataType i
    , empty
    , attrsTowerConstructor i
    , empty
    , attrsInitializer i
    , empty
    , streamsDataType i
    , empty
    , streamsTowerConstructor i
    ]
  where
  rootpath = reverse . drop 2 . reverse
  modAt path = mconcat (punctuate dot (map text path))
  im mname = modAt (modulepath ++ [mname])
  tm mname = modAt (rootpath modulepath ++ ["Ivory","Types", mname])

  imports =
    [ text "import" <+> modAt (rootpath modulepath ++ ["Tower", "Attr"])
    , text "import" <+> im (ifModuleName i) <> dot <> text "Producer"
    , text "import" <+> im (ifModuleName i) <> dot <> text "Consumer"
    , text "import Ivory.Language"
    , text "import Ivory.Tower"
    ]


attrsDataType :: Interface -> Doc
attrsDataType i = text "data" <+> constructor <+> text "(p :: Area * -> *) ="
               </> indent 2 constructor
               </> indent 4 body
  where
  constructor = text (ifModuleName i) <> text "Attrs"
  body = encloseStack lbrace rbrace comma
    [ text n <+> colon <> colon <+> text "p"
                 <+> parens (text (typeIvoryType t))
    | (aname, AttrMethod _ t)  <- interfaceMethods i
    , let n = userEnumValueName aname
    ]

attrsTowerConstructor :: Interface -> Doc
attrsTowerConstructor i = typesig </> decl </> indent 2 body </> indent 2 ret
  where
  constructor = text (ifModuleName i) <> text "Attrs"
  typesig = text "tower" <> constructor <+> colon <> colon
    <+> constructor <+> text "Init"
    <+> text "->"
    <+> text "Tower e" <+> parens (constructor <+> text "Attr")
  decl = text "tower" <> constructor <+> text "ivals = do"
  body = stack
    [ text n <> text "_p <- towerAttr"
       <+> dquotes (text aname)
       <+> parens (text n <+> text "ivals")
    | (aname, AttrMethod _ t)  <- interfaceMethods i
    , let n = userEnumValueName aname
    ]
  ret = text "return" <+> constructor <+> encloseStack lbrace rbrace comma
    [ text n <+> equals <+> text n <> text "_p"
    | (aname, AttrMethod _ t)  <- interfaceMethods i
    , let n = userEnumValueName aname
    ]

attrsInitializer :: Interface -> Doc
attrsInitializer i = typesig </> decl </> indent 2 body
  where
  constructor = text (ifModuleName i) <> text "Attrs"
  typesig = text "init" <> constructor <+> colon <> colon
            <+> constructor <+> text "Init"
  decl = text "init" <> constructor <+> equals <+> constructor
  body = encloseStack lbrace rbrace comma
    [ text n <+> equals <+> text "izero"
    | (aname, AttrMethod _ t)  <- interfaceMethods i
    , let n = userEnumValueName aname
    ]

streamsDataType :: Interface -> Doc
streamsDataType i = text "data" <+> constructor <+> text "(p :: Area * -> *) ="
               </> indent 2 constructor
               </> indent 4 body
  where
  constructor = text (ifModuleName i) <> text "Streams"
  body = encloseStack lbrace rbrace comma
    [ text n <+> colon <> colon <+> text "p"
                 <+> parens (text (typeIvoryType t))
    | (aname, StreamMethod _ t)  <- interfaceMethods i
    , let n = userEnumValueName aname
    ]

streamsTowerConstructor :: Interface -> Doc
streamsTowerConstructor i = typesig </> decl </> indent 2 body </> indent 2 ret
  where
  constructor = text (ifModuleName i) <> text "Streams"
  typesig = text "tower" <> constructor <+> colon <> colon
    <+> text "Tower e"
    <+> parens (constructor <+> text "ChanInput" <> comma
                <+> constructor <+> text "ChanOutput")
  decl = text "tower" <> constructor <+> text "= do"
  body = stack
    [ text n <> text "_c <- channel"
    | (aname, StreamMethod _ t)  <- interfaceMethods i
    , let n = userEnumValueName aname
    ]
  ret = text "return" <+> encloseStack lparen rparen comma
    [ mkstream "fst", mkstream "snd"]
  mkstream acc = constructor </> indent 2 (encloseStack lbrace rbrace comma
    [ text n <+> equals <+> text acc <+> text n <> text "_c"
    | (aname, StreamMethod _ t)  <- interfaceMethods i
    , let n = userEnumValueName aname
    ])
