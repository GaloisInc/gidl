
module Gidl.Backend.Tower.Interface where


import Data.Monoid
import Data.List (intercalate, nub)

import Gidl.Types
import Gidl.Interface
import Gidl.Schema
import Gidl.Backend.Ivory.Types
import Gidl.Backend.Ivory.Schema (ifModuleName)
import Ivory.Artifact
import Text.PrettyPrint.Mainland

interfaceModule :: [String] -> Interface -> Artifact
interfaceModule modulepath ir =
  artifactPath (intercalate "/" modulepath) $
  artifactText (ifModuleName ir ++ ".hs") $
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
    ]
  where
  rootpath = reverse . drop 2 . reverse
  modAt path = mconcat (punctuate dot (map text path))
  im mname = modAt (modulepath ++ [mname])
  tm mname = modAt (rootpath modulepath ++ ["Ivory","Types", mname])

  typeimports = map (importDecl tm)
              $ nub
              $ map importType
              $ interfaceTypes ir

  extraimports =
    [ text "import" <+> modAt (rootpath modulepath ++ ["Ivory", "Types"])
    , text "import" <+> im (ifModuleName ir) <> dot <> text "Producer"
    , text "import" <+> im (ifModuleName ir) <> dot <> text "Consumer"
    , text "import Ivory.Language"
    , text "import Ivory.Stdlib"
    , text "import Ivory.Tower"
    , text "import Ivory.Serialize"
    ]

