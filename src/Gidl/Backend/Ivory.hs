module Gidl.Backend.Ivory where

import Gidl.Types
import Gidl.Interface
import Gidl.Backend.Cabal
import Gidl.Backend.Ivory.Types
import Gidl.Backend.Ivory.Interface

import Ivory.Artifact

import Data.Char (isSpace)
import Text.PrettyPrint.Mainland

ivoryBackend :: TypeEnv -> InterfaceEnv -> String -> String -> [Artifact]
ivoryBackend te@(TypeEnv te') ie@(InterfaceEnv ie') pkgname namespace_raw =
  [ cabalFileArtifact cf
  , makefile
  ] ++
  [ artifactPath "src" m | m <- sourceMods
  ]
  where
  tmods = [ typeModule (namespace ++ ["Types"]) tr
          | (tn, _t) <- te'
          , let tr = typeDescrToRepr tn te
          , isUserDefined tr
          ]
  imods = [] -- DISABLE UNTIL WE GET TYPES RIGHT
  _imods =[ interfaceModule (namespace ++ ["Interface"]) ir
          | (iname, _i) <- ie'
          , let ir = interfaceDescrToRepr iname ie te
          ]
  sourceMods = tmods ++ imods
  cf = (defaultCabalFile pkgname cabalmods deps)
  cabalmods = [ filePathToPackage (artifactFileName m) | m <- sourceMods ]
  deps = words "ivory ivory-stdlib ivory-serialize"

  namespace = dotwords namespace_raw


  dotwords :: String -> [String]
  dotwords s = case dropWhile isDot s of
    "" -> []
    s' -> let  (w, s'') = break isDot s' in w : dotwords s''
  isDot c = (c == '.') || isSpace c

makefile :: Artifact
makefile = artifactText "Makefile" $
  prettyLazyText 80 $ stack
    [ text "IVORY_REPO ?= ../../../ivory"
    , empty
    , text "default:"
    , text "\tcabal build"
    , empty
    , text "create-sandbox:"
    , text "\tcabal sandbox init"
    , text "\tcabal sandbox add-source $(IVORY_REPO)/ivory"
    , text "\tcabal sandbox add-source $(IVORY_REPO)/ivory-artifact"
    , text "\tcabal sandbox add-source $(IVORY_REPO)/ivory-serialize"
    , text "\tcabal sandbox add-source $(IVORY_REPO)/ivory-stdlib"
    , text "\tcabal install --enable-tests --dependencies-only"
    , empty
    , text "test:"
    , text "\tcabal test"
    , empty
    ]

