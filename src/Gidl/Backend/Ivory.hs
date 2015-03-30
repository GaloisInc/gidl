module Gidl.Backend.Ivory where

import Gidl.Types
import Gidl.Interface
import Gidl.Backend.Cabal
import Gidl.Backend.Ivory.Types
import Gidl.Backend.Ivory.Test
import Gidl.Backend.Ivory.Interface

import Ivory.Artifact

import Data.Char (isSpace)

ivoryBackend :: TypeEnv -> InterfaceEnv -> String -> String -> [Artifact]
ivoryBackend te@(TypeEnv te') ie@(InterfaceEnv ie') pkgname namespace_raw =
  [ cabalFileArtifact cf
  , makefile
  , artifactPath "tests" serializeTestMod
  ] ++
  [ artifactPath "src" m | m <- sourceMods
  ]
  where
  tmods = [ typeModule (namespace ++ ["Types"]) tr
          | (tn, _t) <- te'
          , let tr = typeDescrToRepr tn te
          , isUserDefined tr
          ]
  imods = [ interfaceModule (namespace ++ ["Interface"]) ir
          | (iname, _i) <- ie'
          , let ir = interfaceDescrToRepr iname ie te
          ]
  sourceMods = tmods ++ imods
  cf = (defaultCabalFile pkgname cabalmods deps) { tests = [ serializeTest ] }
  cabalmods = [ filePathToPackage (artifactFileName m) | m <- sourceMods ]
  deps = [ "cereal", "QuickCheck" ]

  serializeTest = defaultCabalTest "serialize-test" "SerializeTest.hs"
                      (pkgname:deps)
  serializeTestMod = serializeTestModule namespace
                        [ interfaceDescrToRepr iname ie te | (iname, _i) <- ie']

  namespace = dotwords namespace_raw


  dotwords :: String -> [String]
  dotwords s = case dropWhile isDot s of
    "" -> []
    s' -> let  (w, s'') = break isDot s' in w : dotwords s''
  isDot c = (c == '.') || isSpace c

