module Gidl.Backend.Haskell where

import Gidl.Types
import Gidl.Interface
import Gidl.Backend.Cabal
import Gidl.Backend.Haskell.Types
import Gidl.Backend.Haskell.Test
import Gidl.Backend.Haskell.Interface

import Ivory.Artifact

import Data.Char (isSpace)

haskellBackend :: TypeEnv -> InterfaceEnv -> String -> String -> [Artifact]
haskellBackend te@(TypeEnv te') ie@(InterfaceEnv ie') pkgname namespace_raw =
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

