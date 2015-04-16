module Gidl.Backend.Haskell where

import Gidl.Types
import Gidl.Interface
import Gidl.Backend.Cabal
import Gidl.Backend.Haskell.Types
import Gidl.Backend.Haskell.Test
import Gidl.Backend.Haskell.Interface

import Ivory.Artifact

import Data.Char (isSpace)
import Text.PrettyPrint.Mainland

haskellBackend :: TypeEnv -> InterfaceEnv -> String -> String -> [Artifact]
haskellBackend (TypeEnv te) (InterfaceEnv ie) pkgname namespace_raw =
  [ cabalFileArtifact cf
  , makefile
  , artifactPath "tests" serializeTestMod
  ] ++
  [ artifactPath "src" m | m <- sourceMods
  ]
  where
  tmods = [ typeModule False (namespace ++ ["Types"]) t
          | (_tn, t) <- te
          , isUserDefined t
          ]
  imods = [ interfaceModule False (namespace ++ ["Interface"]) i
          | (_iname, i) <- ie
          ]
  sourceMods = tmods ++ imods
  cf = (defaultCabalFile pkgname cabalmods deps) { tests = [ serializeTest ] }
  cabalmods = [ filePathToPackage (artifactFileName m) | m <- sourceMods ]
  deps = [ "cereal", "QuickCheck" ]

  serializeTest = defaultCabalTest "serialize-test" "SerializeTest.hs"
                      (pkgname:deps)
  serializeTestMod = serializeTestModule namespace (map snd ie)

  namespace = dotwords namespace_raw


  dotwords :: String -> [String]
  dotwords s = case dropWhile isDot s of
    "" -> []
    s' -> let  (w, s'') = break isDot s' in w : dotwords s''
  isDot c = (c == '.') || isSpace c

makefile :: Artifact
makefile = artifactText "Makefile" $
  prettyLazyText 80 $ stack
    [ text "default:"
    , text "\tcabal build"
    , empty
    , text "create-sandbox:"
    , text "\tcabal sandbox init"
    , text "\tcabal install --enable-tests --dependencies-only"
    , empty
    , text "test:"
    , text "\tcabal test"
    , empty
    ]

