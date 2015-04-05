module Gidl.Backend.Ivory where

import Gidl.Types
import Gidl.Interface
import Gidl.Schema
import Gidl.Backend.Cabal
import Gidl.Backend.Ivory.Types
import Gidl.Backend.Ivory.Interface
import Gidl.Backend.Ivory.Unpack

import Ivory.Artifact

import Data.Char (isSpace)
import Text.PrettyPrint.Mainland

ivoryBackend :: TypeEnv -> InterfaceEnv -> String -> String -> [Artifact]
ivoryBackend (TypeEnv te) (InterfaceEnv ie) pkgname namespace_raw =
  [ cabalFileArtifact cf
  , artifactPath "tests" $ codegenTest namespace
  , makefile
  ] ++
  [ artifactPath "src" m | m <- sourceMods
  ]
  where
  userDefinedTypes = [ t | (_,t) <- te, isUserDefined t ]
  tmods = [ typeModule (namespace ++ ["Types"]) t
          | t <- userDefinedTypes ]
  imods =[ [ interfaceModule (namespace ++ ["Interface"]) i (producerSchema i)
           , interfaceModule (namespace ++ ["Interface"]) i (consumerSchema i)
           ]
         | (_iname, i) <- ie
         ]
  sourceMods = tmods
            ++ concat imods
            ++ [ typeUmbrella namespace userDefinedTypes
               , unpackModule namespace
               ]
  cf = (defaultCabalFile pkgname cabalmods deps) { executables = [ cg_exe ] }
  cg_exe = defaultCabalExe (pkgname ++ "-gen") "CodeGen.hs"
              (deps ++ (words "ivory-backend-c") ++ [pkgname])
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
    , text "\tcabal sandbox add-source $(IVORY_REPO)/ivory-opts"
    , text "\tcabal sandbox add-source $(IVORY_REPO)/ivory-backend-c"
    , text "\tcabal install --enable-tests --dependencies-only"
    , empty
    , text "test:"
    , text "\tcabal run -- --src-dir=codegen-out"
    , empty
    ]

codegenTest :: [String] -> Artifact
codegenTest modulepath = artifactText "CodeGen.hs" $
  prettyLazyText 80 $ stack
    [ text "module Main where"
    , text "import Ivory.Compile.C.CmdlineFrontend"
    , text "import Ivory.Serialize"
    , text "import" <+> cat (punctuate dot (map text modulepath)) <> text ".Types"
    , text "main :: IO ()"
    , text "main = compile (serializeModule:typeModules) serializeArtifacts"
    ]


