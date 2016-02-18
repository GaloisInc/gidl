module Gidl.Backend.Ivory where

import Ivory.Artifact
import Ivory.Artifact.Template

import Data.Char (isSpace)
import Data.List (intercalate, nub)
import Text.PrettyPrint.Mainland

import qualified Paths_gidl as P

import Gidl.Interface
import Gidl.Schema
import Gidl.Backend.Cabal
import Gidl.Backend.Ivory.Types
import Gidl.Backend.Ivory.Schema

ivoryBackend :: FilePath -> [Interface] -> String -> String -> [Artifact]
ivoryBackend ivoryRepo iis pkgname namespace_raw =
  [ cabalFileArtifact cf
  , makefile cg_exe_name
  , stackfile ivoryRepo
  , artifactPath "tests" $ codegenTest namespace
  ] ++ map (artifactPath "src") sources
  where
  sources = ivorySources iis namespace
  namespace = dotwords namespace_raw

  cf = (defaultCabalFile pkgname cabalmods ivoryDeps) { executables = [ cg_exe ] }
  cg_exe = defaultCabalExe cg_exe_name "CodeGen.hs"
              (ivoryDeps ++ ivoryTestDeps ++ [pkgname])
  cg_exe_name = pkgname ++ "-gen"
  cabalmods = map (filePathToPackage . artifactFileName) sources

ivoryDeps :: [String]
ivoryDeps =
  [ "ivory"
  , "ivory-serialize"
  , "ivory-stdlib"
  ]

ivoryTestDeps :: [String]
ivoryTestDeps =
  [ "ivory-backend-c"
  ]

ivorySources :: [Interface] -> [String] -> [Artifact]
ivorySources iis namespace =
  tmods ++ concat smods ++ [ typeUmbrella namespace userDefinedTypes
                           , unpackModule namespace
                           ]
  where
  userDefinedTypes = nub [ t | i <- iis, t <- interfaceTypes i, isUserDefined t ]
  tmods = [ typeModule (namespace ++ ["Types"]) t
          | t <- userDefinedTypes ]
  smods = [ [ schemaModule (namespace ++ ["Interface"]) i (producerSchema i)
            , schemaModule (namespace ++ ["Interface"]) i (consumerSchema i) ]
          | i <- iis ]

dotwords :: String -> [String]
dotwords s = case dropWhile isDot s of
  "" -> []
  s' -> let  (w, s'') = break isDot s' in w : dotwords s''
  where
  isDot c = (c == '.') || isSpace c

makefile :: FilePath -> Artifact
makefile cg_exe_name =
  artifactCabalFileTemplate P.getDataDir "support/ivory/Makefile.template"
    [("exe_name", cg_exe_name)]

stackfile :: FilePath -> Artifact
stackfile ivory = artifactText "stack.yaml" $
  prettyLazyText 1000 $ stack
    [ text "resolver: lts-5.3"
    , empty
    , text "packages:"
    , text "- '.'"
    , text ("- location: " ++ ivory)
    , text "  extra-dep: true"
    , text "  subdirs:"
    , text "    - ivory"
    , text "    - ivory-artifact"
    , text "    - ivory-backend-c"
    , text "    - ivory-opts"
    , text "    - ivory-serialize"
    , text "    - ivory-stdlib"
    , empty
    , text "extra-deps:"
    , text "  - exception-mtl-0.4"
    , text "  - ghc-srcspan-plugin-0.2.1.0"
    , text "  - language-c-quote-0.11.4"
    , text "  - mainland-pretty-0.4.1.2"
    , text "  - symbol-0.2.4"
    , empty
    , text "install-ghc: true"
    , empty
    ]


codegenTest :: [String] -> Artifact
codegenTest modulepath =
  artifactCabalFileTemplate P.getDataDir fname
    [("module_path", intercalate "." modulepath )]
  where
  fname = "support/ivory/CodeGen.hs.template"

unpackModule :: [String] -> Artifact
unpackModule modulepath =
  artifactPath (intercalate "/" modulepath) $
  artifactCabalFileTemplate P.getDataDir fname
    [("module_path", intercalate "." modulepath )]
  where
  fname = "support/ivory/Unpack.hs.template"
