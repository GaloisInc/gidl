module Gidl.Backend.Ivory where

import Ivory.Artifact
import Ivory.Artifact.Template

import Data.Char (isSpace)
import Data.List (intercalate, nub)

import qualified Paths_gidl as P

import Gidl.Interface
import Gidl.Schema
import Gidl.Backend.Cabal
import Gidl.Backend.Ivory.Types
import Gidl.Backend.Ivory.Schema

ivoryBackend :: [Interface] -> String -> String -> [Artifact]
ivoryBackend iis pkgname namespace_raw =
  [ cabalFileArtifact cf
  , artifactPath "tests" $ codegenTest namespace
  , makefile
  ] ++ map (artifactPath "src") sources
  where
  sources = ivorySources iis namespace
  namespace = dotwords namespace_raw

  cf = (defaultCabalFile pkgname cabalmods deps) { executables = [ cg_exe ] }
  cg_exe = defaultCabalExe (pkgname ++ "-gen") "CodeGen.hs"
              (deps ++ (words "ivory-backend-c") ++ [pkgname])
  cabalmods = map (filePathToPackage . artifactFileName) sources
  deps = words "ivory ivory-stdlib ivory-serialize"



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

makefile :: Artifact
makefile = artifactCabalFile P.getDataDir "support/ivory/Makefile"

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
