module Gidl.Backend.Tower where

import Data.List (intercalate)

import Ivory.Artifact
import Ivory.Artifact.Template

import qualified Paths_gidl as P

import Gidl.Types
import Gidl.Interface
import Gidl.Schema
import Gidl.Backend.Cabal
import Gidl.Backend.Ivory (dotwords, ivorySources)
import Gidl.Backend.Tower.Interface

towerBackend :: TypeEnv -> InterfaceEnv -> String -> String -> [Artifact]
towerBackend te ie pkgname namespace_raw =
  [ cabalFileArtifact cf
  , makefile
  , defaultconf
  , artifactPath "tests" (codegenTest namespace)
  ] ++ map (artifactPath "src") sources
  where
  namespace = dotwords namespace_raw

  sources = isources ++ tsources

  tsources = towerSources ie (namespace ++ ["Tower"])

  isources = ivorySources te ie (namespace ++ ["Ivory"])

  cf = (defaultCabalFile pkgname cabalmods deps) { executables = [ cg_exe ] }
  cabalmods = map (filePathToPackage . artifactFileName) sources
  deps = words "ivory ivory-stdlib ivory-serialize tower"
  cg_exe = defaultCabalExe (pkgname ++ "-gen") "CodeGen.hs"
            (deps ++ (words "tower-config tower-freertos-stm32") ++ [pkgname])


towerSources :: InterfaceEnv -> [String] -> [Artifact]
towerSources (InterfaceEnv ie) namespace = towerInterfaces
  where
  towerInterfaces = concat
    [ [ interfaceModule (namespace ++ ["Tower", "Interface"]) i (producerSchema i)
      , interfaceModule (namespace ++ ["Tower", "Interface"]) i (consumerSchema i) ]
    | (_iname, i) <- ie ]

makefile :: Artifact
makefile = artifactCabalFile P.getDataDir "support/tower/Makefile"

defaultconf :: Artifact
defaultconf = artifactCabalFile P.getDataDir "support/tower/default.conf"

codegenTest :: [String] -> Artifact
codegenTest modulepath =
  artifactCabalFileTemplate P.getDataDir fname
    [("module_path", intercalate "." modulepath )]
  where
  fname = "support/tower/CodeGen.hs.template"

