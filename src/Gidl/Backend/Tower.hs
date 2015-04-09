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
import Gidl.Backend.Ivory.Schema (ifModuleName)
import Gidl.Backend.Tower.Schema

towerBackend :: TypeEnv -> InterfaceEnv -> String -> String -> [Artifact]
towerBackend te ie pkgname namespace_raw =
  [ cabalFileArtifact cf
  , makefile
  , defaultconf
  , artifactPath "tests" (codegenTest ie namespace)
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
    [ [ schemaModule (namespace ++ ["Interface"]) i (producerSchema i)
      , schemaModule (namespace ++ ["Interface"]) i (consumerSchema i) ]
    | (_iname, i) <- ie ]

makefile :: Artifact
makefile = artifactCabalFile P.getDataDir "support/tower/Makefile"

defaultconf :: Artifact
defaultconf = artifactCabalFile P.getDataDir "support/tower/default.conf"

codegenTest :: InterfaceEnv -> [String] -> Artifact
codegenTest (InterfaceEnv ie) modulepath =
  artifactCabalFileTemplate P.getDataDir fname
    [("module_path",intercalate "." modulepath)
    ,("imports", intercalate "\n"
                  [ "import "
                    ++ interfaceImport (ifModuleName i) "Producer"
                    ++ "\n"
                    ++ "import "
                    ++ interfaceImport (ifModuleName i) "Consumer"
                  | (_, i) <- ie
                  ])
    ,("app_body", intercalate "\n  " (concat [ interfaceTest i | (_, i) <- ie ]))
    ]
  where
  fname = "support/tower/CodeGen.hs.template"
  interfaceImport i j = intercalate "." (modulepath ++ ["Tower", "Interface", i, j])

  interfaceTest :: Interface -> [String]
  interfaceTest i = [ schemaTest (producerSchema i)
                    , schemaTest (consumerSchema i)
                    ]
    where
    schemaTest :: Schema -> String
    schemaTest (Schema _ []) = []
    schemaTest (Schema schemaName _)
      =  (inputFuncName ((ifModuleName i) ++ schemaName))
      ++ " (snd c) >>= \\i -> "
      ++ (outputFuncName ((ifModuleName i) ++ schemaName))
      ++ " i >>= \\(_ :: ChanOutput (Array 80 (Stored Uint8))) -> return ()"
