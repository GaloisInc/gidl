module Gidl.Backend.Tower where

import Data.List (intercalate)
import Text.PrettyPrint.Mainland

import Ivory.Artifact
import Ivory.Artifact.Template

import qualified Paths_gidl as P

import Gidl.Interface
import Gidl.Schema
import Gidl.Backend.Cabal
import Gidl.Backend.Ivory (dotwords, ivorySources)
import Gidl.Backend.Ivory.Schema (ifModuleName)
import Gidl.Backend.Tower.Schema
import Gidl.Backend.Tower.Server

towerBackend :: FilePath -> FilePath -> FilePath
             -> [Interface] -> String -> String -> [Artifact]
towerBackend ivoryRepo towerRepo ivoryTowerSTM32Repo iis pkgname namespace_raw =
  [ cabalFileArtifact cf
  , makefile
  , stackfile ivoryRepo towerRepo ivoryTowerSTM32Repo
  , defaultconf
  , artifactPath "tests" (codegenTest iis namespace)
  ] ++ map (artifactPath "src") sources
  where
  namespace = dotwords namespace_raw

  sources = isources ++ tsources
         ++ [ attrModule (namespace ++ ["Tower"])
                         (namespace ++ ["Ivory","Types"]) ]

  tsources = towerSources iis (namespace ++ ["Tower"])

  isources = ivorySources iis (namespace ++ ["Ivory"])

  cf = (defaultCabalFile pkgname cabalmods towerDeps) { tests = [ cg_test ] }
  cabalmods = map (filePathToPackage . artifactFileName) sources
  cg_test = defaultCabalTest cg_test_name "CodeGen.hs"
            (towerDeps ++ towerTestDeps ++ [pkgname])
  cg_test_name = pkgname ++ "-gen"

towerDeps :: [String]
towerDeps =
  [ "ivory"
  , "ivory-serialize"
  , "ivory-stdlib"
  , "tower"
  ]

towerTestDeps :: [String]
towerTestDeps =
  [ "tower-config"
  , "tower-freertos-stm32"
  ]

towerSources :: [Interface] -> [String] -> [Artifact]
towerSources iis namespace = towerInterfaces
  where
  towerInterfaces = concat
    [ [ schemaModule    ifnamespace i (producerSchema i)
      , schemaModule    ifnamespace i (consumerSchema i)
      , serverModule    ifnamespace i
      , umbrellaModule  ifnamespace i
      ]
    | i <- iis ]
  ifnamespace = namespace ++ ["Interface"]

makefile :: Artifact
makefile =
  artifactCabalFileTemplate P.getDataDir "support/tower/Makefile.template" []

stackfile :: FilePath -> FilePath -> FilePath -> Artifact
stackfile ivoryRepo towerRepo ivoryTowerSTM32Repo = artifactText "stack.yaml" $
  prettyLazyText 1000 $ stack
    [ text "resolver: lts-6.10"
    , empty
    , text "packages:"
    , text "- '.'"
    , text ("- location: " ++ ivoryRepo)
    , text "  extra-dep: true"
    , text "  subdirs:"
    , text "    - ivory"
    , text "    - ivory-artifact"
    , text "    - ivory-backend-c"
    , text "    - ivory-hw"
    , text "    - ivory-opts"
    , text "    - ivory-serialize"
    , text "    - ivory-stdlib"
    , text ("- location: " ++ towerRepo)
    , text "  extra-dep: true"
    , text "  subdirs:"
    , text "    - tower"
    , text "    - tower-config"
    , text "    - tower-hal"
    , text "    - tower-opts"
    , text ("- location: " ++ ivoryTowerSTM32Repo)
    , text "  extra-dep: true"
    , text "  subdirs:"
    , text "    - ivory-bsp-stm32"
    , text "    - ivory-freertos-bindings"
    , text "    - tower-freertos-stm32"
    , empty
    , text "extra-deps:"
    , text "  - exception-mtl-0.4"
    , text "  - ghc-srcspan-plugin-0.2.1.0"
    , text "  - language-c-quote-0.11.6"
    , text "  - mainland-pretty-0.4.1.4"
    , text "  - symbol-0.2.4"
    , empty
    , text "install-ghc: true"
    , empty
    ]

defaultconf :: Artifact
defaultconf = artifactCabalFile P.getDataDir "support/tower/default.conf"

codegenTest :: [Interface] -> [String] -> Artifact
codegenTest iis modulepath =
  artifactCabalFileTemplate P.getDataDir fname
    [("module_path",intercalate "." modulepath)
    ,("imports", intercalate "\n"
                  [ "import "
                    ++ interfaceImport (ifModuleName i) "Producer"
                    ++ "\n"
                    ++ "import "
                    ++ interfaceImport (ifModuleName i) "Consumer"
                  | i <- iis
                  ])
    ,("app_body", intercalate "\n  " (concat [ interfaceTest i | i <- iis ]))
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
      ++ " i >>= \\(_ :: ChanOutput ('Array 80 ('Stored Uint8))) -> return ()"


attrModule :: [String] -> [String] -> Artifact
attrModule modulepath typespath =
  artifactPath (intercalate "/" modulepath) $
  artifactCabalFileTemplate P.getDataDir fname
    [("module_path", intercalate "." modulepath )
    ,("types_path", intercalate "." typespath)
    ]
  where
  fname = "support/tower/Attr.hs.template"
