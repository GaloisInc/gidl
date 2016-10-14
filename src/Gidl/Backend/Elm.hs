{-# LANGUAGE OverloadedStrings #-}

-- | Produce a set of Elm modules for the given interfaces. See the
-- documentation in @Gidl.Backend.Elm.Interface@ for more about the
-- details of the client convenience functions.
module Gidl.Backend.Elm (
    elmBackend
  ) where

import qualified Paths_gidl as P

import Data.List (nub)

import Gidl.Backend.Elm.Common
import Gidl.Backend.Elm.Interface
import Gidl.Backend.Elm.Type
import Gidl.Interface (Interface)
import Gidl.Types (childTypes)

import Ivory.Artifact (Artifact,artifactPath)
import Ivory.Artifact.Template (artifactCabalFileTemplate)

import System.FilePath (pathSeparator)

elmBackend :: [Interface] -> String -> String -> [Artifact]
elmBackend iis pkgName nsStr =
      elmMakefile nsStr
    : elmPackageJson pkgName
    : map (artifactPath "src") sourceMods
  where
    ns = strToNs nsStr
    sourceMods = tmods ++ imods ++ [elmUtilsModule ns]
    types = nub $ concat [ childTypes t
                         | i <- iis
                         , t <- clientMessageTypes i
                         ]
    tmods = [ typeModule ns t
            | t <- types
            , isUserDefined t
            ]
    imods = [ interfaceModule (ns ++ ["Interface"]) i
            | i <- iis
            ]

elmUtilsModule :: Namespace -> Artifact
elmUtilsModule ns =
  artifactPath (foldr1 (\ p rest -> p ++ "/" ++ rest) ns) $
  artifactCabalFileTemplate P.getDataDir "support/elm/Utils.elm.template" env
  where
  env = [ ("module_path", foldr1 (\p rest -> p ++ "." ++ rest) ns) ]

elmPackageJson :: String -> Artifact
elmPackageJson pkgName =
  artifactCabalFileTemplate
    P.getDataDir
    "support/elm/elm-package.json.template"
    env
  where
  env = [ ("package_name", pkgName) ]

elmMakefile :: String -> Artifact
elmMakefile nsStr =
  artifactCabalFileTemplate P.getDataDir "support/elm/Makefile.template" env
  where
  env = [ ( "package_root"
          , map (\c -> if c == '.' then pathSeparator else c) nsStr)
        ]
