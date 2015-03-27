module Gidl.Backend.Haskell where

import Gidl.Types
import Gidl.Parse
import Gidl.Interface
import Gidl.Backend.Cabal
import Gidl.Backend.Haskell.Types
import Gidl.Backend.Haskell.Interface

import Ivory.Artifact

import Data.Maybe (catMaybes)
import System.Exit (exitFailure, exitSuccess)

haskellBackend :: TypeEnv -> InterfaceEnv -> String -> [String] -> [Artifact]
haskellBackend te@(TypeEnv te') ie@(InterfaceEnv ie') pkgname namespace =
  cabalFileArtifact cf : (map (artifactPath "src") (tmods ++ imods))
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
  cf = defaultCabalFile pkgname mods deps
  mods = [ filePathToPackage (artifactFileName m) | m <- (tmods ++ imods)]
  deps = [ "cereal", "QuickCheck" ]


runHaskellBackend :: FilePath -> String -> [String] -> FilePath -> IO ()
runHaskellBackend idlfile pkgname namespace outdir = do
  c <- readFile idlfile
  case parseDecls c of
    Left e -> print e >> exitFailure
    Right (te, ie) -> do
      let as = haskellBackend te ie pkgname namespace
      es <- mapM (putArtifact outdir) as
      case catMaybes es of
        [] -> exitSuccess
        ees -> putStrLn (unlines ees) >> exitFailure


