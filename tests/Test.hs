module Main where

import Ivory.Artifact
import Control.Monad
import Gidl.Types
import Gidl.Interface
import Gidl.Parse
import Gidl.Schema
import Gidl.Backend.Cabal
import Gidl.Backend.Haskell.Types
import Gidl.Backend.Haskell.Interface
import Gidl.Backend.Haskell

main :: IO ()
main = do
  test "tests/testtypes.sexpr"
  runHaskellBackend "tests/testtypes.sexpr"
                    "gidl-haskell-backend-test"
                    (words "Gidl Haskell Test")
                    "tests/gidl-haskell-backend-test"


test :: FilePath -> IO ()
test f = do
  c <- readFile f
  case parseDecls c of
    Left e -> putStrLn e
    Right (te@(TypeEnv te'), ie@(InterfaceEnv ie')) -> do
      {-
      forM_ te' $ \(tn, t) -> do
        putStrLn (tn ++ ":")
        print (typeLeaves t)
        let a = typeModule (words "Sample IDL Haskell Types")
                           (typeDescrToRepr tn te)
        printArtifact a
      -}
      forM_ ie' $ \(iname, _i) -> do
        printArtifact $ interfaceModule (words "Sample IDL Haskell Interface")
                                        (interfaceDescrToRepr iname ie te)
