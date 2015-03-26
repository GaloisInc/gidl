module Main where

import Ivory.Artifact
import Control.Monad
import Gidl.Types
import Gidl.Interface
import Gidl.Parse
import Gidl.Schema
import Gidl.Backend.Cabal
import Gidl.Backend.Haskell.Types
import Gidl.Backend.Haskell

main :: IO ()
main = runHaskellBackend "tests/testtypes.sexpr"
                         "gidl-haskell-backend-test"
                         (words "Gidl Haskell Test")
                         "tests/gidl-haskell-backend-test"

  --test "tests/testtypes.sexpr"

test :: FilePath -> IO ()
test f = do
  c <- readFile f
  case parseDecls c of
    Left e -> print e
    Right (te@(TypeEnv te'), ie@(InterfaceEnv ie')) -> do
      print te
      putStrLn "---"
      as <- forM te' $ \(tn, t) -> do
        putStrLn (tn ++ ":")
        print (typeLeaves t)
        let a = typeModule (words "Sample IDL Haskell Types")
                           (typeDescrToRepr tn te)
        printArtifact a
        return a
      let c = cabalFileArtifact $ defaultCabalFile "sample-idl-haskell"
                                    (map (filePathToPackage . artifactFileName) as)
                                    []
      printArtifact c
      {-
      putStrLn "---"
      print ie
      putStrLn "---"
      forM_ ie' $ \(iname, i) -> do
        putStrLn (iname ++ ":")
        print (interfaceTypes iname ie te)
        print (interfaceParents i)
        putStrLn "---"
        let ir = interfaceDescrToRepr iname ie te
        print (producerSchema ir)
        print (consumerSchema ir)
        putStrLn "---"
      -}
