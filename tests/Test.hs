module Main where

import Ivory.Artifact
import Control.Monad
import Gidl.Types
import Gidl.Interface
import Gidl.Parse
import Gidl.Schema
import Gidl.Backend.Haskell.Types

main :: IO ()
main = test "tests/testtypes.sexpr"

test :: FilePath -> IO ()
test f = do
  c <- readFile f
  case parseDecls c of
    Left e -> print e
    Right (te@(TypeEnv te'), ie@(InterfaceEnv ie')) -> do
      print te
      putStrLn "---"
      forM_ te' $ \(tn, t) -> do
        putStrLn (tn ++ ":")
        print (typeLeaves t)
        printArtifact (typeModule (words "Sample IDL Haskell Types")
                                  (typeDescrToRepr tn te))

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
