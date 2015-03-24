module Main where

import Gidl.Types
import Gidl.Parse

main :: IO ()
main = test "tests/testtypes.sexpr"

test :: FilePath -> IO ()
test f = do
  c <- readFile f
  case parseDecls c of
    Left e -> print e
    Right (te, ie) -> do
      print te
      print ie
