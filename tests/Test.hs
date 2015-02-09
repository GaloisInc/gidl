module Main where

import Gidl.Types
import Gidl.Parse

main :: IO ()
main = test "tests/testtypes.sexpr"

test :: FilePath -> IO ()
test f = do
  c <- readFile f
  print $ parseDecls c
--- below is just a stash

hb_t :: Type
hb_t = StructType $ Struct
          [ ("mode", "mode_t")
          , ("time", "time_micros_t")
          ]

mode_t :: Type
mode_t = StructType $ Struct
          [ ("armed", "bool_t")
          , ("controlsource", "controlsource_t" )
          ]

controlsource_t :: Type
controlsource_t = EnumType $ EnumT Bits8
  [ ("manual", 0)
  , ("auto", 1)
  ]

time_micros_t :: Type
time_micros_t = NewtypeType $ Newtype "uint8_t"

typeEnv' :: TypeEnv
typeEnv' = TypeEnv
  [ ("hb_t", hb_t)
  , ("mode_t", mode_t)
  , ("controlsource_t", controlsource_t)
  , ("time_micros_t", time_micros_t)
  ]


data Sys = Sys TypeEnv Streams Attrs

data Attrs = Attrs [(String, Either Type Attrs)]

data Streams = Streams [(String, Type)]
