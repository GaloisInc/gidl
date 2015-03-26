
module Gidl.Backend.Haskell.Interface where


import Data.Monoid
import Data.List (intercalate, nub)
import Data.Char (toUpper)

import Gidl.Types
import Gidl.Interface
import Ivory.Artifact
import Text.PrettyPrint.Mainland

interfaceModule :: [String] -> InterfaceRepr -> Artifact
interfaceModule modulepath ir =
  artifactPath (intercalate "/" modulepath) $
  artifactText ((ifModuleName ir) ++ ".hs") $
  prettyLazyText 80 $
  stack
    [ text "module"
      <+> tm (ifModuleName ir)
      <+> text "where"
    , empty
    ]
  where
  tm mname = mconcat $ punctuate dot
                     $ map text (modulepath ++ [mname])


ifModuleName :: InterfaceRepr -> String
ifModuleName (InterfaceRepr iname _) = aux iname
  where
  aux :: String -> String
  aux = first_cap . u_to_camel
  first_cap (s:ss) = (toUpper s) : ss
  first_cap []     = []
  u_to_camel ('_':'i':[]) = []
  u_to_camel ('_':[]) = []
  u_to_camel ('_':a:as) = (toUpper a) : u_to_camel as
  u_to_camel (a:as) = a : u_to_camel as
  u_to_camel [] = []
