
module Gidl.Backend.Haskell.Interface where


import Data.Monoid
import Data.List (intercalate, nub)
import Data.Char (toUpper)

import Gidl.Types
import Gidl.Interface
import Gidl.Backend.Haskell.Types
import Ivory.Artifact
import Text.PrettyPrint.Mainland

interfaceModule :: [String] -> InterfaceRepr -> Artifact
interfaceModule modulepath ir@(InterfaceRepr iname i) =
  artifactPath (intercalate "/" modulepath) $
  artifactText ((ifModuleName ir) ++ ".hs") $
  prettyLazyText 80 $
  stack
    [ text "module"
      <+> im (ifModuleName ir)
      <+> text "where"
    , empty
    , stack [ text "import" <+> im (ifModuleName iir)
            | iir <- interfaceParents i
            ]
    , stack $ map (importDecl tm)
            $ nub
            $ map importType
            $ interfaceTypes ir
    ]
  where
  im mname = mconcat $ punctuate dot
                     $ map text (modulepath ++ [mname])
  tm mname = mconcat $ punctuate dot
                     $ map text (typepath modulepath ++ ["Types", mname])
    where typepath = reverse . drop 1 . reverse

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
