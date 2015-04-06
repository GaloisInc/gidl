
module Gidl.Backend.Ivory.Unpack where

import Data.List (intercalate)
import Ivory.Artifact
import Ivory.Artifact.Template
import qualified Paths_gidl as P

unpackModule :: [String] -> Artifact
unpackModule modulepath =
  artifactPath (intercalate "/" modulepath) $
  artifactCabalFileTemplate P.getDataDir fname
    [("module_path", intercalate "." modulepath )]
  where
  fname = "support/ivory/Unpack.hs.template"
