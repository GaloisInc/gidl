module Gidl.Backend.Rpc (
    rpcBackend
  ) where

import qualified Paths_gidl as P

import Gidl.Backend.Cabal
           (cabalFileArtifact,CabalFile(..),defaultCabalFile,filePathToPackage)
import Gidl.Backend.Haskell.Interface (interfaceModule,ifModuleName)
import Gidl.Backend.Haskell.Types (typeModule,isUserDefined,typeModuleName)
import Gidl.Interface (Interface,InterfaceEnv(..))
import Gidl.Types (Type,TypeEnv(..))

import Data.Char (isSpace)
import Data.List (intercalate)
import Ivory.Artifact
           (Artifact(..),artifactPath,artifactFileName,artifactPath
           ,artifactText,artifactCabalFile)
import Ivory.Artifact.Template (artifactCabalFileTemplate)
import Text.PrettyPrint.Mainland
           (Doc,prettyLazyText,text,empty,(<+>),(</>),(<>),char,line,parens
           ,punctuate,stack,sep,tuple,dot,spread,cat)


-- External Interface ----------------------------------------------------------

rpcBackend :: TypeEnv -> InterfaceEnv -> String -> String -> [Artifact]
rpcBackend typeEnv@(TypeEnv te) ifaceEnv@(InterfaceEnv ie) pkgName nsStr =
    cabalFileArtifact (defaultCabalFile pkgName modules buildDeps)
  : artifactCabalFile P.getDataDir "support/rpc/Makefile"
  : map (artifactPath "src") (rpcBaseModule namespace : sourceMods)

  where

  namespace  = strToNs nsStr

  buildDeps  = [ "cereal", "QuickCheck", "snap-core", "snap-server", "stm"
               , "bytestring" ]

  modules    = [ filePathToPackage (artifactFileName m) | m <- sourceMods ]

  sourceMods = tmods ++ imods

  tmods      = [ typeModule (namespace ++ ["Types"]) t
               | (_tn, t) <- te
               , isUserDefined t
               ]

  imods      = concat [ [ interfaceModule (namespace ++ ["Interface"]) i
                        , rpcModule typeEnv namespace i ]
                      | (_iname, i) <- ie
                      ]


rpcBaseModule :: [String] -> Artifact
rpcBaseModule ns =
  artifactPath (foldr (\ p rest -> p ++ "/" ++ rest) "Server" ns) $
  artifactCabalFileTemplate P.getDataDir "support/rpc/Base.hs.template" env
  where
  env = [ ("module_prefix", concatMap (++ ".") ns) ]


-- Utilities -------------------------------------------------------------------

strToNs :: String -> [String]
strToNs str =
  case break (== '.') (dropWhile isSpace str) of

    (a,'.' : b) | null a    ->          strToNs b
                | otherwise -> trim a : strToNs b

    (a,_)       | null a    -> []
                | otherwise -> [trim a]

  where
  trim = takeWhile (not . isSpace)


-- Server Generation -----------------------------------------------------------

rpcModule :: TypeEnv -> [String] -> Interface -> Artifact
rpcModule typeEnv ns iface =
  artifactPath (foldr (\ p rest -> p ++ "/" ++ rest) "Server" ns) $
  artifactText (ifaceMod ++ ".hs") $
  prettyLazyText 80 $
  genServer typeEnv ns iface ifaceMod
  where
  ifaceMod = ifModuleName iface


genServer :: TypeEnv -> [String] -> Interface -> String -> Doc
genServer typeEnv ns iface ifaceMod =
  stack [ moduleHeader     ns ifaceMod
        , importTypes      ns typeEnv
        , importInterface  ns ifaceMod
        , line
        , ppImport False (ppModName (ns ++ ["Server","Base"])) Nothing
        , line
        , webServerImports
        , line
        , line
        , runServer typeEnv iface
        ]


moduleHeader :: [String] -> String -> Doc
moduleHeader ns m =
  spread [ text "module"
         , ppHaskellModule (ns ++ ["Server"]) m
         , tuple [ text "rpcServer", text "Config(..)" ]
         , text "where"
         ]


-- | Import all of the generated type modules from the type environment.
importTypes :: [String] -> TypeEnv -> Doc
importTypes ns (TypeEnv ts) = foldr importType empty ts
  where
  prefix = dots (map text (ns ++ ["Types"]))

  importType (_,t) rest =
    stack [ ppImport False (prefix *. text (typeModuleName t)) Nothing
          , rest
          ]


importInterface :: [String] -> String -> Doc
importInterface ns ifaceName =
  ppImport False (dots (map text (ns ++ ["Interface", ifaceName]))) Nothing


webServerImports :: Doc
webServerImports  =
  stack [ ppImport False (ppModName ["Snap","Http","Server"]) Nothing
        , ppImport True  (ppModName ["Data","ByteString"])    Nothing
        ]


runServer :: TypeEnv -> Interface -> Doc
runServer typeEnv iface = runServerSig </> runServerDef typeEnv iface


runServerSig :: Doc
runServerSig  =
  text "rpcServer" <+> text "::"
                   <+> arrow [ chan, chan, text "Config", text "IO ()" ]
  where
  chan = text "TChan" <+> text "S.ByteString"


-- | Generate a definition for the server.
runServerDef :: TypeEnv -> Interface -> Doc
runServerDef typeEnv iface = text "rpcServer" <+> body
  where
  body = arg "input"  $ \ input  ->
         arg "output" $ \ output ->
         arg "cfg"    $ \ cfg    ->
           char '=' <+> empty


-- Pretty-printing Helpers -----------------------------------------------------

arg :: String -> (Doc -> Doc) -> Doc
arg name k = let x = text name in x <+> k (text name)

arrow :: [Doc] -> Doc
arrow ts = spread (punctuate (text "->") ts)

(*.) :: Doc -> Doc -> Doc
a *. b = a <> dot <> b

dots :: [Doc] -> Doc
dots  = cat . punctuate dot

ppImport :: Bool -> Doc -> Maybe Doc -> Doc
ppImport isQual modName mbAs =
  spread [ text "import"
         , if isQual then text "qualified" else empty
         , modName
         , case mbAs of
             Just alt -> text "as" <+> alt
             Nothing  -> empty
         ]

ppModName :: [String] -> Doc
ppModName  = dots . map text

ppHaskellModule :: [String] -> String -> Doc
ppHaskellModule ns n = foldr (\ m rest -> text m <> char '.' <> rest ) (text n) ns
