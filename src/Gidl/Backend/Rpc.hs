module Gidl.Backend.Rpc (
    rpcBackend
  ) where

import qualified Paths_gidl as P

import Gidl.Backend.Cabal
           (cabalFileArtifact,CabalFile(..),defaultCabalFile,filePathToPackage)
import Gidl.Backend.Haskell.Interface (interfaceModule,ifModuleName)
import Gidl.Backend.Haskell.Types (typeModule,isUserDefined,typeModuleName)
import Gidl.Interface
           (Interface(..),InterfaceEnv(..),MethodName,Method(..),Perm(..))
import Gidl.Types (Type,TypeEnv(..))

import Data.Char (isSpace)
import Data.List (intercalate)
import Ivory.Artifact
           (Artifact(..),artifactPath,artifactFileName,artifactPath
           ,artifactText,artifactCabalFile)
import Ivory.Artifact.Template (artifactCabalFileTemplate)
import Text.PrettyPrint.Mainland
           (Doc,prettyLazyText,text,empty,(<+>),(</>),(<>),char,line,parens
           ,punctuate,stack,sep,tuple,dot,spread,cat,string,indent,hang,nest
           ,(<+/>),align,comma)


-- External Interface ----------------------------------------------------------

rpcBackend :: TypeEnv -> InterfaceEnv -> String -> String -> [Artifact]
rpcBackend typeEnv@(TypeEnv te) ifaceEnv@(InterfaceEnv ie) pkgName nsStr =
    cabalFileArtifact (defaultCabalFile pkgName modules buildDeps)
  : artifactCabalFile P.getDataDir "support/rpc/Makefile"
  : map (artifactPath "src") (rpcBaseModule namespace : sourceMods)

  where

  namespace  = strToNs nsStr

  buildDeps  = [ "cereal", "QuickCheck", "snap-core", "snap-server", "stm"
               , "bytestring", "aeson" ]

  modules    = [ filePathToPackage (artifactFileName m) | m <- sourceMods ]

  sourceMods = tmods ++ imods

  tmods      = [ typeModule True (namespace ++ ["Types"]) t
               | (_tn, t) <- te
               , isUserDefined t
               ]

  imods      = concat [ [ interfaceModule True (namespace ++ ["Interface"]) i
                        , rpcModule typeEnv namespace i ]
                      | (_iname, i) <- ie
                      ]


rpcBaseModule :: [String] -> Artifact
rpcBaseModule ns =
  artifactPath (foldr (\ p rest -> p ++ "/" ++ rest) "Rpc" ns) $
  artifactCabalFileTemplate P.getDataDir "support/rpc/Base.hs.template" env
  where
  env = [ ("module_path", foldr (\p rest -> p ++ "." ++ rest) "Rpc" ns) ]


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


allMethods :: Interface -> [(MethodName,Method)]
allMethods (Interface _ ps ms) = concatMap allMethods ps ++ ms


-- Server Generation -----------------------------------------------------------

rpcModule :: TypeEnv -> [String] -> Interface -> Artifact
rpcModule typeEnv ns iface =
  artifactPath (foldr (\ p rest -> p ++ "/" ++ rest) "Rpc" ns) $
  artifactText (ifaceMod ++ ".hs") $
  prettyLazyText 80 $
  genServer typeEnv ns iface ifaceMod
  where
  ifaceMod = ifModuleName iface


genServer :: TypeEnv -> [String] -> Interface -> String -> Doc
genServer typeEnv ns iface ifaceMod =
  stack [ text "{-# LANGUAGE OverloadedStrings #-}"
        , moduleHeader     ns ifaceMod
        , line
        , importTypes      ns typeEnv
        , importInterface  ns ifaceMod
        , line
        , text "import" <+> (ppModName (ns ++ ["Rpc","Base"]))
        , line
        , webServerImports
        , line
        , line
        , runServer typeEnv iface
        ]


moduleHeader :: [String] -> String -> Doc
moduleHeader ns m =
  spread [ text "module"
         , dots (map text (ns ++ ["Rpc", m]))
         , tuple [ text "rpcServer", text "Config(..)" ]
         , text "where"
         ]


-- | Import all of the generated type modules from the type environment.
importTypes :: [String] -> TypeEnv -> Doc
importTypes ns (TypeEnv ts) = foldr importType empty ts
  where
  prefix = dots (map text (ns ++ ["Types"]))

  importType (_,t) rest =
    (text "import" <+> (prefix *. text (typeModuleName t))) </> rest


importInterface :: [String] -> String -> Doc
importInterface ns ifaceName =
  text "import" <+> (dots (map text (ns ++ ["Interface", ifaceName])))


webServerImports :: Doc
webServerImports  =
  stack [ text "import"           <+> (ppModName ["Snap","Core"])
        , text "import qualified" <+> (ppModName ["Data","ByteString"])
                                  <+> text "as S"
        , text "import"           <+> (ppModName ["Control","Concurrent"])
        , text "import"           <+> (ppModName ["Control","Concurrent","STM"])
        ]


runServer :: TypeEnv -> Interface -> Doc
runServer typeEnv iface = runServerSig </> runServerDef typeEnv iface


runServerSig :: Doc
runServerSig  =
  text "rpcServer" <+> text "::"
                   <+> hang 2 (arrow [ chan, chan, text "Config", text "IO ()" ])
  where
  chan = text "TChan" <+> text "S.ByteString"


-- | Generate a definition for the server.
runServerDef :: TypeEnv -> Interface -> Doc
runServerDef typeEnv iface = hang 2 (text "rpcServer" <+> body)
  where
  body = arg "input"  $ \ input  ->
         arg "output" $ \ output ->
         arg "cfg"    $ \ cfg    ->
           char '=' </>
           nest 2 (text "do" <+> align (stack (stmts cfg)))
             </> text "where"
             </> routesDef
             </> managerDef input output

  stmts cfg = [ text "_ <- forkIO manager"
              , text "runServer" <+> cfg <+> text "routes"
              ]

  routesDef = nest 3 $
    nest 2 (text "routes" <+> char '=' <+/> align (routes typeEnv iface))

  managerDef input output =
    nest 2 (text "manager" <+> char '=' <+/> align (text "..."))


-- | Define one route for each interface member
routes :: TypeEnv -> Interface -> Doc
routes types iface =
  text "route" <+> methods

  where

  methods = align (char '['
         <> stack (punctuate comma (concatMap (mkRoute types) (allMethods iface)))
         <> char ']')

mkRoute :: TypeEnv -> (MethodName,Method) -> [Doc]
mkRoute types (name,method) =
  [ tuple [ text (show name), h ] | h <- handlersFor method ]
  where
  handlersFor (StreamMethod _  ty) = [ readMethod types ty ]
  handlersFor (AttrMethod perm ty) = [ m types ty | m <- permMethods perm ]


permMethods :: Perm -> [ TypeEnv -> Type -> Doc ]
permMethods Read      = [ readMethod              ]
permMethods Write     = [ writeMethod             ]
permMethods ReadWrite = [ readMethod, writeMethod ]


readMethod :: TypeEnv -> Type -> Doc
readMethod types _ = text "writeBS \"read\""

writeMethod :: TypeEnv -> Type -> Doc
writeMethod types _ = text "writeBS \"write\""


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
