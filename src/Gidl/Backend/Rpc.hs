module Gidl.Backend.Rpc (
    rpcBackend
  ) where

import qualified Paths_gidl as P

import Gidl.Backend.Cabal (cabalFileArtifact,defaultCabalFile,filePathToPackage)
import Gidl.Backend.Haskell.Interface (interfaceModule,ifModuleName)
import Gidl.Backend.Haskell.Types (typeModule,isUserDefined,typeModuleName)
import Gidl.Interface
           (Interface(..),InterfaceEnv(..),MethodName,Method(..),Perm(..))
import Gidl.Schema (Schema(..),producerSchema,consumerSchema)
import Gidl.Types (Type,TypeEnv(..))

import Data.Char (isSpace)
import Ivory.Artifact
           (Artifact,artifactPath,artifactFileName,artifactPath,artifactText
           ,artifactCabalFile)
import Ivory.Artifact.Template (artifactCabalFileTemplate)
import Text.PrettyPrint.Mainland
           (Doc,prettyLazyText,text,empty,(<+>),(</>),(<>),char,line,parens
           ,punctuate,stack,tuple,dot,spread,cat,hang,nest,(<+/>),align,comma
           ,braces)


-- External Interface ----------------------------------------------------------

rpcBackend :: TypeEnv -> InterfaceEnv -> String -> String -> [Artifact]
rpcBackend typeEnv@(TypeEnv te) (InterfaceEnv ie) pkgName nsStr =
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
genServer typeEnv ns iface ifaceMod = stack $
  [ text "{-# LANGUAGE RecordWildCards #-}" | useManager ] ++
  [ text "{-# LANGUAGE OverloadedStrings #-}"
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
  , managerDefs
  , runServer useManager typeEnv iface input output
  ]
  where
  (useManager,managerDefs) = managerDef iface output

  (input,output) = queueTypes iface


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


type InputQueue  = Doc
type OutputQueue = Doc

queueTypes :: Interface -> (InputQueue,OutputQueue)
queueTypes iface = (input,output)
  where
  Schema prodName _ = producerSchema iface
  Schema consName _ = consumerSchema iface

  prod = ifModuleName iface ++ prodName
  cons = ifModuleName iface ++ consName

  input  = text "TQueue" <+> text prod
  output = text "TQueue" <+> text cons


runServer :: Bool -> TypeEnv -> Interface -> InputQueue -> OutputQueue -> Doc
runServer useMgr typeEnv iface input output =
  runServerSig input output </> runServerDef useMgr typeEnv iface


runServerSig :: InputQueue -> OutputQueue -> Doc
runServerSig input output =
  text "rpcServer ::" <+> hang 2 (arrow [ input, output
                                        , text "Config"
                                        , text "IO ()" ])

-- | Generate a definition for the server.
runServerDef :: Bool -> TypeEnv -> Interface -> Doc
runServerDef useMgr typeEnv iface =
  hang 2 (text "rpcServer" <+> body)
  where
  body = text "input output cfg" <+> char '=' </>
         nest 2 (text "do" <+> align (stack stmts))
             </> text "where"
             </> routesDef

  stmts = [ text "state <- mkState"                           | useMgr ]
       ++ [ defOutput                                                  ]
       ++ [ text "_ <- forkIO (manager state output output')" | useMgr ]
       ++ [ text "runServer cfg" <+> text "routes"                     ]

  defOutput
    | useMgr    = text "output' <- newTQueue"
    | otherwise = text "let output' = output"

  routesDef = nest 3 $
    nest 2 (text "routes" <+> char '=' <+/> align (routes typeEnv iface))


-- | Define one route for each interface member
routes :: TypeEnv -> Interface -> Doc
routes types iface = text "route" <+> align methods
  where
  methods = char '['
         <> stack (punctuate comma (concatMap (mkRoute types) (allMethods iface)))
         <> char ']'

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


-- The stream manager ----------------------------------------------------------

-- | Define everything associated with the manager, but only if there are stream
-- values to manage.
managerDef :: Interface -> OutputQueue -> (Bool,Doc)
managerDef iface output
  | null streams = (False,empty)
  | otherwise    = (True,stack defs </> empty)
  where

  streams = [ (name,ty) | (name,StreamMethod _ ty) <- allMethods iface ]

  (stateType,stateDecl) = stateDef streams

  defs = [ stateDecl
         , empty
         , mkStateDef streams
         , empty
         , text "manager ::" <+> arrow [ stateType, output, output, text "IO ()" ]
         , nest 2 $ text "manager state output filtered = forever $"
                </> text "do" <+> align stmts
         ]

  stmts = text "msg <- atomically (readTQueue output)"
      </> nest 2 (text "case msg of" </> stack (map mkCase streams ++ [defCase]))

  -- name the producer constructor for a stream element
  Schema prodSuffix _ = producerSchema iface
  prodName ty = text (typeModuleName ty ++ prodSuffix)

  -- update the state for this stream element
  mkCase (n,ty) = prodName ty <+> text "x -> atomically (writeTSampleVar"
                              <+> parens (fieldName n <+> text "state")
                              <+> text "x)"

  defCase = text "notStream -> atomically (writeTQueue filtered notStream)"


-- | Generate the data type used to hold the streaming values, or nothing if
-- there aren't any present in the interface.
stateDef :: [(MethodName,Type)] -> (Doc,Doc)
stateDef streams = (text "State",def)
  where

  def = nest 2 (text "data State = State" <+> braces fields)

  fields = align (stack (punctuate comma (map mkField streams)))

  mkField (name,ty) =
    fieldName name
      <+> text "::"
      <+> text "TSampleVar"
      <+> text (typeModuleName ty)


mkStateDef :: [(MethodName,Type)] -> Doc
mkStateDef streams = stack
  [ text "mkState :: IO State"
  , nest 2 (text "mkState  =" </> nest 3 (text "do" <+> align (stack stmts)))
  ]
  where
  stmts = [ fieldName n <+> text "<- newTSampleVarIO" | (n,_) <- streams ]
       ++ [ text "return State { .. }" ]


-- | Given the name of a stream in the interface, produce the selector for the
-- state data type.
fieldName :: MethodName -> Doc
fieldName name = text "stream_" <> text name


-- Pretty-printing Helpers -----------------------------------------------------

arrow :: [Doc] -> Doc
arrow ts = spread (punctuate (text "->") ts)

(*.) :: Doc -> Doc -> Doc
a *. b = a <> dot <> b

dots :: [Doc] -> Doc
dots  = cat . punctuate dot

ppModName :: [String] -> Doc
ppModName  = dots . map text
