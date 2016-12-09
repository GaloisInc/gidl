module Gidl
  ( run
  ) where

import Prelude ()
import Prelude.Compat

import Data.Char
import Data.Maybe (catMaybes)
import Control.Monad (when)
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import Text.Show.Pretty

import Ivory.Artifact
import Gidl.Parse
import Gidl.Interface
import Gidl.Backend.Elm (elmBackend)
import Gidl.Backend.Haskell
import Gidl.Backend.Ivory
import Gidl.Backend.Rpc (rpcBackend)
import Gidl.Backend.Tower

data OptParser opt = OptParser [String] (opt -> opt)
instance Monoid (OptParser opt) where
  mempty = OptParser [] id
  OptParser as f `mappend` OptParser bs g = OptParser (as ++ bs) (f . g)

success :: (opt -> opt) -> OptParser opt
success  = OptParser []

invalid :: String -> OptParser opt
invalid e = OptParser [e] id

parseOptions :: [OptDescr (OptParser opt)] -> [String]
             -> Either [String] (opt -> opt)
parseOptions opts args = case getOpt Permute opts args of
  (fs,[],[]) -> case mconcat fs of
    OptParser [] f -> Right f
    OptParser es _ -> Left es
  (_,_,es) -> Left es

data Backend
  = HaskellBackend
  | IvoryBackend
  | TowerBackend
  | RpcBackend
  | ElmBackend
  deriving (Eq, Show)

data Opts = Opts
  { backend             :: Backend
  , idlpath             :: FilePath
  , outpath             :: FilePath
  , ivoryrepo           :: FilePath
  , towerrepo           :: FilePath
  , ivorytowerstm32repo :: FilePath
  , packagename         :: String
  , namespace           :: String
  , debug               :: Bool
  , help                :: Bool
  }

initialOpts :: Opts
initialOpts = Opts
  { backend             = error (usage ["must specify a backend"])
  , idlpath             = error (usage ["must specify an idl file"])
  , outpath             = error (usage ["must specify an output path"])
  , ivoryrepo           = "ivory"
  , towerrepo           = "tower"
  , ivorytowerstm32repo = "ivory-tower-stm32"
  , packagename         = error (usage ["must specify a package name"])
  , namespace           = ""
  , debug               = False
  , help                = False
  }

setBackend :: String -> OptParser Opts
setBackend b = case map toUpper b of
  "HASKELL"     -> success (\o -> o { backend = HaskellBackend })
  "IVORY"       -> success (\o -> o { backend = IvoryBackend })
  "TOWER"       -> success (\o -> o { backend = TowerBackend })
  "HASKELL-RPC" -> success (\o -> o { backend = RpcBackend })
  "ELM"         -> success (\o -> o { backend = ElmBackend })
  _             -> invalid e
  where e = "\"" ++ b ++ "\" is not a valid backend.\n"
          ++ "Supported backends: haskell, ivory, tower, haskell-rpc"

setIdlPath :: String -> OptParser Opts
setIdlPath p = success (\o -> o { idlpath = p })

setOutPath :: String -> OptParser Opts
setOutPath p = success (\o -> o { outpath = p })

setIvoryRepo :: String -> OptParser Opts
setIvoryRepo p = success (\o -> o { ivoryrepo = p })

setTowerRepo :: String -> OptParser Opts
setTowerRepo p = success (\o -> o { towerrepo = p })

setIvoryTowerSTM32Repo :: String -> OptParser Opts
setIvoryTowerSTM32Repo p = success (\o -> o { ivorytowerstm32repo = p })

setPackageName :: String -> OptParser Opts
setPackageName p = success (\o -> o { packagename = p })

setNamespace :: String -> OptParser Opts
setNamespace p = success (\o -> o { namespace = p })

setDebug :: OptParser Opts
setDebug = success (\o -> o { debug = True })

setHelp :: OptParser Opts
setHelp = success (\o -> o { help = True })

options :: [OptDescr (OptParser Opts)]
options =
  [ Option "b" ["backend"]   (ReqArg setBackend "BACKEND")
      "code generator backend"
  , Option "i" ["idl"]       (ReqArg setIdlPath "FILE")
      "IDL file"
  , Option "o" ["out"]       (ReqArg setOutPath "DIR")
      "root directory for output"
  , Option []  ["ivory-repo"] (ReqArg setIvoryRepo "REPO")
      "root of ivory.git (for Ivory and Tower backends only)"
  , Option []  ["tower-repo"] (ReqArg setTowerRepo "REPO")
      "root of tower.git (for Tower backend only)"
  , Option []  ["ivory-tower-stm32-repo"]
      (ReqArg setIvoryTowerSTM32Repo "REPO")
      "root of ivory-tower-stm32.git (for Tower backend only)"
  , Option "p" ["package"]   (ReqArg setPackageName "NAME")
      "package name for output"
  , Option "n" ["namespace"] (ReqArg setNamespace "NAME")
      "namespace for output"
  , Option ""  ["debug"]     (NoArg setDebug)
      "enable debugging output"
  , Option "h" ["help"]      (NoArg setHelp)
      "display this message and exit"
  ]

parseOpts :: [String] -> IO Opts
parseOpts args = case parseOptions options args of
  Right f -> let opts = f initialOpts in
    if help opts then putStrLn (usage []) >> exitSuccess
                 else return opts
  Left errs -> putStrLn (usage errs) >> exitFailure


usage :: [String] -> String
usage errs = usageInfo banner options
  where
  banner = unlines (errs ++ ["", "Usage: gidl OPTIONS"])

run :: IO ()
run = do
  args <- getArgs
  opts <- parseOpts args
  idl <- readFile (idlpath opts)
  case parseDecls idl of
    Left e -> putStrLn ("Error parsing " ++ (idlpath opts) ++ ": " ++ e)
              >> exitFailure
    Right (te, ie) -> do
      when (debug opts) $ do
        putStrLn (ppShow te)
        putStrLn (ppShow ie)
      let InterfaceEnv ie' = ie
          interfaces = map snd ie'
          absolutize p name = do
            pAbs <- fmap normalise $
                      if isRelative p
                      then fmap (</> p) getCurrentDirectory
                      else return p
            ex <- doesDirectoryExist pAbs
            when (not ex) $
              error (usage [ "Directory \"" ++ p ++ "\" does not exist."
                           , "Make sure that the " ++ name
                             ++ " repository is checked out." ])
            return pAbs
      b <- case backend opts of
             HaskellBackend -> return haskellBackend
             IvoryBackend -> do
               ivoryAbs <- absolutize (ivoryrepo opts) "Ivory"
               return (ivoryBackend ivoryAbs)
             TowerBackend -> do
               ivoryAbs           <- absolutize (ivoryrepo opts) "Ivory"
               towerAbs           <- absolutize (towerrepo opts) "Tower"
               ivoryTowerSTM32Abs <- absolutize (ivorytowerstm32repo opts)
                                                "ivory-tower-stm32"
               return (towerBackend ivoryAbs towerAbs ivoryTowerSTM32Abs)
             RpcBackend -> return rpcBackend
             ElmBackend -> return elmBackend
      artifactBackend opts (b interfaces (packagename opts) (namespace opts))

  where
  artifactBackend :: Opts -> [Artifact] -> IO ()
  artifactBackend opts as = do
    when (debug opts) $ mapM_ printArtifact as
    es <- mapM (putArtifact (outpath opts)) as
    case catMaybes es of
      [] -> exitSuccess
      ees -> putStrLn (unlines ees) >> exitFailure
