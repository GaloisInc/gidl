module Gidl
  ( run
  ) where

import Data.Char
import Data.Monoid
import Data.Maybe (catMaybes)
import Control.Monad
import System.Console.GetOpt
import System.Environment
import System.Exit

import Ivory.Artifact
import Gidl.Parse
import Gidl.Backend.Haskell
import Gidl.Backend.Ivory

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
  deriving (Eq, Show)

data Opts = Opts
  { backend :: Backend
  , idlpath :: FilePath
  , outpath :: FilePath
  , packagename :: String
  , namespace :: String
  , debug :: Bool
  , help :: Bool
  }

initialOpts :: Opts
initialOpts = Opts
  { backend     = error (usage ["must specify a backend"])
  , idlpath     = error (usage ["must specify an idl file"])
  , outpath     = error (usage ["must specify an output path"])
  , packagename = error (usage ["must specify a package name"])
  , namespace   = ""
  , debug       = False
  , help        = False
  }

setBackend :: String -> OptParser Opts
setBackend b = case map toUpper b of
  "HASKELL" -> success (\o -> o { backend = HaskellBackend })
  "IVORY"   -> success (\o -> o { backend = IvoryBackend })
  _         -> invalid ("\"" ++ b ++ "\" is not a valid backend.\n"
                          ++ "Supported backends: haskell, ivory")

setIdlPath :: String -> OptParser Opts
setIdlPath p = success (\o -> o { idlpath = p })

setOutPath :: String -> OptParser Opts
setOutPath p = success (\o -> o { outpath = p })

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
    Left e -> print e >> exitFailure
    Right (te, ie) ->
      case backend opts of
        HaskellBackend -> artifactBackend opts $
          haskellBackend te ie (packagename opts) (namespace opts)
        IvoryBackend -> artifactBackend opts $
          ivoryBackend te ie (packagename opts) (namespace opts)

  where
  artifactBackend :: Opts -> [Artifact] -> IO ()
  artifactBackend opts as = do
    when (debug opts) $ mapM_ printArtifact as
    es <- mapM (putArtifact (outpath opts)) as
    case catMaybes es of
      [] -> exitSuccess
      ees -> putStrLn (unlines ees) >> exitFailure
