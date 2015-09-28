{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Process (shell, createProcess, readProcess)
import System.Directory (getHomeDirectory, doesDirectoryExist, getCurrentDirectory, setCurrentDirectory)
import System.FilePath.Posix ((</>))
import System.IO.Unsafe (unsafePerformIO)

import Data.Aeson (ToJSON, FromJSON, decode, encode, decode')
import Data.List (tails, groupBy, sortBy, intercalate)
import Control.Exception (catch)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Csv as CSV
import GHC.Generics (Generic)
import qualified Data.Vector as V
import Data.Function (on)

homeDir = unsafePerformIO getCurrentDirectory
configFile = homeDir </> ".mastery.json"
reposDir = homeDir </> ".mastery/"
statsFile = homeDir </> "stats.json"
data GlobalStats = GlobalStats { allLanguages :: [LanguageStats]
                               , allRepos :: [RepoStats] } deriving (Show, Generic)

data LanguageStats = LanguageStats { language :: String
                                   , code :: Int
                                   , comment :: Int
                                   , blank :: Int
                                   , files :: Int } deriving (Show, Generic)

data RepoStats = RepoStats { repo :: Repo
                           , languages :: [LanguageStats] } deriving (Show, Generic)

data Repo = Repo { name :: String,
                   url :: String,
                   ignore :: [String] } deriving (Show, Generic)


data Config = Config { emails :: [String]
                     , repos :: [Repo] } deriving (Show, Generic)

instance ToJSON GlobalStats
instance ToJSON Repo
instance ToJSON LanguageStats
instance ToJSON RepoStats
instance FromJSON Repo
instance FromJSON Config

-- Might throw exception
readConfig :: IO Config
readConfig = do
  contents <-BS.readFile configFile -- might throws exception :/
  config <- case decode' contents of
    Just conf -> return conf
    Nothing   -> error "Problems parsing config file"
  return config

repoDirName :: Repo -> FilePath
repoDirName repo = reposDir </> name
  where name = reverse $ drop 4 $ reverse $head $ dropWhile ('/' `elem`) $ tails (url repo) -- It actually works !!

createRepo :: Repo -> IO ()
createRepo repo = do
  putStrLn $ "cloning " ++ (name repo)
  let repoDir = repoDirName repo
  _ <- readProcess "git" ["clone", url repo, repoDir] ""
  return ()

updateRepo :: Repo -> IO ()
updateRepo repo = do
  putStrLn $ "updating " ++ (name repo)
  let repoDir = repoDirName repo
  setCurrentDirectory repoDir
  _ <- readProcess "git" ["pull"] ""
  return ()

pullRepo :: Repo -> IO ()
pullRepo repo = do
  let repoDir = repoDirName repo
  repoExists <- doesDirectoryExist repoDir
  if repoExists then updateRepo repo else createRepo repo

repoStats :: Repo -> IO (RepoStats)
repoStats repo = do
  let repoDir = repoDirName repo
  let ignoredFiles = if ignore repo == [] then [] else [ "--exclude-dir="++(intercalate "," (ignore repo)) ]
  out <- readProcess "cloc" ([repoDir, "--csv", "--quiet"]  ++ ignoredFiles) ""
  let processedOut = BSC.pack $ unlines $ drop 2 $ lines out
  let eitherStats = CSV.decode CSV.NoHeader processedOut :: Either String (V.Vector (Int, String, Int, Int, Int))
  return $ case eitherStats of
    Left errMsg -> error errMsg
    Right stats -> RepoStats { repo = repo
                             , languages = V.toList $ V.map (\(f, lang, b, c, loc) -> LanguageStats { language = lang
                                                                                                    , files = f
                                                                                                    , blank = b
                                                                                                    , comment = c
                                                                                                    , code = loc }) stats }

aggregateLanguageStats :: [LanguageStats] -> LanguageStats
aggregateLanguageStats ls = LanguageStats { language = language (head ls)
                                          , code = sum $ map code ls
                                          , comment = sum $ map comment ls
                                          , blank = sum $ map blank ls
                                          , files = sum $ map files ls }

computeGlobalStats :: [RepoStats] -> GlobalStats
computeGlobalStats repoStats = GlobalStats { allLanguages = map aggregateLanguageStats languageGroups
                                           , allRepos = repoStats }
  where languageGroups = groupBy ((==) `on` language) $ sortBy (\a b -> compare (language a) (language b)) $ concatMap languages repoStats

main :: IO ()
main = do
  config <- readConfig -- Don't care if crashes
  mapM_ pullRepo (repos config)
  perRepoStats <- mapM repoStats (repos config)
  let globalStats = computeGlobalStats perRepoStats
  BS.writeFile statsFile $ "var stats_json = " `BS.append` encode globalStats
