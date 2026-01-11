module Main where

import           Files                          ( readJsonFile
                                                , alterJsonFile
                                                , writeJsonFile
                                                )

import           GHC.Generics
import           Options.Applicative
import           System.Process                 ( readProcess )
import qualified Data.Aeson                    as A
import           Data.Functor
import           Data.Maybe
import qualified Data.HashMap.Strict           as HM
import           Data.List.Split
import           Data.ByteString.Lazy.Char8     ( pack )
import qualified Control.Monad.Parallel        as P
import           Control.Exception              ( try
                                                , SomeException
                                                )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )

data Command
  = Add String
  | Update

addOpts :: ParserInfo Command
addOpts = Add <$> info
    (strOption $ long "repo" <> short 'r' <> metavar "REPO_ADDRESS" <> help
        "repo address"
    )
    (fullDesc <> progDesc "adds repo in format org/repo" <> header "adds repo")

updateOpts :: ParserInfo Command
updateOpts = Update <$ info
    (pure ())
    (fullDesc <> progDesc "Updates existing repo pointers" <> header
        "Updates existing repo pointers"
    )

opts :: ParserInfo Command
opts = info optsParser desc
  where
    optsParser =
        subparser (command "add" addOpts <> command "update" updateOpts)
            <**> helper
    desc = fullDesc <> progDesc "Handles repo pointers for nix"

main = do
    command <- execParser opts
    case command of
        Add repo -> addRepo repo
        Update   -> updateRepos


data PrefetchData = PrefetchData
    { owner :: String
    , repo :: String
    , rev :: String
    , hash :: String
   } deriving (Generic, A.FromJSON, A.ToJSON, Show)

centralPkgList = "dotfiles/configs/github-pkgs/github-pkgs.json"
compiledPkgList = "dotfiles/configs/github-pkgs/compiled-github-pkgs.json"

addRepo repoAddress = do
    alterJsonFile centralPkgList (repoAddress :)
    updateRepos

updateRepos = do
    repos :: [String] <- readJsonFile centralPkgList
    results           <- P.mapM prefetchGithub repos
    let successfulResults = catMaybes results
    if null successfulResults
        then hPutStrLn stderr "Error: All fetches failed"
        else writeJsonFile compiledPkgList (foldToHashMap successfulResults)

prefetchGithub :: String -> IO (Maybe PrefetchData)
prefetchGithub repoAddress = do
    result <- try (prefetchGithub' repoAddress) :: IO (Either SomeException (Maybe PrefetchData))
    case result of
        Left err -> do
            hPutStrLn stderr $ "Error fetching " <> repoAddress <> ": " <> show err
            return Nothing
        Right val -> return val

prefetchGithub' :: String -> IO (Maybe PrefetchData)
prefetchGithub' repoAddress =
    case splitOn "/" repoAddress of
        [org, repo] -> do
            prefetchResult <- readProcess "nix-prefetch-github" [org, repo] []
            case A.decode $ pack prefetchResult of
                Nothing -> do
                    hPutStrLn stderr $ "Error: Invalid JSON from nix-prefetch-github for " <> org <> "/" <> repo
                    hPutStrLn stderr $ "Output was: " <> prefetchResult
                    return Nothing
                Just parsed -> do
                    putStrLn $ "Updated " <> org <> "/" <> repo
                    return $ Just parsed
        _ -> do
            hPutStrLn stderr $ "Error: Invalid repo address format: " <> repoAddress <> " (expected org/repo)"
            return Nothing

foldToHashMap prefetchResults =
    foldl (<>) HM.empty . map createSingleton $ prefetchResults
  where
    createSingleton prefetchResult =
        HM.singleton (repo prefetchResult) prefetchResult
