module Main where

import           Control.Monad
import           Options.Applicative
import qualified Secrets

data Command
  = Add AddOptions |
    List |
    Clone |
    Sync

data AddOptions = AddOptions
    { fileName :: String
    , filePath :: String
    }
    deriving Show


addOpts :: ParserInfo Command
addOpts = info optsParser desc
  where
    optsParser =
        Add <$> liftA2 AddOptions fileNameFlag filePathFlag <**> helper
    desc = fullDesc <> progDesc "Adds a file to secrets tracking" <> header
        "Secret addition"
    fileNameFlag =
        strOption $ long "name" <> short 'n' <> metavar "FILE_NAME" <> help
            "file name"
    filePathFlag =
        strOption $ long "file" <> short 'f' <> metavar "FILE_PATH" <> help
            "file path"

listOpts :: ParserInfo Command
listOpts = info optsParser desc
  where
    optsParser = List <$ (pure ()) <**> helper
    desc =
        fullDesc <> progDesc "Lists tracked secrets" <> header "Secret listing"

cloneOpts :: ParserInfo Command
cloneOpts = info optsParser desc
  where
    optsParser = Clone <$ (pure ()) <**> helper
    desc =
        fullDesc <> progDesc "Clones all secrets" <> header "secrets cloning"

syncOpts :: ParserInfo Command
syncOpts = info optsParser desc
  where
    optsParser = Sync <$ (pure ()) <**> helper
    desc       = fullDesc <> progDesc "Backs up secrets to storage" <> header
        "secrets backup"

opts :: ParserInfo Command
opts = info optsParser desc
  where
    optsParser =
        subparser
                (  command "add"   addOpts
                <> command "list"  listOpts
                <> command "clone" cloneOpts
                <> command "sync"  syncOpts
                )
            <**> helper
    desc = fullDesc <> progDesc "project management commands"

main = do
    command <- execParser opts
    case command of
        Add (AddOptions { fileName, filePath }) ->
            Secrets.add fileName filePath
        List  -> Secrets.list
        Sync  -> Secrets.sync
        Clone -> putStrLn "pending"
    putStrLn "Done!"
