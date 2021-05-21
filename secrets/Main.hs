module Main where

import           Control.Monad
import           Options.Applicative
import qualified Secrets

data Command
  = Add AddOptions |
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
        Add
            <$>  liftA2
                     AddOptions
                     (  strOption
                     $  long "name"
                     <> short 'n'
                     <> metavar "FILE_NAME"
                     <> help "file name"
                     )
                     (  strOption
                     $  long "file"
                     <> short 'f'
                     <> metavar "FILE_PATH"
                     <> help "file path"
                     )
            <**> helper
    desc = fullDesc <> progDesc "Adds a file to secrets tracking" <> header
        "Secret addition"


cloneOpts :: ParserInfo Command
cloneOpts = Clone <$ info
    (pure ())
    (fullDesc <> progDesc "Clones all secrets" <> header "secrets cloning")

syncOpts :: ParserInfo Command
syncOpts = Sync <$ info
    (pure ())
    (fullDesc <> progDesc "Backs up secrets to storage" <> header
        "secrets back-up"
    )


opts :: ParserInfo Command
opts = info optsParser desc
  where
    optsParser =
        subparser
                (  command "add"   addOpts
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
        Clone -> putStrLn "pending"
        Sync  -> putStrLn "pending"
    putStrLn "Done!"
