module Main where

import           Control.Monad
import           Options.Applicative
-- import qualified Secrets

data Command
  = Add String |
    Clone |
    Sync

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

addOpts :: ParserInfo Command
addOpts = Add <$> info
    (strOption $ long "file" <> short 'f' <> metavar "FILE_PATH" <> help
        "file path"
    )
    (fullDesc <> progDesc "Adds a file to secrets tracking" <> header
        "Secret addition"
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
        Add filePath -> putStrLn "pending"
        Clone        -> putStrLn "pending"
        Sync         -> putStrLn "pending"
    putStrLn "Done!"
