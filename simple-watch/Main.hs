module Main where

import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Chan
import           Control.Monad                  ( forever )
import qualified Data.Aeson                    as A
import qualified Data.Text                     as T
import           Files                          ( readJsonFile' )
import           GHC.Generics
import           System.FSNotify
import           System.Process                 ( callCommand )

data Config = Config
    { src     :: String
    , exclude :: [T.Text]
    , command :: String
    }
    deriving (Generic, A.FromJSON, A.ToJSON)

-- NOTE: Does not work well. Use fswatch instead, see nix-config for details
main = do
    channel            <- newChan
    (config :: Config) <- readJsonFile' ".simple-watch.json"
    withManagerConf watcherConfig $ \mgr -> do
        watchTreeChan mgr
                      (src config)
                      (shouldReportEvent (exclude config))
                      (channel)
        forever $ do
            threadDelay 1000000
            events <- getChanContents channel
            if not . null $ events
                then callCommand (command config)
                else return ()


watcherConfig = defaultConfig

shouldReportEvent excludeSegments event =
    let path = T.pack . eventPath $ event
    in  all (\toIgnore -> not $ toIgnore `T.isInfixOf` path) excludeSegments
