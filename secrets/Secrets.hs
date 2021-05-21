module Secrets where

import           Control.Monad
import qualified Data.HashMap.Strict           as HM
import           Files                          ( alterJsonFile
                                                , alterYamlFile
                                                , readJsonFile
                                                , writeJsonFile
                                                )
import           Options.Applicative
import qualified System.Directory              as Dir

add fileName filePath = do
    ensureSecretsFileExists
    trackFile fileName filePath

ensureSecretsFileExists = do
    homeDir <- Dir.getHomeDirectory
    exists  <- Dir.doesPathExist $ homeDir <> secretsIndexPath
    if not exists
        then writeJsonFile secretsIndexPath
                           (HM.empty :: HM.HashMap String String)
        else return ()

trackFile fileName filePath =
    alterJsonFile secretsIndexPath $ HM.insert fileName filePath

secretsIndexPath = "/.config/secrets/index.json"
