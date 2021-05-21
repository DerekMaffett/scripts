module Secrets where

import           Control.Monad
import qualified Data.HashMap.Strict           as HM
import qualified Data.List                     as List
import qualified Data.Maybe                    as Maybe
import           Files                          ( alterJsonFile
                                                , alterYamlFile
                                                , readJsonFile
                                                , writeJsonFile
                                                )
import           Options.Applicative
import qualified System.Directory              as Dir

add fileName filePath = do
    absoluteFilePath <- Dir.makeAbsolute filePath
    ensureSecretsFileExists
    trackFile fileName absoluteFilePath

ensureSecretsFileExists = do
    homeDir <- Dir.getHomeDirectory
    Dir.createDirectoryIfMissing True $ homeDir <> "/" <> secretsConfigDirPath
    Dir.createDirectoryIfMissing True $ homeDir <> "/" <> secretFilesDirPath
    exists <- Dir.doesPathExist $ homeDir <> secretsIndexPath
    if not exists
        then writeJsonFile secretsIndexPath
                           (HM.empty :: HM.HashMap String String)
        else return ()

trackFile fileName filePath = do
    homeDir <- Dir.getHomeDirectory
    putStrLn filePath
    let secretPath = homeDir <> "/" <> secretFilesDirPath <> "/" <> fileName
    alterJsonFile secretsIndexPath
        $ HM.insert fileName (removeHome homeDir filePath)
    Dir.copyFile filePath secretPath
    Dir.removeFile filePath
    Dir.createFileLink secretPath filePath


removeHome homeDir absoluteFilePath =
    ("~" <>) . Maybe.fromJust . (List.stripPrefix homeDir) $ absoluteFilePath

secretsIndexPath = ".config/secrets/index.json"
secretsConfigDirPath = ".config/secrets"
secretFilesDirPath = ".config/secrets/files"
