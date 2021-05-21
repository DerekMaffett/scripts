module Secrets where

import           Control.Monad
import qualified Data.HashMap.Strict           as HM
import qualified Data.List                     as List
import qualified Data.Maybe                    as Maybe
import           Files                          ( alterJsonFile
                                                , readJsonFile
                                                , writeJsonFile
                                                )
import qualified LastPass
import           Options.Applicative
import qualified System.Directory              as Dir


secretsIndexPath = ".config/secrets/index.json"
secretsConfigDirPath = ".config/secrets"
secretFilesDirPath = ".config/secrets/files"
lastPassSecretPrefix = "__secrets_file_"
lastPassIndexFileName = "__secrets_index"



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



list = do
    homeDir <- Dir.getHomeDirectory
    index   <- readJsonFile secretsIndexPath
    mapM_ (\fileName -> putStrLn fileName)
        $ HM.keys (index :: HM.HashMap String String)



sync = do
    homeDir <- Dir.getHomeDirectory
    LastPass.uploadFile lastPassIndexFileName
                        (homeDir <> "/" <> secretsIndexPath)
    (secretsIndex :: HM.HashMap String String) <- readJsonFile secretsIndexPath
    mapM_
        (\fileName -> LastPass.uploadFile
            (lastPassSecretPrefix <> fileName)
            (homeDir <> "/" <> secretFilesDirPath <> "/" <> fileName)
        )
        (HM.keys secretsIndex)
