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


init = do
    homeDir <- Dir.getHomeDirectory
    Dir.createDirectoryIfMissing True $ homeDir <> "/" <> secretsConfigDirPath
    Dir.createDirectoryIfMissing True $ homeDir <> "/" <> secretFilesDirPath




add fileName filePath = do
    absoluteFilePath <- Dir.makeAbsolute filePath
    ensureSecretsFileExists
    trackFile fileName absoluteFilePath

ensureSecretsFileExists = do
    homeDir <- Dir.getHomeDirectory
    exists  <- Dir.doesPathExist $ homeDir <> "/" <> secretsIndexPath
    if not exists
        then writeJsonFile secretsIndexPath
                           (HM.empty :: HM.HashMap String String)
        else return ()

trackFile fileName filePath = do
    homeDir <- Dir.getHomeDirectory
    let secretPath = homeDir <> "/" <> secretFilesDirPath <> "/" <> fileName
    let contractedLinkPath = contractHome homeDir filePath

    secretFileAlreadyExists <- Dir.doesPathExist secretPath
    if secretFileAlreadyExists
        then error "A secret already exists by that name"
        else return ()

    paths <- HM.elems <$> readIndex
    let fullPaths = expandHome homeDir <$> paths
    if filePath `List.elem` fullPaths
        then error "A secret already is reserved for that file path"
        else return ()

    alterJsonFile secretsIndexPath $ HM.insert fileName contractedLinkPath
    Dir.copyFile filePath secretPath
    Dir.removeFile filePath
    Dir.createFileLink secretPath filePath

contractHome homeDir absoluteFilePath =
    ("~" <>) . Maybe.fromJust . (List.stripPrefix homeDir) $ absoluteFilePath

expandHome homeDir filePath =
    (homeDir <>) . Maybe.fromJust . (List.stripPrefix "~") $ filePath


readIndex = do
    homeDir                             <- Dir.getHomeDirectory
    (index :: HM.HashMap String String) <- readJsonFile secretsIndexPath
    return index


list = do
    keys <- HM.keys <$> readIndex
    mapM_ (\fileName -> putStrLn fileName) keys



sync = do
    homeDir <- Dir.getHomeDirectory
    LastPass.uploadFile lastPassIndexFileName
                        (homeDir <> "/" <> secretsIndexPath)
    keys <- HM.keys <$> readIndex
    mapM_
        (\fileName -> LastPass.uploadFile
            (lastPassSecretPrefix <> fileName)
            (homeDir <> "/" <> secretFilesDirPath <> "/" <> fileName)
        )
        keys



clone = do
    homeDir <- Dir.getHomeDirectory
    LastPass.safeClone lastPassIndexFileName
                       (homeDir <> "/" <> secretsIndexPath)
    indexEntries <- HM.toList <$> readIndex
    mapM_ cloneSecretFile indexEntries

cloneSecretFile (fileName, filePath) = do
    homeDir <- Dir.getHomeDirectory
    let secretPath = homeDir <> "/" <> secretFilesDirPath <> "/" <> fileName
    let linkPath   = expandHome homeDir filePath
    LastPass.safeClone (lastPassSecretPrefix <> fileName) secretPath
    linkPathFileExists <- Dir.doesPathExist linkPath
    if not linkPathFileExists
        then Dir.createFileLink secretPath linkPath
        else do
            isSymbolicLink <- Dir.pathIsSymbolicLink linkPath
            if isSymbolicLink
                then return ()
                else putStrLn (filePath <> " exists outside secrets")
