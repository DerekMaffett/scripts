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

data Preconditon = Precondition
    { condition :: IO Bool
    , autofix   :: Maybe (IO ())
    , message   :: String
    }

testPreconditions = mapM_
    (\Precondition { condition, autofix, message } -> do
        conditionSatisfied <- condition
        if conditionSatisfied
            then return ()
            else case autofix of
                Nothing           -> error message
                Just availableFix -> do
                    putStrLn message
                    availableFix
    )


secretsFolderExists = Precondition
    { condition = do
                      homeDir                <- Dir.getHomeDirectory
                      secretsConfigDirExists <-
                          Dir.doesDirectoryExist
                          $  homeDir
                          <> "/"
                          <> secretsConfigDirPath
                      secretsFilesDirExists <-
                          Dir.doesDirectoryExist
                          $  homeDir
                          <> "/"
                          <> secretFilesDirPath
                      return $ secretsConfigDirExists && secretsFilesDirExists
    , autofix   = Just $ do
                      homeDir <- Dir.getHomeDirectory
                      Dir.createDirectoryIfMissing True
                          $  homeDir
                          <> "/"
                          <> secretsConfigDirPath
                      Dir.createDirectoryIfMissing True
                          $  homeDir
                          <> "/"
                          <> secretFilesDirPath
    , message   = "Secrets directory missing. Creating folders..."
    }


secretsIndexFileExists = do
    homeDir <- Dir.getHomeDirectory
    Dir.doesPathExist $ homeDir <> "/" <> secretsIndexPath

createSecretsFile = do
    writeJsonFile secretsIndexPath (HM.empty :: HM.HashMap String String)

fileNotTracked filePath = do
    homeDir <- Dir.getHomeDirectory
    paths   <- HM.elems <$> readIndex
    let fullPaths = expandHome homeDir <$> paths
    return $ filePath `List.notElem` fullPaths

add fileName relativeFilePath = do
    homeDir  <- Dir.getHomeDirectory
    filePath <- Dir.makeAbsolute relativeFilePath
    let secretPath = homeDir <> "/" <> secretFilesDirPath <> "/" <> fileName
    let contractedLinkPath = contractHome homeDir filePath

    testPreconditions
        [ secretsFolderExists
        , Precondition
            { condition = secretsIndexFileExists
            , autofix   = Just createSecretsFile
            , message   = secretsIndexPath <> " missing, reconstructing..."
            }
        , Precondition { condition = not <$> Dir.doesPathExist secretPath
                       , autofix   = Nothing
                       , message   = "A secret already exists by that name"
                       }
        , Precondition
            { condition = fileNotTracked filePath
            , autofix   = Nothing
            , message   = "A secret already is reserved for that file path"
            }
        ]

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
    testPreconditions
        [ secretsFolderExists
        , Precondition
            { condition = secretsIndexFileExists
            , autofix   = Nothing
            , message   = secretsIndexPath
                              <> " missing. Please clone or add secrets"
            }
        ]

    keys <- HM.keys <$> readIndex
    mapM_ (\fileName -> putStrLn fileName) keys



sync = do
    testPreconditions
        [ secretsFolderExists
        , Precondition
            { condition = secretsIndexFileExists
            , autofix   = Nothing
            , message   = secretsIndexPath
                              <> " missing. Please clone or add secrets"
            }
        ]

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
    testPreconditions [secretsFolderExists]

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
