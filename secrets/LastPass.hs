module LastPass where


import           Control.Monad
import qualified Data.List                     as List
import qualified Data.Maybe                    as Maybe
import qualified System.Directory              as Dir
import           System.Process                 ( callCommand
                                                , readCreateProcess
                                                , shell
                                                )


uploadFile fileName filePath = do
    alreadyExists <- fileExists fileName
    if alreadyExists then removeFile fileName else return ()
    contents <- readFile filePath
    callCommand
        $  "echo \""
        <> contents
        <> "\" | lpass add --non-interactive --notes "
        <> fileName

removeFile fileName = do
    callCommand $ "lpass rm " <> fileName

fileExists fileName = do
    matchingFiles <- readCreateProcess (shell $ "lpass ls " <> fileName) []
    return . (>= 1) . length . lines $ matchingFiles

getFile fileName = do
    note <- readCreateProcess (shell $ "lpass show " <> fileName) []
    return
        . Maybe.fromJust
        . (List.stripPrefix "Notes: ")
        . unlines
        . dropWhile (not . ("Notes: " `List.isPrefixOf`))
        . lines
        $ note

safeClone fileName filePath = do
    fileExistsLocally <- Dir.doesPathExist filePath
    if fileExistsLocally
        then putStrLn (filePath <> " already exists")
        else do
            fileContent <- getFile fileName
            writeFile filePath fileContent
