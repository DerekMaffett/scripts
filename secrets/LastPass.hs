module LastPass where


import           Control.Monad
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
    fileContent <- readCreateProcess (shell $ "lpass show " <> fileName) []
    putStrLn fileContent
