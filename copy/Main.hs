module Main where

import           Data.Char
import           Data.List
import           System.Process

main :: IO ()
main = do
    currentBranch <-
        trim <$> readCreateProcess (shell "git rev-parse --abbrev-ref HEAD") []
    putStr currentBranch
    where trim = dropWhile isSpace . dropWhileEnd isSpace
