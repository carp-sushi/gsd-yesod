{-# LANGUAGE OverloadedStrings #-}

import Application (appMain)

import Data.Maybe (listToMaybe)
import Say (say)
import System.Environment (getArgs)

-- GSD server entry point.
main :: IO ()
main = do
    args <- getArgs
    case listToMaybe args of
        Nothing -> say "Usage: gsd-server <settings-file>"
        Just settingsFile -> appMain settingsFile
