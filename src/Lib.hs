{-# LANGUAGE OverloadedStrings #-}

module Lib (main) where

import Application (runApp)
import Data.Maybe (listToMaybe)
import Say (say)
import System.Environment (getArgs)

-- | Read settings file from command line and start the application.
main :: IO ()
main = do
    args <- getArgs
    case listToMaybe args of
        Nothing -> say "Usage: gsd-server <settings-file>"
        Just settingsFile -> runApp settingsFile
