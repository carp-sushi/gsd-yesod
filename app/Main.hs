import Application (appMain)

import Data.Maybe (listToMaybe)
import System.Environment (getArgs)

-- GSD server entry point.
main :: IO ()
main = do
    args <- getArgs
    case listToMaybe args of
        Nothing -> putStrLn "Usage: gsd-server <settings-file>"
        Just settingsFile -> appMain settingsFile
