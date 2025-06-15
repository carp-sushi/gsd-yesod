{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application (appMain) where

import Control.Monad (when)
import qualified Database as DB
import Foundation
import Handler
import JoinHandler
import Settings (Settings (..), loadSettings)
import Yesod.Core

mkYesodDispatch "App" resourcesApp

appMain :: String -> IO ()
appMain file = do
    appSettings <- loadSettings file
    appConnectionPool <- DB.createPool appSettings
    when (settingsRunMigrations appSettings) $ DB.runMigrations appConnectionPool
    warp (settingsHttpPort appSettings) App{..}
