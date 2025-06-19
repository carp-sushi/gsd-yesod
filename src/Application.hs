{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application (appMain, makeApp) where

import Control.Monad (when)
import qualified Database as DB
import Foundation
import Handler
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort)
import Network.Wai.Middleware.RequestLogger
import Settings (Settings (..), loadSettings)
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet)
import Yesod.Core
import Yesod.Core.Types (loggerSet)
import Yesod.Default.Config2 (makeYesodLogger)

mkYesodDispatch "App" resourcesApp

-- | Create and run the gsd-server application.
appMain :: FilePath -> IO ()
appMain filePath = do
    settings <- loadSettings filePath
    app <- makeApp settings
    waiApp <- makeWaiApplication app
    runSettings
        (setPort (settingsHttpPort settings) defaultSettings)
        waiApp

-- | Create the core application
makeApp :: Settings -> IO App
makeApp appSettings = do
    appConnectionPool <- DB.createPool appSettings
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    when (settingsRunMigrations appSettings) $ DB.runMigrations appConnectionPool
    return App{..}

-- Create a WAI Application and apply logger middlewares.
makeWaiApplication :: App -> IO Application
makeWaiApplication app = do
    logWare <- makeLogWare app
    appPlain <- toWaiAppPlain app
    return $ logWare appPlain

-- Create logging middleware.
makeLogWare :: App -> IO Middleware
makeLogWare app =
    mkRequestLogger
        defaultRequestLoggerSettings
            { outputFormat = Detailed True
            , destination = Logger $ loggerSet $ appLogger app
            }
