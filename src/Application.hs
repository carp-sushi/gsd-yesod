{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application (appMain, makeApp) where

import qualified Database as DB
import Foundation
import Handler
import Settings (Settings (..), loadSettings)

import Control.Monad (when)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort)
import Network.Wai.Middleware.RequestLogger
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
    flip runSettings waiApp $
        setPort (settingsHttpPort settings) defaultSettings

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
            { outputFormat = Detailed False -- no colors
            , destination = Logger $ loggerSet $ appLogger app
            }
