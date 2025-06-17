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
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort)
import Settings (Settings (..), loadSettings)
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet)
import Yesod.Core
import Yesod.Default.Config2 (makeYesodLogger)

mkYesodDispatch "App" resourcesApp

-- | Create and run the gsd-server application.
appMain :: String -> IO ()
appMain file = do
    appSettings <- loadSettings file
    app <- makeApp appSettings
    waiApp <- makeWaiApplication app
    runSettings
        (setPort (settingsHttpPort appSettings) defaultSettings)
        waiApp

-- Create the core application
makeApp :: Settings -> IO App
makeApp appSettings = do
    appConnectionPool <- DB.createPool appSettings
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    when (settingsRunMigrations appSettings) $ DB.runMigrations appConnectionPool
    return App{..}

-- Create a WAI Application and apply logger middlewares.
makeWaiApplication :: App -> IO Application
makeWaiApplication app = do
    appPlain <- toWaiAppPlain app
    mkLogWare <- mkDefaultMiddlewares (appLogger app)
    return $ mkLogWare appPlain
