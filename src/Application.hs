{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application (
    makeApp,
    runApp,
) where

import qualified Database as DB
import Foundation
import Handler
import qualified Logger
import Settings

import Control.Monad (when)
import qualified Network.Wai.Handler.Warp as Warp
import Say (say)
import Yesod.Core

-- Generate dispatch code linking requests for routes to handler functions.
mkYesodDispatch "App" resourcesApp

-- | Create and run the gsd-server application.
runApp :: FilePath -> IO ()
runApp filePath = do
    settings <- loadSettings filePath
    app <- makeApp settings
    waiApp <- makeWaiApplication app
    say $ "Running gsd-server on port " <> settingsHttpPort settings
    Warp.runSettings (warpSettings app) waiApp

-- | Create the core application
makeApp :: Settings -> IO App
makeApp appSettings = do
    appConnectionPool <- DB.createPool appSettings
    appLogger <- Logger.makeAppLogger
    when (settingsRunMigrations appSettings) $
        DB.runMigrations appConnectionPool
    return App{..}

-- Create a WAI Application and apply request logger middleware.
makeWaiApplication :: App -> IO Application
makeWaiApplication app = do
    waiApp <- toWaiAppPlain app
    requestLoggerMiddleware <- Logger.makeRequestLogger app
    return $ requestLoggerMiddleware waiApp

-- Create warp settings for App.
warpSettings :: App -> Warp.Settings
warpSettings app =
    Warp.setPort (settingsReadHttpPort $ appSettings app) $
        Warp.setHost "!4" $ -- means HostIPv4Only - any IPv4 hostname
            Warp.defaultSettings
