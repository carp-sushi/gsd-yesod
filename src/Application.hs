{-# LANGUAGE DataKinds #-}
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
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Logger (clockDateCacher)
import Network.Wai.Middleware.RequestLogger
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet)
import Yesod.Core
import qualified Yesod.Core.Types as YCT

-- Generate dispatch code linking requests for routes to handler functions.
mkYesodDispatch "App" resourcesApp

-- | Create and run the gsd-server application.
appMain :: FilePath -> IO ()
appMain filePath = do
    settings <- loadSettings filePath
    app <- makeApp settings
    waiApp <- makeWaiApplication app
    putStrLn $ "Running gsd-server on port " <> show (settingsHttpPort settings)
    Warp.runSettings (warpSettings app) waiApp

-- | Create the core application
makeApp :: Settings -> IO App
makeApp appSettings = do
    appConnectionPool <- DB.createPool appSettings
    appLogger <- makeAppLogger
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
            , destination = Logger $ YCT.loggerSet $ appLogger app
            }

-- | Create warp settings for App.
warpSettings :: App -> Warp.Settings
warpSettings app =
    Warp.setPort
        (settingsHttpPort $ appSettings app)
        Warp.defaultSettings

-- | Create a yesod logger from a fast-logger logger set.
makeAppLogger :: IO YCT.Logger
makeAppLogger = do
    setter <- newStdoutLoggerSet defaultBufSize
    (getter, _) <- clockDateCacher
    return $! YCT.Logger setter getter
