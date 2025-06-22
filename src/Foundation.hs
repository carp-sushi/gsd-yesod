{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Model (MilestoneId, StoryId, TaskId)
import Settings

import Control.Monad.Logger (LogSource)
import Database.Persist.Sql
import Yesod.Core
import Yesod.Core.Types (Logger)
import Yesod.Persist.Core

-- | The core application type.
data App = App
    { appSettings :: Settings
    , appConnectionPool :: ConnectionPool
    , appLogger :: Logger
    }

-- Generate resources and routes.
mkYesodData "App" $(parseRoutesFile "config/routes")

-- Customize the Yesod application.
instance Yesod App where
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = return Nothing

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger

    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
            settingsLogEverything (appSettings app)
                || level == LevelWarn
                || level == LevelError

-- Set up database persistence for the Yesod application.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB query = do
        (App _ pool _) <- getYesod
        runSqlPool query pool
