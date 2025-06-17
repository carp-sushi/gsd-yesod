{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Control.Monad.Logger (LogSource)
import Database.Persist.Sql
import Model (MilestoneId, StoryId, TaskId)
import Settings
import Yesod.Core
import Yesod.Core.Types (Logger)
import Yesod.Persist.Core

data App = App
    { appSettings :: Settings
    , appConnectionPool :: ConnectionPool
    , appLogger :: Logger
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

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

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        (App _ pool _) <- getYesod
        runSqlPool action pool
