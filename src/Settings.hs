{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Settings (
    Settings (..),
    loadSettings,
) where

import Data.Configurator
import Data.Text (Text)

-- | App settings type.
data Settings = Settings
    { settingsDatabaseUrl :: Text
    , settingsPoolSize :: Int
    , settingsHttpPort :: Int
    , settingsRunMigrations :: Bool
    , settingsLogEverything :: Bool
    }
    deriving (Eq, Ord, Show)

-- | Load settings from file.
loadSettings :: FilePath -> IO Settings
loadSettings filePath = do
    cfg <- load [Required filePath]
    settingsDatabaseUrl <- require cfg "databaseUrl"
    settingsPoolSize <- require cfg "poolSize"
    settingsHttpPort <- require cfg "httpPort"
    settingsRunMigrations <- require cfg "runMigrations"
    settingsLogEverything <- require cfg "logEverything"
    return Settings{..}
