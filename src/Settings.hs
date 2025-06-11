{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Settings (
    Settings (..),
    loadSettings,
) where

import Data.Configurator
import Data.Text

-- | App settings
data Settings = Settings
    { settingsDatabaseUrl :: Text
    , settingsPoolSize :: Int
    , settingsHttpPort :: Int
    , settingsRunMigrations :: Bool
    }
    deriving (Eq, Ord, Show)

-- | Read app config from file.
loadSettings :: FilePath -> IO Settings
loadSettings file = do
    cfg <- load [Required file]
    settingsDatabaseUrl <- require cfg "databaseUrl"
    settingsPoolSize <- require cfg "poolSize"
    settingsHttpPort <- require cfg "httpPort"
    settingsRunMigrations <- require cfg "runMigrations"
    return Settings{..}
