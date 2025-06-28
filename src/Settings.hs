{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Settings (
    Settings (..),
    loadSettings,
    settingsReadHttpPort,
) where

import Data.Configurator
import Data.Text (Text, unpack)

-- | App settings type.
data Settings = Settings
    { settingsDatabaseUrl :: Text
    , settingsPoolSize :: Int
    , settingsHttpPort :: Text
    , settingsRunMigrations :: Bool
    , settingsVerboseLogging :: Bool
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
    settingsVerboseLogging <- require cfg "verboseLogging"
    return Settings{..}

-- | Read HTTP port from settings as an Int.
settingsReadHttpPort :: Settings -> Int
settingsReadHttpPort =
    read . unpack . settingsHttpPort
