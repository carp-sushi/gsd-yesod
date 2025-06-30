module Database (
    createPool,
    runMigrations,
) where

import Model (migrateAll)
import Settings (Settings (..))

import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import Data.Text.Encoding (encodeUtf8)
import Database.Persist.Postgresql (
    ConnectionPool,
    createPostgresqlPool,
    runMigrationSilent,
    runSqlPool,
 )

-- | Create a database connection pool.
createPool :: Settings -> IO ConnectionPool
createPool settings =
    if settingsVerboseLogging settings
        then mkPool runStdoutLoggingT
        else mkPool runNoLoggingT
  where
    mkPool loggingT =
        loggingT $
            createPostgresqlPool
                (encodeUtf8 $ settingsDatabaseUrl settings)
                (settingsPoolSize settings)

-- | Run SQL migrations on a database.
runMigrations :: ConnectionPool -> IO ()
runMigrations pool =
    runNoLoggingT $ do
        _ <- runSqlPool (runMigrationSilent migrateAll) pool
        return ()
