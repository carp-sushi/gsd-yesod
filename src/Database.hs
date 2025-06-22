module Database (
    createPool,
    runMigrations,
) where

import Control.Monad.Logger (runNoLoggingT)
import Data.Text.Encoding (encodeUtf8)
import Database.Persist.Postgresql (ConnectionPool, createPostgresqlPool, runMigrationSilent, runSqlPool)
import Model (migrateAll)
import Settings (Settings (..))

-- | Create a database connection pool.
createPool :: Settings -> IO ConnectionPool
createPool settings =
    runNoLoggingT $
        createPostgresqlPool
            (encodeUtf8 $ settingsDatabaseUrl settings)
            (settingsPoolSize settings)

-- | Run SQL migrations on a database.
runMigrations :: ConnectionPool -> IO ()
runMigrations pool =
    runNoLoggingT $ do
        _ <- runSqlPool (runMigrationSilent migrateAll) pool
        return ()
