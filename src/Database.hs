module Database (
    createPool,
    runMigrations,
) where

import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
import Database.Persist.Postgresql (ConnectionPool, createPostgresqlPool, runMigrationSilent, runSqlPool)
import Model (migrateAll)
import Settings (Settings (..))

-- | Create a database connection pool.
createPool :: Settings -> IO ConnectionPool
createPool settings =
    if (settingsLogEverything settings)
        then createPoolStdoutLogging url size
        else createPoolNoLogging url size
  where
    url = encodeUtf8 $ settingsDatabaseUrl settings
    size = settingsPoolSize settings

-- | Run SQL migrations on a database.
runMigrations :: ConnectionPool -> IO ()
runMigrations pool =
    runNoLoggingT $ do
        _ <- runSqlPool (runMigrationSilent migrateAll) pool
        return ()

-- TODO: remove duplication in these functions

createPoolStdoutLogging :: ByteString -> Int -> IO ConnectionPool
createPoolStdoutLogging url size =
    runStdoutLoggingT $
        createPostgresqlPool url size

createPoolNoLogging :: ByteString -> Int -> IO ConnectionPool
createPoolNoLogging url size =
    runNoLoggingT $
        createPostgresqlPool url size
