{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module TestSupport
    ( module TestSupport
    , module X
    ) where

import Application (makeApp)
import Control.Monad.Reader (ReaderT, ask)
import Data.Text (Text, intercalate)
import Database.Persist.Sql  (SqlPersistM, runSqlPersistMPool, rawExecute, rawSql, unSingle)
import Database.Persist.SqlBackend (SqlBackend, getEscapedRawName)
import Text.Shakespeare.Text (st)

import Database.Persist as X hiding (get)
import Foundation       as X
import Model            as X
import Settings         as X
import Test.Hspec       as X
import Yesod.Core       as X
import Yesod.Test       as X

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query =
    runSqlPersistMPool query (appConnectionPool app)

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    settings <- loadSettings "config/settings_test"
    app <- makeApp settings
    truncateTables app
    return (app, id) -- id disables access logging

truncateTables :: App -> IO ()
truncateTables app = runDBWithApp app $ do
    tables <- getTables
    sqlBackend <- ask
    let escapedTables = fmap (\t -> getEscapedRawName t sqlBackend) tables
        query = "TRUNCATE TABLE " <> intercalate ", " escapedTables
    rawExecute query []

getTables :: (MonadUnliftIO m) => ReaderT SqlBackend m [Text]
getTables = do
    tables <- rawSql [st|
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public'
        AND table_type = 'BASE TABLE';
    |] []
    return $ fmap unSingle tables
