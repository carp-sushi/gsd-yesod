module Logger (makeAppLogger, makeRequestLogger) where

import Foundation (App (..))
import Network.Wai (Middleware)
import Network.Wai.Logger (clockDateCacher)
import Network.Wai.Middleware.RequestLogger (
    Destination (Logger),
    OutputFormat (Detailed),
    defaultRequestLoggerSettings,
    destination,
    mkRequestLogger,
    outputFormat,
 )
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet)
import qualified Yesod.Core.Types as YCT

-- Create request logging middleware.
makeRequestLogger :: App -> IO Middleware
makeRequestLogger app =
    mkRequestLogger
        defaultRequestLoggerSettings
            { outputFormat = Detailed False -- no colors
            , destination = Logger $ YCT.loggerSet $ appLogger app
            }

-- | Create a yesod logger from a fast-logger logger set.
makeAppLogger :: IO YCT.Logger
makeAppLogger = do
    setter <- newStdoutLoggerSet defaultBufSize
    (getter, _) <- clockDateCacher
    return $ YCT.Logger setter getter
