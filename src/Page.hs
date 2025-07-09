{-# LANGUAGE OverloadedStrings #-}

module Page (
    readLimitParam,
    readPageParams,
) where

import Data.Text (Text, unpack)
import Foundation
import Text.Read (readMaybe)
import Yesod.Core

-- | Read an optional limit query param as an integer. If not provided, default to 100.
readLimitParam :: Handler Int
readLimitParam = do
    param <- lookupGetParam "limit"
    return $ (clamp . parseInt) param
  where
    clamp Nothing = 100
    clamp (Just n) = max 1 (min n 1000)

-- | Read page parameters from request query params.
readPageParams :: Handler (Int, Int, Int)
readPageParams = do
    psParam <- lookupGetParam "pageSize"
    pnParam <- lookupGetParam "pageNumber"
    let pageSize = parsePageSize psParam
        pageNumber = parsePageNumber pnParam
        offset = pageSize * (pageNumber - 1)
    return (pageSize, pageNumber, offset)

-- Parse page size and clamp it within a set range.
parsePageSize :: Maybe Text -> Int
parsePageSize = clamp . parseInt
  where
    clamp Nothing = 10
    clamp (Just n) = max 1 (min n 100)

-- Parse page number and clamp it within a set range.
parsePageNumber :: Maybe Text -> Int
parsePageNumber = clamp . parseInt
  where
    clamp Nothing = 1
    clamp (Just n) = max n 1

-- Convert text to int if defined.
parseInt :: Maybe Text -> Maybe Int
parseInt mt =
    mt >>= readMaybe . unpack
