{-# LANGUAGE OverloadedStrings #-}

module Page (
    pageJson,
    readPageParams,
) where

import Data.Text (Text, unpack)
import Foundation
import Text.Read (readMaybe)
import Yesod.Core

-- | Create a page JSON object.
pageJson :: (ToJSON a) => Int -> Int -> [a] -> Value
pageJson pageSize pageNumber pageData =
    object
        [ "pageSize" .= pageSize
        , "pageNumber" .= pageNumber
        , "nextPageNumber" .= (pageNumber + 1)
        , "pageData" .= pageData
        ]

-- | Read SQL limit and offset from request query params.
readPageParams :: Handler (Int, Int, Int)
readPageParams = do
    maybePageSize <- lookupGetParam "pageSize"
    maybePageNumber <- lookupGetParam "pageNumber"
    let pageSize = parsePageSize maybePageSize
        pageNumber = parsePageNumber maybePageNumber
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
