{-# LANGUAGE OverloadedStrings #-}

module Page where

import Data.String.Conversions (cs)
import Data.Text (Text)
import Text.Read (readMaybe)
import Yesod.Core

import Foundation

-- | Read query limit and offset from request query params.
readLimitOffsetParams :: Handler (Int, Int)
readLimitOffsetParams = do
    maybePageSize <- lookupGetParam "pageSize"
    maybePageNumber <- lookupGetParam "pageNumber"
    let (pageSize, pageNumber) = readPageParams maybePageSize maybePageNumber
        offset = calculateOffset pageSize pageNumber
    return (pageSize, offset)

-- | Parse and get page size and page number.
readPageParams :: Maybe Text -> Maybe Text -> (Int, Int)
readPageParams maybeLimit maybeOffset =
    (getPageSize $ parseInt maybeLimit, getPageNumber $ parseInt maybeOffset)

-- | Calculate the query offset for a given page size and page number.
calculateOffset :: Int -> Int -> Int
calculateOffset pageSize pageNumber = pageSize * (pageNumber - 1)

-- Convert text page params to integers.
parseInt :: Maybe Text -> Maybe Int
parseInt (Just t) = readMaybe (cs t)
parseInt Nothing = Nothing

-- Get page limit or default
getPageSize :: Maybe Int -> Int
getPageSize Nothing = 10
getPageSize (Just limit) = max 1 (min limit 100)

-- Get page offset or default
getPageNumber :: Maybe Int -> Int
getPageNumber Nothing = 1
getPageNumber (Just offset) = max offset 1
