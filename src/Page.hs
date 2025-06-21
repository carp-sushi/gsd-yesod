{-# LANGUAGE OverloadedStrings #-}

module Page where

import Data.String.Conversions (cs)
import Data.Text (Text)
import Foundation
import Text.Read (readMaybe)
import Yesod.Core

-- | Read SQL limit and offset from request query params.
readPageParams :: Handler (Int, Int)
readPageParams = do
    maybePageSize <- lookupGetParam "pageSize"
    maybePageNumber <- lookupGetParam "pageNumber"
    let (pageSize, pageNumber) = parsePageParams maybePageSize maybePageNumber
    return (pageSize, pageSize * (pageNumber - 1))

-- | Parse and get page size and page number.
parsePageParams :: Maybe Text -> Maybe Text -> (Int, Int)
parsePageParams maybePageSize maybePageNumber =
    ( clampPageSize $ parseInt maybePageSize
    , clampPageNumber $ parseInt maybePageNumber
    )

-- Convert text page params to integers.
parseInt :: Maybe Text -> Maybe Int
parseInt (Just t) = readMaybe (cs t)
parseInt Nothing = Nothing

-- Determine page size and clamp it within a set range.
clampPageSize :: Maybe Int -> Int
clampPageSize Nothing = 10
clampPageSize (Just pageSize) = max 1 (min pageSize 100)

-- Determine page number and clamp it within a set range.
clampPageNumber :: Maybe Int -> Int
clampPageNumber Nothing = 1
clampPageNumber (Just pageNumber) = max pageNumber 1
