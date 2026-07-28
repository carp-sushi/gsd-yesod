{-# LANGUAGE OverloadedStrings #-}

module Page (readPageParams) where

import Data.Text (Text, unpack)
import Foundation
import Text.Read (readMaybe)
import Yesod.Core

-- | Read page parameters from request query params.
readPageParams :: Handler (Int, Int, Int)
readPageParams = do
    pageSize <- readPageSize
    pageNumber <- readPageNumber
    let pageOffset = pageSize * (pageNumber - 1)
    pure (pageSize, pageNumber, pageOffset)

-- Read page size query param
readPageSize :: Handler Int
readPageSize = do
    param <- lookupGetParam "pageSize"
    pure $ parsePageSize param

-- Parse page size and clamp it within a set range.
parsePageSize :: Maybe Text -> Int
parsePageSize = clamp . parseInt
  where
    -- default page size
    clamp Nothing = 10
    -- page size must be a positive integer between 1 and 100
    clamp (Just n) = max 1 (min n 100)

-- Read page number query param
readPageNumber :: Handler Int
readPageNumber = do
    param <- lookupGetParam "pageNumber"
    pure $ parsePageNumber param

-- Parse page number and clamp it within a set range.
parsePageNumber :: Maybe Text -> Int
parsePageNumber = clamp . parseInt
  where
    -- default page number
    clamp Nothing = 1
    -- page number must be a positive integer >= 1
    clamp (Just n) = max n 1

-- Convert text to int if defined.
parseInt :: Maybe Text -> Maybe Int
parseInt mt =
    mt >>= readMaybe . unpack
