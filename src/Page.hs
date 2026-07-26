{-# LANGUAGE OverloadedStrings #-}

module Page (readPageParams) where

import Data.Text (Text, unpack)
import Foundation
import Text.Read (readMaybe)
import Yesod.Core

-- | Read page parameters from request query params.
readPageParams :: Handler (Int, Int, Int)
readPageParams = do
    psParam <- lookupGetParam "pageSize"
    pnParam <- lookupGetParam "pageNumber"
    let pageSize = parsePageSize psParam
        pageNumber = parsePageNumber pnParam
        pageOffset = pageSize * (pageNumber - 1)
    pure (pageSize, pageNumber, pageOffset)

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
