module Page (getPageParams) where

import Data.String.Conversions (cs)
import Data.Text (Text)
import Text.Read (readMaybe)

-- Helper to parse and get page limit and offset.
getPageParams :: Maybe Text -> Maybe Text -> (Int, Int)
getPageParams maybeLimit maybeOffset =
    (getLimit $ parseInt maybeLimit, getOffset $ parseInt maybeOffset)

-- Helper to convert text page params to integers.
parseInt :: Maybe Text -> Maybe Int
parseInt (Just t) = readMaybe (cs t)
parseInt Nothing = Nothing

-- Helper to get page limit or default
getLimit :: Maybe Int -> Int
getLimit Nothing = 10
getLimit (Just size) = max 1 (min size 100)

-- Helper to get page offset or default
getOffset :: Maybe Int -> Int
getOffset Nothing = 0
getOffset (Just offset) = max offset 0
