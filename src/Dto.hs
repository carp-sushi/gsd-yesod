{-# LANGUAGE OverloadedStrings #-}

module Dto (
    pageDto,
    storyDto,
    taskDto,
    milestoneDto,
) where

import Data.Aeson
import Model

-- | Create a JSON data transfer object for a story.
storyDto :: StoryId -> Story -> Value
storyDto storyId (Story name) =
    object
        [ "id" .= storyId
        , "name" .= name
        ]

-- | Create a JSON data transfer object for a task.
taskDto :: TaskId -> Task -> Value
taskDto taskId (Task storyId name status) =
    object
        [ "id" .= taskId
        , "name" .= name
        , "status" .= status
        , "storyId" .= storyId
        ]

-- | Create a JSON data transfer object for a milestone.
milestoneDto :: MilestoneId -> Milestone -> Value
milestoneDto storyId (Milestone name startDate completeDate) =
    object
        [ "id" .= storyId
        , "name" .= name
        , "startDate" .= startDate
        , "completeDate" .= completeDate
        ]

-- | Create a page JSON object.
pageDto :: (ToJSON a) => Int -> Int -> [a] -> Value
pageDto pageSize pageNumber pageData =
    object
        [ "pageSize" .= pageSize
        , "pageNumber" .= pageNumber
        , "nextPageNumber" .= (pageNumber + 1)
        , "pageData" .= pageData
        ]
