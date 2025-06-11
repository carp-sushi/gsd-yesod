{-# LANGUAGE OverloadedStrings #-}

module Handler where

import Control.Monad (when)
import Database.Persist.Sql
import Foundation
import Model
import Page (getPageParams)
import Yesod.Core
import Yesod.Persist.Core (get404, runDB)

-- | List a page of stories.
getStoriesR :: Handler Value
getStoriesR = do
    maybeLimit <- lookupGetParam "limit"
    maybeOffset <- lookupGetParam "offset"
    let (limit, offset) = getPageParams maybeLimit maybeOffset
    stories <- runDB $ selectList [] [LimitTo limit, OffsetBy offset, Desc StoryId]
    returnJson stories

-- | Get a story by entity key.
getStoryR :: StoryId -> Handler Value
getStoryR storyId = do
    story <- runDB $ get404 storyId
    returnJson $ storyDto storyId story

-- | Delete a story by entity key.
deleteStoryR :: StoryId -> Handler ()
deleteStoryR storyId = do
    runDB $ delete storyId

-- | Create a new story.
postStoriesR :: Handler Value
postStoriesR = do
    story <- (requireCheckJsonBody :: Handler Story)
    inserted <- runDB $ insertEntity story
    returnJson inserted

-- | Update a story.
putStoryR :: StoryId -> Handler Value
putStoryR storyId = do
    story <- (requireCheckJsonBody :: Handler Story)
    runDB $ update storyId [StoryName =. storyName story]
    returnJson $ storyDto storyId story

-- | Create a JSON data transfer object for a story.
storyDto :: StoryId -> Story -> Value
storyDto storyId (Story name) =
    object ["id" .= toJSON storyId, "name" .= toJSON name]

-- | Get tasks for a story.
getTasksR :: StoryId -> Handler Value
getTasksR storyId = do
    maybeLimit <- lookupGetParam "limit"
    maybeOffset <- lookupGetParam "offset"
    let (limit, offset) = getPageParams maybeLimit maybeOffset
    tasks <- runDB $ selectList [TaskStoryId ==. storyId] [LimitTo limit, OffsetBy offset]
    returnJson tasks

-- | Get a story by entity key.
getTaskR :: StoryId -> TaskId -> Handler Value
getTaskR storyId taskId = do
    task <- runDB $ get404 taskId
    when (storyId /= taskStoryId task) $ invalidArgs ["Task story mismatch"]
    returnJson $ taskDto taskId task

-- | Delete a story by entity key.
deleteTaskR :: StoryId -> TaskId -> Handler ()
deleteTaskR storyId taskId = do
    task <- runDB $ get404 taskId
    when (storyId /= taskStoryId task) $ invalidArgs ["Task story mismatch"]
    runDB $ delete taskId

-- | Create a new task.
postTasksR :: StoryId -> Handler Value
postTasksR storyId = do
    task <- (requireCheckJsonBody :: Handler Task)
    when (storyId /= taskStoryId task) $ invalidArgs ["Task story mismatch"]
    inserted <- runDB $ insertEntity task
    returnJson inserted

-- | Update a task.
putTaskR :: StoryId -> TaskId -> Handler Value
putTaskR storyId taskId = do
    task <- (requireCheckJsonBody :: Handler Task)
    when (storyId /= taskStoryId task) $ invalidArgs ["Task story mismatch"]
    runDB $ update taskId [TaskName =. taskName task, TaskStatus =. taskStatus task]
    returnJson $ taskDto taskId task

-- | Create a JSON data transfer object for a story.
taskDto :: TaskId -> Task -> Value
taskDto taskId (Task storyId name status) =
    object
        [ "id" .= toJSON taskId
        , "name" .= toJSON name
        , "status" .= toJSON status
        , "storyId" .= toJSON storyId
        ]
