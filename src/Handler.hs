{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler where

import Control.Monad (when)
import Data.Maybe (isNothing)
import Database.Persist.Sql
import Foundation
import Model
import Page (getPageParams)
import qualified Query as Q
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

-- | Get a story.
getStoryR :: StoryId -> Handler Value
getStoryR storyId =
    runDB (get404 storyId)
        >>= returnJson . storyDto storyId

-- | Delete a story and its tasks.
deleteStoryR :: StoryId -> Handler ()
deleteStoryR storyId = do
    runDB $ do
        _ <- get404 storyId
        deleteWhere [TaskStoryId ==. storyId]
        deleteWhere [MilestoneStoryStoryId ==. storyId]
        delete storyId

-- | Create a story.
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

-- | List a page of tasks for a story.
getTasksR :: StoryId -> Handler Value
getTasksR storyId = do
    maybeLimit <- lookupGetParam "limit"
    maybeOffset <- lookupGetParam "offset"
    let (limit, offset) = getPageParams maybeLimit maybeOffset
    tasks <- runDB $ selectList [TaskStoryId ==. storyId] [LimitTo limit, OffsetBy offset]
    returnJson tasks

-- | Get a task.
getTaskR :: StoryId -> TaskId -> Handler Value
getTaskR storyId taskId = do
    task <- runDB $ get404 taskId
    when (storyId /= taskStoryId task) $
        invalidArgs ["StoryId mismatch: URI does not match request body"]
    returnJson $ taskDto taskId task

-- | Delete a task.
deleteTaskR :: StoryId -> TaskId -> Handler ()
deleteTaskR storyId taskId = do
    task <- runDB $ get404 taskId
    when (storyId /= taskStoryId task) $
        invalidArgs ["StoryId mismatch: URI does not match request body"]
    runDB $ delete taskId

-- | Create a task.
postTasksR :: StoryId -> Handler Value
postTasksR storyId = do
    task <- (requireCheckJsonBody :: Handler Task)
    when (storyId /= taskStoryId task) $
        invalidArgs ["StoryId mismatch: URI does not match request body"]
    inserted <- runDB $ insertEntity task
    returnJson inserted

-- | Update a task.
putTaskR :: StoryId -> TaskId -> Handler Value
putTaskR storyId taskId = do
    task <- (requireCheckJsonBody :: Handler Task)
    when (storyId /= taskStoryId task) $
        invalidArgs ["StoryId mismatch: URI does not match request body"]
    runDB $ update taskId [TaskName =. taskName task, TaskStatus =. taskStatus task]
    returnJson $ taskDto taskId task

-- | Create a JSON data transfer object for a task.
taskDto :: TaskId -> Task -> Value
taskDto taskId (Task storyId name status) =
    object
        [ "id" .= toJSON taskId
        , "name" .= toJSON name
        , "status" .= toJSON status
        , "storyId" .= toJSON storyId
        ]

-- | List a page of milestones.
getMilestonesR :: Handler Value
getMilestonesR = do
    maybeLimit <- lookupGetParam "limit"
    maybeOffset <- lookupGetParam "offset"
    let (limit, offset) = getPageParams maybeLimit maybeOffset
    milestones <- runDB $ selectList [] [LimitTo limit, OffsetBy offset, Desc MilestoneStartDate]
    returnJson milestones

-- | Get a milestone.
getMilestoneR :: MilestoneId -> Handler Value
getMilestoneR milestoneId =
    runDB (get404 milestoneId)
        >>= returnJson . milestoneDto milestoneId

-- | Create a milestone.
postMilestonesR :: Handler Value
postMilestonesR = do
    milestone <- (requireCheckJsonBody :: Handler Milestone)
    inserted <- runDB $ insertEntity milestone
    returnJson inserted

-- | Delete a milestone and unlink any stories.
deleteMilestoneR :: MilestoneId -> Handler ()
deleteMilestoneR milestoneId = do
    runDB $ do
        _ <- get404 milestoneId
        deleteWhere [MilestoneStoryMilestoneId ==. milestoneId]
        delete milestoneId

-- | Update a milestone.
putMilestoneR :: MilestoneId -> Handler Value
putMilestoneR milestoneId = do
    milestone <- (requireCheckJsonBody :: Handler Milestone)
    runDB $ do
        _ <- get404 milestoneId
        update
            milestoneId
            [ MilestoneName =. milestoneName milestone
            , MilestoneStartDate =. milestoneStartDate milestone
            , MilestoneCompleteDate =. milestoneCompleteDate milestone
            ]
    returnJson $
        milestoneDto milestoneId milestone

-- | Create a JSON data transfer object for a milestone.
milestoneDto :: MilestoneId -> Milestone -> Value
milestoneDto storyId (Milestone name startDate completeDate) =
    object
        [ "id" .= toJSON storyId
        , "name" .= toJSON name
        , "startDate" .= toJSON startDate
        , "completeDate" .= toJSON completeDate
        ]

-- | Link a story to a milestone.
postMilestoneStoriesR :: MilestoneId -> Handler Value
postMilestoneStoriesR milestoneId = do
    req <- (requireCheckJsonBody :: Handler MilestoneStory)

    when (milestoneId /= milestoneStoryMilestoneId req) $
        invalidArgs ["MilestoneId mismatch: URI does not match request body"]

    maybeLink <-
        runDB $
            Q.findMilestoneStory
                (milestoneStoryMilestoneId req)
                (milestoneStoryStoryId req)

    case maybeLink of
        Just link -> do
            $(logWarn) "Milestone story link already exists"
            returnJson link
        Nothing -> do
            inserted <- runDB $ do
                _ <- get404 milestoneId
                _ <- get404 $ milestoneStoryStoryId req
                insertEntity req
            returnJson inserted

-- | List all stories for a milestone.
getMilestoneStoriesR :: MilestoneId -> Handler Value
getMilestoneStoriesR milestoneId =
    runDB (Q.findMilestoneStories milestoneId)
        >>= returnJson

-- | List all milestones a story is linked to.
getStoryMilestonesR :: StoryId -> Handler Value
getStoryMilestonesR storyId =
    runDB (Q.findStoryMilestones storyId)
        >>= returnJson

-- | Delete a link between a milestone and a story.
deleteMilestoneStoryR :: MilestoneId -> StoryId -> Handler ()
deleteMilestoneStoryR milestoneId storyId = do
    maybeLink <- runDB $ Q.findMilestoneStory milestoneId storyId
    when (isNothing maybeLink) $ notFound
    runDB $
        deleteWhere
            [ MilestoneStoryMilestoneId ==. milestoneId
            , MilestoneStoryStoryId ==. storyId
            ]
