{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler where

import Dto (milestoneDto, storyDto, taskDto)
import Foundation
import Model
import Page (readLimitOffsetParams)
import qualified Query as Q

import Control.Monad (when)
import Data.Maybe (isNothing)
import Database.Persist.Sql
import Yesod.Core
import Yesod.Persist.Core (get404, runDB)

-- | List a page of stories.
getStoriesR :: Handler Value
getStoriesR = do
    (limit, offset) <- readLimitOffsetParams
    stories <- runDB $ selectList [] [LimitTo limit, OffsetBy offset, Asc StoryId]
    returnJson stories

-- | Get a story.
getStoryR :: StoryId -> Handler Value
getStoryR storyId =
    runDB (get404 storyId)
        >>= returnJson . storyDto storyId

-- | Delete a story and any relations.
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
    story <- requireCheckJsonBody :: Handler Story
    inserted <- runDB $ insertEntity story
    returnJson inserted

-- | Update a story.
putStoryR :: StoryId -> Handler Value
putStoryR storyId = do
    story <- requireCheckJsonBody :: Handler Story
    runDB $ update storyId [StoryName =. storyName story]
    returnJson $ storyDto storyId story

-- | List a page of tasks for a story.
getTasksR :: StoryId -> Handler Value
getTasksR storyId = do
    (limit, offset) <- readLimitOffsetParams
    tasks <- runDB $ selectList [TaskStoryId ==. storyId] [LimitTo limit, OffsetBy offset]
    returnJson tasks

-- | Get a task.
getTaskR :: StoryId -> TaskId -> Handler Value
getTaskR storyId taskId = do
    task <- runDB $ get404 taskId
    validateStoryId storyId task
    returnJson $ taskDto taskId task

-- | Delete a task.
deleteTaskR :: StoryId -> TaskId -> Handler ()
deleteTaskR storyId taskId = do
    task <- runDB $ get404 taskId
    validateStoryId storyId task
    runDB $ delete taskId

-- | Create a task.
postTasksR :: StoryId -> Handler Value
postTasksR storyId = do
    task <- requireCheckJsonBody :: Handler Task
    validateStoryId storyId task
    inserted <- runDB $ insertEntity task
    returnJson inserted

-- | Update a task.
putTaskR :: StoryId -> TaskId -> Handler Value
putTaskR storyId taskId = do
    task <- requireCheckJsonBody :: Handler Task
    validateStoryId storyId task
    runDB $ update taskId [TaskName =. taskName task, TaskStatus =. taskStatus task]
    returnJson $ taskDto taskId task

-- | List a page of milestones.
getMilestonesR :: Handler Value
getMilestonesR = do
    (limit, offset) <- readLimitOffsetParams
    milestones <- runDB $ selectList [] [LimitTo limit, OffsetBy offset, Asc MilestoneStartDate]
    returnJson milestones

-- | Get a milestone.
getMilestoneR :: MilestoneId -> Handler Value
getMilestoneR milestoneId =
    runDB (get404 milestoneId)
        >>= returnJson . milestoneDto milestoneId

-- | Create a milestone.
postMilestonesR :: Handler Value
postMilestonesR = do
    milestone <- requireCheckJsonBody :: Handler Milestone
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
    milestone <- requireCheckJsonBody :: Handler Milestone
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

-- | Link a story to a milestone.
postMilestoneStoriesR :: MilestoneId -> Handler Value
postMilestoneStoriesR milestoneId = do
    req <- requireCheckJsonBody :: Handler MilestoneStory

    when (milestoneId /= milestoneStoryMilestoneId req) $
        invalidArgs ["MilestoneId mismatch: URI does not match request body"]

    let storyId = milestoneStoryStoryId req
    maybeLink <- runDB $ Q.findMilestoneStory milestoneId storyId

    case maybeLink of
        Just link -> do
            $logWarn "Milestone story link already exists"
            returnJson link
        Nothing -> do
            inserted <- runDB $ do
                _ <- get404 milestoneId
                _ <- get404 storyId
                insertEntity req
            returnJson inserted

-- | List all stories linked to a milestone.
getMilestoneStoriesR :: MilestoneId -> Handler Value
getMilestoneStoriesR milestoneId =
    runDB (Q.findMilestoneStories milestoneId)
        >>= returnJson

-- | List all milestones linked to a story.
getStoryMilestonesR :: StoryId -> Handler Value
getStoryMilestonesR storyId =
    runDB (Q.findStoryMilestones storyId)
        >>= returnJson

-- | Delete a link between a milestone and a story.
deleteMilestoneStoryR :: MilestoneId -> StoryId -> Handler ()
deleteMilestoneStoryR milestoneId storyId = do
    maybeLink <- runDB $ Q.findMilestoneStory milestoneId storyId
    when (isNothing maybeLink) $ invalidArgs ["Milestone not linked to story"]
    runDB $
        deleteWhere
            [ MilestoneStoryMilestoneId ==. milestoneId
            , MilestoneStoryStoryId ==. storyId
            ]

-- | Validate that a story ID from the URI matches the story ID in a task.
validateStoryId :: StoryId -> Task -> Handler ()
validateStoryId storyId task =
    when (storyId /= taskStoryId task) $
        invalidArgs ["StoryId mismatch: URI does not match request body"]
