{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler where

import Dto
import Foundation
import Model
import Page
import qualified Query

import Control.Monad (when)
import Data.Maybe (isNothing)
import Database.Persist.Sql
import Yesod.Core
import Yesod.Persist.Core (get404, runDB)

-- | List a page of stories.
getStoriesR :: Handler Value
getStoriesR = do
    (pageSize, pageNumber, pageOffset) <- readPageParams
    stories <- runDB $ selectList [] [LimitTo pageSize, OffsetBy pageOffset, Asc StoryId]
    returnJson $
        pageDto pageSize pageNumber stories

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
    updated <- runDB $ do
        update storyId [StoryName =. storyName story, StoryPoints =. storyPoints story]
        get404 storyId
    returnJson $ storyDto storyId updated

-- | List a page of tasks for a story.
getTasksR :: StoryId -> Handler Value
getTasksR storyId = do
    (pageSize, pageNumber, pageOffset) <- readPageParams
    tasks <- runDB $ do
        selectList
            [TaskStoryId ==. storyId]
            [LimitTo pageSize, OffsetBy pageOffset, Asc TaskId]
    returnJson $
        pageDto pageSize pageNumber tasks

-- | Get a task.
getTaskR :: StoryId -> TaskId -> Handler Value
getTaskR storyId taskId = do
    task <- runDB $ get404 taskId
    validateTaskStoryId storyId task
    returnJson $ taskDto taskId task

-- | Delete a task.
deleteTaskR :: StoryId -> TaskId -> Handler ()
deleteTaskR storyId taskId = do
    task <- runDB $ get404 taskId
    validateTaskStoryId storyId task
    runDB $ delete taskId

-- | Create a task.
postTasksR :: StoryId -> Handler Value
postTasksR storyId = do
    task <- requireCheckJsonBody :: Handler Task
    validateTaskStoryId storyId task
    inserted <- runDB $ do
        _ <- get404 storyId
        insertEntity task
    returnJson inserted

-- | Update a task.
putTaskR :: StoryId -> TaskId -> Handler Value
putTaskR storyId taskId = do
    task <- requireCheckJsonBody :: Handler Task
    validateTaskStoryId storyId task
    updated <- runDB $ do
        _ <- get404 storyId
        update taskId [TaskName =. taskName task, TaskStatus =. taskStatus task, TaskStoryId =. storyId]
        get404 taskId
    returnJson $ taskDto taskId updated

-- | List a page of milestones.
getMilestonesR :: Handler Value
getMilestonesR = do
    (pageSize, pageNumber, pageOffset) <- readPageParams
    milestones <- runDB $ do
        selectList
            []
            [ LimitTo pageSize
            , OffsetBy pageOffset
            , Asc MilestoneStartDate
            , Desc MilestoneCompleteDate
            ]
    returnJson $
        pageDto pageSize pageNumber milestones

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
    updated <- runDB $ do
        update
            milestoneId
            [ MilestoneName =. milestoneName milestone
            , MilestoneStartDate =. milestoneStartDate milestone
            , MilestoneCompleteDate =. milestoneCompleteDate milestone
            ]
        get404 milestoneId
    returnJson $
        milestoneDto milestoneId updated

-- | Link a story to a milestone.
postMilestoneStoriesR :: MilestoneId -> Handler Value
postMilestoneStoriesR milestoneId = do
    milestoneStory <- requireCheckJsonBody :: Handler MilestoneStory

    when (milestoneId /= milestoneStoryMilestoneId milestoneStory) $
        invalidArgs
            ["MilestoneId mismatch: URI does not match request body"]

    entity <- runDB $ do
        let storyId = milestoneStoryStoryId milestoneStory
        maybeEntity <- Query.findMilestoneStory milestoneId storyId
        case maybeEntity of
            Just entity -> do
                $logWarn "Milestone story link already exists"
                pure entity
            Nothing -> do
                _ <- get404 milestoneId
                _ <- get404 storyId
                insertEntity milestoneStory

    let (Entity _ ms) = entity
    returnJson ms

-- | List all stories linked to a milestone.
getMilestoneStoriesR :: MilestoneId -> Handler Value
getMilestoneStoriesR milestoneId = do
    (pageSize, pageNumber, pageOffset) <- readPageParams
    let limit = fromIntegral pageSize
        offset = fromIntegral pageOffset
    stories <- runDB $ do
        _ <- get404 milestoneId
        Query.selectMilestoneStories milestoneId limit offset
    returnJson $
        pageDto pageSize pageNumber stories

-- | List all milestones linked to a story.
getStoryMilestonesR :: StoryId -> Handler Value
getStoryMilestonesR storyId = do
    (pageSize, pageNumber, pageOffset) <- readPageParams
    let limit = fromIntegral pageSize
        offset = fromIntegral pageOffset
    milestones <- runDB $ do
        _ <- get404 storyId
        Query.selectStoryMilestones storyId limit offset
    returnJson $
        pageDto pageSize pageNumber milestones

-- | Delete a link between a milestone and a story.
deleteMilestoneStoryR :: MilestoneId -> StoryId -> Handler ()
deleteMilestoneStoryR milestoneId storyId = do
    maybeEntity <- runDB $ Query.findMilestoneStory milestoneId storyId
    when (isNothing maybeEntity) notFound -- Milestone not linked to story
    runDB $
        deleteWhere
            [ MilestoneStoryMilestoneId ==. milestoneId
            , MilestoneStoryStoryId ==. storyId
            ]

-- | Validate that a story ID from the URI matches the story ID in a task.
validateTaskStoryId :: StoryId -> Task -> Handler ()
validateTaskStoryId storyId task =
    when (storyId /= taskStoryId task) $
        invalidArgs ["StoryId mismatch: URI does not match request body"]
