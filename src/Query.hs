{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

module Query(
    findMilestoneStory,
    selectStoryMilestones,
    selectMilestoneStories,
) where

import Control.Monad.IO.Class (MonadIO)
import Data.Int (Int64)
import Database.Esqueleto.Experimental
import Model

-- | Find a link between a milestone and a story if it exists.
findMilestoneStory ::
    (MonadIO m) =>
    MilestoneId ->
    StoryId ->
    SqlPersistT m (Maybe (Entity MilestoneStory))
findMilestoneStory milestoneId storyId =
    selectOne $ do
        ms <- from $ table @MilestoneStory
        where_ $
            ms ^. MilestoneStoryMilestoneId ==. val milestoneId &&.
            ms ^. MilestoneStoryStoryId ==. val storyId
        return ms

-- | Select milestones linked to a story.
selectStoryMilestones ::
    (MonadIO m) =>
    StoryId ->
    Int64 ->
    SqlPersistT m [Entity Milestone]
selectStoryMilestones storyId maxRows =
    select $ do
        (m :& ms) <- from $
            table @Milestone
            `innerJoin`
            table @MilestoneStory
            `on` \(m :& ms) -> m ^. MilestoneId ==. ms ^. MilestoneStoryMilestoneId
        where_ $
            ms ^. MilestoneStoryStoryId ==. val storyId
        orderBy
            [desc $ m ^. MilestoneStartDate]
        limit
            maxRows
        return m

-- | Select stories linked to a milestone.
selectMilestoneStories ::
    (MonadIO m) =>
    MilestoneId ->
    Int64 ->
    SqlPersistT m [Entity Story]
selectMilestoneStories milestoneId maxRows =
    select $ do
        (s :& ms) <- from $
            table @Story
            `innerJoin`
            table @MilestoneStory
            `on` \(s :& ms) -> s ^. StoryId ==. ms ^. MilestoneStoryStoryId
        where_ $
            ms ^. MilestoneStoryMilestoneId ==. val milestoneId
        orderBy
            [asc $ s ^. StoryId]
        limit
            maxRows
        return s
