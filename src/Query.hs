{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

module Query where

import Control.Monad.IO.Class (MonadIO)
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
    SqlPersistT m [Entity Milestone]
selectStoryMilestones storyId =
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
        return m

-- | Select stories linked to a milestone.
selectMilestoneStories ::
    (MonadIO m) =>
    MilestoneId ->
    SqlPersistT m [Entity Story]
selectMilestoneStories milestoneId =
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
        return s
