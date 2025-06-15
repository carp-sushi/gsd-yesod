{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

module JoinHandler where

import Database.Esqueleto.Experimental hiding (Value)
import Foundation
import Model
import Yesod.Core
import Yesod.Persist.Core (runDB)

-- | List all stories for a milestone.
getMilestoneStoriesR :: MilestoneId -> Handler Value
getMilestoneStoriesR milestoneId = do
    stories <- runDB $
        select $ do
            (s :& ms) <- from $
                table @Story `innerJoin` table @MilestoneStory `on` do
                    \(s :& ms) -> s ^. StoryId ==. ms ^. MilestoneStoryStoryId
            where_ $
                ms ^. MilestoneStoryMilestoneId ==. val milestoneId
            pure s
    returnJson stories

-- | List all milestones a story is linked to.
getStoryMilestonesR :: StoryId -> Handler Value
getStoryMilestonesR storyId = do
    milestones <- runDB $
        select $ do
            (m :& ms) <- from $
                table @Milestone `innerJoin` table @MilestoneStory `on` do
                    \(m :& ms) -> m ^. MilestoneId ==. ms ^. MilestoneStoryMilestoneId
            where_ $
                ms ^. MilestoneStoryStoryId ==. val storyId
            pure m
    returnJson milestones
