{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
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
    result <- runDB $
        select $ do
            (s :& ms) <-
                from $
                    table @Story
                        `InnerJoin` table @MilestoneStory
                            `on` do \(s :& ms) -> (s ^. StoryId ==. ms ^. MilestoneStoryStoryId)
            where_ $ ms ^. MilestoneStoryMilestoneId ==. val milestoneId
            pure (s, ms)
    returnJson $ fmap fst result
