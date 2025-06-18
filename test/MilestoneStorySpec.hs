{-# LANGUAGE OverloadedStrings #-}

module MilestoneStorySpec (spec) where

import TestSupport
import Data.Aeson
import Data.Time.Clock (getCurrentTime)

spec :: Spec
spec = withApp $ do
    describe "link milestone to story" $ do
        it "returns 200 when JSON body is valid" $ do
            startDate <- liftIO $ getCurrentTime
            (milestoneId, storyId) <- runDB $ do
                mid <- insert $ Milestone "Test Milestone" (Just startDate) Nothing
                sid <- insert $ Story "Test Story"
                return (mid, sid)
            let body = object
                    [ "milestoneId" .= milestoneId
                    , "storyId" .= storyId
                    ]
            request $ do
                setMethod "POST"
                setUrl $ MilestoneStoriesR milestoneId
                setRequestBody $ encode body
                addRequestHeader ("Content-Type", "application/json")
            statusIs 200

    describe "delete milestone story link" $ do
        it "returns 200" $ do
            (milestoneId, storyId) <- runDB $ do
                mid <- insert $ Milestone "Test Milestone" Nothing Nothing
                sid <- insert $ Story "Test Story"
                _ <- insert $ MilestoneStory mid sid
                return (mid, sid)
            request $ do
                setMethod "DELETE"
                setUrl $ MilestoneStoryR milestoneId storyId
                addRequestHeader ("Accept", "application/json")
            statusIs 200
