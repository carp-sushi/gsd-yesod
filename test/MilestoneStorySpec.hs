{-# LANGUAGE OverloadedStrings #-}

module MilestoneStorySpec (spec) where

import TestSupport
import Data.Aeson
import Data.Time.Clock (getCurrentTime)
import Database.Persist.Sql (toSqlKey)

spec :: Spec
spec = withApp $ do
    describe "link milestone to story" $ do
        it "returns 200 when JSON body is valid" $ do
            startDate <- liftIO $ getCurrentTime
            (milestoneId, storyId) <- runDB $ do
                mid <- insert $ Milestone "Test Milestone" (Just startDate) Nothing
                sid <- insert $ Story "Test Story" 1
                pure (mid, sid)
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
        it "returns 200 when a link exists" $ do
            (milestoneId, storyId) <- runDB $ do
                mid <- insert $ Milestone "Test Milestone" Nothing Nothing
                sid <- insert $ Story "Test Story" 1
                _ <- insert $ MilestoneStory mid sid
                pure (mid, sid)
            request $ do
                setMethod "DELETE"
                setUrl $ MilestoneStoryR milestoneId storyId
                addRequestHeader ("Accept", "application/json")
            statusIs 200

        it "returns 404 when a link does not exist" $ do
            request $ do
                setMethod "DELETE"
                setUrl $ MilestoneStoryR (toSqlKey 0) (toSqlKey 0)
                addRequestHeader ("Accept", "application/json")
            statusIs 404
