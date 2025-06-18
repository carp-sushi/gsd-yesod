{-# LANGUAGE OverloadedStrings #-}

module MilestoneSpec (spec) where

import TestSupport
import Data.Aeson
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)

spec :: Spec
spec = withApp $ do
    describe "list milestones" $ do
        it "returns 200" $ do
            request $ do
                setMethod "GET"
                setUrl MilestonesR
                addRequestHeader ("Accept", "application/json")
            statusIs 200

    describe "get milestone" $ do
        it "returns 200" $ do
            startDate <- liftIO $ getCurrentTime
            milestoneId <- runDB $ insert $
                Milestone "Test Milestone" (Just startDate) Nothing
            request $ do
                setMethod "GET"
                setUrl $ MilestoneR milestoneId
                addRequestHeader ("Accept", "application/json")
            statusIs 200

    describe "create milestone" $ do
        it "returns 200 when JSON body is valid" $ do
            let body = object [ "name" .= ("Test Milestone" :: Text) ]
            request $ do
                setMethod "POST"
                setUrl MilestonesR
                setRequestBody $ encode body
                addRequestHeader ("Content-Type", "application/json")
            statusIs 200

        it "returns 400 when JSON body is invalid" $ do
            let body = object [ "foo" .= ("Test Milestone" :: Value) ]
            request $ do
                setMethod "POST"
                setUrl MilestonesR
                setRequestBody $ encode body
                addRequestHeader ("Content-Type", "application/json")
            statusIs 400

    describe "update milestone" $ do
        it "returns 200 when JSON body is valid" $ do
            milestoneId <- runDB $ insert $ Milestone "Test Milestone" Nothing Nothing
            let body = object ["name" .= ("Updated Milestone" :: Text)]
            request $ do
                setMethod "PUT"
                setUrl $ MilestoneR milestoneId
                setRequestBody $ encode body
                addRequestHeader ("Content-Type", "application/json")
            statusIs 200

        it "returns 400 when JSON body is invalid" $ do
            milestoneId <- runDB $ insert $ Milestone "Test Milestone" Nothing Nothing
            let body = object [ "foo" .= ("Test Milestone" :: Value) ]
            request $ do
                setMethod "PUT"
                setUrl $ MilestoneR milestoneId
                setRequestBody $ encode body
                addRequestHeader ("Content-Type", "application/json")
            statusIs 400

    describe "delete milestone" $ do
        it "returns 200" $ do
            startDate <- liftIO $ getCurrentTime
            milestoneId <- runDB $ insert $
                Milestone "Test Milestone" (Just startDate) Nothing
            request $ do
                setMethod "DELETE"
                setUrl $ MilestoneR milestoneId
            statusIs 200
