{-# LANGUAGE OverloadedStrings #-}

module StorySpec (spec) where

import TestSupport
import Data.Aeson
import Data.Text (Text)

spec :: Spec
spec = withApp $ do
    describe "list stories" $ do
        it "returns 200" $ do
            request $ do
                setMethod "GET"
                setUrl StoriesR
                addRequestHeader ("Accept", "application/json")
            statusIs 200

    describe "get story" $ do
        it "returns 200" $ do
            storyId <- runDB $ insert $ Story "Test Story"
            request $ do
                setMethod "GET"
                setUrl $ StoryR storyId
                addRequestHeader ("Accept", "application/json")
            statusIs 200

    describe "create story" $ do
        it "returns 200 when JSON body is valid" $ do
            let body = object [ "name" .= ("Test Story" :: Text) ]
            request $ do
                setMethod "POST"
                setUrl StoriesR
                setRequestBody $ encode body
                addRequestHeader ("Content-Type", "application/json")
            statusIs 200

        it "returns 400 when JSON body is invalid" $ do
            let body = object [ "foo" .= ("Test Story" :: Value) ]
            request $ do
                setMethod "POST"
                setUrl StoriesR
                setRequestBody $ encode body
                addRequestHeader ("Content-Type", "application/json")
            statusIs 400

    describe "update story" $ do
        it "returns 200 when JSON body is valid" $ do
            storyId <- runDB $ insert $ Story "Test Story"
            let body = object ["name" .= ("Updated Story" :: Text)]
            request $ do
                setMethod "PUT"
                setUrl $ StoryR storyId
                setRequestBody $ encode body
                addRequestHeader ("Content-Type", "application/json")
            statusIs 200

        it "returns 400 when JSON body is invalid" $ do
            storyId <- runDB $ insert $ Story "Test Story"
            let body = object [ "foo" .= ("Test Story" :: Value) ]
            request $ do
                setMethod "PUT"
                setUrl $ StoryR storyId
                setRequestBody $ encode body
                addRequestHeader ("Content-Type", "application/json")
            statusIs 400

    describe "delete story" $ do
        it "returns 200" $ do
            storyId <- runDB $ insert $ Story "Test Story"
            request $ do
                setMethod "DELETE"
                setUrl $ StoryR storyId
            statusIs 200
