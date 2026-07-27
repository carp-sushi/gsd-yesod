{-# LANGUAGE OverloadedStrings #-}

module StorySpec (spec) where

import TestSupport
import Data.Aeson
import Data.Text (Text)
import Database.Persist.Sql (toSqlKey)

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
        it "returns 200 when a story exists" $ do
            storyId <- runDB $ insert $ Story "Test Story" 1
            request $ do
                setMethod "GET"
                setUrl $ StoryR storyId
                addRequestHeader ("Accept", "application/json")
            statusIs 200

        it "returns 404 when a story does not exist" $ do
            request $ do
                setMethod "GET"
                setUrl $ StoryR (toSqlKey 0)
                addRequestHeader ("Accept", "application/json")
            statusIs 404

    describe "create story" $ do
        it "returns 200 when JSON body is valid" $ do
            let body = object [ "name" .= ("Test Story" :: Text), "points" .= (1 :: Int) ]
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
            storyId <- runDB $ insert $ Story "Test Story" 1
            let body = object ["name" .= ("Updated Story" :: Text), "points" .= (2 :: Int) ]
            request $ do
                setMethod "PUT"
                setUrl $ StoryR storyId
                setRequestBody $ encode body
                addRequestHeader ("Content-Type", "application/json")
            statusIs 200

        it "returns 400 when JSON body is invalid" $ do
            storyId <- runDB $ insert $ Story "Test Story" 1
            let body = object [ "foo" .= ("Test Story" :: Value) ]
            request $ do
                setMethod "PUT"
                setUrl $ StoryR storyId
                setRequestBody $ encode body
                addRequestHeader ("Content-Type", "application/json")
            statusIs 400

    describe "delete story" $ do
        it "returns 200 when a story is deleted" $ do
            storyId <- runDB $ insert $ Story "Test Story" 1
            request $ do
                setMethod "DELETE"
                setUrl $ StoryR storyId
            statusIs 200

        it "returns 404 when a story does not exist" $ do
            request $ do
                setMethod "DELETE"
                setUrl $ StoryR (toSqlKey 0)
            statusIs 404
