{-# LANGUAGE OverloadedStrings #-}

module TaskSpec (spec) where

import TestSupport
import Data.Aeson
import Data.Text (Text)

spec :: Spec
spec = withApp $ do
    describe "list tasks" $ do
        it "returns 200" $ do
            storyId <- runDB $ insert $ Story "Test Story"
            request $ do
                setMethod "GET"
                setUrl $ TasksR storyId
                addRequestHeader ("Accept", "application/json")
            statusIs 200

    describe "get task" $ do
        it "returns 200" $ do
            (storyId, taskId) <- runDB $ do
                sid <- insert $ Story "Test Story"
                tid <- insert $ Task sid "Test Task" Todo
                return (sid, tid)
            request $ do
                setMethod "GET"
                setUrl $ TaskR storyId taskId
                addRequestHeader ("Accept", "application/json")
            statusIs 200

    describe "create task" $ do
        it "returns 200 when JSON body is valid" $ do
            storyId <- runDB $ insert $ Story "Test Story"
            let body = object
                    [ "name" .= ("Test Task" :: Text)
                    , "status" .= Todo
                    , "storyId" .= storyId
                    ]
            request $ do
                setMethod "POST"
                setUrl $ TasksR storyId
                setRequestBody $ encode body
                addRequestHeader ("Content-Type", "application/json")
            statusIs 200

        it "returns 400 when JSON body is invalid" $ do
            storyId <- runDB $ insert $ Story "Test Story"
            let body = object [ "foo" .= ("Test Task" :: Value) ]
            request $ do
                setMethod "POST"
                setUrl $ TasksR storyId
                setRequestBody $ encode body
                addRequestHeader ("Content-Type", "application/json")
            statusIs 400

    describe "update task" $ do
        it "returns 200 when JSON body is valid" $ do
            (storyId, taskId) <- runDB $ do
                sid <- insert $ Story "Test Story"
                tid <- insert $ Task sid "Test Task" Todo
                return (sid, tid)
            let body = object
                    [ "storyId" .= storyId
                    , "name" .= ("Updated Task" :: Text)
                    , "status" .= Done
                    ]
            request $ do
                setMethod "PUT"
                setUrl $ TaskR storyId taskId
                setRequestBody $ encode body
                addRequestHeader ("Content-Type", "application/json")
            statusIs 200

        it "returns 400 when JSON body is invalid" $ do
            (storyId, taskId) <- runDB $ do
                sid <- insert $ Story "Test Story"
                tid <- insert $ Task sid "Test Task" Todo
                return (sid, tid)
            let body = object [ "foo" .= ("Test Task" :: Value) ]
            request $ do
                setMethod "PUT"
                setUrl $ TaskR storyId taskId
                setRequestBody $ encode body
                addRequestHeader ("Content-Type", "application/json")
            statusIs 400

    describe "delete task" $ do
        it "returns 200" $ do
            (storyId, taskId) <- runDB $ do
                sid <- insert $ Story "Test Story"
                tid <- insert $ Task sid "Test Task" Todo
                return (sid, tid)
            request $ do
                setMethod "DELETE"
                setUrl $ TaskR storyId taskId
            statusIs 200
