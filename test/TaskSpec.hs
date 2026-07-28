{-# LANGUAGE OverloadedStrings #-}

module TaskSpec (spec) where

import TestSupport
import Data.Aeson
import Data.Text (Text)
import Database.Persist.Sql (toSqlKey)

spec :: Spec
spec = withApp $ do
    describe "list tasks for a story" $ do
        it "returns 200" $ do
            storyId <- runDB $ insert $ Story "Test Story" 1
            request $ do
                setMethod "GET"
                setUrl $ StoryTasksR storyId
                addRequestHeader ("Accept", "application/json")
            statusIs 200

    describe "get task" $ do
        it "returns 200 when a task exists" $ do
            taskId <- runDB $ do
                sid <- insert $ Story "Test Story" 1
                tid <- insert $ Task sid "Test Task" Todo
                pure tid
            request $ do
                setMethod "GET"
                setUrl $ TaskR taskId
                addRequestHeader ("Accept", "application/json")
            statusIs 200

        it "returns 404 when a task does not exist" $ do
            request $ do
                setMethod "GET"
                setUrl $ TaskR (toSqlKey 0)
                addRequestHeader ("Accept", "application/json")
            statusIs 404

    describe "create task" $ do
        it "returns 200 when JSON body is valid" $ do
            storyId <- runDB $ insert $ Story "Test Story" 1
            let body = object
                    [ "name" .= ("Test Task" :: Text)
                    , "status" .= Todo
                    , "storyId" .= storyId
                    ]
            request $ do
                setMethod "POST"
                setUrl $ TasksR
                setRequestBody $ encode body
                addRequestHeader ("Content-Type", "application/json")
            statusIs 200

        it "returns 400 when JSON body is invalid" $ do
            let body = object [ "foo" .= ("Test Task" :: Value) ]
            request $ do
                setMethod "POST"
                setUrl $ TasksR
                setRequestBody $ encode body
                addRequestHeader ("Content-Type", "application/json")
            statusIs 400

    describe "update task" $ do
        it "returns 200 when JSON body is valid" $ do
            (storyId, taskId) <- runDB $ do
                sid <- insert $ Story "Test Story" 1
                tid <- insert $ Task sid "Test Task" Todo
                pure (sid, tid)
            let body = object
                    [ "storyId" .= storyId
                    , "name" .= ("Updated Task" :: Text)
                    , "status" .= Done
                    ]
            request $ do
                setMethod "PUT"
                setUrl $ TaskR taskId
                setRequestBody $ encode body
                addRequestHeader ("Content-Type", "application/json")
            statusIs 200

        it "returns 400 when JSON body is invalid" $ do
            taskId <- runDB $ do
                sid <- insert $ Story "Test Story" 1
                tid <- insert $ Task sid "Test Task" Todo
                pure tid
            let body = object [ "foo" .= ("Test Task" :: Value) ]
            request $ do
                setMethod "PUT"
                setUrl $ TaskR taskId
                setRequestBody $ encode body
                addRequestHeader ("Content-Type", "application/json")
            statusIs 400

    describe "delete task" $ do
        it "returns 200 when a task is deleted" $ do
            taskId <- runDB $ do
                sid <- insert $ Story "Test Story" 1
                tid <- insert $ Task sid "Test Task" Todo
                pure tid
            request $ do
                setMethod "DELETE"
                setUrl $ TaskR taskId
            statusIs 200

        it "returns 404 when a task does not exist" $ do
            request $ do
                setMethod "DELETE"
                setUrl $ TaskR (toSqlKey 0)
            statusIs 404
