{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Concurrent (forkIO, killThread, newMVar, readMVar, swapMVar)
import Control.Concurrent.Map (empty)
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Data.Functor (void)
import Data.UUID.V4 (nextRandom)
import Lib (app)
import Network.HTTP.Types (methodPost)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Application)
import Spec.Framework
  ( State (State, createDefinitionTriggered, createSchemaTriggered),
    clientConfig,
    getReceivedMessage,
    runServer,
  )
import Spec.Util (getFreePort)
import Test.Hspec
  ( ActionWith,
    Spec,
    after,
    aroundAllWith,
    beforeAll,
    beforeAllWith,
    describe,
    hspec,
    it,
    shouldBe,
  )
import Test.Hspec.Wai (get, getState, post, request, shouldRespondWith)
import Test.Hspec.Wai.JSON (json)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  beforeAll ((,) <$> getFreePort <*> initialState) $
    after resetState $
      aroundAllWith withFramework $
        beforeAllWith withApplication $ do
          describe "POST /api/invitations" $
            it "should create invitation" $
              post "/api/invitations" "" `shouldRespondWith` [json|{url: "my-url"}|]

          describe "GET /api/connections" $
            it "should get active connections" $
              get "/api/connections"
                `shouldRespondWith` [json|{
                results: [
                  {
                    name: "Bob",
                    connectionId: "8765431234"
                  }
                ]
              }|]

          describe "POST /api/messages" $
            it "should send message" $ do
              -- given
              guid <- liftIO nextRandom
              message <- liftIO nextRandom

              -- when
              request
                methodPost
                "/api/messages"
                [(hContentType, "application/json")]
                [json|{connectionId: #{guid}, text: #{message}}|]
                `shouldRespondWith` 204

              -- then
              state <- getState
              receivedMessage <- liftIO $ getReceivedMessage state $ show guid
              liftIO $ receivedMessage `shouldBe` Just (show message)

          describe "POST /api/schemas" $
            it "should create new credential schema" $ do
              -- when
              post "/api/schemas" "" `shouldRespondWith` 204

              -- then
              state <- getState

              createSchemaTriggeredTimes <-
                liftIO $
                  readMVar $
                    createSchemaTriggered state
              liftIO $ createSchemaTriggeredTimes `shouldBe` 1

              createDefinitionTriggeredTimes <-
                liftIO $
                  readMVar $
                    createDefinitionTriggered state
              liftIO $ createDefinitionTriggeredTimes `shouldBe` 1

withFramework :: ActionWith (Int, State) -> ActionWith (Int, State)
withFramework action (port, state) =
  bracket
    (forkIO $ runServer port state)
    ( \threadId -> do
        putStrLn "Stopping framework..."
        killThread threadId
    )
    (const $ action (port, state))

withApplication :: (Int, State) -> IO (State, Application)
withApplication (port, state) = (state,) <$> app (clientConfig port)

initialState :: IO State
initialState =
  State
    <$> empty
    <*> nextRandom
    <*> newMVar 0
    <*> newMVar 0

resetState :: (Int, State) -> IO ()
resetState
  ( _,
    State
      { createSchemaTriggered = createSchemaTriggered',
        createDefinitionTriggered = createDefinitionTriggered'
      }
    ) =
    void $
      swapMVar createSchemaTriggered' 0
        >> swapMVar createDefinitionTriggered' 0
