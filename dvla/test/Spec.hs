{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Map (empty)
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.UUID.V4 (nextRandom)
import Lib (app)
import Network.HTTP.Types (methodPost)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Application)
import Spec.Framework (State (State), clientConfig, getReceivedMessage, runServer)
import Spec.Util (getFreePort)
import Test.Hspec
  ( ActionWith,
    Spec,
    aroundAllWith,
    beforeAll,
    beforeAllWith,
    describe,
    hspec,
    it,
    shouldBe,
  )
import Test.Hspec.Wai (get, post, request, shouldRespondWith, getState)
import Test.Hspec.Wai.JSON (json)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  beforeAll getFreePort $
    beforeAllWith initialState $
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
withApplication (port, state) = app (clientConfig port) <&> (state,)

initialState :: Int -> IO (Int, State)
initialState port = empty <&> State <&> (port,)
