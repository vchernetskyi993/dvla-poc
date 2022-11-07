{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.UUID.V4 (nextRandom)
import Lib (app)
import Network.HTTP.Types (methodPost)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Application)
import Spec.Framework (clientConfig, runServer, getReceivedMessage)
import Spec.Util (getFreePort)
import Test.Hspec
  ( ActionWith,
    Spec,
    aroundAllWith,
    beforeAll,
    beforeAllWith,
    describe,
    hspec,
    it, shouldBe,
  )
import Test.Hspec.Wai (get, post, request, shouldRespondWith)
import Test.Hspec.Wai.JSON (json)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  beforeAll getFreePort $
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
            receivedMessage <- liftIO $ getReceivedMessage $ show guid
            liftIO $ receivedMessage `shouldBe` Just (show message)

withFramework :: ActionWith Int -> ActionWith Int
withFramework action port =
  bracket
    (forkIO $ runServer port)
    ( \threadId -> do
        putStrLn "Stopping framework..."
        killThread threadId
    )
    (const $ action port)

withApplication :: Int -> IO ((), Application)
withApplication port = app (clientConfig port) <&> ((),)
