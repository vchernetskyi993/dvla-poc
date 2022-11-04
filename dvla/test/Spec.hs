{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Data.Functor ((<&>))
import Lib (app)
import Network.Wai (Application)
import Spec.Framework (clientConfig, runServer)
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
  )
import Test.Hspec.Wai (get, post, shouldRespondWith)
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
