{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Spec.Framework (clientConfig, server, runServer) where

import Config (FrameworkClientConfig (FrameworkClientConfig, host, port, secret))
import Data.Aeson (Object)
import Data.Aeson.KeyMap (singleton)
import Network.Wai.Handler.Warp qualified as Warp
import Servant
  ( Header,
    JSON,
    Post,
    Proxy (Proxy),
    Server,
    err401,
    serve,
    throwError,
    type (:>),
  )

adminSecret :: String
adminSecret = "admin-secret"

clientConfig :: Int -> FrameworkClientConfig
clientConfig frameworkPort =
  FrameworkClientConfig
    { host = "127.0.0.1",
      port = frameworkPort,
      secret = adminSecret
    }

type API =
  "connections"
    :> "create-invitation"
    :> Header "X-API-KEY" String
    :> Post '[JSON] Object

server :: Server API
server Nothing = throwError err401
server (Just apiKey)
  | apiKey /= adminSecret = throwError err401
  | otherwise =
      return $ singleton "invitation_url" "my-url"

runServer :: Int -> IO ()
runServer frameworkPort = do
  putStrLn $ "Framework listening on port: " <> show frameworkPort
  Warp.run frameworkPort (serve (Proxy :: Proxy API) server)
