{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Spec.Framework (clientConfig, server, runServer) where

import Config (FrameworkClientConfig (FrameworkClientConfig, host, port, secret))
import Control.Monad (when)
import Data.Aeson (Object, Value (Array, Object))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap as KeyMap (fromList, singleton)
import Data.ByteString.Char8 (unpack)
import Data.Vector qualified as Vector (fromList)
import Network.Wai (Request (requestHeaders))
import Network.Wai.Handler.Warp qualified as Warp
import Servant
  ( AuthProtect,
    Context (EmptyContext, (:.)),
    Get,
    JSON,
    Post,
    Proxy (Proxy),
    Server,
    err401,
    errBody,
    serveWithContext,
    throwError,
    type (:<|>) ((:<|>)),
    type (:>),
  )
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)

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
  AuthProtect "api-key-auth"
    :> "connections"
    :> ( Get '[JSON] Object
           :<|> "create-invitation"
             :> Post '[JSON] Object
       )

asAesonObject :: [(Key, Value)] -> Value
asAesonObject = Object . KeyMap.fromList

server :: Server API
server () =
  return
    ( singleton
        "results"
        ( Array $
            Vector.fromList
              [ asAesonObject
                  [ ("state", "request"),
                    ("connection_id", "1234556678"),
                    ("their_label", "Alice")
                  ],
                asAesonObject
                  [ ("state", "active"),
                    ("connection_id", "8765431234"),
                    ("their_label", "Bob")
                  ]
              ]
        )
    )
    :<|> return (singleton "invitation_url" "my-url")

runServer :: Int -> IO ()
runServer frameworkPort = do
  putStrLn $ "Framework listening on port: " <> show frameworkPort
  Warp.run
    frameworkPort
    ( serveWithContext
        (Proxy :: Proxy API)
        (authHandler :. EmptyContext)
        server
    )

authHandler :: AuthHandler Request ()
authHandler = mkAuthHandler $ \req -> do
  case lookup "X-API-KEY" $ requestHeaders req of
    Nothing -> throwError err401 {errBody = "X-API-KEY header is required"}
    Just apiKey ->
      when (unpack apiKey /= adminSecret) $
        throwError err401 {errBody = "Invalid API key"}

type instance AuthServerData (AuthProtect "api-key-auth") = ()
