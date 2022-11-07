{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Spec.Framework (clientConfig, server, runServer, getReceivedMessage) where

import Config (FrameworkClientConfig (FrameworkClientConfig, host, port, secret))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, Object, Value (Array, Object))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap as KeyMap (fromList, singleton)
import Data.ByteString.Char8 (unpack)
import Data.Vector qualified as Vector (fromList)
import GHC.Conc (atomically)
import GHC.Generics (Generic)
import Network.Wai (Request (requestHeaders))
import Network.Wai.Handler.Warp qualified as Warp
import Servant
  ( AuthProtect,
    Capture,
    Context (EmptyContext, (:.)),
    Get,
    Handler,
    JSON,
    NoContent (NoContent),
    Post,
    PostNoContent,
    Proxy (Proxy),
    ReqBody,
    Server,
    err401,
    errBody,
    serveWithContext,
    throwError,
    type (:<|>) ((:<|>)),
    type (:>),
  )
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import StmContainers.Map as StmMap (Map, insert, lookup, newIO)

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
           :<|> Capture "connectionId" String
             :> "send-message"
             :> ReqBody '[JSON] Message
             :> PostNoContent
       )

newtype Message = Message {content :: String} deriving (Eq, Show, Generic)

instance FromJSON Message

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
    :<|> saveMessage

messages :: IO (Map String String)
messages = newIO

saveMessage :: String -> Message -> Handler NoContent
saveMessage connection (Message text) = do
  existingMessages <- liftIO messages
  liftIO $ atomically $ insert text connection existingMessages
  return NoContent

getReceivedMessage :: String -> IO (Maybe String)
getReceivedMessage connection = do
  existingMessages <- messages
  atomically $ StmMap.lookup connection existingMessages

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
  case Prelude.lookup "X-API-KEY" $ requestHeaders req of
    Nothing -> throwError err401 {errBody = "X-API-KEY header is required"}
    Just apiKey ->
      when (unpack apiKey /= adminSecret) $
        throwError err401 {errBody = "Invalid API key"}

type instance AuthServerData (AuthProtect "api-key-auth") = ()
