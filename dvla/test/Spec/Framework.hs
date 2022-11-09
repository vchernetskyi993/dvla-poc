{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Spec.Framework
  ( clientConfig,
    server,
    runServer,
    getReceivedMessage,
    State (..),
  )
where

import Config (FrameworkClientConfig (FrameworkClientConfig, host, port, secret))
import Control.Concurrent (MVar, modifyMVar_)
import Control.Concurrent.Map as CMap (Map, insert, lookup)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
  ( FromJSON,
    Object,
    Value (Array, Object),
    encode,
    object,
    (.=),
  )
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap as KeyMap (fromList, singleton)
import Data.ByteString.Char8 (unpack)
import Data.Functor (void)
import Data.UUID (UUID)
import Data.Vector qualified as Vector (fromList)
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
    err400,
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

data State = State
  { messages :: Map String String,
    schemaId :: UUID,
    createSchemaTriggered :: MVar Int,
    definitionId :: UUID,
    createDefinitionTriggered :: MVar Int
  }

type API =
  AuthProtect "api-key-auth"
    :> ( "connections"
           :> ( Get '[JSON] Object
                  :<|> "create-invitation"
                    :> Post '[JSON] Object
                  :<|> Capture "connectionId" String
                    :> "send-message"
                    :> ReqBody '[JSON] Message
                    :> PostNoContent
              )
           :<|> "schemas"
             :> ReqBody '[JSON] Value
             :> Post '[JSON] Value
           :<|> "credential-definitions"
             :> ReqBody '[JSON] Value
             :> Post '[JSON] Value
       )

newtype Message = Message {content :: String} deriving (Eq, Show, Generic)

instance FromJSON Message

asAesonObject :: [(Key, Value)] -> Value
asAesonObject = Object . KeyMap.fromList

server :: State -> Server API
server state () =
  ( return
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
      :<|> saveMessage state
  )
    :<|> createSchema state
    :<|> createDefinition

saveMessage :: State -> String -> Message -> Handler NoContent
saveMessage state connection (Message text) = do
  liftIO $
    putStrLn $
      "Test framework received message: {\"connection\":\""
        <> connection
        <> "\",\"text\":\""
        <> text
        <> "\"}"
  void $ liftIO $ insert connection text $ messages state
  return NoContent

getReceivedMessage :: State -> String -> IO (Maybe String)
getReceivedMessage state connection =
  CMap.lookup connection $ messages state

createSchema :: State -> Value -> Handler Value
createSchema State {createSchemaTriggered = createSchemaTriggered'} body = do
  liftIO $ modifyMVar_ createSchemaTriggered' $ pure . (+ 1)

  when (body /= expectedBody) $ do
    liftIO $
      putStrLn $
        "Invalid body to create schema, expected: "
          <> show (encode expectedBody)
          <> ", but got: "
          <> show (encode body)
    throwError err400

  return $ object []
  where
    expectedBody =
      object
        [ "attributes" .= (["firstName", "lastName", "category"] :: [String]),
          "schema_name" .= ("driver license" :: String),
          "schema_version" .= ("1.0" :: String)
        ]

createDefinition :: Value -> Handler Value
createDefinition _body = return $ object []

runServer :: Int -> State -> IO ()
runServer frameworkPort state = do
  putStrLn $ "Framework listening on port: " <> show frameworkPort
  Warp.run
    frameworkPort
    ( serveWithContext
        (Proxy :: Proxy API)
        (authHandler :. EmptyContext)
        $ server state
    )

authHandler :: AuthHandler Request ()
authHandler = mkAuthHandler $ \req -> do
  case Prelude.lookup "X-API-KEY" $ requestHeaders req of
    Nothing -> throwError err401 {errBody = "X-API-KEY header is required"}
    Just apiKey ->
      when (unpack apiKey /= adminSecret) $
        throwError err401 {errBody = "Invalid API key"}

type instance AuthServerData (AuthProtect "api-key-auth") = ()
