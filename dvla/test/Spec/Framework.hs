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
    ToJSON (toJSONList),
    Value (Array, Object),
    encode,
    object,
    (.=),
  )
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap as KeyMap (fromList, singleton)
import Data.ByteString.Char8 (unpack)
import Data.Functor (void)
import Data.UUID (UUID, toString)
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
    createDefinitionTriggered :: MVar Int,
    definitionId :: UUID,
    credentialOffers :: MVar [Value],
    revocations :: MVar [Value]
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
             :> ( ReqBody '[JSON] Value
                    :> Post '[JSON] Value
                    :<|> "created" :> Get '[JSON] Value
                )
           :<|> "issue-credential"
             :> ( "send-offer"
                    :> ReqBody '[JSON] Value
                    :> PostNoContent
                    :<|> "records" :> Get '[JSON] Value
                )
           :<|> "revocation"
             :> "revoke"
             :> ReqBody '[JSON] Value
             :> PostNoContent
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
    :<|> (createDefinition state :<|> fetchDefinitions state)
    :<|> (issueCredential state :<|> fetchCredentials)
    :<|> revokeCredential state

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
createSchema
  State
    { createSchemaTriggered = createSchemaTriggered',
      schemaId = schemaId'
    }
  body = do
    liftIO $ modifyMVar_ createSchemaTriggered' $ pure . (+ 1)

    assertBody body expectedBody

    return $ object ["schema_id" .= toString schemaId']
    where
      expectedBody =
        object
          [ "attributes" .= (["first_name", "last_name", "category", "dob"] :: [String]),
            "schema_name" .= ("driver_license" :: String),
            "schema_version" .= ("1.0" :: String)
          ]

createDefinition :: State -> Value -> Handler Value
createDefinition
  State
    { createDefinitionTriggered = createDefinitionTriggered',
      schemaId = schemaId'
    }
  body = do
    liftIO $ modifyMVar_ createDefinitionTriggered' $ pure . (+ 1)

    assertBody body expectedBody

    return $ object []
    where
      expectedBody =
        object
          [ "schema_id" .= toString schemaId',
            "support_revocation" .= True,
            "revocation_registry_size" .= (100 :: Int)
          ]

fetchDefinitions :: State -> Handler Value
fetchDefinitions
  State
    { definitionId = definitionId'
    } =
    return $
      object
        [ "credential_definition_ids" .= toJSONList [toString definitionId']
        ]

issueCredential :: State -> Value -> Handler NoContent
issueCredential State {credentialOffers = credentialOffers'} body = do
  liftIO $ modifyMVar_ credentialOffers' $ pure . (body :)
  return NoContent

fetchCredentials :: Handler Value
fetchCredentials =
  return $
    object
      [ "results"
          .= [ object
                 [ "revoc_reg_id" .= ("qwetrtyui" :: String),
                   "revocation_id" .= ("1" :: String),
                   "connection_id" .= ("3333445566" :: String),
                   "credential_offer_dict"
                     .= object
                       [ "credential_preview"
                           .= object
                             [ "attributes"
                                 .= [ object
                                        [ "name" .= ("first_name" :: String),
                                          "value" .= ("Alice" :: String)
                                        ],
                                      object
                                        [ "name" .= ("last_name" :: String),
                                          "value" .= ("Doe" :: String)
                                        ],
                                      object
                                        [ "name" .= ("category" :: String),
                                          "value" .= ("B1" :: String)
                                        ],
                                      object
                                        [ "name" .= ("dob" :: String),
                                          "value" .= ("19891109" :: String)
                                        ]
                                    ]
                             ]
                       ]
                 ]
             ]
      ]

revokeCredential :: State -> Value -> Handler NoContent
revokeCredential State {revocations = revocations'} body = do
  liftIO $ modifyMVar_ revocations' $ pure . (body :)
  return NoContent

assertBody :: Value -> Value -> Handler ()
assertBody actual expected = when (actual /= expected) $ do
  liftIO $
    putStrLn $
      "Invalid body to create schema, expected: "
        <> show (encode expected)
        <> ", but got: "
        <> show (encode actual)
  throwError err400

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
