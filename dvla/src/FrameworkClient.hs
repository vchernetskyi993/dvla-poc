{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module FrameworkClient
  ( createFrameworkClient,
    createInvitation,
    getConnections,
    Connection (..),
    ConnectionState (..),
    sendMessage,
    SendMessageBody (..),
  )
where

import Api (Results)
import Config (FrameworkClientConfig (port), host, secret)
import Data.Aeson
  ( FromJSON (parseJSON),
    Object,
    ToJSON,
    Value,
    withObject,
    (.:),
  )
import Data.Aeson.Types (Parser)
import Data.ByteString.Char8 (pack)
import Data.String (IsString (fromString))
import GHC.Generics (Generic)
import Network.HTTP.Client
  ( defaultManagerSettings,
    managerModifyRequest,
    newManager,
    requestHeaders,
  )
import Servant
  ( Capture,
    Get,
    JSON,
    NoContent,
    Post,
    PostNoContent,
    Proxy (..),
    ReqBody,
    type (:<|>) ((:<|>)),
    type (:>),
  )
import Servant.Client
  ( BaseUrl (BaseUrl),
    ClientEnv,
    ClientM,
    Scheme (Http),
    client,
    mkClientEnv,
  )

type API =
  "connections"
    :> ( "create-invitation" :> Post '[JSON] Object
           :<|> Get '[JSON] (Results Connection)
           :<|> Capture "connectionId" String
             :> "send-message"
             :> ReqBody '[JSON] SendMessageBody
             :> PostNoContent
       )

newtype SendMessageBody = SendMessageBody
  { content :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON SendMessageBody

data ConnectionState = Active | Other deriving (Eq, Show)

instance IsString ConnectionState where
  fromString :: String -> ConnectionState
  fromString "active" = Active
  fromString _ = Other

data Connection = Connection
  { connectionId :: String,
    name :: String,
    state :: ConnectionState
  }
  deriving (Eq, Show)

instance FromJSON Connection where
  parseJSON :: Value -> Parser Connection
  parseJSON = withObject "Connection" $ \obj -> do
    connectionId' <- obj .: "connection_id"
    label <- obj .: "their_label"
    state' <- obj .: "state"
    return
      Connection
        { connectionId = connectionId',
          name = label,
          state = fromString state'
        }

instance FromJSON (Results Connection)

api :: Proxy API
api = Proxy

createInvitation :: ClientM Object
getConnections :: ClientM (Results Connection)
sendMessage :: String -> SendMessageBody -> ClientM NoContent
(createInvitation :<|> getConnections :<|> sendMessage) = client api

createFrameworkClient :: FrameworkClientConfig -> IO ClientEnv
createFrameworkClient config = do
  manager <-
    newManager
      defaultManagerSettings
        { managerModifyRequest = \req ->
            return
              req
                { requestHeaders =
                    ("X-API-KEY", pack $ secret config) : requestHeaders req
                }
        }
  return $
    mkClientEnv
      manager
      (BaseUrl Http (host config) (port config) "")
