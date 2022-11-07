{-# LANGUAGE DataKinds #-}
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
  )
where

import Api (Results)
import Config (FrameworkClientConfig (port), host, secret)
import Data.Aeson
  ( FromJSON (parseJSON),
    Object,
    Value,
    withObject,
    (.:),
  )
import Data.Aeson.Types (Parser)
import Data.ByteString.Char8 (pack)
import Data.String (IsString (fromString))
import Network.HTTP.Client
  ( defaultManagerSettings,
    managerModifyRequest,
    newManager,
    requestHeaders,
  )
import Servant (Get, JSON, Post, Proxy (..), type (:<|>) ((:<|>)), type (:>))
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
       )

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
(createInvitation :<|> getConnections) = client api

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
