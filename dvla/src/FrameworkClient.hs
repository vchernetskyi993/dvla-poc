{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
    createSchema,
    Schema (..),
    SchemaId (..),
    createDefinition,
    fetchDefinitionIds,
    CredentialDefinitionIds (..),
    issueCredential,
    CredentialOffer (..),
    CredentialPreview (..),
    Attribute (..),
  )
where

import Api (Results)
import Config (FrameworkClientConfig (port), host, secret)
import Data.Aeson
  ( FromJSON (parseJSON),
    Object,
    ToJSON (toJSON),
    Value,
    object,
    withObject,
    (.:),
    (.=),
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
    :<|> "schemas" :> ReqBody '[JSON] Schema :> Post '[JSON] SchemaId
    :<|> "credential-definitions"
      :> ( ReqBody '[JSON] SchemaId :> PostNoContent
             :<|> "created" :> Get '[JSON] CredentialDefinitionIds
         )
    :<|> "issue-credential"
      :> "send-offer"
      :> ReqBody '[JSON] CredentialOffer
      :> PostNoContent

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
createSchema :: Schema -> ClientM SchemaId
createDefinition :: SchemaId -> ClientM NoContent
fetchDefinitionIds :: ClientM CredentialDefinitionIds
issueCredential :: CredentialOffer -> ClientM NoContent
( (createInvitation :<|> getConnections :<|> sendMessage)
    :<|> createSchema
    :<|> (createDefinition :<|> fetchDefinitionIds)
    :<|> issueCredential
  ) = client api

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

data Schema = Schema
  { attributes :: ![String],
    name :: !String,
    version :: !String
  }
  deriving (Eq, Show)

instance ToJSON Schema where
  toJSON :: Schema -> Value
  toJSON Schema {attributes = attributes', name = name', version = version'} =
    object
      [ "attributes" .= attributes',
        "schema_name" .= name',
        "schema_version" .= version'
      ]

newtype SchemaId = SchemaId {schemaId :: String} deriving (Eq, Show)

instance FromJSON SchemaId where
  parseJSON :: Value -> Parser SchemaId
  parseJSON = withObject "SchemaId" $ \obj -> do
    schemaId' <- obj .: "schema_id"
    return (SchemaId {schemaId = schemaId'})

instance ToJSON SchemaId where
  toJSON :: SchemaId -> Value
  toJSON SchemaId {schemaId = schemaId'} =
    object
      [ "schema_id" .= schemaId'
      ]

data Attribute = Attribute
  { name :: !String,
    value :: !String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Attribute

newtype CredentialPreview = CredentialPreview {attributes :: [Attribute]} deriving (Eq, Show)

instance ToJSON CredentialPreview where
  toJSON :: CredentialPreview -> Value
  toJSON CredentialPreview {attributes = attributes'} =
    object
      [ "@type" .= ("https://didcomm.org/issue-credential/2.0/credential-preview" :: String),
        "attributes" .= attributes'
      ]

data CredentialOffer = CredentialOffer
  { connectionId :: !String,
    definitionId :: !String,
    credentialPreview :: !CredentialPreview
  }
  deriving (Eq, Show)

instance ToJSON CredentialOffer where
  toJSON :: CredentialOffer -> Value
  toJSON
    CredentialOffer
      { connectionId = connectionId',
        credentialPreview = preview,
        definitionId = definitionId'
      } =
      object
        [ "connection_id" .= connectionId',
          "cred_def_id" .= definitionId',
          "credential_preview" .= preview,
          "auto_issue" .= True
        ]

newtype CredentialDefinitionIds = CredentialDefinitionIds
  { ids :: [String]
  }
  deriving (Eq, Show)

instance FromJSON CredentialDefinitionIds where
  parseJSON :: Value -> Parser CredentialDefinitionIds
  parseJSON = withObject "CredentialDefinitionIds" $ \obj -> do
    ids' <- obj .: "credential_definition_ids"
    return (CredentialDefinitionIds {ids = ids'})
