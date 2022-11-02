{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api (API, Invitation (..)) where

import Data.Aeson (FromJSON, Object, ToJSON)
import GHC.Generics (Generic)
import Servant
  ( Capture,
    JSON,
    Post,
    PostNoContent,
    Raw,
    ReqBody,
    type (:<|>),
    type (:>),
  )

type API =
  "api"
    :> ( "invitations" :> Post '[JSON] Invitation
           :<|> "webhooks"
             :> "topic"
             :> Capture "topic" String
             :> ReqBody '[JSON] Object
             :> PostNoContent
           :<|> "messages"
             :> ReqBody '[JSON] Message
             :> PostNoContent
           :<|> "schemas"
             :> PostNoContent
           :<|> "licenses"
             :> ReqBody '[JSON] License
             :> PostNoContent
       )
    :<|> Raw

newtype Invitation = Invitation
  { url :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Invitation

data Message = Message
  { connectionId :: String,
    text :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Message

instance FromJSON Message

data License = License
  { firstName :: String,
    lastName :: String,
    category :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON License

instance FromJSON License
