{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( API,
    Invitation (..),
    Results (..),
    ConnectionDto (..),
    Message (..),
    Credential (..),
    License (..),
    LicenseData (..),
    Revocation (..),
  )
where

import Control.Applicative (Alternative (empty))
import Control.Monad (MonadPlus, join)
import Data.Aeson (FromJSON, Object, ToJSON)
import Data.Functor ((<&>))
import GHC.Base (Alternative ((<|>)))
import GHC.Generics (Generic)
import Servant
  ( Capture,
    DeleteNoContent,
    Get,
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
           :<|> "connections" :> Get '[JSON] (Results ConnectionDto)
           :<|> "messages"
             :> ReqBody '[JSON] Message
             :> PostNoContent
           :<|> "schemas" :> PostNoContent
           :<|> "licenses"
             :> ( ReqBody '[JSON] (Credential License)
                    :> PostNoContent
                    :<|> Get '[JSON] (Results LicenseData)
                    :<|> Capture "connectionId" String :> DeleteNoContent
                )
       )
    :<|> Raw

newtype Invitation = Invitation
  { url :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Invitation

newtype Results a = Results
  { results :: [a]
  }
  deriving (Eq, Show, Generic)

instance ToJSON (Results ConnectionDto)

instance Functor Results where
  fmap :: (a -> b) -> Results a -> Results b
  fmap f a = Results $ results a <&> f

instance Applicative Results where
  pure :: a -> Results a
  pure = Results . pure
  (<*>) :: Results (a -> b) -> Results a -> Results b
  (<*>) (Results f) (Results a) = Results $ f <*> a

instance Monad Results where
  (>>=) :: Results a -> (a -> Results b) -> Results b
  (>>=) (Results a) f = Results $ join $ a <&> f <&> results

instance Alternative Results where
  empty :: Results a
  empty = Results []
  (<|>) :: Results a -> Results a -> Results a
  (<|>) (Results a) (Results b) = Results $ a <|> b

instance MonadPlus Results

data ConnectionDto = ConnectionDto
  { connectionId :: String,
    name :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON ConnectionDto

data Message = Message
  { connectionId :: String,
    text :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Message

instance FromJSON Message

data License = License
  { firstName :: !String,
    lastName :: !String,
    category :: !String,
    dateOfBirth :: !String
  }
  deriving (Eq, Show, Generic)

instance ToJSON License

instance FromJSON License

data Credential a = Credential
  { connectionId :: !String,
    attributes :: !a
  }
  deriving (Eq, Show, Generic)

instance FromJSON (Credential License)

data LicenseData = LicenseData
  { connectionId :: !String,
    license :: !License,
    revocation :: !Revocation
  }
  deriving (Eq, Show, Generic)

instance ToJSON LicenseData

instance ToJSON (Results LicenseData)

data Revocation = Revocation
  { id :: !String,
    registry :: !String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Revocation
