{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module FrameworkClient
  ( createFrameworkClient,
    createInvitation,
  )
where

import Data.Aeson (Object)
import Data.ByteString.Char8 (pack)
import Data.Functor ((<&>))
import Network.HTTP.Client
  ( defaultManagerSettings,
    managerModifyRequest,
    newManager,
    requestHeaders,
  )
import Servant (JSON, Post, Proxy (..), type (:>))
import Servant.Client
  ( BaseUrl (BaseUrl),
    ClientEnv,
    ClientM,
    Scheme (Http),
    client,
    mkClientEnv,
  )
import System.Environment (getEnv)

type API =
  "connections" :> "create-invitation" :> Post '[JSON] Object

api :: Proxy API
api = Proxy

createInvitation :: ClientM Object
createInvitation = client api

createFrameworkClient :: IO ClientEnv
createFrameworkClient = do
  manager <-
    newManager
      defaultManagerSettings
        { managerModifyRequest = \req ->
            getEnv "FRAMEWORK_SECRET" <&> \apiKey ->
              req
                { requestHeaders =
                    ("X-API-KEY", pack apiKey) : requestHeaders req
                }
        }
  host <- getEnv "FRAMEWORK_HOST"
  port <- getEnv "FRAMEWORK_PORT"
  return $ mkClientEnv manager (BaseUrl Http host (read port) "")
