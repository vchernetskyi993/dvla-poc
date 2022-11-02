{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module FrameworkClient
  ( createFrameworkClient,
    createInvitation,
  )
where

import Config (FrameworkClientConfig (port), host, secret)
import Data.Aeson (Object)
import Data.ByteString.Char8 (pack)
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

type API =
  "connections" :> "create-invitation" :> Post '[JSON] Object

api :: Proxy API
api = Proxy

createInvitation :: ClientM Object
createInvitation = client api

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
