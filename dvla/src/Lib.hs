{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( startApp,
    app,
  )
where

import Configuration.Dotenv (defaultConfig, loadFile, onMissingFile)
import Control.Monad (void)
import Data.Aeson (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)
import Servant
  ( Get,
    JSON,
    Proxy (..),
    Server,
    serve,
    type (:>),
  )
import System.Environment (getEnv)

data User = User
  { userId :: Int,
    userFirstName :: String,
    userLastName :: String
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[JSON] [User]

startApp :: IO ()
startApp = do
  void $ onMissingFile (loadFile defaultConfig) (return [])
  withStdoutLogger $ \logger -> do
    portStr <- getEnv "SERVER_PORT"
    let port = read portStr
        settings = setPort port $ setLogger logger defaultSettings

    putStrLn ("Listening on port " <> show port)
    runSettings settings app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users

users :: [User]
users =
  [ User 1 "Isaac" "Newton",
    User 2 "Albert" "Einstein"
  ]
