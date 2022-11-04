{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

module Server (server) where

import Api (API, Invitation (Invitation))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode, (.:))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy.Char8 as BSL (pack, unpack)
import Data.Maybe (fromJust)
import FrameworkClient (createInvitation)
import Servant
  ( Handler,
    NoContent (NoContent),
    Server,
    err500,
    errBody,
    serveDirectoryFileServer,
    throwError,
    type (:<|>) ((:<|>)),
  )
import Servant.Client (ClientEnv, runClientM)

server :: ClientEnv -> Server API
server client =
  ( generateInvitation client
      -- TODO: handle events
      :<|> ( \topic body -> do
               liftIO $ putStrLn $ "Topic: " <> topic <> ", body: " <> unpack (encode body)
               return NoContent
           )
      -- TODO: list connections
      -- TODO: send message
      :<|> ( \message -> do
               liftIO $ putStrLn $ "Sending: " <> unpack (encode message)
               return NoContent
           )
      -- TODO: generate schema
      :<|> ( do
               liftIO $ putStrLn "Generating driver license schema..."
               return NoContent
           )
      -- TODO: issue driver license
      :<|> ( \license -> do
               liftIO $ putStrLn $ "Issuing license: " <> unpack (encode license)
               return NoContent
           )
  )
    :<|> serveDirectoryFileServer "ui/build"

generateInvitation :: ClientEnv -> Handler Invitation
generateInvitation client = do
  result <- liftIO $ runClientM createInvitation client
  case result of
    (Left err) -> throwError err500 {errBody = BSL.pack $ show err}
    (Right obj) -> do
      let url = fromJust $ parseMaybe (.: "invitation_url") obj :: String
      return (Invitation url)
