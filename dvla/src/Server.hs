{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

module Server (server) where

import Api
  ( API,
    ConnectionDto (ConnectionDto, connectionId),
    Invitation (Invitation),
    Message (Message, connectionId),
    Results,
    name,
    text,
  )
import Control.Monad (mfilter)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode, (.:))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy.Char8 as BSL (pack, unpack)
import Data.Functor (void, (<&>))
import Data.Maybe (fromJust)
import FrameworkClient
  ( Connection (connectionId, name, state),
    ConnectionState (Active),
    SendMessageBody (SendMessageBody, content),
    createInvitation,
    getConnections,
    sendMessage,
  )
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
import Servant.Client (ClientEnv, ClientError, ClientM, runClientM)

server :: ClientEnv -> Server API
server client =
  ( generateInvitation client
      :<|> ( \topic body -> do
               liftIO $ putStrLn $ "Topic: " <> topic <> ", body: " <> unpack (encode body)
               return NoContent
           )
      :<|> fetchConnections client
      :<|> sendMessage' client
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
  result <- performFrameworkRequest client createInvitation

  let url = fromJust $ parseMaybe (.: "invitation_url") result :: String
  return (Invitation url)

fetchConnections :: ClientEnv -> Handler (Results ConnectionDto)
fetchConnections client = do
  performFrameworkRequest client $
    getConnections
      <&> mfilter ((== Active) . state)
      <&> fmap toConnectionDto

toConnectionDto :: Connection -> ConnectionDto
toConnectionDto c =
  ConnectionDto
    { Api.connectionId = FrameworkClient.connectionId c,
      Api.name = FrameworkClient.name c
    }

sendMessage' :: ClientEnv -> Message -> Handler NoContent
sendMessage' client Message {connectionId = cid, text = message} = do
  void $
    performFrameworkRequest client $
      sendMessage cid $
        SendMessageBody {content = message}
  return NoContent

performFrameworkRequest :: ClientEnv -> ClientM a -> Handler a
performFrameworkRequest client request =
  liftIO (runClientM request client)
    >>= unwrapFrameworkResponse

unwrapFrameworkResponse :: Either ClientError a -> Handler a
unwrapFrameworkResponse (Left err) = throwError err500 {errBody = BSL.pack $ show err}
unwrapFrameworkResponse (Right val) = return val
