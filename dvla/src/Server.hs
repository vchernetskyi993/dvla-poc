{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Server (server) where

import Api (API, ConnectionDto (ConnectionDto), Invitation (Invitation), Results, connectionId, name)
import Control.Monad (mfilter)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode, (.:))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy.Char8 as BSL (pack, unpack)
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import FrameworkClient (Connection (state), ConnectionState (Active), createInvitation, getConnections)
import FrameworkClient qualified as FC (Connection (connectionId, name))
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
toConnectionDto c = ConnectionDto {connectionId = FC.connectionId c, name = FC.name c}

performFrameworkRequest :: ClientEnv -> ClientM a -> Handler a
performFrameworkRequest client request =
  liftIO (runClientM request client)
    >>= unwrapFrameworkResponse

unwrapFrameworkResponse :: Either ClientError a -> Handler a
unwrapFrameworkResponse (Left err) = throwError err500 {errBody = BSL.pack $ show err}
unwrapFrameworkResponse (Right val) = return val
