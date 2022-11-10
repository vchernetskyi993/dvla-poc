{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

module Server (server) where

import Api
  ( API,
    ConnectionDto (ConnectionDto, connectionId),
    Credential (Credential, attributes, connectionId),
    Invitation (Invitation),
    License (License, category, firstName, lastName),
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
  ( Attribute (Attribute, name, value),
    Connection (Connection, connectionId, name, state),
    ConnectionState (Active),
    CredentialDefinitionIds (ids),
    CredentialOffer (CredentialOffer, connectionId, credentialPreview, definitionId),
    CredentialPreview (CredentialPreview, attributes),
    Schema (Schema, attributes, name, version),
    SendMessageBody (SendMessageBody, content),
    createDefinition,
    createInvitation,
    createSchema,
    fetchDefinitionIds,
    getConnections,
    issueCredential,
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
      :<|> generateLicenseSchema client
      :<|> issueLicense client
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
toConnectionDto Connection {name = name', connectionId = connectionId'} =
  ConnectionDto
    { connectionId = connectionId',
      name = name'
    }

sendMessage' :: ClientEnv -> Message -> Handler NoContent
sendMessage' client Message {connectionId = cid, text = message} = do
  void $
    performFrameworkRequest client $
      sendMessage cid $
        SendMessageBody {content = message}
  return NoContent

generateLicenseSchema :: ClientEnv -> Handler NoContent
generateLicenseSchema client = do
  schemaId <-
    performFrameworkRequest client $
      createSchema $
        Schema
          { attributes = ["first_name", "last_name", "category"],
            name = "driver_license",
            version = "1.0"
          }

  performFrameworkRequest client $ createDefinition schemaId

issueLicense :: ClientEnv -> Credential License -> Handler NoContent
issueLicense client credential = do
  definitionId' <- performFrameworkRequest client fetchDefinitionIds

  performFrameworkRequest client $
    issueCredential $
      toCredentialOffer credential $
        head $
          ids definitionId'

toCredentialOffer :: Credential License -> String -> CredentialOffer
toCredentialOffer
  Credential
    { connectionId = connectionId',
      attributes = License {firstName = firstName', lastName = lastName', category = category'}
    }
  definitionId' =
    CredentialOffer
      { connectionId = connectionId',
        definitionId = definitionId',
        credentialPreview =
          CredentialPreview
            { attributes =
                [ Attribute {name = "first_name", value = firstName'},
                  Attribute {name = "last_name", value = lastName'},
                  Attribute {name = "category", value = category'}
                ]
            }
      }

performFrameworkRequest :: ClientEnv -> ClientM a -> Handler a
performFrameworkRequest client request =
  liftIO (runClientM request client)
    >>= unwrapFrameworkResponse

unwrapFrameworkResponse :: Either ClientError a -> Handler a
unwrapFrameworkResponse (Left err) = throwError err500 {errBody = BSL.pack $ show err}
unwrapFrameworkResponse (Right val) = return val
