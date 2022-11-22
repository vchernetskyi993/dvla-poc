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
    LicenseData (LicenseData, connectionId),
    Message (Message, connectionId),
    Results,
    Revocation (Revocation, id),
    dateOfBirth,
    license,
    name,
    registry,
    revocation,
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
    CredentialDefinition (CredentialDefinition),
    CredentialDefinitionIds (ids),
    CredentialOffer (CredentialOffer, connectionId, credentialPreview, definitionId),
    CredentialPreview (CredentialPreview, attributes),
    CredentialRecord (CredentialRecord, attributes, connectionId),
    Schema (Schema, attributes, name, version),
    SchemaId (SchemaId, schemaId),
    SendMessageBody (SendMessageBody, content),
    createDefinition,
    createInvitation,
    createSchema,
    fetchCredentials,
    fetchDefinitionIds,
    getConnections,
    issueCredential,
    revocationId,
    revocationRegistryId,
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
      :<|> fetchLicenses client
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
  SchemaId {schemaId = schemaId'} <-
    performFrameworkRequest client $
      createSchema $
        Schema
          { attributes = ["first_name", "last_name", "category", "dob"],
            name = "driver_license",
            version = "1.0"
          }

  performFrameworkRequest client $ createDefinition $ CredentialDefinition schemaId'

issueLicense :: ClientEnv -> Credential License -> Handler NoContent
issueLicense client credential = do
  definitionId' <- performFrameworkRequest client fetchDefinitionIds

  performFrameworkRequest client $
    issueCredential $
      toCredentialOffer credential $
        head $
          ids definitionId'

fetchLicenses :: ClientEnv -> Handler (Results LicenseData)
fetchLicenses client = do
  credentials <- performFrameworkRequest client fetchCredentials

  return $ toLicenseData <$> credentials

toLicenseData :: CredentialRecord -> LicenseData
toLicenseData
  CredentialRecord
    { revocationId = revocationId',
      revocationRegistryId = revocationRegistryId',
      connectionId = connectionId',
      attributes = attributes'
    } =
    LicenseData
      { connectionId = connectionId',
        license =
          License
            { firstName = firstName',
              lastName = lastName',
              category = category',
              dateOfBirth = dateOfBirth'
            },
        revocation =
          Revocation
            { id = revocationId',
              registry = revocationRegistryId'
            }
      }
    where
      getAttribute attrName =
        value $
          head $
            filter (\Attribute {name = name'} -> name' == attrName) attributes'
      firstName' = getAttribute "first_name"
      lastName' = getAttribute "last_name"
      category' = getAttribute "category"
      dateOfBirth' = getAttribute "dob"

toCredentialOffer :: Credential License -> String -> CredentialOffer
toCredentialOffer
  Credential
    { connectionId = connectionId',
      attributes =
        License
          { firstName = firstName',
            lastName = lastName',
            category = category',
            dateOfBirth = dob
          }
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
                  Attribute {name = "category", value = category'},
                  Attribute {name = "dob", value = dob}
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
