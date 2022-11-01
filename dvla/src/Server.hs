{-# LANGUAGE ExplicitNamespaces #-}

module Server (server) where

import Api (API, Invitation (..))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 (unpack)
import Servant (NoContent (..), Server, type (:<|>) (..))

server :: Server API
server =
  -- TODO: generate invitation
  return (Invitation "http://my.awesome.url")
    -- TODO: handle events
    :<|> ( \topic body -> do
             liftIO (putStrLn ("Topic: " <> topic <> ", body: " <> unpack (encode body)))
             return NoContent
         )
    -- TODO: send message
    :<|> ( \message -> do
             liftIO (putStrLn ("Sending: " <> unpack (encode message)))
             return NoContent
         )
    -- TODO: generate schema
    :<|> ( do
             liftIO (putStrLn "Generating driver license schema...")
             return NoContent
         )
    -- TODO: issue driver license
    :<|> ( \license -> do
             liftIO (putStrLn ("Issuing license: " <> unpack (encode license)))
             return NoContent
         )
