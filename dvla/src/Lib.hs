module Lib
  ( startApp,
    app,
  )
where

import Api (API)
import Config (FrameworkClientConfig, ServerConfig (port), framework, loadConfig, server)
import FrameworkClient (createFrameworkClient)
import GHC.IO.Handle (BufferMode (LineBuffering))
import GHC.IO.Handle.FD (stderr, stdout)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort, setLogger)
import Network.Wai.Logger (withStdoutLogger)
import Servant (Application, Proxy (..), serve)
import Server (server)
import System.IO (hSetBuffering)

startApp :: IO ()
startApp = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  config <- loadConfig
  withStdoutLogger $ \logger -> do
    let serverPort = port $ Config.server config
        settings = setPort serverPort $ setLogger logger defaultSettings

    putStrLn $ "Listening on port " <> show serverPort
    app' <- app $ framework config
    runSettings settings app'

app :: FrameworkClientConfig -> IO Application
app config = do
  client <- createFrameworkClient config
  return $ serve api $ Server.server client

api :: Proxy API
api = Proxy
