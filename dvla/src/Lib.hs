module Lib
  ( startApp,
    app,
  )
where

import Api (API)
import FrameworkClient (createFrameworkClient)
import GHC.IO.Handle (BufferMode (LineBuffering))
import GHC.IO.Handle.FD (stderr, stdout)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)
import Servant (Application, Proxy (..), serve)
import Servant.Client (ClientEnv)
import Server (server)
import System.IO (hSetBuffering)
import Config (loadConfig, ServerConfig(port), server, framework)

startApp :: IO ()
startApp = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  config <- loadConfig
  withStdoutLogger $ \logger -> do
    let serverPort = port $ Config.server config
        settings = setPort serverPort $ setLogger logger defaultSettings

    putStrLn $ "Listening on port " <> show serverPort
    client <- createFrameworkClient $ framework config
    runSettings settings $ app client

app :: ClientEnv -> Application
app client = serve api $ Server.server client

api :: Proxy API
api = Proxy
