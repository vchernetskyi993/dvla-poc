module Lib
  ( startApp,
    app,
  )
where

import Api (API)
import Configuration.Dotenv (defaultConfig, loadFile, onMissingFile)
import Control.Monad (void)
import GHC.IO.Handle (BufferMode (LineBuffering))
import GHC.IO.Handle.FD (stderr, stdout)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)
import Servant (Application, Proxy (..), serve)
import Server (server)
import System.Environment (getEnv)
import System.IO (hSetBuffering)

startApp :: IO ()
startApp = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
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
