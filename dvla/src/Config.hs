{-# LANGUAGE DuplicateRecordFields #-}

module Config
  ( loadConfig,
    MainConfig (..),
    ServerConfig (..),
    FrameworkClientConfig (..),
  )
where

import Configuration.Dotenv (defaultConfig, loadFile, onMissingFile)
import Control.Monad (void)
import System.Environment (getEnv)

data MainConfig = MainConfig
  { server :: !ServerConfig,
    framework :: !FrameworkClientConfig
  }

newtype ServerConfig = ServerConfig {port :: Int}

data FrameworkClientConfig = FrameworkClientConfig
  { host :: !String,
    port :: !Int,
    secret :: !String
  }

loadConfig :: IO MainConfig
loadConfig = do
  void $ onMissingFile (loadFile defaultConfig) (return [])
  serverPort <- getEnv "SERVER_PORT"

  frameworkHost <- getEnv "FRAMEWORK_HOST"
  frameworkPort <- getEnv "FRAMEWORK_PORT"
  frameworkSecret <- getEnv "FRAMEWORK_SECRET"

  return
    MainConfig
      { server = ServerConfig $ read serverPort,
        framework =
          FrameworkClientConfig
            { host = frameworkHost,
              port = read frameworkPort,
              secret = frameworkSecret
            }
      }
