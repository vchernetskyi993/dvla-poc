module Spec.Util (getFreePort) where

import Network.Socket
  ( Family (AF_INET),
    SockAddr (SockAddrInet),
    SocketType (Stream),
    bind,
    close,
    defaultPort,
    defaultProtocol,
    socket,
    socketPort,
    tupleToHostAddress,
  )

getFreePort :: IO Int
getFreePort = do
  sock <- socket AF_INET Stream defaultProtocol
  bind sock $ SockAddrInet defaultPort $ tupleToHostAddress (127, 0, 0, 1)
  port <- socketPort sock
  close sock
  return $ fromIntegral $ toInteger port
