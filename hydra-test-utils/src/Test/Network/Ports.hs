{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Utility module providing functions to find and allocate random ports.
module Test.Network.Ports where

import Hydra.Prelude

import Data.List (
  isInfixOf,
 )
import qualified Data.List as List
import Foreign.C.Error (
  Errno (..),
  eCONNREFUSED,
 )
import GHC.IO.Exception (
  IOException (..),
 )
import Network.Socket (
  Family (AF_INET),
  PortNumber,
  SockAddr (..),
  SocketType (Stream),
  close',
  connect,
  socket,
  tupleToHostAddress,
 )
import Network.Wai.Handler.Warp (openFreePort)
import System.Random.Shuffle (
  shuffleM,
 )

-- | Find a TCPv4 port which is likely to be free for listening on
-- @localhost@. This binds a socket, receives an OS-assigned port, then closes
-- the socket.
--
-- Note that this is vulnerable to race conditions if another process binds the
-- port returned by 'getRandomPort' before this process does.
--
-- Do not use this unless you have no other option.
getRandomPort :: IO PortNumber
getRandomPort = do
  (port, sock) <- openFreePort
  liftIO $ close' sock
  return $ fromIntegral port

-- | Find a free TCPv4 port and pass it to the given 'action'.
--
-- Should be used only for testing, see 'getRandomPort' for limitations.
withFreePort :: (Int -> IO ()) -> IO ()
withFreePort action = getRandomPort >>= action . fromIntegral

-- | Checks whether @connect()@ to a given TCPv4 `SockAddr` succeeds or
-- returns `eCONNREFUSED`.
--
-- Rethrows connection exceptions in all other cases (e.g. when the host
-- is unroutable).
--
-- Code courtesy of nh2: https://stackoverflow.com/a/57022572
isPortOpen :: SockAddr -> IO Bool
isPortOpen sockAddr = do
  bracket (socket AF_INET Stream 6 {- TCP -}) close' $ \sock -> do
    res <- try $ connect sock sockAddr
    case res of
      Right () -> return True
      Left e
        | (Errno <$> ioe_errno e) == Just eCONNREFUSED -> pure False
        | "WSAECONNREFUSED" `isInfixOf` show e -> pure False
        | otherwise -> throwIO e

-- | Get the underlying port number for the given socket address. Fails for UNIX
-- socket addresses which aren't bound to any TCP port.
unsafePortNumber :: SockAddr -> PortNumber
unsafePortNumber = \case
  SockAddrInet p _ -> p
  SockAddrInet6 p _ _ _ -> p
  SockAddrUnix _ -> error "unsafePortNumber: no port for unix sockets."

-- | Creates a `SockAttr` from host IP and port number.
--
-- Example:
-- > simpleSockAddr (127,0,0,1) 8000
simpleSockAddr :: (Word8, Word8, Word8, Word8) -> PortNumber -> SockAddr
simpleSockAddr addr port = SockAddrInet port (tupleToHostAddress addr)

-- | Get a list of random TCPv4 ports that currently do not have any servers
-- listening on them. It may return less than the requested number of ports.
--
-- Note that this method of allocating ports is subject to race
-- conditions. Production code should use better methods such as passing a
-- listening socket to the child process.
randomUnusedTCPPorts :: Int -> IO [Int]
randomUnusedTCPPorts count = do
  usablePorts <- shuffleM [1024 .. 49151]
  -- FIXME:  we should select count ports after checking they are free, not before
  sort <$> filterM unused (take count usablePorts)
 where
  unused = fmap not . isPortOpen . simpleSockAddr (127, 0, 0, 1) . fromIntegral

-- | Get a single unused random TCPv4 port.
randomUnusedTCPPort :: IO Int
randomUnusedTCPPort = do
  usablePorts <- shuffleM [1024 .. 49151]
  List.head <$> filterM unused (take 10 usablePorts)
 where
  unused = fmap not . isPortOpen . simpleSockAddr (127, 0, 0, 1) . fromIntegral
