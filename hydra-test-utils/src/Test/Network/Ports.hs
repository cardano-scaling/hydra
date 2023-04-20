{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Utility module providing functions to find and allocate random ports.
module Test.Network.Ports where

import Hydra.Prelude

import Control.Monad.Class.MonadSTM (writeTVar)
import Network.Socket (
  PortNumber,
  close',
 )
import Network.Wai.Handler.Warp (openFreePort)

-- | Find a TCPv4 port which is likely to be free for listening on
-- @localhost@. This binds a socket, receives an OS-assigned port, then closes
-- the socket.
--
-- Note that this is vulnerable to race conditions if another process binds the
-- port returned by 'getRandomPort' before this process does.
--
-- Do not use this unless you have no other option.
getRandomPort :: TVar IO [Int] -> IO PortNumber
getRandomPort tvar = do
  (port, sock) <- openFreePort
  liftIO $ close' sock
  mport <- atomically $ recordFreePort tvar port
  case mport of
    Nothing -> threadDelay 1 >> getRandomPort tvar
    Just p -> return $ fromIntegral p

-- | Find a free TCPv4 port and pass it to the given 'action'.
--
-- NOTE: Should be used only for testing, see 'getRandomPort' for limitations.
withFreePort :: TVar IO [Int] -> (PortNumber -> IO a) -> IO a
withFreePort tvar action = getRandomPort tvar >>= action

-- | Find the specified number of free ports.
--
-- NOTE: Should be used only for testing, see 'getRandomPort' for limitations.
randomUnusedTCPPorts :: TVar IO [Int] -> Int -> IO [Int]
randomUnusedTCPPorts tvar count =
  fmap fromIntegral
    <$> replicateM count (withFreePort tvar (\port -> return port))

-- | Internal function to maybe obtain the free port.
recordFreePort :: TVar IO [Int] -> Int -> STM IO (Maybe Int)
recordFreePort tvar port = do
  freePorts <- readTVar tvar
  if port `elem` freePorts
    then return Nothing
    else do
      writeTVar tvar (port : freePorts)
      return $ Just port
