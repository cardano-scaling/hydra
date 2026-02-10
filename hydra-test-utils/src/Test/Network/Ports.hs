-- | Utility module providing functions to find and allocate random ports.
module Test.Network.Ports where

import Hydra.Prelude

import "network" Network.Socket (
  PortNumber,
 )
import "port-utils" Network.Socket.Free (getFreePort)

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
  fromIntegral <$> getFreePort

-- | Find a free TCPv4 port and pass it to the given 'action'.
--
-- NOTE: Should be used only for testing, see 'getRandomPort' for limitations.
withFreePort :: (PortNumber -> IO a) -> IO a
withFreePort action = getRandomPort >>= action

-- | Find the specified number of free ports.
--
-- NOTE: Should be used only for testing, see 'getRandomPort' for limitations.
randomUnusedTCPPorts :: Int -> IO [Int]
randomUnusedTCPPorts count =
  fmap fromIntegral
    <$> replicateM count (withFreePort return)
