-- | Utility module providing functions to find and allocate random ports.
module Test.Network.Ports where

import Hydra.Prelude

import Network.Socket (
  Family (AF_INET),
  PortNumber,
  SockAddr (SockAddrInet),
  Socket,
  SocketType (Stream),
  bind,
  close,
  defaultProtocol,
  socket,
  socketPort,
  tupleToHostAddress,
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
getRandomPort = bracket bindFreshLoopback close socketPort

-- | Find a free TCPv4 port and pass it to the given 'action'.
--
-- NOTE: Should be used only for testing, see 'getRandomPort' for limitations.
withFreePort :: (PortNumber -> IO a) -> IO a
withFreePort action = getRandomPort >>= action

-- | Find the specified number of free ports.
--
-- The returned ports are guaranteed mutually unique: we bind all @count@
-- sockets at once and only read their OS-assigned ports while every socket is
-- still bound, then release them in one go. A naive sequential bind-close-read
-- loop can hand back the same just-released ephemeral port twice on busy CI
-- runners, which then explodes downstream as @EADDRINUSE@.
--
-- NOTE: Should be used only for testing, see 'getRandomPort' for limitations.
randomUnusedTCPPorts :: Int -> IO [Int]
randomUnusedTCPPorts count =
  bracket
    (replicateM count bindFreshLoopback)
    (mapM_ close)
    (fmap (fmap fromIntegral) . mapM socketPort)

bindFreshLoopback :: IO Socket
bindFreshLoopback = do
  s <- socket AF_INET Stream defaultProtocol
  bind s (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  pure s
