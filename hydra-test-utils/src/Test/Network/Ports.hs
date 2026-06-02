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

-- | Like 'withFreePort' but also reserves the derived companion port,
-- in the same sense as 'randomUnusedTCPPortsWithDerived' — i.e. the
-- companion is verified free at allocation time. Use this for tests
-- that spin up a subprocess (such as etcd) which itself binds a port
-- computed from the configured one.
withFreePortAndDerived :: (PortNumber -> PortNumber) -> (PortNumber -> IO a) -> IO a
withFreePortAndDerived derive action = do
  [p] <- randomUnusedTCPPortsWithDerived derive 1
  action (fromIntegral p)

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

-- | Find @count@ free TCPv4 ports such that for each returned port @p@, the
-- /derived/ port @derive p@ is also free at allocation time.
--
-- This is needed for tests that drive a subprocess which itself opens a
-- companion port computed from the configured one — e.g. etcd, whose client
-- port is @listen - 2622@ in this codebase. A vanilla 'randomUnusedTCPPorts'
-- guarantees the @count@ primary ports are mutually unique but says nothing
-- about the derived companions, which can be in use by anything on the host
-- (other processes, other test fixtures, TIME_WAIT, etc.). The result is a
-- flaky 'bind: address already in use' the moment the subprocess starts.
--
-- We acquire the primary port from the OS, then try to bind its companion to
-- prove it's free; if the companion bind fails we drop the primary and
-- retry, up to a generous bound (collisions on the companion are rare in
-- the ephemeral range, so we should converge quickly).
--
-- NOTE: There's still a small race between releasing the bound sockets and
-- the subprocess binding them, but it's the same race 'randomUnusedTCPPorts'
-- already has, and not the root cause of the etcd flakes.
randomUnusedTCPPortsWithDerived ::
  (PortNumber -> PortNumber) ->
  Int ->
  IO [Int]
randomUnusedTCPPortsWithDerived derive count =
  bracket (acquire count [] (count * 20)) release (pure . map (\(p, _, _) -> fromIntegral p))
 where
  acquire :: Int -> [(PortNumber, Socket, Socket)] -> Int -> IO [(PortNumber, Socket, Socket)]
  acquire 0 acc _ = pure acc
  acquire _ _ 0 =
    fail $
      "randomUnusedTCPPortsWithDerived: ran out of retries trying to pair "
        <> show count
        <> " primary ports with free derived ports."
  acquire k acc budget = do
    peerSock <- bindFreshLoopback
    peerPortNum <- socketPort peerSock
    let companion = derive peerPortNum
    eClient <- try $ bindSpecificLoopback companion
    case eClient of
      Left (_ :: SomeException) -> do
        close peerSock
        acquire k acc (budget - 1)
      Right clientSock ->
        acquire (k - 1) ((peerPortNum, peerSock, clientSock) : acc) (budget - 1)

  release :: [(PortNumber, Socket, Socket)] -> IO ()
  release = mapM_ (\(_, ps, cs) -> close ps `finally` close cs)

bindSpecificLoopback :: PortNumber -> IO Socket
bindSpecificLoopback portNumber = do
  s <- socket AF_INET Stream defaultProtocol
  bind s (SockAddrInet portNumber (tupleToHostAddress (127, 0, 0, 1)))
  pure s
