-- | Utility module providing functions to find and allocate random ports.
--
-- Ports are handed out from a private band below the kernel's ephemeral range
-- (see 'portBandStart'), tracked in a process-global registry so no two
-- allocations in one test process can collide, and verified free with an
-- actual bind before being returned.
--
-- Rationale: consumers (etcd, cardano-node, warp, ...) bind their port some
-- time after allocation, and tests that restart their subprocess leave the
-- port unbound in between. Any OS-assigned loopback port (a @bind(0)@, or the
-- source port of a @connect(2)@) handed out during such a window used to be
-- able to land on the reserved port and pin it, since the kernel draws those
-- from @ip_local_port_range@. Allocating below that range makes the windows
-- collision-free against everything except an explicit bind of a specific
-- port in the band by a concurrent process; concurrent test processes of the
-- same user coordinate through per-port file locks (see 'tryLockPort'), which
-- the kernel releases when the owning process dies.
module Test.Network.Ports where

import Hydra.Prelude

import Control.Exception (IOException)
import Data.Bits ((.|.))
import Data.Set qualified as Set
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.C.Types (CInt (..))
import Network.Socket (
  Family (AF_INET),
  PortNumber,
  SockAddr (SockAddrInet),
  Socket,
  SocketType (Stream),
  bind,
  close,
  defaultProtocol,
  mkSocket,
  setCloseOnExecIfNeeded,
  socket,
  tupleToHostAddress,
  withFdSocket,
 )
import System.Directory (createDirectoryIfMissing)
import System.FileLock (FileLock, SharedExclusive (Exclusive), tryLockFile)
import System.FilePath ((</>))
import System.IO (hPutStrLn)
import System.IO.Unsafe (unsafePerformIO)
import System.Info (os)
import System.Process (getCurrentPid)

-- | Lowest port handed out by the allocator. The band must sit below the
-- ephemeral range (32768 and up on Linux by default, 49152 on darwin) so that
-- OS-assigned ports can never land on an allocated one.
portBandStart :: PortNumber
portBandStart = 24000

-- | One past the highest port handed out by the allocator.
portBandEnd :: PortNumber
portBandEnd = 32768

-- | All ports ever handed out (or found occupied) in this process, including
-- derived companion ports. Never released: the band is large enough for any
-- test run, and never reusing a port means a subprocess still draining its
-- listen socket cannot break a later allocation.
{-# NOINLINE reservedPortsRef #-}
reservedPortsRef :: IORef (Set PortNumber)
reservedPortsRef = unsafePerformIO $ newIORef mempty

-- | Next candidate port. Seeded from the pid so concurrent test processes on
-- one machine start in different parts of the band.
{-# NOINLINE nextCandidateRef #-}
nextCandidateRef :: IORef PortNumber
nextCandidateRef = unsafePerformIO $ do
  pid <- getCurrentPid
  let bandSize = toInteger portBandEnd - toInteger portBandStart
  newIORef $ portBandStart + fromInteger ((toInteger pid * 2657) `mod` bandSize)

-- | Warn (once) when the kernel's ephemeral range overlaps the band, which
-- would void the no-collision-with-OS-assigned-ports guarantee.
{-# NOINLINE ephemeralRangeCheck #-}
ephemeralRangeCheck :: ()
ephemeralRangeCheck = unsafePerformIO $ do
  lower <-
    try @_ @SomeException (readFileBS "/proc/sys/net/ipv4/ip_local_port_range") <&> \case
      Right bs | (lo : _) <- words (decodeUtf8 bs) -> readMaybe (toString lo)
      _ -> Nothing
  whenJust lower $ \(lo :: Int) ->
    when (lo < fromIntegral portBandEnd) $
      hPutStrLn stderr $
        "Test.Network.Ports: ip_local_port_range starts at "
          <> show lo
          <> ", overlapping the test port band; OS-assigned ports may collide with allocations"

-- | Allocate one port from the band, optionally together with a derived
-- companion port. The primary (and companion, when given) are registered in
-- 'reservedPortsRef' and proven bindable before being returned. Ports that
-- turn out to be occupied by something outside this process stay registered,
-- so they are never tried again.
allocatePort :: Maybe (PortNumber -> PortNumber) -> IO PortNumber
allocatePort mDerive =
  ephemeralRangeCheck `seq` go (2 * fromIntegral (portBandEnd - portBandStart))
 where
  go :: Int -> IO PortNumber
  go 0 = fail "Test.Network.Ports: exhausted the private port band"
  go n = do
    p <- atomicModifyIORef' nextCandidateRef $ \c ->
      (if succ c >= portBandEnd then portBandStart else succ c, c)
    let ps = p : maybe [] (\f -> [f p]) mDerive
    fresh <- atomicModifyIORef' reservedPortsRef $ \rs ->
      if any (`Set.member` rs) ps
        then (rs, False)
        else (foldr Set.insert rs ps, True)
    if not fresh
      then go (n - 1)
      else do
        locked <- and <$> mapM tryLockPort ps
        if not locked
          then go (n - 1)
          else
            try @_ @IOException (verifyBindable ps) >>= \case
              Right () -> pure p
              Left _ -> go (n - 1)

  verifyBindable = bindAll []

  bindAll held [] = mapM_ close held
  bindAll held (p : rest) = do
    s <- bindSpecificLoopback p `onException` mapM_ close held
    bindAll (s : held) rest

-- | Find a free TCPv4 port for listening on @localhost@.
--
-- The port is unique for the lifetime of the test process, see 'allocatePort'.
getRandomPort :: IO PortNumber
getRandomPort = allocatePort Nothing

-- | Find a free TCPv4 port and pass it to the given 'action'.
--
-- NOTE: Should be used only for testing.
withFreePort :: (PortNumber -> IO a) -> IO a
withFreePort action = getRandomPort >>= action

-- | Like 'withFreePort' but also reserves the derived companion port,
-- in the same sense as 'randomUnusedTCPPortsWithDerived'. Use this for tests
-- that spin up a subprocess (such as etcd) which itself binds a port
-- computed from the configured one.
withFreePortAndDerived :: (PortNumber -> PortNumber) -> (PortNumber -> IO a) -> IO a
withFreePortAndDerived derive action = allocatePort (Just derive) >>= action

-- | Find the specified number of free ports, mutually unique for the lifetime
-- of the test process.
--
-- NOTE: Should be used only for testing.
randomUnusedTCPPorts :: Int -> IO [Int]
randomUnusedTCPPorts count =
  replicateM count (fromIntegral <$> allocatePort Nothing)

-- | Find @count@ free TCPv4 ports such that for each returned port @p@, the
-- /derived/ port @derive p@ is also free (and reserved alongside it).
--
-- This is needed for tests that drive a subprocess which itself opens a
-- companion port computed from the configured one; e.g. etcd, whose client
-- port is @listen - 2622@ in this codebase.
randomUnusedTCPPortsWithDerived ::
  (PortNumber -> PortNumber) ->
  Int ->
  IO [Int]
randomUnusedTCPPortsWithDerived derive count =
  replicateM count (fromIntegral <$> allocatePort (Just derive))

-- | All port locks taken by this process, held only to keep them reachable
-- for the process lifetime.
{-# NOINLINE portLocksRef #-}
portLocksRef :: IORef [FileLock]
portLocksRef = unsafePerformIO $ newIORef []

-- | Take the machine-wide lock for a port, so concurrent test processes (of
-- the same user) cannot allocate it too. The lock is an OS file lock held for
-- the process lifetime; the kernel releases it when the process dies, so
-- crashed runs cannot leak reservations. Returns False when someone else
-- holds the lock. Degrades to True (no cross-process protection, the
-- in-process registry still applies) when the lock cannot be taken for any
-- reason other than contention.
tryLockPort :: PortNumber -> IO Bool
tryLockPort p =
  go `catch` \(_ :: IOException) -> pure True
 where
  go = do
    dir <- portLockDir
    createDirectoryIfMissing True dir
    tryLockFile (dir </> show p) Exclusive >>= \case
      Just lock -> modifyIORef' portLocksRef (lock :) $> True
      Nothing -> pure False

-- | Fixed, per-user lock directory. Deliberately not TMPDIR, which differs
-- between nix shells and would defeat cross-process coordination.
portLockDir :: IO FilePath
portLockDir = do
  user <- fromMaybe "unknown" <$> lookupEnv "USER"
  pure $ "/tmp" </> "hydra-test-port-locks-" <> user

bindSpecificLoopback :: PortNumber -> IO Socket
bindSpecificLoopback portNumber = do
  s <- openStreamSocketCloexec
  bind s (SockAddrInet portNumber (tupleToHostAddress (127, 0, 0, 1)))
  pure s

foreign import ccall unsafe "socket"
  c_socket :: CInt -> CInt -> CInt -> IO CInt

-- Linux socket(2) constants, identical on x86_64 and aarch64. The network
-- package offers no CLOEXEC-at-creation (its 'socket' sets the flag via a
-- separate fcntl); keep in sync with the twin in Hydra.Network.Etcd, which
-- cannot depend on this test-only package.
afInet, sockStream, sockNonBlock, sockCloexec :: CInt
afInet = 2
sockStream = 1
sockNonBlock = 0x800
sockCloexec = 0x80000

-- | Create an AF_INET stream socket with the close-on-exec flag set
-- ATOMICALLY at creation. 'Network.Socket.socket' sets it via a separate
-- fcntl, and a subprocess spawned by a concurrent thread in the window
-- between socket() and that fcntl inherits the fd: once the parent binds the
-- shared file description and closes its copy, the subprocess keeps the port
-- bound (invisibly, as an UNCONN socket) for its whole lifetime. With ~40
-- etcd spawns racing hundreds of sentinel sockets per suite run, that window
-- was hit regularly.
openStreamSocketCloexec :: IO Socket
openStreamSocketCloexec
  | os == "linux" = do
      -- SOCK_NONBLOCK matches what the GHC IO manager expects of the sockets
      -- it manages.
      fd <- throwErrnoIfMinus1 "socket" (c_socket afInet (sockStream .|. sockNonBlock .|. sockCloexec) 0)
      mkSocket fd
  | otherwise = do
      s <- socket AF_INET Stream defaultProtocol
      withFdSocket s setCloseOnExecIfNeeded
      pure s
