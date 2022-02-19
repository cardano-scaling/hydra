module Hydra.Network.Topology where

import Control.Exception (IOException)
import Control.Monad.Class.MonadSTM (putTMVar)
import qualified Data.Aeson as Aeson
import Hydra.Network (Host)
import Hydra.Prelude

newtype NetworkTopology = NetworkTopology {hosts :: [Host]}
  deriving newtype (Eq, Show, ToJSON, FromJSON)

-- | Watch changes to given 'filepath' and update given 'peers' variable
-- when it contains a valid config.
--
-- TODO: This watcher works in a very naive way, trying to decode the
-- given file's content periodically and retrying if it fails. We could
-- make it more robust by using filesystem notifications but this seems
-- somewhat complicated as the we could make this more robust by using
-- another mechanism to configure the node, eg. an admin API.
withConfigWatcher :: FilePath -> TMVar IO NetworkTopology -> IO a -> IO a
withConfigWatcher filepath peers action =
  withAsync watchConfig $ const action
 where
  watchConfig = do
    try (Aeson.eitherDecodeFileStrict' filepath) >>= \case
      -- File does not exist, cannot be read, is locked...
      -- This should be transient and if not, some other/higher level
      -- timeout should handle it.
      Left (_ioex :: IOException) -> retry
      -- Configuration is invalid. This could happend because we are trying
      -- to read while some other process is writing the data, so we retry.
      Right (Left _err) -> retry
      Right (Right topology) -> atomically (putTMVar peers topology)

  retry = threadDelay 1 >> watchConfig
