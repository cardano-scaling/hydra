{-# LANGUAGE DuplicateRecordFields #-}

-- | Utilities used across hydra-cluster
module Hydra.Cluster.Util where

import Hydra.Prelude

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Hydra.Cardano.Api (
  Key (VerificationKey, getVerificationKey),
  NetworkId,
  PaymentKey,
  SigningKey,
  TextEnvelopeError (TextEnvelopeAesonDecodeError),
  TxId,
  deserialiseFromTextEnvelope,
  textEnvelopeToJSON,
 )
import Hydra.Cluster.Fixture (Actor, actorName, fundsOf)
import Hydra.Node.UnsyncedPeriod (defaultUnsyncedPeriodFor)
import Hydra.Options (
  CardanoChainConfig (..),
  ChainBackendOptions (..),
  ChainConfig (..),
  DirectOptions (..),
  defaultCardanoChainConfig,
 )
import Hydra.Tx.ContestationPeriod (ContestationPeriod)
import Hydra.Tx.DepositPeriod (DepositPeriod)
import Hydra.Tx.DepositPeriod qualified as DP
import Hydra.Tx.Secret (Secret, mkSecret)
import Paths_hydra_cluster qualified as Pkg
import System.FilePath ((<.>), (</>))
import Test.Hydra.Prelude (failure)
import Test.Hydra.Tx.Gen (genSigningKey)
import Test.QuickCheck (generate)

-- | Lookup a config file similar reading a file from disk.
-- If the env variable `HYDRA_CONFIG_DIR` is set, filenames will be
-- resolved relative to its value otherwise they will be looked up in the
-- package's data path.
readConfigFile :: FilePath -> IO ByteString
readConfigFile source = do
  filename <-
    lookupEnv "HYDRA_CONFIG_DIR"
      >>= maybe (Pkg.getDataFileName ("config" </> source)) (pure . (</> source))
  BS.readFile filename

-- | Get the "well-known" keys for given actor. The signing key is
-- 'Secret'-wrapped so callers cannot accidentally log or serialise it.
keysFor :: Actor -> IO (VerificationKey PaymentKey, Secret (SigningKey PaymentKey))
keysFor actor = do
  bs <- readConfigFile ("credentials" </> actorName actor <.> "sk")
  let res =
        first TextEnvelopeAesonDecodeError (Aeson.eitherDecodeStrict bs)
          >>= deserialiseFromTextEnvelope
  case res of
    Left err ->
      fail $ "cannot decode text envelope from '" <> show bs <> "', error: " <> show err
    Right sk -> pure (getVerificationKey sk, mkSecret sk)

-- | Create and save new signing key at the provided path, returning the
-- key 'Secret'-wrapped.
-- NOTE: Uses 'TextEnvelope' format.
createAndSaveSigningKey :: FilePath -> IO (Secret (SigningKey PaymentKey))
createAndSaveSigningKey path = do
  sk <- generate genSigningKey
  writeFileLBS path $ textEnvelopeToJSON (Just "Key used to commit funds into a Head") sk
  pure (mkSecret sk)

-- | Expected time between blocks (on average)
type BlockTime = NominalDiffTime

-- | Timing parameters that determine the behavior of a (cluster of) hydra-node.
data Timing = Timing
  { blockTime :: BlockTime
  , contestationPeriod :: ContestationPeriod
  , depositPeriod :: DepositPeriod
  , depositActivation :: DepositPeriod
  -- ^ Time a deposit must mature before it becomes active. Configured
  -- independently from 'depositPeriod'; the smart constructors default it to the
  -- same value so deposits activate as fast as they expire, but tests can set it
  -- separately to exercise decoupled activation.
  }
  deriving stock (Show)

-- | Truncate a duration to a whole-second 'DepositPeriod'.
truncatedDepositPeriod :: NominalDiffTime -> DepositPeriod
truncatedDepositPeriod = DP.DepositPeriod . fromInteger . truncate

-- | Set up reasonable timing parameters for testing given a 'BlockTime'.
mkTestTiming :: BlockTime -> Timing
mkTestTiming = mkTestTiming' 1

-- | Like 'mkTestTiming' but scales 'depositPeriod' by the number of concurrent
-- deposits expected. Each increment tx must be processed sequentially on-chain,
-- so N concurrent deposits require N times the base deposit period.
mkTestTiming' :: Int -> BlockTime -> Timing
mkTestTiming' numDeposits blockTime =
  Timing
    { blockTime
    , contestationPeriod = truncate $ 20 * blockTime
    , depositPeriod
    , depositActivation = depositPeriod
    }
 where
  depositPeriod = truncatedDepositPeriod $ fromIntegral numDeposits * 20 * blockTime

-- | Timing for the smoke test run by the @hydra-cluster@ executable against a
-- public network, where a block takes ~20s and the run is dominated by waiting
-- out 'depositActivation' and the contestation period rather than by anything
-- the scenario asserts. Both waits are shaped by @maxGraceTime = 200@ in
-- 'Hydra.Chain.Direct.Handlers'.
--
-- A deposit becomes active at @created + depositActivation@, where @created@ is
-- the deposit tx's upper validity bound, itself a grace time ahead of the chain
-- tip. The grace time is not ours to set, so 'depositActivation' is the whole
-- lever: at one block time the wait drops from @200 + 400@ to @200 + 20@.
--
-- The contestation deadline is @closeTxUpperBound + contestationPeriod@, and
-- the close tx's upper bound is @now + min contestationPeriod maxGraceTime@, so
-- closing costs @min cp 200 + cp@. Both terms fall together below 200s; the
-- price is that the close and increment transactions get a @cp@-long window to
-- be included (five blocks here), and there is no resubmit on expiry.
--
-- 'unsyncedPeriod' is deliberately left at the node's default of @cp \/ 2@.
-- Drift is only sampled when a block arrives ('handleOutOfSync' runs from the
-- @Tick@ input, which 'Hydra.Chain.Direct.Handlers.onRollForward' is the sole
-- source of), so it measures block processing lag rather than the gap between
-- blocks, and it must stay under @min cp maxGraceTime@ anyway: a node acting
-- while drifted further than that builds close transactions whose validity
-- bound is already in the past.
--
-- 'depositPeriod' deliberately keeps its 'mkTestTiming' value. It is not on the
-- critical path -- it sets how long a deposit stays active, not how long
-- anything waits -- and shortening it only eats that window, which is
-- @depositPeriod - graceTime@ with @graceTime@ up to @maxGraceTime@. At or
-- below 200s a deposit would expire the moment it activates and the increment
-- could never be posted. 'Test.Hydra.Cluster.UtilSpec' guards this.
mkSmokeTiming :: BlockTime -> Timing
mkSmokeTiming blockTime =
  Timing
    { blockTime
    , contestationPeriod = truncate $ max 1 (5 * blockTime)
    , depositPeriod = truncatedDepositPeriod $ max 1 (20 * blockTime)
    , depositActivation = truncatedDepositPeriod $ max 1 blockTime
    }

-- | Get a timeout until a deposit should have happened given a 'Timing'. A
-- deposit becomes active after 'depositActivation' and then needs about one
-- 'depositPeriod' to be picked up and incremented, so both are accounted for
-- (with the defaults where they are equal this is @2 * depositPeriod@).
--
-- The slack term covers the two on-chain round trips (deposit and increment:
-- submit, include, observe) plus multi-node processing. Those costs are
-- dominated by fixed latencies, not by block time, so the slack has a
-- constant floor; with the devnet's 0.1s blocks a pure @5 * blockTime@ came
-- to 0.5s and timed out regularly on loaded CI runners.
depositTimeout :: Timing -> NominalDiffTime
depositTimeout Timing{blockTime, depositPeriod, depositActivation} =
  DP.toNominalDiffTime depositActivation + DP.toNominalDiffTime depositPeriod + max 5 (20 * blockTime)

-- | Budget for observing the effect of one L1 transaction on the API:
-- submission, block inclusion, chain-follower observation and node
-- processing. The constant floor covers the fixed latencies, which dominate
-- at devnet block times; a pure blockTime multiple (e.g. a literal 3s) fired
-- regularly on loaded CI runners.
onChainObservationBudget :: NominalDiffTime -> NominalDiffTime
onChainObservationBudget blockTime = 5 + 10 * blockTime

-- | Budget for a hydra-node (re)start up to its Greetings. Process spawn,
-- etcd bootstrap and websocket connect are fixed costs, unrelated to block
-- time, so this is a constant.
nodeStartupBudget :: NominalDiffTime
nodeStartupBudget = 20

-- | Create a (test) chain config for a given actor.
chainConfigFor ::
  HasCallStack =>
  Actor ->
  FilePath ->
  ChainBackendOptions ->
  -- | Transaction ids at which Hydra scripts should have been published.
  [TxId] ->
  [Actor] ->
  Timing ->
  IO ChainConfig
chainConfigFor me targetDir opts txids actors timing =
  chainConfigFor' me targetDir opts txids actors contestationPeriod depositPeriod depositActivation
 where
  Timing{contestationPeriod, depositPeriod, depositActivation} = timing

chainConfigFor' ::
  HasCallStack =>
  Actor ->
  FilePath ->
  ChainBackendOptions ->
  -- | Transaction ids at which Hydra scripts should have been published.
  [TxId] ->
  [Actor] ->
  ContestationPeriod ->
  DepositPeriod ->
  -- | Deposit activation, independent from the deposit period.
  DepositPeriod ->
  IO ChainConfig
chainConfigFor' me targetDir opts hydraScriptsTxId them contestationPeriod depositPeriod depositActivation = do
  when (me `elem` them) $
    failure $
      show me <> " must not be in " <> show them

  copyFile me "vk"
  copyFile me "sk"
  copyFile (fundsOf me) "vk"
  copyFile (fundsOf me) "sk"

  forM_ them $ \actor ->
    copyFile actor "vk"
  pure $
    Cardano
      defaultCardanoChainConfig
        { hydraScriptsTxId
        , cardanoSigningKey = actorFilePath me "sk"
        , cardanoVerificationKeys = [actorFilePath himOrHer "vk" | himOrHer <- them]
        , contestationPeriod
        , depositPeriod
        , depositActivation
        , unsyncedPeriod = defaultUnsyncedPeriodFor contestationPeriod
        , chainBackendOptions = opts
        }
 where
  actorFilePath actor fileType = targetDir </> actorFileName actor fileType
  actorFileName actor fileType = actorName actor <.> fileType

  copyFile actor fileType = do
    let fileName = actorFileName actor fileType
        filePath = actorFilePath actor fileType
    readConfigFile ("credentials" </> fileName) >>= writeFileBS filePath

modifyConfig :: (CardanoChainConfig -> CardanoChainConfig) -> ChainConfig -> ChainConfig
modifyConfig fn = \case
  Cardano config -> Cardano $ fn config
  x -> x

setNetworkId :: NetworkId -> ChainConfig -> ChainConfig
setNetworkId networkId = \case
  Cardano config@CardanoChainConfig{chainBackendOptions} ->
    case chainBackendOptions of
      Direct direct@DirectOptions{} -> Cardano config{chainBackendOptions = Direct direct{networkId = networkId}}
      _ -> Cardano config
  x -> x
