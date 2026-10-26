{-# LANGUAGE DuplicateRecordFields #-}

-- | Utilities used across hydra-cluster
module Hydra.Cluster.Util where

import Hydra.Prelude

import Control.Exception (SomeAsyncException (..))
import Crypto.Random (getRandomBytes)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Secret (Secret, mkSecret, withSecret)
import Hydra.Cardano.Api (
  AsType (AsPaymentKey),
  File (..),
  FileError,
  HasTextEnvelope,
  Key (VerificationKey, verificationKeyHash),
  NetworkId,
  PaymentKey,
  SigningKey,
  TextEnvelopeError (TextEnvelopeAesonDecodeError),
  TxId,
  deserialiseFromTextEnvelope,
  generateSigningKey,
  serialiseToRawBytesHexText,
  textEnvelopeToJSON,
  writeLazyByteStringFileWithOwnerPermissions,
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
import Hydra.Tx (Party, deriveParty)
import Hydra.Tx.ContestationPeriod (ContestationPeriod)
import Hydra.Tx.Crypto (HydraKey, getVerificationKey)
import Hydra.Tx.Crypto qualified as Crypto
import Hydra.Tx.DepositPeriod (DepositPeriod)
import Hydra.Tx.DepositPeriod qualified as DP
import Paths_hydra_cluster qualified as Pkg
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removeFile, renameDirectory, renameFile)
import System.FilePath (takeFileName, (<.>), (</>))
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
--
-- NOTE: These keys are committed to this repository and shipped as package
-- data-files, so they are public and must never hold real funds. See
-- 'Participant' for what a run against a public network uses instead.
keysFor :: Actor -> IO (VerificationKey PaymentKey, Secret (SigningKey PaymentKey))
keysFor actor = do
  let path = "credentials" </> actorName actor <.> "sk"
  sk <- decodeSigningKey path =<< readConfigFile path
  pure (getVerificationKey sk, sk)

-- | Decode a 'TextEnvelope'-encoded payment signing key, 'Secret'-wrapped.
--
-- NOTE: The failure names the file but never quotes it. A partly written key
-- still holds key material, and this error reaches the logs.
decodeSigningKey :: FilePath -> ByteString -> IO (Secret (SigningKey PaymentKey))
decodeSigningKey path bs =
  case first TextEnvelopeAesonDecodeError (Aeson.eitherDecodeStrict bs) >>= deserialiseFromTextEnvelope of
    Left err -> fail $ "cannot decode text envelope at " <> path <> ": " <> show err
    Right sk -> pure (mkSecret sk)

-- | Create and save new signing key at the provided path, returning the
-- key 'Secret'-wrapped.
-- NOTE: Uses 'TextEnvelope' format.
createAndSaveSigningKey :: FilePath -> IO (Secret (SigningKey PaymentKey))
createAndSaveSigningKey path = do
  sk <- generate genSigningKey
  writeFileLBS path $ textEnvelopeToJSON (Just "Key used to commit funds into a Head") sk
  pure (mkSecret sk)

-- | 'try' that lets asynchronous exceptions through, so that an interrupt
-- during a sweep is not traced as a failure and carried on from.
trySync :: IO a -> IO (Either SomeException a)
trySync action =
  try action >>= \case
    Left e | Just (SomeAsyncException _) <- fromException e -> throwIO e
    result -> pure result

-- * Ephemeral run credentials

-- | Credentials a run generates for itself, none of them derivable from this
-- repository: the head participant's Cardano and Hydra keys, and the wallet key
-- it commits funds from.
--
-- Each is written to 'keyDir' before it is funded, so a run that dies before
-- returning its funds leaves them recoverable -- the Hydra key included, which
-- is what closes or aborts a head left open.
data Participant = Participant
  { cardanoVk :: VerificationKey PaymentKey
  , cardanoSk :: Secret (SigningKey PaymentKey)
  , hydraSk :: Secret (SigningKey HydraKey)
  , walletVk :: VerificationKey PaymentKey
  , walletSk :: Secret (SigningKey PaymentKey)
  , keyDir :: FilePath
  -- ^ Directory holding this run's key files.
  }

-- | The 'Party' a 'Participant' appears as in a head.
participantParty :: Participant -> Party
participantParty Participant{hydraSk} = deriveParty hydraSk

-- | Directory holding the keys of runs against this working directory.
--
-- Deliberately not named @state-@ anything: the runner wipes @state-*@ before
-- every run, and these keys have to outlive that.
participantKeysDir :: FilePath -> FilePath
participantKeysDir workDir = workDir </> "participant-keys"

-- | Where 'archiveParticipantKeys' moves the keys of a run whose funds are back.
sweptParticipantKeysDir :: FilePath -> FilePath
sweptParticipantKeysDir workDir = workDir </> "participant-keys-swept"

-- | Path a hydra-node is pointed at with @--cardano-signing-key@.
participantCardanoKeyFile :: FilePath -> FilePath
participantCardanoKeyFile keyDir = keyDir </> "cardano.sk"

participantHydraKeyFile :: FilePath -> FilePath
participantHydraKeyFile keyDir = keyDir </> "hydra.sk"

participantWalletKeyFile :: FilePath -> FilePath
participantWalletKeyFile keyDir = keyDir </> "wallet.sk"

-- | Marker a run leaves while it has a head open on chain.
--
-- The address cannot tell: the node returns its own init change there, so a
-- sweep of it succeeds whether or not a head was left behind.
participantHeadOpenFile :: FilePath -> FilePath
participantHeadOpenFile keyDir = keyDir </> "head-open"

markHeadOpen :: FilePath -> IO ()
markHeadOpen keyDir = writeFileBS (participantHeadOpenFile keyDir) ""

clearHeadOpen :: FilePath -> IO ()
clearHeadOpen keyDir = removeFile (participantHeadOpenFile keyDir)

headMayBeOpen :: FilePath -> IO Bool
headMayBeOpen = doesFileExist . participantHeadOpenFile

-- | Every key of a run that can hold funds, and so has to be swept.
participantPaymentKeyFiles :: [FilePath -> FilePath]
participantPaymentKeyFiles = [participantCardanoKeyFile, participantWalletKeyFile]

-- | Generate a fresh 'Participant' under 'participantKeysDir', in a directory
-- named after the Cardano key's hash so runs cannot clobber each other.
--
-- NOTE: Every key comes from system entropy. 'genSigningKey' and the
-- 'Arbitrary' instances are seedable test generators, which is the wrong tool
-- for keys holding real funds and a head's signing authority.
generateParticipant :: FilePath -> IO Participant
generateParticipant workDir = do
  rawCardanoSk <- generateSigningKey AsPaymentKey
  rawWalletSk <- generateSigningKey AsPaymentKey
  hydraSk <- Crypto.generateSigningKey <$> getRandomBytes 32
  let cardanoVk = getVerificationKey rawCardanoSk
      keyDir =
        participantKeysDir workDir
          </> toString (serialiseToRawBytesHexText $ verificationKeyHash cardanoVk)
  createDirectoryIfMissing True keyDir
  writeSigningKey (participantCardanoKeyFile keyDir) rawCardanoSk
  writeSigningKey (participantWalletKeyFile keyDir) rawWalletSk
  withSecret hydraSk $ writeSigningKey (participantHydraKeyFile keyDir)
  pure
    Participant
      { cardanoVk
      , cardanoSk = mkSecret rawCardanoSk
      , hydraSk
      , walletVk = getVerificationKey rawWalletSk
      , walletSk = mkSecret rawWalletSk
      , keyDir
      }

-- | Write a signing key in 'TextEnvelope' format, readable only by its owner,
-- failing loudly if it cannot be persisted: a key that was funded but never
-- written is unrecoverable.
--
-- Staged and renamed into place, which is atomic: a kill partway through
-- otherwise leaves a truncated key that no run can read or sweep. These sit in
-- the state directory of a shared runner, where the default mode would leave
-- mainnet keys readable by every account on the host.
writeSigningKey :: HasTextEnvelope (SigningKey k) => FilePath -> SigningKey k -> IO ()
writeSigningKey path sk = do
  let staging = path <.> "tmp"
  written <- writeLazyByteStringFileWithOwnerPermissions (File staging) (textEnvelopeToJSON Nothing sk)
  either (fail . show) pure (written :: Either (FileError ()) ())
  renameFile staging path

-- | The key directories of runs that did not sweep, each with the payment keys
-- to return funds from, or the reason they could not be read.
--
-- A run killed midway through writing its keys leaves a file missing or
-- truncated. A missing one is skipped, and one that will not decode is reported
-- against its own directory alone so it cannot hide every other run's keys.
-- Entries that are not directories are ignored.
leftoverParticipantKeys :: FilePath -> IO [(FilePath, Either Text [Secret (SigningKey PaymentKey)])]
leftoverParticipantKeys workDir = do
  let dir = participantKeysDir workDir
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
      names <- listDirectory dir
      keyDirs <- filterM doesDirectoryExist ((dir </>) <$> sort names)
      forM keyDirs $ \keyDir -> (keyDir,) <$> readPaymentKeys keyDir
 where
  readPaymentKeys :: FilePath -> IO (Either Text [Secret (SigningKey PaymentKey)])
  readPaymentKeys keyDir =
    first (show @Text @SomeException) <$> trySync (fmap catMaybes . forM participantPaymentKeyFiles $ readKey keyDir)

  readKey :: FilePath -> (FilePath -> FilePath) -> IO (Maybe (Secret (SigningKey PaymentKey)))
  readKey keyDir keyFile = do
    let path = keyFile keyDir
    present <- doesFileExist path
    if present
      then Just <$> (decodeSigningKey path =<< BS.readFile path)
      else pure Nothing

-- | Move a run's keys aside once its funds are back, reporting why if it could
-- not be done. Moved rather than deleted, so a directory archived on a wrong
-- call is still recoverable by hand.
archiveParticipantKeys :: FilePath -> FilePath -> IO (Either Text ())
archiveParticipantKeys workDir keyDir =
  first (show @Text @SomeException) <$> trySync go
 where
  go = do
    let swept = sweptParticipantKeysDir workDir
        dest = swept </> takeFileName keyDir
    createDirectoryIfMissing True swept
    alreadyThere <- doesDirectoryExist dest
    -- A name is a key hash, so a clash means this was archived and recreated.
    -- Keep both rather than write over keys already archived.
    if alreadyThere
      then fail $ "refusing to overwrite already archived keys at " <> dest
      else renameDirectory keyDir dest

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
-- out 'depositActivation' rather than by anything the scenario asserts.
--
-- A deposit becomes active at @created + depositActivation@, where @created@ is
-- the deposit tx's upper validity bound, set a grace time ahead of the chain
-- tip by 'Hydra.Chain.Direct.Handlers.draftDepositTx'. That grace time caps at
-- @min maxGraceTime (depositPeriod / 2)@, 200s here, so the wait is
-- @200 + depositActivation@ and only the second term is ours: at one block
-- time it drops from 200 + 400 to 200 + 20.
--
-- 'contestationPeriod' is cut to the point where the close transaction's
-- validity window stops changing, and no further. The contestation deadline is
-- @closeTxUpperBound + contestationPeriod@ with the close tx bounded at
-- @now + min contestationPeriod maxGraceTime@, so at @10 * blockTime@ = 200s
-- the wait halves from 600s to 400s while that @min@ still yields 200s, exactly
-- as it does at 'mkTestTiming'\'s 400s. Going below 200s would start shortening
-- the window every close, contest and increment has to be included in, with no
-- resubmit on expiry, and would drag two things with it: the derived
-- @unsyncedPeriod@ (half the contestation period, against a Blockfrost follower
-- that only observes blocks with a successor and so lags a block gap by
-- construction) and 'Hydra.Chain.Blockfrost.Client.submissionRetryPolicy',
-- whose ~180s worst case is documented against a 200s window.
--
-- 'depositPeriod' keeps its 'mkTestTiming' value. It is not on the critical
-- path -- it sets how long a deposit stays active, not how long anything waits
-- -- and shortening it only shrinks windows: the grace time is capped at half
-- of it, so both the deposit tx's validity and the active window
-- @depositPeriod - graceTime@ fall with it. 'Test.Hydra.Cluster.UtilSpec'
-- guards the value used here.
mkSmokeTiming :: BlockTime -> Timing
mkSmokeTiming blockTime =
  Timing
    { blockTime
    , contestationPeriod = truncate $ max 1 (10 * blockTime)
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
    cardanoChainConfig
      opts
      hydraScriptsTxId
      (actorFilePath me "sk")
      [actorFilePath himOrHer "vk" | himOrHer <- them]
      contestationPeriod
      depositPeriod
      depositActivation
 where
  actorFilePath actor fileType = targetDir </> actorFileName actor fileType
  actorFileName actor fileType = actorName actor <.> fileType

  copyFile actor fileType = do
    let fileName = actorFileName actor fileType
        filePath = actorFilePath actor fileType
    readConfigFile ("credentials" </> fileName) >>= writeFileBS filePath

-- | Chain config for a head run by a generated 'Participant', as the solo
-- scenarios that move real funds on public networks do.
--
-- Unlike 'chainConfigFor' this copies nothing out of @config/credentials@: the
-- node is pointed at the participant's own key, so no public fixture signing
-- key is written into the state directory at all.
chainConfigForParticipant ::
  Participant ->
  ChainBackendOptions ->
  -- | Transaction ids at which Hydra scripts should have been published.
  [TxId] ->
  Timing ->
  ChainConfig
chainConfigForParticipant Participant{keyDir} opts hydraScriptsTxId timing =
  cardanoChainConfig
    opts
    hydraScriptsTxId
    (participantCardanoKeyFile keyDir)
    []
    contestationPeriod
    depositPeriod
    depositActivation
 where
  Timing{contestationPeriod, depositPeriod, depositActivation} = timing

-- | Assemble a chain config from key files that are already on disk.
cardanoChainConfig ::
  ChainBackendOptions ->
  -- | Transaction ids at which Hydra scripts should have been published.
  [TxId] ->
  -- | Path of the node's own Cardano signing key.
  FilePath ->
  -- | Paths of the other participants' Cardano verification keys.
  [FilePath] ->
  ContestationPeriod ->
  DepositPeriod ->
  -- | Deposit activation, independent from the deposit period.
  DepositPeriod ->
  ChainConfig
cardanoChainConfig opts hydraScriptsTxId cardanoSigningKey cardanoVerificationKeys contestationPeriod depositPeriod depositActivation =
  Cardano
    defaultCardanoChainConfig
      { hydraScriptsTxId
      , cardanoSigningKey
      , cardanoVerificationKeys
      , contestationPeriod
      , depositPeriod
      , depositActivation
      , unsyncedPeriod = defaultUnsyncedPeriodFor contestationPeriod
      , chainBackendOptions = opts
      }

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
