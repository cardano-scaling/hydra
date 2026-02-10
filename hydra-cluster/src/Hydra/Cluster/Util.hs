-- | Utilities used across hydra-cluster
module Hydra.Cluster.Util where

import Hydra.Prelude

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
import Hydra.Chain.Backend (ChainBackend)
import Hydra.Chain.Backend qualified as Backend
import Hydra.Cluster.Fixture (Actor, actorName, fundsOf)
import Hydra.Node.DepositPeriod (DepositPeriod)
import Hydra.Node.UnsyncedPeriod (defaultUnsyncedPeriodFor)
import Hydra.Options (
  CardanoChainConfig (..),
  ChainBackendOptions (..),
  ChainConfig (..),
  DirectOptions (..),
  defaultCardanoChainConfig,
  defaultDepositPeriod,
 )
import Hydra.Tx.ContestationPeriod (ContestationPeriod)
import Paths_hydra_cluster qualified as Pkg
import Test.Hydra.Prelude (failure)
import Test.Hydra.Tx.Gen (genSigningKey)
import Test.QuickCheck (generate)
import "aeson" Data.Aeson qualified as Aeson
import "bytestring" Data.ByteString qualified as BS
import "filepath" System.FilePath ((<.>), (</>))

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

-- | Get the "well-known" keys for given actor.
keysFor :: Actor -> IO (VerificationKey PaymentKey, SigningKey PaymentKey)
keysFor actor = do
  bs <- readConfigFile ("credentials" </> actorName actor <.> "sk")
  let res =
        first TextEnvelopeAesonDecodeError (Aeson.eitherDecodeStrict bs)
          >>= deserialiseFromTextEnvelope
  case res of
    Left err ->
      fail $ "cannot decode text envelope from '" <> show bs <> "', error: " <> show err
    Right sk -> pure (getVerificationKey sk, sk)

-- | Create and save new signing key at the provided path.
-- NOTE: Uses 'TextEnvelope' format.
createAndSaveSigningKey :: FilePath -> IO (SigningKey PaymentKey)
createAndSaveSigningKey path = do
  sk <- generate genSigningKey
  writeFileLBS path $ textEnvelopeToJSON (Just "Key used to commit funds into a Head") sk
  pure sk

chainConfigFor ::
  ChainBackend backend =>
  HasCallStack =>
  Actor ->
  FilePath ->
  backend ->
  -- | Transaction ids at which Hydra scripts should have been published.
  [TxId] ->
  [Actor] ->
  ContestationPeriod ->
  IO ChainConfig
chainConfigFor me targetDir backend txids actors cp = chainConfigFor' me targetDir backend txids actors cp defaultDepositPeriod

chainConfigFor' ::
  ChainBackend backend =>
  HasCallStack =>
  Actor ->
  FilePath ->
  backend ->
  -- | Transaction ids at which Hydra scripts should have been published.
  [TxId] ->
  [Actor] ->
  ContestationPeriod ->
  DepositPeriod ->
  IO ChainConfig
chainConfigFor' me targetDir backend hydraScriptsTxId them contestationPeriod depositPeriod = do
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
        , unsyncedPeriod = defaultUnsyncedPeriodFor contestationPeriod
        , chainBackendOptions = Backend.getOptions backend
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
