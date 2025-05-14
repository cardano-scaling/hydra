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
  SocketPath,
  TextEnvelopeError (TextEnvelopeAesonDecodeError),
  TxId,
  deserialiseFromTextEnvelope,
  textEnvelopeToJSON,
 )
import Hydra.Cluster.Fixture (Actor, actorName, fundsOf)
import Hydra.Options (ChainConfig (..), DirectChainConfig (..), defaultDirectChainConfig)
import Hydra.Tx.ContestationPeriod (ContestationPeriod)
import Hydra.Tx.DepositDeadline (DepositDeadline)
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
  HasCallStack =>
  Actor ->
  FilePath ->
  SocketPath ->
  -- | Transaction ids at which Hydra scripts should have been published.
  [TxId] ->
  [Actor] ->
  ContestationPeriod ->
  DepositDeadline ->
  IO ChainConfig
chainConfigFor me targetDir nodeSocket hydraScriptsTxId them contestationPeriod depositDeadline = do
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
    Direct
      defaultDirectChainConfig
        { nodeSocket
        , hydraScriptsTxId
        , cardanoSigningKey = actorFilePath me "sk"
        , cardanoVerificationKeys = [actorFilePath himOrHer "vk" | himOrHer <- them]
        , contestationPeriod
        , depositDeadline
        }
 where
  actorFilePath actor fileType = targetDir </> actorFileName actor fileType
  actorFileName actor fileType = actorName actor <.> fileType

  copyFile actor fileType = do
    let fileName = actorFileName actor fileType
        filePath = actorFilePath actor fileType
    readConfigFile ("credentials" </> fileName) >>= writeFileBS filePath

modifyConfig :: (DirectChainConfig -> DirectChainConfig) -> ChainConfig -> ChainConfig
modifyConfig fn = \case
  Direct config -> Direct $ fn config
  x -> x

setNetworkId :: NetworkId -> ChainConfig -> ChainConfig
setNetworkId networkId = \case
  Direct config -> Direct config{networkId}
  x -> x
