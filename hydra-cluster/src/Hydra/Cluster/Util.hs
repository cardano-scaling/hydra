-- | Utilities used across hydra-cluster
module Hydra.Cluster.Util where

import Hydra.Prelude

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Hydra.Cardano.Api (
  AsType (AsPaymentKey, AsSigningKey),
  HasTypeProxy (AsType),
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
import Hydra.ContestationPeriod (ContestationPeriod)
import Hydra.Ledger.Cardano (genSigningKey)
import Hydra.Options (ChainConfig (..), DirectChainConfig (..), defaultDirectChainConfig)
import Paths_hydra_cluster qualified as Pkg
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (<.>), (</>))
import Test.Hydra.Prelude (failure)
import Test.QuickCheck (generate)

-- | Read a file from the config/ repository directory. See 'getDataFileName'
-- how to override the path to config/.
readConfigFile :: FilePath -> IO ByteString
readConfigFile source = do
  Pkg.getDataFileName ("config" </> source) >>= BS.readFile

-- | Copy a file from the config/ repository directory to a target path. See
-- 'getDataFileName' how to override the path to config/.
copyConfigFile :: FilePath -> FilePath -> IO ()
copyConfigFile source target = do
  createDirectoryIfMissing True (takeDirectory target)
  readConfigFile source >>= BS.writeFile target

-- | Get the "well-known" keys for given actor.
keysFor :: Actor -> IO (VerificationKey PaymentKey, SigningKey PaymentKey)
keysFor actor = do
  bs <- readConfigFile ("credentials" </> actorName actor <.> "sk")
  let res =
        first TextEnvelopeAesonDecodeError (Aeson.eitherDecodeStrict bs)
          >>= deserialiseFromTextEnvelope asSigningKey
  case res of
    Left err ->
      fail $ "cannot decode text envelope from '" <> show bs <> "', error: " <> show err
    Right sk -> pure (getVerificationKey sk, sk)
 where
  asSigningKey :: AsType (SigningKey PaymentKey)
  asSigningKey = AsSigningKey AsPaymentKey

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
  -- | Transaction id at which Hydra scripts should have been published.
  TxId ->
  [Actor] ->
  ContestationPeriod ->
  IO ChainConfig
chainConfigFor me targetDir nodeSocket hydraScriptsTxId them contestationPeriod = do
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
