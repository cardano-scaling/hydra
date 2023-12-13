{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Utilities used across hydra-cluster
module Hydra.Cluster.Util where

import Hydra.Prelude

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Map qualified as Map
import Hydra.Cardano.Api (
  Address,
  AsType (AsPaymentKey, AsSigningKey),
  HasTypeProxy (AsType),
  IsMaryEraOnwards,
  IsShelleyBasedEra,
  Key (VerificationKey, getVerificationKey, verificationKeyHash),
  NetworkId,
  PaymentKey,
  ShelleyAddr,
  SigningKey,
  SocketPath,
  StakeAddressReference (NoStakeAddress),
  TextEnvelopeError (TextEnvelopeAesonDecodeError),
  Tx,
  UTxO' (UTxO),
  VerificationKey (GenesisUTxOVerificationKey, PaymentVerificationKey),
  deserialiseFromTextEnvelope,
  genesisUTxOPseudoTxIn,
  mkTxOutValue,
  mkVkAddress,
  textEnvelopeToJSON,
 )
import Hydra.Cardano.Api.Prelude (PaymentCredential (PaymentCredentialByKey), ReferenceScript (ReferenceScriptNone), TxOut (TxOut), TxOutDatum (TxOutDatumNone), Value, makeShelleyAddress)
import Hydra.Cluster.Fixture (Actor, actorName)
import Hydra.ContestationPeriod (ContestationPeriod)
import Hydra.Ledger (IsTx (UTxOType))
import Hydra.Ledger.Cardano (genSigningKey)
import Hydra.Options (ChainConfig (..), OfflineConfig (OfflineConfig, initialUTxOFile, ledgerGenesisFile), defaultChainConfig)
import Paths_hydra_cluster qualified as Pkg
import System.FilePath ((<.>), (</>))
import Test.Hydra.Prelude (failure)
import Test.QuickCheck (generate)

-- import CardanoClient (buildAddress)

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

offlineConfigFor :: [(Actor, Value)] -> FilePath -> NetworkId -> IO OfflineConfig
offlineConfigFor actorToVal targetDir networkId = do
  initialUtxoForActors actorToVal networkId >>= offlineConfigForUTxO @Tx targetDir

offlineConfigForUTxO :: forall tx. IsTx tx => FilePath -> UTxOType tx -> IO OfflineConfig
offlineConfigForUTxO targetDir utxo = do
  utxoPath <- seedInitialUTxOFromOffline @tx targetDir utxo
  pure $
    OfflineConfig
      { initialUTxOFile = utxoPath
      , ledgerGenesisFile = Nothing
      }

seedInitialUTxOFromOffline :: IsTx tx => FilePath -> UTxOType tx -> IO FilePath
seedInitialUTxOFromOffline targetDir utxo = do
  let destinationPath = targetDir </> "utxo.json"
  writeFileBS destinationPath . toStrict . Aeson.encode $ utxo

  -- Aeson.throwDecodeStrict =<< readFileBS (targetDir </> "utxo.json")
  pure destinationPath

buildAddress :: VerificationKey PaymentKey -> NetworkId -> Address ShelleyAddr
buildAddress vKey networkId =
  makeShelleyAddress networkId (PaymentCredentialByKey $ verificationKeyHash vKey) NoStakeAddress

initialUtxoWithFunds ::
  forall era ctx.
  (IsShelleyBasedEra era, IsMaryEraOnwards era) =>
  NetworkId ->
  [(VerificationKey PaymentKey, Value)] ->
  IO (UTxO' (TxOut ctx era))
initialUtxoWithFunds networkId valueMap =
  pure
    . UTxO
    . Map.fromList
    . map (\(vKey, val) -> (txin vKey, txout vKey val))
    $ valueMap
 where
  txout vKey val =
    TxOut
      (mkVkAddress networkId vKey)
      (mkTxOutValue val)
      TxOutDatumNone
      ReferenceScriptNone
  txin vKey = genesisUTxOPseudoTxIn networkId (verificationKeyHash . castKey $ vKey)
  castKey (PaymentVerificationKey vkey) = GenesisUTxOVerificationKey vkey

initialUtxoForActors :: [(Actor, Value)] -> NetworkId -> IO (UTxOType Tx)
initialUtxoForActors actorToVal networkId = do
  initialUtxoWithFunds networkId =<< vkToVal
 where
  vkForActor actor = fmap fst (keysFor actor)
  vkToVal =
    forM actorToVal $ \(actor, val) ->
      vkForActor actor <&> (,val)

chainConfigFor :: HasCallStack => Actor -> FilePath -> SocketPath -> [Actor] -> ContestationPeriod -> IO ChainConfig
chainConfigFor me targetDir nodeSocket them cp = do
  when (me `elem` them) $
    failure $
      show me <> " must not be in " <> show them
  readConfigFile ("credentials" </> skName me) >>= writeFileBS (skTarget me)
  readConfigFile ("credentials" </> vkName me) >>= writeFileBS (vkTarget me)
  forM_ them $ \actor ->
    readConfigFile ("credentials" </> vkName actor) >>= writeFileBS (vkTarget actor)
  pure $
    defaultChainConfig
      { nodeSocket
      , cardanoSigningKey = skTarget me
      , cardanoVerificationKeys = [vkTarget himOrHer | himOrHer <- them]
      , contestationPeriod = cp :: ContestationPeriod
      }
 where
  skTarget x = targetDir </> skName x
  vkTarget x = targetDir </> vkName x
  skName x = actorName x <.> ".sk"
  vkName x = actorName x <.> ".vk"
