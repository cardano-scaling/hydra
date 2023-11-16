{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


-- | Utilities used across hydra-cluster
module Hydra.Cluster.Util where

import Hydra.Prelude

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Hydra.Cardano.Api (
  AsType (AsPaymentKey, AsSigningKey),
  HasTypeProxy (AsType),
  Key (VerificationKey, getVerificationKey),
  PaymentKey,
  SigningKey,
  SocketPath,
  TextEnvelopeError (TextEnvelopeAesonDecodeError),
  deserialiseFromTextEnvelope,
  textEnvelopeToJSON,
 )
import Hydra.Cluster.Fixture (Actor, actorName)
import Hydra.ContestationPeriod (ContestationPeriod)
import Hydra.Ledger.Cardano (genSigningKey)
import Hydra.Options (ChainConfig (..), defaultChainConfig)
import Paths_hydra_cluster qualified as Pkg
import System.FilePath ((<.>), (</>))
import Test.Hydra.Prelude (failure, Expectation, shouldBe)
import Test.QuickCheck (generate)
import Hydra.Chain (PostChainTx)
import Hydra.Chain (ChainEvent)
import Hydra.Ledger (IsTx)
import Hydra.Party (Party)
import Hydra.Chain (OnChainTx(OnInitTx, contestationPeriod, parties))
import Hydra.Ledger (IsTx(UTxOType))

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

seedInitialUTxOFromOffline :: IsTx tx => UTxOType tx -> FilePath -> IO ()
seedInitialUTxOFromOffline utxo targetDir = do
  -- i assume a static file might be too rigid ? we can keep around constants and then write them to disk for each test
  -- readConfigFile "initial-utxo.json" >>= writeFileBS (targetDir </> "initial-utxo.json")

  writeFileBS (targetDir </> "utxo.json") . toStrict . Aeson.encode $ utxo 
  
  -- Aeson.throwDecodeStrict =<< readFileBS (targetDir </> "utxo.json")
  pure ()
  
