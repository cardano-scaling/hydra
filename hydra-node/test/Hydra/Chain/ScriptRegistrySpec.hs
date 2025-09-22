module Hydra.Chain.ScriptRegistrySpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Chain.ScriptRegistry (PublishScriptException (..), publishHydraScripts)

import Hydra.Cardano.Api (
  Address,
  NetworkId (..),
  PaymentKey,
  ReferenceScript,
  ShelleyAddr,
  SigningKey,
  SystemStart (..),
  TxOut,
  UTxO,
  VerificationKey,
  lovelaceToValue,
  mkVkAddress,
  pattern ShelleyAddressInEra,
  pattern TxOut,
  pattern TxOutDatumNone,
  pattern ReferenceScriptNone,
 )
import Hydra.Chain.Backend (ChainBackend (..))
import Hydra.Chain.Blockfrost.Client (
  APIBlockfrostError (..),
  BlockfrostException (..),
 )
import Hydra.Chain.CardanoClient (QueryPoint (..))
import Hydra.Options (BlockfrostOptions (..), ChainBackendOptions (..))
import Test.Hydra.Tx.Gen (genKeyPair)
import Test.QuickCheck (generate)

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Api.PParams (emptyPParams)
import Hydra.Ledger.Cardano.Evaluate (eraHistoryWithoutHorizon)

spec :: Spec
spec = describe "publishHydraScripts" $ do
  it "returns a list of TxIds when publishing is successful" $ do
    (vk, sk) <- generate genKeyPair
    txIn <- generate arbitrary
    let utxo =
          UTxO.singleton
            txIn
            ( TxOut
                (mkVkAddress Mainnet vk)
                (lovelaceToValue 100_000_000)
                TxOutDatumNone
                ReferenceScriptNone
            )
    let backend = SuccessfulBackend vk utxo
    txIds <- publishHydraScripts backend sk
    length txIds `shouldBe` 3

  it "throws PublishingFundsMissing error if no UTxO is found for the given address" $ do
    (vk, sk) <- generate genKeyPair
    let backend = ATestBackend vk
    publishHydraScripts backend sk `shouldThrow` \case
      PublishingFundsMissing{} -> True
      _ -> False

-- | A test backend that will throw 'NoUTxOFound' on 'queryUTxOFor' call.
data ATestBackend = ATestBackend (VerificationKey PaymentKey)

instance ChainBackend ATestBackend where
  queryUTxOFor (ATestBackend vk) _ vk'
    | vk == vk' =
        throwIO $ BlockfrostError (NoUTxOFound (toAddress vk))
  queryUTxOFor _ _ vk' =
    failure $ "queryUTxOFor received unexpected VerificationKey: " <> show vk'

  -- Other methods are not needed for this test.
  -- These are functions that are not directly called by 'publishHydraScripts'.
  queryGenesisParameters _ = undefined
  queryScriptRegistry _ _ = undefined
  queryUTxO _ _ = undefined
  queryUTxOByTxIn _ _ = undefined
  queryTip _ = undefined
  submitTransaction _ _ = undefined
  awaitTransaction _ _ = undefined
  getBlockTime _ = undefined
  getOptions _ = Blockfrost BlockfrostOptions{projectPath = "./"}
  queryNetworkId _ = pure Mainnet
  queryProtocolParameters _ _ = pure emptyPParams
  querySystemStart _ _ = SystemStart <$> liftIO getCurrentTime
  queryEraHistory _ _ = pure eraHistoryWithoutHorizon
  queryStakePools _ _ = pure mempty

-- | A test backend that simulates a successful script publishing.
data SuccessfulBackend = SuccessfulBackend (VerificationKey PaymentKey) UTxO

instance ChainBackend SuccessfulBackend where
  queryUTxOFor (SuccessfulBackend vk u) _ vk'
    | vk == vk' =
        pure u
  queryUTxOFor _ _ vk' =
    failure $ "queryUTxOFor received unexpected VerificationKey: " <> show vk'

  submitTransaction _ _ = pure ()

  awaitTransaction _ _ _ = pure mempty

  queryNetworkId _ = pure Mainnet
  queryProtocolParameters _ _ = pure emptyPParams
  querySystemStart _ _ = SystemStart <$> liftIO getCurrentTime
  queryEraHistory _ _ = pure eraHistoryWithoutHorizon
  queryStakePools _ _ = pure mempty
  getOptions _ = Blockfrost BlockfrostOptions{projectPath = "./"}

  -- Other methods are not needed for this test.
  queryGenesisParameters _ = undefined
  queryScriptRegistry _ _ = undefined
  queryUTxO _ _ = undefined
  queryUTxOByTxIn _ _ = undefined
  queryTip _ = undefined
  getBlockTime _ = undefined

toAddress :: VerificationKey PaymentKey -> Address ShelleyAddr
toAddress vk =
  case mkVkAddress Mainnet vk of
    ShelleyAddressInEra addr -> addr
