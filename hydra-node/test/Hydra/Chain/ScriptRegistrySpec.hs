module Hydra.Chain.ScriptRegistrySpec where

import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude

import "hydra-node" Hydra.Chain.ScriptRegistry (PublishScriptException (..), publishHydraScripts)

import "QuickCheck" Test.QuickCheck (generate)
import "hydra-cardano-api" Hydra.Cardano.Api (
  Address,
  NetworkId (..),
  PaymentKey,
  ShelleyAddr,
  SystemStart (..),
  UTxO,
  VerificationKey,
  lovelaceToValue,
  mkVkAddress,
  pattern ByronAddressInEra,
  pattern ReferenceScriptNone,
  pattern ShelleyAddressInEra,
  pattern TxOut,
  pattern TxOutDatumNone,
 )
import "hydra-node" Hydra.Chain.Backend (ChainBackend (..))
import "hydra-node" Hydra.Chain.Blockfrost.Client (
  APIBlockfrostError (..),
  BlockfrostException (..),
 )
import "hydra-node" Hydra.Options (ChainBackendOptions (..), defaultBlockfrostOptions)
import "hydra-tx" Test.Hydra.Tx.Gen (genKeyPair)

import "cardano-api" Cardano.Api.UTxO qualified as UTxO
import "cardano-ledger-api" Cardano.Ledger.Api.PParams (emptyPParams)
import "hydra-tx" Hydra.Ledger.Cardano.Evaluate (eraHistoryWithoutHorizon)

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
newtype ATestBackend = ATestBackend (VerificationKey PaymentKey)

instance ChainBackend ATestBackend where
  queryUTxOFor (ATestBackend vk) _ vk'
    | vk == vk' =
        throwIO $ BlockfrostError (NoUTxOFound (toAddress vk))
  queryUTxOFor _ _ vk' =
    failure $ "queryUTxOFor received unexpected VerificationKey: " <> show vk'

  -- Other methods are not needed for this test.
  -- These are functions that are not directly called by 'publishHydraScripts'.
  queryGenesisParameters _ = error "queryGenesisParameters"
  queryScriptRegistry _ _ = error "queryScriptRegistry"
  queryUTxO _ _ = error "queryUTxO"
  queryUTxOByTxIn _ _ = error "queryUTxOByTxIn"
  queryTip _ = error "queryTip"
  submitTransaction _ _ = error "submitTransaction"
  awaitTransaction _ _ = error "awaitTransaction"
  getBlockTime _ = error "getBlockTime"
  getOptions _ = Blockfrost defaultBlockfrostOptions
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
  getOptions _ = Blockfrost defaultBlockfrostOptions

  -- Other methods are not needed for this test.
  queryGenesisParameters _ = error "queryGenesisParameters"
  queryScriptRegistry _ _ = error "queryScriptRegistry"
  queryUTxO _ _ = error "queryUTxO"
  queryUTxOByTxIn _ _ = error "queryUTxOByTxIn"
  queryTip _ = error "queryTip"
  getBlockTime _ = error "getBlockTime"

toAddress :: VerificationKey PaymentKey -> Address ShelleyAddr
toAddress vk =
  case mkVkAddress Mainnet vk of
    ShelleyAddressInEra addr -> addr
    ByronAddressInEra{} -> error "toAddress: Byron address not supported"
