module Hydra.Chain.ScriptRegistrySpec where

import Hydra.Prelude
import Test.Hydra.Prelude hiding (HydraTestnet (..))

import Hydra.Chain.ScriptRegistry (PublishScriptException (..), publishHydraScripts)

import Hydra.Cardano.Api (
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
import Hydra.Chain.Backend (ChainBackend (..))
import Hydra.Chain.Blockfrost.Client (
  APIBlockfrostError (..),
  BlockfrostException (..),
 )
import Test.Hydra.Tx.Gen (genKeyPair)
import Test.QuickCheck (generate)

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Api.PParams (emptyPParams)
import Test.Hydra.Ledger.Cardano.Fixtures (eraHistoryWithoutHorizon)

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
    txIds <- runSuccessfulBackend (vk, utxo) $ publishHydraScripts sk
    length txIds `shouldBe` 2

  it "throws PublishingFundsMissing error if no UTxO is found for the given address" $ do
    (vk, sk) <- generate genKeyPair
    runATestBackend vk (publishHydraScripts sk) `shouldThrow` \case
      PublishingFundsMissing{} -> True
      _ -> False

-- | A test backend that will throw 'NoUTxOFound' on 'queryUTxOFor' call.
newtype ATestBackend a = ATestBackend (ReaderT (VerificationKey PaymentKey) IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

runATestBackend :: VerificationKey PaymentKey -> ATestBackend a -> IO a
runATestBackend vk (ATestBackend m) = runReaderT m vk

instance ChainBackend ATestBackend where
  queryUTxOFor _ vk' = ATestBackend $ do
    vk <- ask
    if vk == vk'
      then throwIO $ BlockfrostError (NoUTxOFound (toAddress vk))
      else failure $ "queryUTxOFor received unexpected VerificationKey: " <> show vk'

  -- Other methods are not needed for this test.
  queryGenesisParameters = error "queryGenesisParameters"
  queryScriptRegistry _ = error "queryScriptRegistry"
  queryUTxO _ = error "queryUTxO"
  queryUTxOByTxIn _ = error "queryUTxOByTxIn"
  queryTip = error "queryTip"
  submitTransaction _ = error "submitTransaction"
  awaitTransaction _ _ = error "awaitTransaction"
  getBlockTime = error "getBlockTime"
  getQueryDelay = pure 0
  queryNetworkId = pure Mainnet
  queryProtocolParameters _ = pure emptyPParams
  querySystemStart _ = SystemStart <$> liftIO getCurrentTime
  queryEraHistory _ = pure eraHistoryWithoutHorizon
  queryStakePools _ = pure mempty

-- | A test backend that simulates a successful script publishing.
newtype SuccessfulBackend a = SuccessfulBackend (ReaderT (VerificationKey PaymentKey, UTxO) IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

runSuccessfulBackend :: (VerificationKey PaymentKey, UTxO) -> SuccessfulBackend a -> IO a
runSuccessfulBackend env (SuccessfulBackend m) = runReaderT m env

instance ChainBackend SuccessfulBackend where
  queryUTxOFor _ vk' = SuccessfulBackend $ do
    (vk, u) <- ask
    if vk == vk'
      then pure u
      else failure $ "queryUTxOFor received unexpected VerificationKey: " <> show vk'

  submitTransaction _ = pure ()

  awaitTransaction _ _ = pure mempty

  queryNetworkId = pure Mainnet
  queryProtocolParameters _ = pure emptyPParams
  querySystemStart _ = SystemStart <$> liftIO getCurrentTime
  queryEraHistory _ = pure eraHistoryWithoutHorizon
  queryStakePools _ = pure mempty
  getQueryDelay = pure 0

  -- Other methods are not needed for this test.
  queryGenesisParameters = error "queryGenesisParameters"
  queryScriptRegistry _ = error "queryScriptRegistry"
  queryUTxO _ = error "queryUTxO"
  queryUTxOByTxIn _ = error "queryUTxOByTxIn"
  queryTip = error "queryTip"
  getBlockTime = error "getBlockTime"

toAddress :: VerificationKey PaymentKey -> Address ShelleyAddr
toAddress vk =
  case mkVkAddress Mainnet vk of
    ShelleyAddressInEra addr -> addr
    ByronAddressInEra{} -> error "toAddress: Byron address not supported"
