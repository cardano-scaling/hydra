-- | A basic cardano-node client that can talk to a local cardano-node.
--
-- The idea of this module is to provide a Haskell interface on top of cardano-cli's API,
-- using cardano-api types.
module CardanoClient where

import Hydra.Prelude

-- We use quite a lot of stuff from the API so enumerating them all is pointless and
-- clutters the code
import Cardano.Api

import Cardano.Api.Shelley (ProtocolParameters (protocolParamTxFeeFixed, protocolParamTxFeePerByte), VerificationKey (PaymentVerificationKey))
import Cardano.CLI.Shelley.Run.Address (buildShelleyAddress)
import Cardano.CLI.Shelley.Run.Query (executeQuery, queryQueryTip)
import qualified Cardano.Ledger.Keys as Keys
import qualified Data.Set as Set
import qualified Hydra.Chain.Direct.Wallet as Hydra
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (SubmitResult (..))

type NodeSocket = FilePath

-- | Build an address give a key.
--
-- From <runAddressBuild https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/src/Cardano/CLI/Shelley/Run/Address.hs#L106>
-- Throws 'CardanoClientException' if the query fails.
buildAddress :: Hydra.VerificationKey -> NetworkId -> IO (Address ShelleyAddr)
buildAddress vKey networkId = do
  let shelleyKey = PaymentVerificationKey $ Keys.VKey vKey
  runExceptT (buildShelleyAddress shelleyKey Nothing networkId) >>= \case
    Left err -> throwIO $ BuildAddressException (show err)
    Right addr -> pure addr

-- |Query UTxO for all given addresses.
--
-- This query is specialised for Shelley addresses in Alonzo era.
-- Throws 'CardanoClientException' if query fails.
queryUtxo :: NetworkId -> FilePath -> [Address ShelleyAddr] -> IO (UTxO AlonzoEra)
queryUtxo networkId socket addresses =
  let query =
        QueryInEra
          AlonzoEraInCardanoMode
          ( QueryInShelleyBasedEra
              ShelleyBasedEraAlonzo
              ( QueryUTxO
                  (QueryUTxOByAddress (Set.fromList $ map AddressShelley addresses))
              )
          )
   in runQuery networkId socket query

-- |Query current protocol parameters.
--
-- Throws 'CardanoClientException' if query fails.
queryProtocolParameters :: NetworkId -> FilePath -> IO ProtocolParameters
queryProtocolParameters networkId socket =
  let query =
        QueryInEra
          AlonzoEraInCardanoMode
          ( QueryInShelleyBasedEra
              ShelleyBasedEraAlonzo
              QueryProtocolParameters
          )
   in runQuery networkId socket query

runQuery :: NetworkId -> FilePath -> QueryInMode CardanoMode (Either EraMismatch a) -> IO a
runQuery networkId socket query =
  runExceptT (executeQuery AlonzoEra cardanoModeParams (localNodeConnectInfo networkId socket) query) >>= \case
    Left err -> throwIO $ QueryException (show err)
    Right utxo -> pure utxo

localNodeConnectInfo :: NetworkId -> FilePath -> LocalNodeConnectInfo CardanoMode
localNodeConnectInfo = LocalNodeConnectInfo cardanoModeParams

cardanoModeParams :: ConsensusModeParams CardanoMode
cardanoModeParams = CardanoModeParams $ EpochSlots defaultByronEpochSlots
 where
  -- NOTE(AB): extracted from Parsers in cardano-cli, this is needed to run in 'cardanoMode' which
  -- is the default for cardano-cli
  defaultByronEpochSlots = 21600 :: Word64

queryTipSlotNo :: NetworkId -> FilePath -> IO SlotNo
queryTipSlotNo networkId socket = do
  tip <- fst <$> queryQueryTip (localNodeConnectInfo networkId socket) Nothing
  pure $ case tip of
    ChainTipAtGenesis -> 0
    ChainTip slotNo _ _ -> slotNo

-- | Build a "raw" transaction from a bunch of inputs, outputs and fees.
buildRaw :: [TxIn] -> [TxOut AlonzoEra] -> SlotNo -> Lovelace -> IO (TxBody AlonzoEra)
buildRaw txIns txOuts invalidAfter fee = do
  let txBodyContent =
        TxBodyContent
          (map (,BuildTxWith $ KeyWitness KeyWitnessForSpending) txIns)
          (TxInsCollateral CollateralInAlonzoEra [])
          txOuts
          (TxFeeExplicit TxFeesExplicitInAlonzoEra fee)
          (TxValidityNoLowerBound, TxValidityUpperBound ValidityUpperBoundInAlonzoEra invalidAfter)
          (TxMetadataInEra TxMetadataInAlonzoEra (TxMetadata mempty))
          (TxAuxScripts AuxScriptsInAlonzoEra [])
          (BuildTxWith TxExtraScriptDataNone)
          (TxExtraKeyWitnesses ExtraKeyWitnessesInAlonzoEra [])
          (BuildTxWith Nothing)
          (TxWithdrawals WithdrawalsInAlonzoEra [])
          (TxCertificates CertificatesInAlonzoEra [] (BuildTxWith mempty))
          TxUpdateProposalNone
          (TxMintValue MultiAssetInAlonzoEra mempty (BuildTxWith mempty))
          TxScriptValidityNone

  either (throwIO . TransactionBuildRawException . show) pure $ makeTransactionBody txBodyContent

calculateMinFee :: NetworkId -> TxBody AlonzoEra -> Sizes -> ProtocolParameters -> Lovelace
calculateMinFee networkId txBody Sizes{inputs, outputs, witnesses} pparams =
  let tx = makeSignedTransaction [] txBody
   in estimateTransactionFee
        networkId
        (protocolParamTxFeeFixed pparams)
        (protocolParamTxFeePerByte pparams)
        tx
        inputs
        outputs
        0
        witnesses

data Sizes = Sizes
  { inputs :: Int
  , outputs :: Int
  , witnesses :: Int
  }
  deriving (Eq, Show)

defaultSizes :: Sizes
defaultSizes = Sizes{inputs = 0, outputs = 0, witnesses = 0}

-- | Sign a transaction body with given signing key.
sign :: Hydra.SigningKey -> TxBody AlonzoEra -> Tx AlonzoEra
sign signingKey txBody =
  makeSignedTransaction [makeShelleyKeyWitness txBody (WitnessPaymentKey $ PaymentSigningKey signingKey)] txBody

-- | Submit a (signed) transaction to the node.
--
-- Throws 'CardanoClientException' if submission fails.
submit :: NetworkId -> FilePath -> Tx AlonzoEra -> IO ()
submit networkId socket tx =
  submitTxToNodeLocal (localNodeConnectInfo networkId socket) (TxInMode tx AlonzoEraInCardanoMode) >>= \case
    SubmitSuccess -> pure ()
    SubmitFail err -> throwIO $ SubmitException $ show err

data CardanoClientException
  = BuildAddressException Text
  | QueryException Text
  | TransactionBuildRawException Text
  | SubmitException Text
  deriving (Show)

instance Exception CardanoClientException
