-- | A basic cardano-node client that can talk to a local cardano-node.
--
-- The idea of this module is to provide a Haskell interface on top of cardano-cli's API,
-- using cardano-api types.
module CardanoClient where

import Hydra.Prelude

-- We use quite a lot of stuff from the API so enumerating them all is pointless and
-- clutters the code
import Cardano.Api

import Cardano.Api.Shelley (ProtocolParameters (protocolParamTxFeeFixed, protocolParamTxFeePerByte))
import qualified Data.Set as Set
import qualified Hydra.Chain.Direct.Wallet as Hydra
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (SubmitResult (..))

type NodeSocket = FilePath

-- | Build an address give a key.
--
-- From <runAddressBuild https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/src/Cardano/CLI/Shelley/Run/Address.hs#L106>
-- Throws 'CardanoClientException' if the query fails.
buildAddress :: VerificationKey PaymentKey -> NetworkId -> Address ShelleyAddr
buildAddress vKey networkId =
  makeShelleyAddress networkId (PaymentCredentialByKey $ verificationKeyHash vKey) NoStakeAddress

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

-- | Extract ADA value from an output
-- NOTE(AB): there is txOutValueToLovelace in more recent cardano-api versions which
-- serves same purpose
txOutLovelace :: TxOut era -> Lovelace
txOutLovelace (TxOut _ val _) =
  case val of
    TxOutAdaOnly _ l -> l
    TxOutValue _ v -> selectLovelace v

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
  queryNodeLocalState (localNodeConnectInfo networkId socket) Nothing query >>= \case
    Left err -> throwIO $ QueryException (show err)
    Right (Left eraMismatch) -> throwIO $ QueryException (show eraMismatch)
    Right (Right result) -> pure result

localNodeConnectInfo :: NetworkId -> FilePath -> LocalNodeConnectInfo CardanoMode
localNodeConnectInfo = LocalNodeConnectInfo cardanoModeParams

cardanoModeParams :: ConsensusModeParams CardanoMode
cardanoModeParams = CardanoModeParams $ EpochSlots defaultByronEpochSlots
 where
  -- NOTE(AB): extracted from Parsers in cardano-cli, this is needed to run in 'cardanoMode' which
  -- is the default for cardano-cli
  defaultByronEpochSlots = 21600 :: Word64

queryTipSlotNo :: NetworkId -> FilePath -> IO SlotNo
queryTipSlotNo networkId socket =
  getLocalChainTip (localNodeConnectInfo networkId socket) >>= \case
    ChainTipAtGenesis -> pure 0
    ChainTip slotNo _ _ -> pure slotNo

-- | Build a "raw" transaction from a bunch of inputs, outputs and fees.
buildRaw :: [TxIn] -> [TxOut AlonzoEra] -> SlotNo -> Lovelace -> Either TxBodyError (TxBody AlonzoEra)
buildRaw txIns txOuts invalidAfter fee =
  makeTransactionBody txBodyContent
 where
  txBodyContent =
    TxBodyContent
      (map (,BuildTxWith $ KeyWitness KeyWitnessForSpending) txIns)
      (TxInsCollateral CollateralInAlonzoEra [])
      txOuts
      (TxFeeExplicit TxFeesExplicitInAlonzoEra fee)
      (TxValidityNoLowerBound, TxValidityUpperBound ValidityUpperBoundInAlonzoEra invalidAfter)
      (TxMetadataInEra TxMetadataInAlonzoEra (TxMetadata noMetadataMap))
      (TxAuxScripts AuxScriptsInAlonzoEra [])
      (BuildTxWith TxExtraScriptDataNone)
      (TxExtraKeyWitnesses ExtraKeyWitnessesInAlonzoEra [])
      (BuildTxWith noProtocolParameters)
      (TxWithdrawals WithdrawalsInAlonzoEra [])
      (TxCertificates CertificatesInAlonzoEra [] (BuildTxWith noStakeCredentialWitnesses))
      TxUpdateProposalNone
      (TxMintValue MultiAssetInAlonzoEra noMintedValue (BuildTxWith noPolicyIdToWitnessMap))
      TxScriptValidityNone
  noProtocolParameters = Nothing
  noMintedValue = mempty
  noPolicyIdToWitnessMap = mempty
  noMetadataMap = mempty
  noStakeCredentialWitnesses = mempty

calculateMinFee :: NetworkId -> TxBody AlonzoEra -> Sizes -> ProtocolParameters -> Lovelace
calculateMinFee networkId txBody Sizes{inputs, outputs, witnesses} pparams =
  let tx = makeSignedTransaction [] txBody
      noByronWitnesses = 0
   in estimateTransactionFee
        networkId
        (protocolParamTxFeeFixed pparams)
        (protocolParamTxFeePerByte pparams)
        tx
        inputs
        outputs
        noByronWitnesses
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
  | BuildRawException TxBodyError
  | SubmitException Text
  deriving (Show)

instance Exception CardanoClientException
