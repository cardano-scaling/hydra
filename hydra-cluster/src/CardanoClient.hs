-- | A cardano-node client used in end-to-end tests and benchmarks.
--
-- This modules contains some more functions besides the re-exported basic
-- querying of hydra-node's 'Hydra.Chain.CardanoClient'.
module CardanoClient (
  module Hydra.Chain.CardanoClient,
  module CardanoClient,
) where

import Hydra.Prelude

import Hydra.Cardano.Api hiding (Block)
import Hydra.Chain.CardanoClient

import qualified Cardano.Api.UTxO as UTxO
import qualified Data.Map as Map
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (SubmitResult (..))

-- TODO(SN): DRY with Hydra.Cardano.Api

-- | Build an address give a key.
--
-- From <runAddressBuild https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/src/Cardano/CLI/Shelley/Run/Address.hs#L106>
-- Throws 'CardanoClientException' if the query fails.
buildAddress :: VerificationKey PaymentKey -> NetworkId -> Address ShelleyAddr
buildAddress vKey networkId =
  makeShelleyAddress networkId (PaymentCredentialByKey $ verificationKeyHash vKey) NoStakeAddress

buildScriptAddress :: Script -> NetworkId -> Address ShelleyAddr
buildScriptAddress script networkId =
  let hashed = hashScript script
   in makeShelleyAddress networkId (PaymentCredentialByScript hashed) NoStakeAddress

queryStakePools :: NetworkId -> FilePath -> IO (Set PoolId)
queryStakePools networkId socket =
  let query =
        QueryInEra
          AlonzoEraInCardanoMode
          ( QueryInShelleyBasedEra
              ShelleyBasedEraAlonzo
              QueryStakePools
          )
   in runQuery networkId socket query

-- | Build a "raw" transaction from a bunch of inputs, outputs and fees.
buildRaw :: [TxIn] -> [TxOut CtxTx] -> Lovelace -> Either TxBodyError TxBody
buildRaw ins outs fee =
  makeTransactionBody bodyContent
 where
  noProtocolParameters = Nothing
  bodyContent =
    TxBodyContent
      (map (,BuildTxWith $ KeyWitness KeyWitnessForSpending) ins)
      TxInsCollateralNone
      outs
      (TxFeeExplicit fee)
      (TxValidityNoLowerBound, TxValidityNoUpperBound)
      TxMetadataNone
      TxAuxScriptsNone
      TxExtraKeyWitnessesNone
      (BuildTxWith noProtocolParameters)
      TxWithdrawalsNone
      TxCertificatesNone
      TxUpdateProposalNone
      TxMintValueNone
      TxScriptValidityNone

build ::
  NetworkId ->
  FilePath ->
  Address ShelleyAddr ->
  [(TxIn, Maybe (Script, ScriptData, ScriptRedeemer))] ->
  [TxIn] ->
  [TxOut CtxTx] ->
  IO (Either TxBodyErrorAutoBalance TxBody)
build networkId socket changeAddress ins collateral outs = do
  pparams <- queryProtocolParameters networkId socket
  systemStart <- querySystemStart networkId socket
  eraHistory <- queryEraHistory networkId socket
  stakePools <- queryStakePools networkId socket
  utxo <- queryUTxOByTxIn networkId socket (map fst ins)
  pure $
    second balancedTxBody $
      makeTransactionBodyAutoBalance
        AlonzoEraInCardanoMode
        systemStart
        eraHistory
        pparams
        stakePools
        (UTxO.toApi utxo)
        (bodyContent pparams)
        (ShelleyAddressInEra changeAddress)
        noOverrideWitness
 where
  bodyContent pparams =
    TxBodyContent
      (map mkWitness ins)
      (TxInsCollateral collateral)
      outs
      dummyFee
      (TxValidityNoLowerBound, TxValidityNoUpperBound)
      (TxMetadataInEra (TxMetadata noMetadataMap))
      (TxAuxScripts [])
      (TxExtraKeyWitnesses [])
      (BuildTxWith $ Just pparams)
      (TxWithdrawals WithdrawalsInAlonzoEra [])
      (TxCertificates CertificatesInAlonzoEra [] (BuildTxWith noStakeCredentialWitnesses))
      TxUpdateProposalNone
      (TxMintValue noMintedValue (BuildTxWith noPolicyIdToWitnessMap))
      TxScriptValidityNone
  noMintedValue = mempty
  noPolicyIdToWitnessMap = mempty
  noMetadataMap = mempty
  noStakeCredentialWitnesses = mempty
  noOverrideWitness = Nothing
  dummyFee = TxFeeExplicit (Lovelace 0)

  mkWitness ::
    (TxIn, Maybe (Script, ScriptData, ScriptRedeemer)) ->
    (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn))
  mkWitness (txIn, Nothing) = (txIn, BuildTxWith $ KeyWitness KeyWitnessForSpending)
  mkWitness (txIn, Just (PlutusScript script, datum, redeemer)) = (txIn, BuildTxWith $ ScriptWitness ScriptWitnessForSpending sWit)
   where
    sWit =
      PlutusScriptWitness
        script
        (ScriptDatumForTxIn datum)
        redeemer
        (ExecutionUnits 0 0)

calculateMinFee :: NetworkId -> TxBody -> Sizes -> ProtocolParameters -> Lovelace
calculateMinFee networkId body Sizes{inputs, outputs, witnesses} pparams =
  let tx = makeSignedTransaction [] body
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
sign :: SigningKey PaymentKey -> TxBody -> Tx
sign signingKey body =
  makeSignedTransaction
    [makeShelleyKeyWitness body (WitnessPaymentKey signingKey)]
    body

-- | Submit a (signed) transaction to the node.
--
-- Throws 'CardanoClientException' if submission fails.
submit :: NetworkId -> FilePath -> Tx -> IO ()
submit networkId socket tx =
  submitTxToNodeLocal (localNodeConnectInfo networkId socket) (TxInMode tx AlonzoEraInCardanoMode) >>= \case
    SubmitSuccess -> pure ()
    SubmitFail err -> throwIO $ ClientSubmitException{reason = show err, tx}

data CardanoClientException
  = ClientQueryException QueryException
  | ClientSubmitException {reason :: Text, tx :: Tx}
  deriving (Show)

instance Exception CardanoClientException

-- TODO: This should return a 'UTxO' (from Hydra.Ledger.Cardano)
waitForPayment ::
  NetworkId ->
  FilePath ->
  Lovelace ->
  Address ShelleyAddr ->
  IO UTxO
waitForPayment networkId socket amount addr =
  go
 where
  go = do
    utxo <- queryUTxO networkId socket [addr]
    let expectedPayment = selectPayment utxo
    if expectedPayment /= mempty
      then pure $ UTxO expectedPayment
      else threadDelay 1 >> go

  selectPayment (UTxO utxo) =
    Map.filter ((== amount) . selectLovelace . txOutValue) utxo

waitForUTxO ::
  NetworkId ->
  FilePath ->
  UTxO ->
  IO ()
waitForUTxO networkId nodeSocket utxo =
  forM_ (snd <$> UTxO.pairs utxo) forEachUTxO
 where
  forEachUTxO :: TxOut CtxUTxO -> IO ()
  forEachUTxO = \case
    TxOut (ShelleyAddressInEra addr@ShelleyAddress{}) value _ -> do
      void $
        waitForPayment
          networkId
          nodeSocket
          (selectLovelace value)
          addr
    txOut ->
      error $ "Unexpected TxOut " <> show txOut

waitForTransaction ::
  NetworkId ->
  FilePath ->
  Tx ->
  IO UTxO
waitForTransaction networkId socket tx =
  go
 where
  ins = Map.keys (UTxO.toMap $ utxoFromTx tx)
  go = do
    utxo <- queryUTxOByTxIn networkId socket ins
    if null utxo
      then go
      else pure utxo

mkGenesisTx ::
  NetworkId ->
  ProtocolParameters ->
  -- | Owner of the 'initialFund'.
  SigningKey PaymentKey ->
  -- | Amount of initialFunds
  Lovelace ->
  -- | Recipients and amounts to pay in this transaction.
  [(VerificationKey PaymentKey, Lovelace)] ->
  Tx
mkGenesisTx networkId pparams signingKey initialAmount recipients =
  case buildRaw [initialInput] (recipientOutputs <> [changeOutput]) fee of
    Left err -> error $ "Fail to build genesis transations: " <> show err
    Right tx -> sign signingKey tx
 where
  initialInput =
    genesisUTxOPseudoTxIn
      networkId
      (unsafeCastHash $ verificationKeyHash $ getVerificationKey signingKey)

  fee = calculateMinFee networkId rawTx Sizes{inputs = 1, outputs = length recipients + 1, witnesses = 1} pparams
  rawTx = case buildRaw [initialInput] [] 0 of
    Left err -> error $ "Fail to build genesis transactions: " <> show err
    Right tx -> tx

  totalSent = foldMap snd recipients

  changeAddr = mkVkAddress networkId (getVerificationKey signingKey)
  changeOutput =
    TxOut
      changeAddr
      (lovelaceToValue $ initialAmount - totalSent - fee)
      TxOutDatumNone

  recipientOutputs =
    flip map recipients $ \(vk, ll) ->
      TxOut
        (mkVkAddress networkId vk)
        (lovelaceToValue ll)
        TxOutDatumNone
