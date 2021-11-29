{-# LANGUAGE TypeApplications #-}

-- | A basic cardano-node client that can talk to a local cardano-node.
--
-- The idea of this module is to provide a Haskell interface on top of cardano-cli's API,
-- using cardano-api types.
module CardanoClient where

import Hydra.Prelude

-- We use quite a lot of stuff from the API so enumerating them all is pointless and
-- clutters the code
import Cardano.Api

import Cardano.Api.Shelley (
  PoolId,
  ProtocolParameters (protocolParamTxFeeFixed, protocolParamTxFeePerByte),
 )
import Cardano.Ledger.Keys (VKey (VKey))
import Cardano.Slotting.Time (SystemStart)
import CardanoNode (RunningNode (RunningNode))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Hydra.Chain.Direct.Util as Cardano
import qualified Hydra.Chain.Direct.Util as Hydra
import Hydra.Ledger.Cardano (
  CardanoTx,
  Utxo,
  Utxo' (Utxo),
  VerificationKey (PaymentVerificationKey),
  lovelaceToTxOutValue,
  mkVkAddress,
  unsafeCastHash,
  utxoFromTx,
  utxoMap,
  utxoPairs,
 )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (SubmitResult (..))

type NodeSocket = FilePath

-- TODO(SN): DRY with Hydra.Ledger.Cardano module

-- | Build an address give a key.
--
-- From <runAddressBuild https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/src/Cardano/CLI/Shelley/Run/Address.hs#L106>
-- Throws 'CardanoClientException' if the query fails.
buildAddress :: VerificationKey PaymentKey -> NetworkId -> Address ShelleyAddr
buildAddress vKey networkId =
  makeShelleyAddress networkId (PaymentCredentialByKey $ verificationKeyHash vKey) NoStakeAddress

buildScriptAddress :: Script PlutusScriptV1 -> NetworkId -> Address ShelleyAddr
buildScriptAddress script networkId =
  let hashed = hashScript script
   in makeShelleyAddress networkId (PaymentCredentialByScript hashed) NoStakeAddress

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

queryUtxoByTxIn :: NetworkId -> FilePath -> [TxIn] -> IO (UTxO AlonzoEra)
queryUtxoByTxIn networkId socket inputs =
  let query =
        QueryInEra
          AlonzoEraInCardanoMode
          ( QueryInShelleyBasedEra
              ShelleyBasedEraAlonzo
              (QueryUTxO (QueryUTxOByTxIn (Set.fromList inputs)))
          )
   in runQuery networkId socket query

-- | Extract ADA value from an output
-- NOTE(AB): there is txOutValueToLovelace in more recent cardano-api versions which
-- serves same purpose
txOutLovelace :: TxOut ctx era -> Lovelace
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

querySystemStart :: NetworkId -> FilePath -> IO SystemStart
querySystemStart networkId socket =
  queryNodeLocalState (localNodeConnectInfo networkId socket) Nothing QuerySystemStart >>= \case
    Left err -> throwIO $ QueryException (show err)
    Right result -> pure result

queryEraHistory :: NetworkId -> FilePath -> IO (EraHistory CardanoMode)
queryEraHistory networkId socket =
  queryNodeLocalState (localNodeConnectInfo networkId socket) Nothing (QueryEraHistory CardanoModeIsMultiEra) >>= \case
    Left err -> throwIO $ QueryException (show err)
    Right result -> pure result

-- | Build a "raw" transaction from a bunch of inputs, outputs and fees.
buildRaw :: [TxIn] -> [TxOut CtxTx AlonzoEra] -> Lovelace -> Either TxBodyError (TxBody AlonzoEra)
buildRaw txIns txOuts fee =
  makeTransactionBody txBodyContent
 where
  txBodyContent =
    TxBodyContent
      (map (,BuildTxWith $ KeyWitness KeyWitnessForSpending) txIns)
      TxInsCollateralNone
      txOuts
      (TxFeeExplicit TxFeesExplicitInAlonzoEra fee)
      (TxValidityNoLowerBound, TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra)
      TxMetadataNone
      TxAuxScriptsNone
      TxExtraKeyWitnessesNone
      (BuildTxWith noProtocolParameters)
      TxWithdrawalsNone
      TxCertificatesNone
      TxUpdateProposalNone
      TxMintNone
      TxScriptValidityNone

  noProtocolParameters = Nothing

build ::
  NetworkId ->
  FilePath ->
  Address ShelleyAddr ->
  [(TxIn, Maybe (Script PlutusScriptV1, ScriptData, ScriptRedeemer))] ->
  [TxIn] ->
  [TxOut CtxTx AlonzoEra] ->
  IO (Either TxBodyErrorAutoBalance (TxBody AlonzoEra))
build networkId socket changeAddress txIns collateral txOuts = do
  pparams <- queryProtocolParameters networkId socket
  systemStart <- querySystemStart networkId socket
  eraHistory <- queryEraHistory networkId socket
  stakePools <- queryStakePools networkId socket
  utxo <- queryUtxoByTxIn networkId socket (map fst txIns)
  pure $
    second extractBody $
      makeTransactionBodyAutoBalance
        AlonzoEraInCardanoMode
        systemStart
        eraHistory
        pparams
        stakePools
        utxo
        (txBodyContent pparams)
        (AddressInEra (ShelleyAddressInEra ShelleyBasedEraAlonzo) changeAddress)
        noOverrideWitness
 where
  txBodyContent pparams =
    TxBodyContent
      (map mkWitness txIns)
      (TxInsCollateral CollateralInAlonzoEra collateral)
      txOuts
      dummyFee
      (TxValidityNoLowerBound, TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra)
      (TxMetadataInEra TxMetadataInAlonzoEra (TxMetadata noMetadataMap))
      (TxAuxScripts AuxScriptsInAlonzoEra [])
      (TxExtraKeyWitnesses ExtraKeyWitnessesInAlonzoEra [])
      (BuildTxWith $ Just pparams)
      (TxWithdrawals WithdrawalsInAlonzoEra [])
      (TxCertificates CertificatesInAlonzoEra [] (BuildTxWith noStakeCredentialWitnesses))
      TxUpdateProposalNone
      (TxMintValue MultiAssetInAlonzoEra noMintedValue (BuildTxWith noPolicyIdToWitnessMap))
      TxScriptValidityNone
  noMintedValue = mempty
  noPolicyIdToWitnessMap = mempty
  noMetadataMap = mempty
  noStakeCredentialWitnesses = mempty
  noOverrideWitness = Nothing
  dummyFee = TxFeeExplicit TxFeesExplicitInAlonzoEra $ Lovelace 0

  mkWitness ::
    (TxIn, Maybe (Script PlutusScriptV1, ScriptData, ScriptRedeemer)) ->
    (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn AlonzoEra))
  mkWitness (txIn, Nothing) = (txIn, BuildTxWith $ KeyWitness KeyWitnessForSpending)
  mkWitness (txIn, Just (PlutusScript PlutusScriptV1 script, datum, redeemer)) = (txIn, BuildTxWith $ ScriptWitness ScriptWitnessForSpending sWit)
   where
    sWit =
      PlutusScriptWitness
        PlutusScriptV1InAlonzo
        PlutusScriptV1
        script
        (ScriptDatumForTxIn datum)
        redeemer
        (ExecutionUnits 0 0)

  extractBody (BalancedTxBody txBody _ _) = txBody

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
  = QueryException Text
  | SubmitException Text
  deriving (Show)

instance Exception CardanoClientException

-- TODO: This should return a 'Utxo' (from Hydra.Ledger.Cardano)
waitForPayment ::
  NetworkId ->
  FilePath ->
  Lovelace ->
  Address ShelleyAddr ->
  IO (UTxO AlonzoEra)
waitForPayment networkId socket amount addr = go
 where
  go = do
    utxo <- queryUtxo networkId socket [addr]
    let expectedPayment = selectPayment utxo
    if expectedPayment /= mempty
      then pure $ UTxO expectedPayment
      else threadDelay 1 >> go

  selectPayment (UTxO utxo) =
    Map.filter ((== amount) . txOutLovelace) utxo

waitForUtxo ::
  NetworkId ->
  FilePath ->
  Utxo ->
  IO ()
waitForUtxo networkId nodeSocket utxo =
  forM_ (snd <$> utxoPairs utxo) forEachUtxo
 where
  forEachUtxo :: TxOut CtxUTxO AlonzoEra -> IO ()
  forEachUtxo = \case
    TxOut (AddressInEra (ShelleyAddressInEra ShelleyBasedEraAlonzo) addr) value _ -> do
      void $
        waitForPayment
          networkId
          nodeSocket
          (txOutValueToLovelace value)
          addr
    txOut ->
      error $ "Unexpected TxOut " <> show txOut

-- TODO: This should return a 'Utxo' (from Hydra.Ledger.Cardano)
waitForTransaction ::
  NetworkId ->
  FilePath ->
  CardanoTx ->
  IO (UTxO AlonzoEra)
waitForTransaction networkId socket tx = go
 where
  txIns = Map.keys (utxoMap $ utxoFromTx tx)
  go = do
    utxo <- queryUtxoByTxIn networkId socket txIns
    if null (unUTxO utxo)
      then go
      else pure utxo

mkGenesisTx ::
  NetworkId ->
  ProtocolParameters ->
  -- | Amount of initialFunds
  Lovelace ->
  -- | Owner of the 'initialFund'.
  SigningKey PaymentKey ->
  -- | Recipient of this transaction.
  VerificationKey PaymentKey ->
  -- |Amount to pay
  Lovelace ->
  CardanoTx
mkGenesisTx networkId pparams initialAmount signingKey@(PaymentSigningKey sk) verificationKey amount =
  let initialInput =
        genesisUTxOPseudoTxIn
          networkId
          (unsafeCastHash $ verificationKeyHash $ getVerificationKey signingKey)

      rawTx = case buildRaw [initialInput] [] 0 of
        Left err -> error $ "Fail to build genesis transactions: " <> show err
        Right tx -> tx
      fee = calculateMinFee networkId rawTx Sizes{inputs = 1, outputs = 2, witnesses = 1} pparams

      changeAddr = mkVkAddress networkId (getVerificationKey signingKey)
      changeOutput =
        TxOut
          changeAddr
          (lovelaceToTxOutValue $ initialAmount - amount - fee)
          TxOutDatumNone

      recipientAddr = mkVkAddress networkId verificationKey
      recipientOutput =
        TxOut
          recipientAddr
          (lovelaceToTxOutValue amount)
          TxOutDatumNone
   in case buildRaw [initialInput] [recipientOutput, changeOutput] fee of
        Left err -> error $ "Fail to build genesis transations: " <> show err
        Right tx -> sign sk tx

generatePaymentToCommit ::
  NetworkId ->
  RunningNode ->
  Cardano.SigningKey ->
  Cardano.VerificationKey ->
  Natural ->
  IO Utxo
generatePaymentToCommit networkId (RunningNode _ nodeSocket) sk vk lovelace = do
  UTxO availableUtxo <- queryUtxo networkId nodeSocket [spendingAddress]
  let inputs = (,Nothing) <$> Map.keys availableUtxo
  build networkId nodeSocket spendingAddress inputs [] [theOutput] >>= \case
    Left e -> error (show e)
    Right body -> do
      let tx = sign sk body
      submit networkId nodeSocket tx
      convertUtxo <$> waitForPayment networkId nodeSocket amountLovelace receivingAddress
 where
  spendingSigningKey = PaymentSigningKey sk

  receivingVerificationKey = PaymentVerificationKey $ VKey vk

  spendingAddress = buildAddress (getVerificationKey spendingSigningKey) networkId

  receivingAddress = buildAddress receivingVerificationKey networkId

  theOutput =
    TxOut
      (shelleyAddressInEra receivingAddress)
      (lovelaceToTxOutValue amountLovelace)
      TxOutDatumNone

  amountLovelace = Lovelace $ fromIntegral lovelace

  convertUtxo (UTxO ledgerUtxo) = Utxo ledgerUtxo
