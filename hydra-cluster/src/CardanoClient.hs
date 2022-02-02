-- | A basic cardano-node client that can talk to a local cardano-node.
--
-- The idea of this module is to provide a Haskell interface on top of cardano-cli's API,
-- using cardano-api types.
module CardanoClient where

import Hydra.Prelude

import Hydra.Cardano.Api

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Slotting.Time (SystemStart)
import CardanoNode (RunningNode (RunningNode))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Hydra.Chain.Direct.Util as Hydra
import Hydra.Ledger.Cardano (
  CardanoTx,
 )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (SubmitResult (..))

type NodeSocket = FilePath

data CardanoClient = CardanoClient
  { queryUTxOByAddress :: [Address ShelleyAddr] -> IO UTxO
  , networkId :: NetworkId
  }

mkCardanoClient :: NetworkId -> FilePath -> CardanoClient
mkCardanoClient networkId filePath =
  CardanoClient
    { queryUTxOByAddress = queryUTxO networkId filePath
    , networkId
    }

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
queryUTxO :: NetworkId -> FilePath -> [Address ShelleyAddr] -> IO UTxO
queryUTxO networkId socket addresses =
  let query =
        QueryInEra
          AlonzoEraInCardanoMode
          ( QueryInShelleyBasedEra
              ShelleyBasedEraAlonzo
              ( QueryUTxO
                  (QueryUTxOByAddress (Set.fromList $ map AddressShelley addresses))
              )
          )
   in UTxO.fromApi <$> runQuery networkId socket query

queryUTxOByTxIn :: NetworkId -> FilePath -> [TxIn] -> IO UTxO
queryUTxOByTxIn networkId socket inputs =
  let query =
        QueryInEra
          AlonzoEraInCardanoMode
          ( QueryInShelleyBasedEra
              ShelleyBasedEraAlonzo
              (QueryUTxO (QueryUTxOByTxIn (Set.fromList inputs)))
          )
   in UTxO.fromApi <$> runQuery networkId socket query

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
  utxo <- queryUTxOByTxIn networkId socket (map fst txIns)
  pure $
    second extractBody $
      makeTransactionBodyAutoBalance
        AlonzoEraInCardanoMode
        systemStart
        eraHistory
        pparams
        stakePools
        (UTxO.toApi utxo)
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
sign :: SigningKey PaymentKey -> TxBody AlonzoEra -> Tx AlonzoEra
sign signingKey txBody =
  makeSignedTransaction [makeShelleyKeyWitness txBody (WitnessPaymentKey signingKey)] txBody

-- | Submit a (signed) transaction to the node.
--
-- Throws 'CardanoClientException' if submission fails.
submit :: NetworkId -> FilePath -> Tx AlonzoEra -> IO ()
submit networkId socket tx =
  submitTxToNodeLocal (localNodeConnectInfo networkId socket) (TxInMode tx AlonzoEraInCardanoMode) >>= \case
    SubmitSuccess -> pure ()
    SubmitFail err -> throwIO $ SubmitException{reason = show err, tx}

data CardanoClientException
  = QueryException Text
  | SubmitException {reason :: Text, tx :: Tx AlonzoEra}
  deriving (Show)

instance Exception CardanoClientException

-- TODO: This should return a 'UTxO' (from Hydra.Ledger.Cardano)
waitForPayment ::
  NetworkId ->
  FilePath ->
  Lovelace ->
  Address ShelleyAddr ->
  IO UTxO
waitForPayment networkId socket amount addr = go
 where
  go = do
    utxo <- queryUTxO networkId socket [addr]
    let expectedPayment = selectPayment utxo
    if expectedPayment /= mempty
      then pure $ UTxO expectedPayment
      else threadDelay 1 >> go

  selectPayment (UTxO utxo) =
    Map.filter ((== amount) . txOutLovelace) utxo

waitForUTxO ::
  NetworkId ->
  FilePath ->
  UTxO ->
  IO ()
waitForUTxO networkId nodeSocket utxo =
  forM_ (snd <$> UTxO.pairs utxo) forEachUTxO
 where
  forEachUTxO :: TxOut CtxUTxO AlonzoEra -> IO ()
  forEachUTxO = \case
    TxOut (AddressInEra (ShelleyAddressInEra ShelleyBasedEraAlonzo) addr) value _ -> do
      void $
        waitForPayment
          networkId
          nodeSocket
          (txOutValueToLovelace value)
          addr
    txOut ->
      error $ "Unexpected TxOut " <> show txOut

-- TODO: This should return a 'UTxO' (from Hydra.Ledger.Cardano)
waitForTransaction ::
  NetworkId ->
  FilePath ->
  CardanoTx ->
  IO UTxO
waitForTransaction networkId socket tx = go
 where
  txIns = Map.keys (UTxO.toMap $ utxoFromTx tx)
  go = do
    utxo <- queryUTxOByTxIn networkId socket txIns
    if null utxo
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
mkGenesisTx networkId pparams initialAmount signingKey verificationKey amount =
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
          (TxOutDatumHash ScriptDataInAlonzoEra (hashScriptData $ fromPlutusData Hydra.markerDatum))

      recipientAddr = mkVkAddress networkId verificationKey
      recipientOutput =
        TxOut
          recipientAddr
          (lovelaceToTxOutValue amount)
          TxOutDatumNone
   in case buildRaw [initialInput] [recipientOutput, changeOutput] fee of
        Left err -> error $ "Fail to build genesis transations: " <> show err
        Right tx -> sign signingKey tx

generatePaymentToCommit ::
  NetworkId ->
  RunningNode ->
  SigningKey PaymentKey ->
  VerificationKey PaymentKey ->
  Lovelace ->
  IO UTxO
generatePaymentToCommit networkId (RunningNode _ nodeSocket) spendingSigningKey receivingVerificationKey lovelace = do
  UTxO availableUTxO <- queryUTxO networkId nodeSocket [spendingAddress]
  let inputs = (,Nothing) <$> Map.keys (Map.filter (not . Hydra.isMarkedOutput) availableUTxO)
  build networkId nodeSocket spendingAddress inputs [] [theOutput] >>= \case
    Left e -> error (show e)
    Right body -> do
      let tx = sign spendingSigningKey body
      submit networkId nodeSocket tx
      convertUTxO <$> waitForPayment networkId nodeSocket lovelace receivingAddress
 where
  spendingAddress = buildAddress (getVerificationKey spendingSigningKey) networkId

  receivingAddress = buildAddress receivingVerificationKey networkId

  theOutput =
    TxOut
      (shelleyAddressInEra receivingAddress)
      (lovelaceToTxOutValue lovelace)
      TxOutDatumNone

  convertUTxO (UTxO ledgerUTxO) = UTxO ledgerUTxO

postSeedPayment :: NetworkId -> ProtocolParameters -> Lovelace -> FilePath -> SigningKey PaymentKey -> Lovelace -> IO ()
postSeedPayment networkId pparams initialAmount nodeSocket signingKey amountLovelace = do
  let genesisTx = mkGenesisTx networkId pparams initialAmount signingKey verificationKey amountLovelace
  submit networkId nodeSocket genesisTx
  void $ waitForPayment networkId nodeSocket amountLovelace address
 where
  verificationKey = getVerificationKey signingKey

  address = buildAddress verificationKey networkId
