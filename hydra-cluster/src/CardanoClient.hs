-- | A basic cardano-node client that can talk to a local cardano-node.
--
-- The idea of this module is to provide a Haskell interface on top of cardano-cli's API,
-- using cardano-api types.
module CardanoClient where

import Hydra.Prelude

import Hydra.Cardano.Api

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Slotting.Time (SystemStart)
import qualified Data.Map as Map
import qualified Data.Set as Set
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

buildScriptAddress :: Script -> NetworkId -> Address ShelleyAddr
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

-- | Query the whole UTxO from node. Useful for debugging, but should obviously
-- not be used in production code.
queryUTxOWhole :: NetworkId -> FilePath -> IO UTxO
queryUTxOWhole networkId socket =
  let query =
        QueryInEra
          AlonzoEraInCardanoMode
          ( QueryInShelleyBasedEra
              ShelleyBasedEraAlonzo
              (QueryUTxO QueryUTxOWhole)
          )
   in UTxO.fromApi <$> runQuery networkId socket query

-- | Query current protocol parameters.
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
      (TxValidityNoLowerBound, TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra)
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
        (AddressInEra ShelleyAddressInAnyEra changeAddress)
        noOverrideWitness
 where
  bodyContent pparams =
    TxBodyContent
      (map mkWitness ins)
      (TxInsCollateral collateral)
      outs
      dummyFee
      (TxValidityNoLowerBound, TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra)
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
    SubmitFail err -> throwIO $ SubmitException{reason = show err, tx}

data CardanoClientException
  = QueryException Text
  | SubmitException {reason :: Text, tx :: Tx}
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
    TxOut (AddressInEra _ addr@ShelleyAddress{}) value _ -> do
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
