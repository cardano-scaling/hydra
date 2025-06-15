module Hydra.Chain.Backend where

import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.Map.Strict qualified as Map
import Hydra.Cardano.Api
import Hydra.Chain.CardanoClient qualified as CardanoClient
import Hydra.Contract.Dummy (dummyMintingScript)
import Hydra.Options (ChainBackendOptions)
import Hydra.Tx (ScriptRegistry)

blockfrostProjectPath :: FilePath
blockfrostProjectPath = "blockfrost-project.txt"

class ChainBackend a where
  queryGenesisParameters :: (MonadIO m, MonadThrow m) => a -> m (GenesisParameters ShelleyEra)
  queryScriptRegistry :: (MonadIO m, MonadThrow m) => a -> [TxId] -> m ScriptRegistry
  queryNetworkId :: (MonadIO m, MonadThrow m) => a -> m NetworkId
  queryTip :: (MonadIO m, MonadThrow m) => a -> m ChainPoint
  queryUTxO :: (MonadIO m, MonadThrow m) => a -> [Address ShelleyAddr] -> m UTxO
  queryUTxOByTxIn :: (MonadIO m, MonadThrow m) => a -> [TxIn] -> m UTxO
  queryEraHistory :: (MonadIO m, MonadThrow m) => a -> CardanoClient.QueryPoint -> m EraHistory
  querySystemStart :: (MonadIO m, MonadThrow m) => a -> CardanoClient.QueryPoint -> m SystemStart
  queryProtocolParameters :: (MonadIO m, MonadThrow m) => a -> CardanoClient.QueryPoint -> m (PParams LedgerEra)
  queryStakePools :: (MonadIO m, MonadThrow m) => a -> CardanoClient.QueryPoint -> m (Set PoolId)
  queryUTxOFor :: (MonadIO m, MonadThrow m) => a -> CardanoClient.QueryPoint -> VerificationKey PaymentKey -> m UTxO
  submitTransaction :: (MonadIO m, MonadThrow m) => a -> Tx -> m ()
  awaitTransaction :: (MonadIO m, MonadThrow m) => a -> Tx -> VerificationKey PaymentKey -> m UTxO
  getOptions :: a -> ChainBackendOptions
  getBlockTime :: a -> (MonadIO m, MonadThrow m) => m NominalDiffTime

buildTransaction ::
  ChainBackend backend =>
  backend ->
  -- | Change address to send
  AddressInEra ->
  -- | Unspent transaction outputs to spend.
  UTxO ->
  -- | Collateral inputs.
  [TxIn] ->
  -- | Outputs to create.
  [TxOut CtxTx] ->
  IO (Either (TxBodyErrorAutoBalance Era) Tx)
buildTransaction backend changeAddress body utxoToSpend outs = do
  pparams <- queryProtocolParameters backend CardanoClient.QueryTip
  buildTransactionWithPParams pparams backend changeAddress body utxoToSpend outs Nothing

buildTransactionWithMintingScript ::
  ChainBackend backend =>
  backend ->
  -- | Change address to send
  AddressInEra ->
  -- | Unspent transaction outputs to spend.
  UTxO ->
  -- | Collateral inputs.
  [TxIn] ->
  -- | Outputs to create.
  [TxOut CtxTx] ->
  Maybe PlutusScript ->
  IO (Either (TxBodyErrorAutoBalance Era) Tx)
buildTransactionWithMintingScript backend changeAddress body utxoToSpend outs mintingScript = do
  pparams <- queryProtocolParameters backend CardanoClient.QueryTip
  buildTransactionWithPParams pparams backend changeAddress body utxoToSpend outs mintingScript

-- | Construct a simple payment consuming some inputs and producing some
-- outputs (no certificates or withdrawals involved).
--
-- On success, the returned transaction is fully balanced. On error, return
-- `TxBodyErrorAutoBalance`.
buildTransactionWithPParams ::
  ChainBackend backend =>
  -- | Protocol parameters
  PParams LedgerEra ->
  backend ->
  -- | Change address to send
  AddressInEra ->
  -- | Unspent transaction outputs to spend.
  UTxO ->
  -- | Collateral inputs.
  [TxIn] ->
  -- | Outputs to create.
  [TxOut CtxTx] ->
  Maybe PlutusScript ->
  IO (Either (TxBodyErrorAutoBalance Era) Tx)
buildTransactionWithPParams pparams backend changeAddress utxoToSpend collateral outs mintingScript = do
  systemStart <- querySystemStart backend CardanoClient.QueryTip
  eraHistory <- queryEraHistory backend CardanoClient.QueryTip
  stakePools <- queryStakePools backend CardanoClient.QueryTip
  pure $ buildTransactionWithPParams' pparams systemStart eraHistory stakePools changeAddress utxoToSpend collateral outs mintingScript

-- | NOTE: If there are any non ADA assets present in the output 'Value' and
-- minting scrips is specified this function will mint them using provided
-- script as the script witness.
buildTransactionWithPParams' ::
  -- | Protocol parameters
  PParams LedgerEra ->
  SystemStart ->
  EraHistory ->
  Set PoolId ->
  -- | Change address to send
  AddressInEra ->
  -- | Unspent transaction outputs to spend.
  UTxO ->
  -- | Collateral inputs.
  [TxIn] ->
  -- | Outputs to create.
  [TxOut CtxTx] ->
  Maybe PlutusScript ->
  Either (TxBodyErrorAutoBalance Era) Tx
buildTransactionWithPParams' pparams systemStart eraHistory stakePools changeAddress utxoToSpend collateral outs mintingScript = do
  buildTransactionWithBody pparams systemStart eraHistory stakePools changeAddress bodyContent utxoToSpend
 where
  mintValue =
    case mintingScript of
      Nothing -> TxMintValueNone
      Just _ ->
        let mintingWitness =
              mkScriptWitness dummyMintingScript NoScriptDatumForMint (toScriptData ())
            toMint = valueToPolicyAssets (foldMap txOutValue outs)
         in if null toMint
              then TxMintValueNone
              else
                TxMintValue $
                  Map.fromList $
                    ( \(pid, assets) ->
                        ( pid
                        ,
                          ( assets
                          , BuildTxWith mintingWitness
                          )
                        )
                    )
                      <$> Map.toList toMint
  auxScripts =
    if mintValue == TxMintValueNone
      then TxAuxScriptsNone
      else
        TxAuxScripts
          ( maybeToList $
              toScriptInEra
                ShelleyBasedEraConway
                ( toScriptInAnyLang $
                    PlutusScript $
                      fromMaybe dummyMintingScript mintingScript
                )
          )
  -- NOTE: 'makeTransactionBodyAutoBalance' overwrites this.
  bodyContent =
    TxBodyContent
      { txIns = withWitness <$> toList (UTxO.inputSet utxoToSpend)
      , txInsCollateral = TxInsCollateral collateral
      , txInsReference = TxInsReferenceNone
      , txOuts = outs
      , txTotalCollateral = TxTotalCollateralNone
      , txReturnCollateral = TxReturnCollateralNone
      , txFee = TxFeeExplicit 0
      , txValidityLowerBound = TxValidityNoLowerBound
      , txValidityUpperBound = TxValidityNoUpperBound
      , txMetadata = TxMetadataNone
      , txAuxScripts = auxScripts
      , txExtraKeyWits = TxExtraKeyWitnessesNone
      , txProtocolParams = BuildTxWith $ Just $ LedgerProtocolParameters pparams
      , txWithdrawals = TxWithdrawalsNone
      , txCertificates = TxCertificatesNone
      , txUpdateProposal = TxUpdateProposalNone
      , txMintValue = mintValue
      , txScriptValidity = TxScriptValidityNone
      , txProposalProcedures = Nothing
      , txVotingProcedures = Nothing
      , txCurrentTreasuryValue = Nothing
      , txTreasuryDonation = Nothing
      }

buildTransactionWithBody ::
  -- | Protocol parameters
  PParams LedgerEra ->
  -- | System start
  SystemStart ->
  -- | Change address to send
  EraHistory ->
  Set PoolId ->
  AddressInEra ->
  -- | Body
  TxBodyContent BuildTx ->
  -- | Unspent transaction outputs to spend.
  UTxO ->
  Either (TxBodyErrorAutoBalance Era) Tx
buildTransactionWithBody pparams systemStart eraHistory stakePools changeAddress body utxoToSpend = do
  second (flip Tx [] . balancedTxBody) $
    makeTransactionBodyAutoBalance
      shelleyBasedEra
      systemStart
      (toLedgerEpochInfo eraHistory)
      (LedgerProtocolParameters pparams)
      stakePools
      mempty
      mempty
      utxoToSpend
      body
      changeAddress
      Nothing
