module Hydra.Chain.Backend where

import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Hydra.Cardano.Api
import Hydra.Chain.CardanoClient qualified as CardanoClient
import Hydra.Options (ChainBackendOptions)
import Hydra.Tx (ScriptRegistry)

blockfrostProjectPath :: FilePath
blockfrostProjectPath = "./blockfrost-project.txt"

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
  buildTransactionWithPParams pparams backend changeAddress body utxoToSpend outs

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
  IO (Either (TxBodyErrorAutoBalance Era) Tx)
buildTransactionWithPParams pparams backend changeAddress utxoToSpend collateral outs = do
  systemStart <- querySystemStart backend CardanoClient.QueryTip
  eraHistory <- queryEraHistory backend CardanoClient.QueryTip
  stakePools <- queryStakePools backend CardanoClient.QueryTip
  pure $ buildTransactionWithPParams' pparams systemStart eraHistory stakePools changeAddress utxoToSpend collateral outs

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
  Either (TxBodyErrorAutoBalance Era) Tx
buildTransactionWithPParams' pparams systemStart eraHistory stakePools changeAddress utxoToSpend collateral outs = do
  buildTransactionWithBody pparams systemStart eraHistory stakePools changeAddress bodyContent utxoToSpend
 where
  -- NOTE: 'makeTransactionBodyAutoBalance' overwrites this.
  bodyContent =
    TxBodyContent
      (withWitness <$> toList (UTxO.inputSet utxoToSpend))
      (TxInsCollateral collateral)
      TxInsReferenceNone
      outs
      TxTotalCollateralNone
      TxReturnCollateralNone
      (TxFeeExplicit 0)
      TxValidityNoLowerBound
      TxValidityNoUpperBound
      TxMetadataNone
      TxAuxScriptsNone
      TxExtraKeyWitnessesNone
      (BuildTxWith $ Just $ LedgerProtocolParameters pparams)
      TxWithdrawalsNone
      TxCertificatesNone
      TxUpdateProposalNone
      TxMintValueNone
      TxScriptValidityNone
      Nothing
      Nothing
      Nothing
      Nothing

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
