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

class ChainBackend m where
  queryGenesisParameters :: m (GenesisParameters ShelleyEra)
  queryScriptRegistry :: [TxId] -> m ScriptRegistry
  queryNetworkId :: m NetworkId
  queryTip :: m ChainPoint
  queryUTxO :: [Address ShelleyAddr] -> m UTxO
  queryUTxOByTxIn :: [TxIn] -> m UTxO
  queryEraHistory :: CardanoClient.QueryPoint -> m EraHistory
  querySystemStart :: CardanoClient.QueryPoint -> m SystemStart
  queryProtocolParameters :: CardanoClient.QueryPoint -> m (PParams LedgerEra)
  queryStakePools :: CardanoClient.QueryPoint -> m (Set PoolId)
  queryUTxOFor :: CardanoClient.QueryPoint -> VerificationKey PaymentKey -> m UTxO
  submitTransaction :: Tx -> m ()
  awaitTransaction :: Tx -> VerificationKey PaymentKey -> m UTxO
  getBlockTime :: m NominalDiffTime
  -- | Get the delay to use between backend queries for rate limiting
  getQueryDelay :: m NominalDiffTime

buildTransaction ::
  ChainBackend m =>
  MonadIO m =>
  -- | Change address to send
  AddressInEra ->
  -- | Unspent transaction outputs to spend.
  UTxO ->
  -- | Collateral inputs.
  [TxIn] ->
  -- | Outputs to create.
  [TxOut CtxTx] ->
  m (Either (TxBodyErrorAutoBalance Era) Tx)
buildTransaction changeAddress body utxoToSpend outs = do
  pparams <- queryProtocolParameters CardanoClient.QueryTip
  buildTransactionWithPParams pparams changeAddress body utxoToSpend outs Nothing

buildTransactionWithMintingScript ::
  ChainBackend m =>
  MonadIO m =>
  -- | Change address to send
  AddressInEra ->
  -- | Unspent transaction outputs to spend.
  UTxO ->
  -- | Collateral inputs.
  [TxIn] ->
  -- | Outputs to create.
  [TxOut CtxTx] ->
  Maybe PlutusScript ->
  m (Either (TxBodyErrorAutoBalance Era) Tx)
buildTransactionWithMintingScript changeAddress body utxoToSpend outs mintingScript = do
  pparams <- queryProtocolParameters CardanoClient.QueryTip
  buildTransactionWithPParams pparams changeAddress body utxoToSpend outs mintingScript

-- | Construct a simple payment consuming some inputs and producing some
-- outputs (no certificates or withdrawals involved).
--
-- On success, the returned transaction is fully balanced. On error, return
-- `TxBodyErrorAutoBalance`.
buildTransactionWithPParams ::
  ChainBackend m =>
  MonadIO m =>
  -- | Protocol parameters
  PParams LedgerEra ->
  -- | Change address to send
  AddressInEra ->
  -- | Unspent transaction outputs to spend.
  UTxO ->
  -- | Collateral inputs.
  [TxIn] ->
  -- | Outputs to create.
  [TxOut CtxTx] ->
  Maybe PlutusScript ->
  m (Either (TxBodyErrorAutoBalance Era) Tx)
buildTransactionWithPParams pparams changeAddress utxoToSpend collateral outs mintingScript = do
  systemStart <- querySystemStart CardanoClient.QueryTip
  eraHistory <- queryEraHistory CardanoClient.QueryTip
  stakePools <- queryStakePools CardanoClient.QueryTip
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
