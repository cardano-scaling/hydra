module Hydra.Chain.Backend where

import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.Map.Strict qualified as Map
import Hydra.Cardano.Api
import Hydra.Chain.CardanoClient qualified as CardanoClient
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

buildTransaction ::
  ChainBackend m =>
  MonadIO m =>
  -- | Change address to send
  AddressInEra ->
  -- | Unspent transaction outputs to spend.
  UTxO ->
  -- | Outputs to create.
  [TxOut CtxTx] ->
  m (Either (TxBodyErrorAutoBalance Era) Tx)
buildTransaction changeAddress utxoToSpend outs = do
  pparams <- queryProtocolParameters CardanoClient.QueryTip
  buildTransactionWithPParams pparams changeAddress utxoToSpend outs Nothing

buildTransactionWithMintingScript ::
  ChainBackend m =>
  MonadIO m =>
  -- | Change address to send
  AddressInEra ->
  -- | Unspent transaction outputs to spend.
  UTxO ->
  -- | Outputs to create.
  [TxOut CtxTx] ->
  -- | Optional minting script and its collateral inputs.
  Maybe (PlutusScript, [TxIn]) ->
  m (Either (TxBodyErrorAutoBalance Era) Tx)
buildTransactionWithMintingScript changeAddress utxoToSpend outs minting = do
  pparams <- queryProtocolParameters CardanoClient.QueryTip
  buildTransactionWithPParams pparams changeAddress utxoToSpend outs minting

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
  -- | Outputs to create.
  [TxOut CtxTx] ->
  -- | Optional minting script and its collateral inputs.
  Maybe (PlutusScript, [TxIn]) ->
  m (Either (TxBodyErrorAutoBalance Era) Tx)
buildTransactionWithPParams pparams changeAddress utxoToSpend outs minting = do
  systemStart <- querySystemStart CardanoClient.QueryTip
  eraHistory <- queryEraHistory CardanoClient.QueryTip
  stakePools <- queryStakePools CardanoClient.QueryTip
  pure $ buildTransactionWithPParams' pparams systemStart eraHistory stakePools changeAddress utxoToSpend outs minting

-- | Mint the non-ADA assets in the outputs when a minting script is supplied.
-- The script and its collateral are used only when those assets are non-empty.
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
  -- | Outputs to create.
  [TxOut CtxTx] ->
  -- | Optional minting script and its collateral inputs.
  Maybe (PlutusScript, [TxIn]) ->
  Either (TxBodyErrorAutoBalance Era) Tx
buildTransactionWithPParams' pparams systemStart eraHistory stakePools changeAddress utxoToSpend outs minting = do
  buildTransactionWithBody pparams systemStart eraHistory stakePools changeAddress bodyContent utxoToSpend
 where
  mintValue =
    case minting of
      Nothing -> TxMintValueNone
      Just (mintingScript, _) ->
        let mintingWitness =
              mkScriptWitness mintingScript NoScriptDatumForMint (toScriptData ())
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
    case minting of
      Just (mintingScript, _)
        | mintValue /= TxMintValueNone ->
            TxAuxScripts
              ( maybeToList $
                  toScriptInEra
                    ShelleyBasedEraConway
                    (toScriptInAnyLang $ PlutusScript mintingScript)
              )
      _ -> TxAuxScriptsNone
  -- NOTE: Since cardano-api 11.4, balancing rejects collateral inputs on
  -- transactions that run no Plutus script, and the minting script is the only
  -- script this builder can attach.
  collateralInputs
    | mintValue == TxMintValueNone = TxInsCollateralNone
    | otherwise = maybe TxInsCollateralNone (TxInsCollateral . snd) minting

  -- NOTE: 'makeTransactionBodyAutoBalance' overwrites this.
  bodyContent =
    TxBodyContent
      { txIns = withWitness <$> toList (UTxO.inputSet utxoToSpend)
      , txInsCollateral = collateralInputs
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
      utxoToSpend
      body
      changeAddress
      Nothing
