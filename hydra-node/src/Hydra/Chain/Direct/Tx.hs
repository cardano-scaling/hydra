{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Smart constructors for creating Hydra protocol transactions to be used in
-- the 'Hydra.Chain.Direct' way of talking to the main-chain.
--
-- This module also encapsulates the transaction format used when talking to the
-- cardano-node, which is currently different from the 'Hydra.Ledger.Cardano',
-- thus we have not yet "reached" 'isomorphism'.
module Hydra.Chain.Direct.Tx where

import Hydra.Ledger.Cardano
import Hydra.Prelude

import Cardano.Binary (decodeFull', serialize')
import qualified Data.Map as Map
import Data.Time (Day (ModifiedJulianDay), UTCTime (UTCTime))
import Hydra.Chain (HeadParameters (..), OnChainTx (..))
import qualified Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.HeadState as Head
import qualified Hydra.Contract.Initial as Initial
import Hydra.Data.ContestationPeriod (contestationPeriodFromDiffTime, contestationPeriodToDiffTime)
import Hydra.Data.Party (partyFromVerKey, partyToVerKey)
import qualified Hydra.Data.Party as OnChain
import qualified Hydra.Ledger.Cardano as Api
import Hydra.Party (MultiSigned, Party (Party), toPlutusSignatures, vkey)
import Hydra.Snapshot (Snapshot (..))
import Ledger.Typed.Scripts (DatumType)
import Ledger.Value (AssetClass (..), currencyMPSHash)
import Ouroboros.Consensus.Util (eitherToMaybe)
import Plutus.V1.Ledger.Api (MintingPolicyHash, fromBuiltin, fromData)
import qualified Plutus.V1.Ledger.Api as Plutus
import Plutus.V1.Ledger.Value (assetClass, currencySymbol, tokenName)
import Plutus.V2.Ledger.Api (toBuiltin)

-- * Post Hydra Head transactions

-- | Maintains information needed to construct on-chain transactions
-- depending on the current state of the head.
data OnChainHeadState
  = None
  | Initial
      { -- | The state machine UTxO produced by the Init transaction
        -- This output should always be present and 'threaded' across all
        -- transactions.
        -- NOTE(SN): The Head's identifier is somewhat encoded in the TxOut's address
        -- XXX(SN): Data and [OnChain.Party] are overlapping
        threadOutput :: (TxIn, TxOut CtxUTxO Era, ScriptData, [OnChain.Party])
      , initials :: [(TxIn, TxOut CtxUTxO Era, ScriptData)]
      , commits :: [(TxIn, TxOut CtxUTxO Era, ScriptData)]
      }
  | OpenOrClosed
      { -- | The state machine UTxO produced by the Init transaction
        -- This output should always be present and 'threaded' across all
        -- transactions.
        -- NOTE(SN): The Head's identifier is somewhat encoded in the TxOut's address
        -- XXX(SN): Data and [OnChain.Party] are overlapping
        threadOutput :: (TxIn, TxOut CtxUTxO Era, ScriptData, [OnChain.Party])
      }
  | Final
  deriving (Eq, Show, Generic)

-- FIXME: should not be hardcoded, for testing purposes only
threadToken :: AssetClass
threadToken = assetClass (currencySymbol "hydra") (tokenName "token")

-- FIXME: should not be hardcoded
policyId :: MintingPolicyHash
(policyId, _) = first currencyMPSHash (unAssetClass threadToken)

-- | Create the init transaction from some 'HeadParameters' and a single TxIn
-- which will be used as unique parameter for minting NFTs.
initTx ::
  NetworkId ->
  -- | Participant's cardano public keys.
  [VerificationKey PaymentKey] ->
  HeadParameters ->
  TxIn ->
  CardanoTx
initTx networkId cardanoKeys HeadParameters{contestationPeriod, parties} txIn =
  unsafeBuildTransaction $
    emptyTxBody
      & addVkInputs [txIn]
      & addOutputs (headOutput : map (mkInitialOutput networkId) cardanoKeys)
 where
  headOutput =
    TxOut headAddress headValue headDatum
  headScript =
    fromPlutusScript $ Head.validatorScript policyId
  headAddress =
    mkScriptAddress @PlutusScriptV1 networkId headScript
  headValue =
    lovelaceToTxOutValue $ Lovelace 2_000_000
  headDatum =
    mkTxOutDatum $
      Head.Initial
        (contestationPeriodFromDiffTime contestationPeriod)
        (map (partyFromVerKey . vkey) parties)

mkInitialOutput :: NetworkId -> VerificationKey PaymentKey -> TxOut CtxTx Era
mkInitialOutput networkId (toPlutusKeyHash . verificationKeyHash -> vkh) =
  TxOut initialAddress initialValue (mkInitialDatum vkh)
 where
  -- FIXME: should really be the minted PTs plus some ADA to make the ledger happy
  initialValue =
    lovelaceToTxOutValue $ Lovelace 2_000_000
  initialAddress =
    mkScriptAddress @PlutusScriptV1 networkId initialScript
  initialScript =
    fromPlutusScript Initial.validatorScript
  mkInitialDatum =
    mkTxOutDatum . Initial.datum

-- | Craft a commit transaction which includes the "committed" utxo as a datum.
--
-- TODO: Remove all the qualified 'Api' once the refactoring is over and there's
-- no more import clash with 'cardano-ledger'.
--
-- TODO: Get rid of Ledger types in the signature and fully rely on Cardano.Api
commitTx ::
  NetworkId ->
  Party ->
  -- | A single UTxO to commit to the Head
  -- We currently limit committing one UTxO to the head because of size limitations.
  Maybe (TxIn, TxOut CtxUTxO Era) ->
  -- | The inital output (sent to each party) which should contain the PT and is
  -- locked by initial script
  (TxIn, Hash PaymentKey) ->
  Tx Era
commitTx networkId party utxo (initialInput, vkh) =
  unsafeBuildTransaction $
    emptyTxBody
      & addInputs [(initialInput, initialWitness)]
      & addVkInputs [commit | Just (commit, _) <- [utxo]]
      & addOutputs [commitOutput]
 where
  initialWitness =
    BuildTxWith $ mkScriptWitness initialScript initialDatum initialRedeemer
  initialScript =
    fromPlutusScript Initial.validatorScript
  initialDatum =
    mkDatumForTxIn $ Initial.datum $ toPlutusKeyHash vkh
  initialRedeemer =
    mkRedeemerForTxIn $ Initial.redeemer ()

  commitOutput =
    TxOut commitAddress commitValue commitDatum
  commitScript =
    fromPlutusScript Commit.validatorScript
  commitAddress =
    mkScriptAddress @PlutusScriptV1 networkId commitScript
  -- FIXME: We should add the value from the initialIn too because it contains the PTs
  commitValue =
    mkTxOutValue $
      lovelaceToValue 2_000_000 <> maybe mempty (txOutValue . snd) utxo
  commitDatum =
    mkTxOutDatum $ mkCommitDatum party (Head.validatorHash policyId) utxo

-- FIXME: WIP
mkCommitDatum :: Party -> Plutus.ValidatorHash -> Maybe (Api.TxIn, Api.TxOut Api.CtxUTxO Api.Era) -> Plutus.Datum
mkCommitDatum (partyFromVerKey . vkey -> party) headValidatorHash utxo =
  Commit.datum (party, headValidatorHash, serializedUtxo)
 where
  serializedUtxo = case utxo of
    Nothing ->
      Nothing
    Just (i, o) ->
      Just
        ( Commit.SerializedTxOutRef (toBuiltin $ serialize' $ Api.toLedgerTxIn i)
        , Commit.SerializedTxOut (toBuiltin $ serialize' $ Api.toLedgerTxOut o)
        )

-- | Create a transaction collecting all "committed" utxo and opening a Head,
-- i.e. driving the Head script state.
collectComTx ::
  NetworkId ->
  -- | Everything needed to spend the Head state-machine output.
  -- FIXME(SN): should also contain some Head identifier/address and stored Value (maybe the TxOut + Data?)
  (TxIn, ScriptData, [OnChain.Party]) ->
  -- | Data needed to spend the commit output produced by each party.
  -- Should contain the PT and is locked by @ν_commit@ script.
  Map TxIn (TxOut CtxUTxO Era, ScriptData) ->
  CardanoTx
-- TODO(SN): utxo unused means other participants would not "see" the opened
-- utxo when observing. Right now, they would be trusting the OCV checks this
-- and construct their "world view" from observed commit txs in the HeadLogic
collectComTx networkId (headInput, ScriptDatumForTxIn -> headDatumBefore, parties) commits =
  unsafeBuildTransaction $
    emptyTxBody
      & addInputs ((headInput, headWitness) : (mkCommit <$> Map.toList commits))
      & addOutputs [headOutput]
 where
  headWitness =
    BuildTxWith $ mkScriptWitness headScript headDatumBefore headRedeemer
  headScript =
    fromPlutusScript $ Head.validatorScript policyId
  headRedeemer =
    mkRedeemerForTxIn Head.CollectCom
  headOutput =
    TxOut
      (mkScriptAddress @PlutusScriptV1 networkId headScript)
      (mkTxOutValue $ lovelaceToValue 2_000_000 <> commitValue)
      headDatumAfter
  headDatumAfter =
    mkTxOutDatum Head.Open{Head.parties = parties, utxoHash}
  -- NOTE: We hash tx outs in an order that is recoverable on-chain.
  -- The simplest thing to do, is to make sure commit inputs are in the same
  -- order as their corresponding committed utxo.
  extractSerialisedTxOut d =
    case fromData $ toPlutusData d of
      Nothing -> error "SNAFU"
      Just ((_, _, Just (_, o)) :: DatumType Commit.Commit) -> Just o
      _ -> Nothing
  utxoHash =
    Head.hashPreSerializedCommits $
      mapMaybe (extractSerialisedTxOut . snd . snd) $ sortOn fst $ Map.toList commits
  mkCommit (commitInput, (_commitOutput, commitDatum)) =
    ( commitInput
    , mkCommitWitness commitDatum
    )
  mkCommitWitness (ScriptDatumForTxIn -> commitDatum) =
    BuildTxWith $ mkScriptWitness commitScript commitDatum commitRedeemer
  commitValue =
    mconcat $ txOutValue . fst <$> Map.elems commits
  commitScript =
    fromPlutusScript Commit.validatorScript
  commitRedeemer =
    mkRedeemerForTxIn ()

-- | Create a transaction closing a head with given snapshot number and utxo.
closeTx ::
  Snapshot CardanoTx ->
  -- | Multi-signature of the whole snapshot
  MultiSigned (Snapshot CardanoTx) ->
  -- | Everything needed to spend the Head state-machine output.
  -- FIXME(SN): should also contain some Head identifier/address and stored Value (maybe the TxOut + Data?)
  (TxIn, TxOut CtxUTxO Era, ScriptData) ->
  Tx Era
closeTx Snapshot{number, utxo} sig (headInput, headOutputBefore, ScriptDatumForTxIn -> headDatumBefore) =
  unsafeBuildTransaction $
    emptyTxBody
      & addInputs [(headInput, headWitness)]
      & addOutputs [headOutputAfter]
 where
  headWitness =
    BuildTxWith $ mkScriptWitness headScript headDatumBefore headRedeemer
  headScript =
    fromPlutusScript $ Head.validatorScript policyId
  headRedeemer =
    mkRedeemerForTxIn
      Head.Close
        { snapshotNumber = toInteger number
        , signature = toPlutusSignatures sig
        , utxoHash
        }
  headOutputAfter =
    modifyTxOutDatum (const headDatumAfter) headOutputBefore
  headDatumAfter =
    mkTxOutDatum
      Head.Closed
        { snapshotNumber = toInteger number
        , utxoHash
        }
  utxoHash = toBuiltin $ hashTxOuts $ toList utxo

fanoutTx ::
  -- | Snapshotted Utxo to fanout on layer 1
  Utxo ->
  -- | Everything needed to spend the Head state-machine output.
  -- FIXME(SN): should also contain some Head identifier/address and stored Value (maybe the TxOut + Data?)
  (TxIn, ScriptData) ->
  Tx Era
fanoutTx utxo (headInput, ScriptDatumForTxIn -> headDatumBefore) =
  unsafeBuildTransaction $
    emptyTxBody
      & addInputs [(headInput, headWitness)]
      & addOutputs fanoutOutputs
 where
  headWitness =
    BuildTxWith $ mkScriptWitness headScript headDatumBefore headRedeemer
  headScript =
    fromPlutusScript $ Head.validatorScript policyId
  headRedeemer =
    mkRedeemerForTxIn (Head.Fanout $ fromIntegral $ length utxo)

  fanoutOutputs =
    foldr ((:) . toTxContext) [] utxo

data AbortTxError = OverlappingInputs
  deriving (Show)

-- | Create transaction which aborts a head by spending the Head output and all
-- other "initial" outputs.
abortTx ::
  -- | Network identifier for address discrimination
  NetworkId ->
  -- | Everything needed to spend the Head state-machine output.
  (TxIn, ScriptData) ->
  -- | Data needed to spend the inital output sent to each party to the Head
  -- which should contain the PT and is locked by initial script.
  Map TxIn ScriptData ->
  Either AbortTxError CardanoTx
abortTx networkId (headInput, ScriptDatumForTxIn -> headDatumBefore) initialInputs
  | isJust (lookup headInput initialInputs) =
    Left OverlappingInputs
  | otherwise =
    Right $
      unsafeBuildTransaction $
        emptyTxBody
          & addInputs ((headInput, headWitness) : (mkAbort <$> Map.toList initialInputs))
          & addOutputs [headOutput]
 where
  headWitness =
    BuildTxWith $ mkScriptWitness headScript headDatumBefore headRedeemer
  headScript =
    fromPlutusScript $ Head.validatorScript policyId
  headRedeemer =
    mkRedeemerForTxIn Head.Abort

  -- FIXME:
  -- (a) Abort need to reimburse participants that have committed!
  -- (b) There's in principle no need to output any SM output here, it's over.
  headOutput =
    TxOut
      (mkScriptAddress @PlutusScriptV1 networkId headScript)
      (mkTxOutValue $ lovelaceToValue 2_000_000)
      headDatumAfter
  headDatumAfter =
    mkTxOutDatum Head.Final

  -- NOTE: Abort datums contain the datum of the spent state-machine input, but
  -- also, the datum of the created output which is necessary for the
  -- state-machine on-chain validator to control the correctness of the
  -- transition.
  mkAbort (initialInput, ScriptDatumForTxIn -> initialDatum) =
    (initialInput, mkAbortWitness initialDatum)
  mkAbortWitness initialDatum =
    BuildTxWith $ mkScriptWitness initialScript initialDatum initialRedeemer
  initialScript =
    fromPlutusScript Initial.validatorScript
  initialRedeemer =
    mkRedeemerForTxIn $ Initial.redeemer ()

-- * Observe Hydra Head transactions

-- XXX(SN): We should log decisions why a tx is not an initTx etc. instead of
-- only returning a Maybe, i.e. 'Either Reason (OnChainTx tx, OnChainHeadState)'
observeInitTx ::
  NetworkId ->
  Party ->
  CardanoTx ->
  Maybe (OnChainTx CardanoTx, OnChainHeadState)
observeInitTx networkId party (getTxBody -> txBody) = do
  (ix, headOut, headData, Head.Initial cp ps) <- findFirst headOutput indexedOutputs
  let parties = map convertParty ps
  let cperiod = contestationPeriodToDiffTime cp
  guard $ party `elem` parties
  pure
    ( OnInitTx cperiod parties
    , Initial
        { threadOutput =
            ( mkTxIn txBody ix
            , toCtxUTxOTxOut headOut
            , fromAlonzoData headData
            , ps
            )
        , initials
        , commits = []
        }
    )
 where
  TxBody TxBodyContent{txOuts} = txBody

  headOutput = \case
    (ix, out@(TxOut _ _ (TxOutDatum _ d))) ->
      (ix,out,toAlonzoData d,) <$> fromData (toPlutusData d)
    _ -> Nothing

  indexedOutputs = zip [0 ..] txOuts

  initialOutputs = filter (isInitial . snd) indexedOutputs

  initials =
    mapMaybe
      ( \(i, o) -> do
          dat <- getDatum o
          pure (mkTxIn txBody i, toCtxUTxOTxOut o, dat)
      )
      initialOutputs

  isInitial (TxOut addr _ _) = addr == initialAddress

  initialAddress = mkScriptAddress @PlutusScriptV1 networkId initialScript

  initialScript = fromPlutusScript Initial.validatorScript

convertParty :: OnChain.Party -> Party
convertParty = Party . partyToVerKey

-- | Identify a commit tx by looking for an output which pays to v_commit.
observeCommitTx ::
  NetworkId ->
  CardanoTx ->
  Maybe (OnChainTx CardanoTx, (TxIn, TxOut CtxUTxO Era, ScriptData))
observeCommitTx networkId (getTxBody -> txBody) = do
  (commitIn, commitOut) <- findTxOutByAddress commitAddress txBody
  dat <- getDatum commitOut
  (party, _, committedUtxo) <- fromData @(DatumType Commit.Commit) $ toPlutusData dat
  convertedUtxo <- convertUtxo committedUtxo
  let onChainTx = OnCommitTx (convertParty party) convertedUtxo
  pure (onChainTx, (commitIn, toCtxUTxOTxOut commitOut, dat))
 where
  convertUtxo :: Maybe (Commit.SerializedTxOutRef, Commit.SerializedTxOut) -> Maybe Utxo
  convertUtxo = \case
    Nothing -> Just mempty
    Just (Commit.SerializedTxOutRef inBytes, Commit.SerializedTxOut outBytes) ->
      -- XXX(SN): these errors might be more severe and we could throw an
      -- exception here?
      eitherToMaybe $ do
        txIn <- fromLedgerTxIn <$> decodeFull' (fromBuiltin inBytes)
        txOut <- fromLedgerTxOut <$> decodeFull' (fromBuiltin outBytes)
        pure $ singletonUtxo (txIn, txOut)

  commitAddress = mkScriptAddress @PlutusScriptV1 networkId commitScript

  commitScript = fromPlutusScript Commit.validatorScript

-- REVIEW(SN): Is this really specific to commit only, or wouldn't we be able to
-- filter all 'knownUtxo' after observing any protocol tx?
observeCommit ::
  NetworkId ->
  CardanoTx ->
  OnChainHeadState ->
  Maybe (OnChainTx CardanoTx, OnChainHeadState)
observeCommit networkId tx = \case
  Initial{threadOutput, initials, commits} -> do
    (onChainTx, commitTriple) <- observeCommitTx networkId tx
    -- NOTE(SN): A commit tx has been observed and thus we can remove all it's
    -- inputs from our tracked initials
    let commitIns = inputs tx
    let initials' = filter (\(i, _, _) -> i `notElem` commitIns) initials
    let commits' = commitTriple : commits
    pure
      ( onChainTx
      , Initial
          { threadOutput
          , initials = initials'
          , commits = commits'
          }
      )
  _ -> Nothing

-- TODO(SN): obviously the observeCollectComTx/observeAbortTx can be DRYed.. deliberately hold back on it though

-- | Identify a collectCom tx by lookup up the input spending the Head output
-- and decoding its redeemer.
observeCollectComTx ::
  -- | A Utxo set to lookup tx inputs
  Utxo ->
  CardanoTx ->
  Maybe (OnChainTx CardanoTx, OnChainHeadState)
observeCollectComTx utxo tx = do
  (headInput, headOutput) <- findScriptOutput @PlutusScriptV1 utxo headScript
  redeemer <- findRedeemerSpending (getTxBody tx) headInput
  oldHeadDatum <- lookupDatum tx headOutput
  datum <- fromData $ toPlutusData oldHeadDatum
  case (datum, redeemer) of
    (Head.Initial{parties}, Head.CollectCom) -> do
      (newHeadInput, newHeadOutput) <- findScriptOutput @PlutusScriptV1 (utxoFromTx tx) headScript
      newHeadDatum <- lookupDatum tx newHeadOutput
      pure
        ( OnCollectComTx
        , OpenOrClosed
            { threadOutput =
                ( newHeadInput
                , newHeadOutput
                , newHeadDatum
                , parties
                )
            }
        )
    _ -> Nothing
 where
  headScript = fromPlutusScript $ Head.validatorScript policyId

-- | Identify a close tx by lookup up the input spending the Head output and
-- decoding its redeemer.
observeCloseTx ::
  -- | A Utxo set to lookup tx inputs
  Utxo ->
  CardanoTx ->
  Maybe (OnChainTx CardanoTx, OnChainHeadState)
observeCloseTx utxo tx = do
  (headInput, headOutput) <- findScriptOutput @PlutusScriptV1 utxo headScript
  redeemer <- findRedeemerSpending (getTxBody tx) headInput
  oldHeadDatum <- lookupDatum tx headOutput
  datum <- fromData $ toPlutusData oldHeadDatum
  case (datum, redeemer) of
    (Head.Open{parties}, Head.Close{snapshotNumber = onChainSnapshotNumber}) -> do
      (newHeadInput, newHeadOutput) <- findScriptOutput @PlutusScriptV1 (utxoFromTx tx) headScript
      newHeadDatum <- lookupDatum tx newHeadOutput
      snapshotNumber <- integerToNatural onChainSnapshotNumber
      pure
        ( OnCloseTx{contestationDeadline, snapshotNumber}
        , OpenOrClosed
            { threadOutput =
                ( newHeadInput
                , newHeadOutput
                , newHeadDatum
                , parties
                )
            }
        )
    _ -> Nothing
 where
  headScript = fromPlutusScript $ Head.validatorScript policyId

  -- FIXME(SN): store in/read from datum
  contestationDeadline = UTCTime (ModifiedJulianDay 0) 0

-- | Identify a fanout tx by lookup up the input spending the Head output and
-- decoding its redeemer.
--
-- TODO: Ideally, the fanout does not produce any state-machine output. That
-- means, to observe it, we need to look for a transaction with an input spent
-- from a known script (the head state machine script) with a "fanout" redeemer.
observeFanoutTx ::
  -- | A Utxo set to lookup tx inputs
  Utxo ->
  CardanoTx ->
  Maybe (OnChainTx CardanoTx, OnChainHeadState)
observeFanoutTx utxo tx = do
  headInput <- fst <$> findScriptOutput @PlutusScriptV1 utxo headScript
  findRedeemerSpending (getTxBody tx) headInput
    >>= \case
      Head.Fanout{} -> pure (OnFanoutTx, Final)
      _ -> Nothing
 where
  headScript = fromPlutusScript $ Head.validatorScript policyId

-- | Identify an abort tx by looking up the input spending the Head output and
-- decoding its redeemer.
observeAbortTx ::
  -- | A Utxo set to lookup tx inputs
  Utxo ->
  CardanoTx ->
  Maybe (OnChainTx CardanoTx, OnChainHeadState)
observeAbortTx utxo (Tx body _) = do
  headInput <- fst <$> findScriptOutput @PlutusScriptV1 utxo headScript
  findRedeemerSpending body headInput >>= \case
    Head.Abort -> pure (OnAbortTx, Final)
    _ -> Nothing
 where
  -- FIXME(SN): make sure this is aborting "the right head / your head" by not hard-coding policyId
  headScript = fromPlutusScript $ Head.validatorScript policyId

-- * Functions related to OnChainHeadState

-- | Provide a UTXO map for given OnChainHeadState. Used by the TinyWallet and
-- the direct chain component to lookup inputs for balancing / constructing txs.
-- XXX(SN): This is a hint that we might want to track the Utxo directly?
knownUtxo :: OnChainHeadState -> Map TxIn (TxOut CtxUTxO Era)
knownUtxo = \case
  Initial{threadOutput, initials, commits} ->
    Map.fromList $ take2 threadOutput : (take2' <$> (initials <> commits))
  OpenOrClosed{threadOutput = (i, o, _, _)} ->
    Map.singleton i o
  _ ->
    mempty
 where
  take2 (i, o, _, _) = (i, o)
  take2' (i, o, _) = (i, o)

-- | Look for the "initial" which corresponds to given cardano verification key.
ownInitial ::
  VerificationKey PaymentKey ->
  [(TxIn, TxOut CtxUTxO Era, ScriptData)] ->
  Maybe (TxIn, Hash PaymentKey)
ownInitial vkey =
  foldl' go Nothing
 where
  go (Just x) _ = Just x
  go Nothing (i, _, dat) = do
    let vkh = verificationKeyHash vkey
    pkh <- fromData (toPlutusData dat)
    guard $ pkh == toPlutusKeyHash vkh
    pure (i, vkh)

-- * Helpers

-- | Find first occurrence including a transformation.
findFirst :: Foldable t => (a -> Maybe b) -> t a -> Maybe b
findFirst fn = getFirst . foldMap (First . fn)
