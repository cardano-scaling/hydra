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

import Hydra.Cardano.Api
import Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Binary (decodeFull', serialize')
import qualified Data.Map as Map
import Data.Time (Day (ModifiedJulianDay), UTCTime (UTCTime))
import Hydra.Chain (HeadId (..), HeadParameters (..), OnChainTx (..))
import qualified Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.HeadState as Head
import qualified Hydra.Contract.HeadTokens as HeadTokens
import qualified Hydra.Contract.Initial as Initial
import Hydra.Data.ContestationPeriod (contestationPeriodFromDiffTime, contestationPeriodToDiffTime)
import Hydra.Data.Party (partyFromVerKey, partyToVerKey)
import qualified Hydra.Data.Party as OnChain
import Hydra.Ledger.Cardano (hashTxOuts)
import Hydra.Ledger.Cardano.Builder (addInputs, addOutputs, addVkInputs, emptyTxBody, mintTokens, unsafeBuildTransaction)
import Hydra.Party (MultiSigned, Party (Party), toPlutusSignatures, vkey)
import Hydra.Snapshot (Snapshot (..))
import Ledger.Typed.Scripts (DatumType)
import Ledger.Value (AssetClass (..), currencyMPSHash)
import Plutus.V1.Ledger.Api (MintingPolicyHash, fromBuiltin, fromData)
import qualified Plutus.V1.Ledger.Api as Plutus
import Plutus.V1.Ledger.Value (assetClass, currencySymbol, tokenName)
import Plutus.V2.Ledger.Api (toBuiltin)

type UTxOWithScript = (TxIn, TxOut CtxUTxO, ScriptData)

hydraHeadV1 :: ByteString
hydraHeadV1 = "HydraHeadV1"

headPolicyId :: TxIn -> PolicyId
headPolicyId =
  scriptPolicyId . PlutusScript . headTokenScript
 where
  headTokenScript =
    fromPlutusScript @PlutusScriptV1 . HeadTokens.validatorScript . toPlutusTxOutRef

hydraHeadV1AssetName :: AssetName
hydraHeadV1AssetName = AssetName hydraHeadV1

-- FIXME: should not be hardcoded, for testing purposes only
threadToken :: AssetClass
threadToken = assetClass (currencySymbol "hydra") (tokenName "token")

-- FIXME: should not be hardcoded
policyId :: MintingPolicyHash
(policyId, _) = first currencyMPSHash (unAssetClass threadToken)

-- FIXME: sould not be hardcoded
headValue :: Value
headValue = lovelaceToValue (Lovelace 2_000_000)

-- * Create Hydra Head transactions

-- | Create the init transaction from some 'HeadParameters' and a single TxIn
-- which will be used as unique parameter for minting NFTs.
initTx ::
  NetworkId ->
  -- | Participant's cardano public keys.
  [VerificationKey PaymentKey] ->
  HeadParameters ->
  TxIn ->
  Tx
initTx networkId cardanoKeys parameters txIn =
  unsafeBuildTransaction $
    emptyTxBody
      & addVkInputs [txIn]
      & addOutputs
        ( mkHeadOutputInitial networkId (headPolicyId txIn) parameters :
          map (mkInitialOutput networkId) cardanoKeys
        )
      & mintTokens headTokenScript hydraHeadV1AssetName 1
 where
  headTokenScript =
    fromPlutusScript @PlutusScriptV1 $ HeadTokens.validatorScript (toPlutusTxOutRef txIn)

mkHeadOutput :: NetworkId -> PolicyId -> TxOutDatum ctx -> TxOut ctx
mkHeadOutput networkId tokenPolicyId =
  TxOut
    (mkScriptAddress @PlutusScriptV1 networkId headScript)
    (headValue <> valueFromList [(AssetId tokenPolicyId hydraHeadV1AssetName, 1)])
 where
  headScript = fromPlutusScript $ Head.validatorScript policyId

mkHeadOutputInitial :: NetworkId -> PolicyId -> HeadParameters -> TxOut CtxTx
mkHeadOutputInitial networkId tokenPolicyId HeadParameters{contestationPeriod, parties} =
  mkHeadOutput networkId tokenPolicyId headDatum
 where
  headDatum =
    mkTxOutDatum $
      Head.Initial
        (contestationPeriodFromDiffTime contestationPeriod)
        (map (partyFromVerKey . vkey) parties)

mkInitialOutput :: NetworkId -> VerificationKey PaymentKey -> TxOut CtxTx
mkInitialOutput networkId (toPlutusKeyHash . verificationKeyHash -> pkh) =
  TxOut initialAddress initialValue initialDatum
 where
  -- FIXME: should really be the minted PTs plus some ADA to make the ledger happy
  initialValue =
    headValue
  initialAddress =
    mkScriptAddress @PlutusScriptV1 networkId initialScript
  initialScript =
    fromPlutusScript Initial.validatorScript
  initialDatum =
    mkTxOutDatum $ Initial.datum pkh

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
  Maybe (TxIn, TxOut CtxUTxO) ->
  -- | The inital output (sent to each party) which should contain the PT and is
  -- locked by initial script
  (TxIn, Hash PaymentKey) ->
  Tx
commitTx networkId party utxo (initialInput, vkh) =
  unsafeBuildTransaction $
    emptyTxBody
      & addInputs [(initialInput, initialWitness_)]
      & addVkInputs (maybeToList mCommittedInput)
      & addOutputs [commitOutput]
 where
  initialWitness_ =
    BuildTxWith $ ScriptWitness scriptWitnessCtx $ mkScriptWitness initialScript initialDatum initialRedeemer
  initialScript =
    fromPlutusScript @PlutusScriptV1 Initial.validatorScript
  initialDatum =
    mkScriptDatum $ Initial.datum $ toPlutusKeyHash vkh
  initialRedeemer =
    toScriptData . Initial.redeemer $
      Initial.Commit (toPlutusTxOutRef <$> mCommittedInput)
  mCommittedInput =
    fst <$> utxo
  commitOutput =
    TxOut commitAddress commitValue commitDatum
  commitScript =
    fromPlutusScript Commit.validatorScript
  commitAddress =
    mkScriptAddress @PlutusScriptV1 networkId commitScript
  -- FIXME: We should add the value from the initialIn too because it contains the PTs
  commitValue =
    headValue <> maybe mempty (txOutValue . snd) utxo
  commitDatum =
    mkTxOutDatum $ mkCommitDatum party (Head.validatorHash policyId) utxo

mkCommitDatum :: Party -> Plutus.ValidatorHash -> Maybe (TxIn, TxOut CtxUTxO) -> Plutus.Datum
mkCommitDatum (partyFromVerKey . vkey -> party) headValidatorHash utxo =
  Commit.datum (party, headValidatorHash, serializedUTxO)
 where
  serializedUTxO = case utxo of
    Nothing ->
      Nothing
    Just (_i, o) ->
      Just $ Commit.SerializedTxOut (toBuiltin $ serialize' $ toLedgerTxOut o)

-- | Create a transaction collecting all "committed" utxo and opening a Head,
-- i.e. driving the Head script state.
collectComTx ::
  NetworkId ->
  -- | Everything needed to spend the Head state-machine output.
  (TxIn, TxOut CtxUTxO, ScriptData, [OnChain.Party]) ->
  -- | Data needed to spend the commit output produced by each party.
  -- Should contain the PT and is locked by @ν_commit@ script.
  Map TxIn (TxOut CtxUTxO, ScriptData) ->
  Tx
-- TODO(SN): utxo unused means other participants would not "see" the opened
-- utxo when observing. Right now, they would be trusting the OCV checks this
-- and construct their "world view" from observed commit txs in the HeadLogic
collectComTx networkId (headInput, initialHeadOutput, ScriptDatumForTxIn -> headDatumBefore, parties) commits =
  unsafeBuildTransaction $
    emptyTxBody
      & addInputs ((headInput, headWitness) : (mkCommit <$> Map.toList commits))
      & addOutputs [headOutput]
 where
  headWitness =
    BuildTxWith $ ScriptWitness scriptWitnessCtx $ mkScriptWitness headScript headDatumBefore headRedeemer
  headScript =
    fromPlutusScript @PlutusScriptV1 $ Head.validatorScript policyId
  headRedeemer =
    toScriptData Head.CollectCom
  headOutput =
    TxOut
      (mkScriptAddress @PlutusScriptV1 networkId headScript)
      (txOutValue initialHeadOutput <> commitValue)
      headDatumAfter
  headDatumAfter =
    mkTxOutDatum Head.Open{Head.parties = parties, utxoHash}
  -- NOTE: We hash tx outs in an order that is recoverable on-chain.
  -- The simplest thing to do, is to make sure commit inputs are in the same
  -- order as their corresponding committed utxo.
  extractSerialisedTxOut d =
    case fromData $ toPlutusData d of
      Nothing -> error "SNAFU"
      Just ((_, _, Just o) :: DatumType Commit.Commit) -> Just o
      _ -> Nothing
  utxoHash =
    Head.hashPreSerializedCommits $
      mapMaybe (extractSerialisedTxOut . snd . snd) $ sortOn fst $ Map.toList commits
  mkCommit (commitInput, (_commitOutput, commitDatum)) =
    ( commitInput
    , mkCommitWitness commitDatum
    )
  mkCommitWitness (ScriptDatumForTxIn -> commitDatum) =
    BuildTxWith $ ScriptWitness scriptWitnessCtx $ mkScriptWitness commitScript commitDatum commitRedeemer
  commitValue =
    mconcat $ txOutValue . fst <$> Map.elems commits
  commitScript =
    fromPlutusScript @PlutusScriptV1 Commit.validatorScript
  commitRedeemer =
    toScriptData $ Commit.redeemer Commit.CollectCom

-- | Create a transaction closing a head with given snapshot number and utxo.
closeTx ::
  Snapshot Tx ->
  -- | Multi-signature of the whole snapshot
  MultiSigned (Snapshot Tx) ->
  -- | Everything needed to spend the Head state-machine output.
  -- FIXME(SN): should also contain some Head identifier/address and stored Value (maybe the TxOut + Data?)
  UTxOWithScript ->
  Tx
closeTx Snapshot{number, utxo} sig (headInput, headOutputBefore, ScriptDatumForTxIn -> headDatumBefore) =
  unsafeBuildTransaction $
    emptyTxBody
      & addInputs [(headInput, headWitness)]
      & addOutputs [headOutputAfter]
 where
  headWitness =
    BuildTxWith $ ScriptWitness scriptWitnessCtx $ mkScriptWitness headScript headDatumBefore headRedeemer
  headScript =
    fromPlutusScript @PlutusScriptV1 $ Head.validatorScript policyId
  headRedeemer =
    toScriptData
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
  -- | Snapshotted UTxO to fanout on layer 1
  UTxO ->
  -- | Everything needed to spend the Head state-machine output.
  -- FIXME(SN): should also contain some Head identifier/address and stored Value (maybe the TxOut + Data?)
  (TxIn, ScriptData) ->
  Tx
fanoutTx utxo (headInput, ScriptDatumForTxIn -> headDatumBefore) =
  unsafeBuildTransaction $
    emptyTxBody
      & addInputs [(headInput, headWitness)]
      & addOutputs fanoutOutputs
 where
  headWitness =
    BuildTxWith $ ScriptWitness scriptWitnessCtx $ mkScriptWitness headScript headDatumBefore headRedeemer
  headScript =
    fromPlutusScript @PlutusScriptV1 $ Head.validatorScript policyId
  headRedeemer =
    toScriptData (Head.Fanout $ fromIntegral $ length utxo)

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
  -- | Data needed to spend the initial output sent to each party to the Head.
  -- Should contain the PT and is locked by initial script.
  Map TxIn ScriptData ->
  -- | Data needed to spend commit outputs.
  -- Should contain the PT and is locked by commit script.
  Map TxIn (TxOut CtxUTxO, ScriptData) ->
  Either AbortTxError Tx
abortTx networkId (headInput, ScriptDatumForTxIn -> headDatumBefore) initialsToAbort commitsToAbort
  | isJust (lookup headInput initialsToAbort) =
    Left OverlappingInputs
  | otherwise =
    Right $
      unsafeBuildTransaction $
        emptyTxBody
          & addInputs ((headInput, headWitness) : initialInputs <> commitInputs)
          & addOutputs (headOutput : commitOutputs)
 where
  headWitness =
    BuildTxWith $ ScriptWitness scriptWitnessCtx $ mkScriptWitness headScript headDatumBefore headRedeemer
  headScript =
    fromPlutusScript @PlutusScriptV1 $ Head.validatorScript policyId
  headRedeemer =
    toScriptData Head.Abort

  initialInputs = mkAbortInitial <$> Map.toList initialsToAbort

  commitInputs = mkAbortCommit <$> Map.toList commitsToAbort

  -- FIXME:
  -- (b) There's in principle no need to output any SM output here, it's over.
  headOutput =
    TxOut
      (mkScriptAddress @PlutusScriptV1 networkId headScript)
      headValue
      headDatumAfter
  headDatumAfter =
    mkTxOutDatum Head.Final

  -- NOTE: Abort datums contain the datum of the spent state-machine input, but
  -- also, the datum of the created output which is necessary for the
  -- state-machine on-chain validator to control the correctness of the
  -- transition.
  mkAbortInitial (initialInput, ScriptDatumForTxIn -> initialDatum) =
    (initialInput, mkAbortWitness initialDatum)
  mkAbortWitness initialDatum =
    BuildTxWith $ ScriptWitness scriptWitnessCtx $ mkScriptWitness initialScript initialDatum initialRedeemer
  initialScript =
    fromPlutusScript @PlutusScriptV1 Initial.validatorScript
  initialRedeemer =
    toScriptData $ Initial.redeemer Initial.Abort

  mkAbortCommit (commitInput, (_, ScriptDatumForTxIn -> commitDatum)) =
    (commitInput, mkCommitWitness commitDatum)
  mkCommitWitness commitDatum =
    BuildTxWith $ ScriptWitness scriptWitnessCtx $ mkScriptWitness commitScript commitDatum commitRedeemer
  commitScript =
    fromPlutusScript @PlutusScriptV1 Commit.validatorScript
  commitRedeemer =
    toScriptData (Commit.redeemer Commit.Abort)

  commitOutputs = mapMaybe (mkCommitOutput . snd) $ Map.elems commitsToAbort

  mkCommitOutput :: ScriptData -> Maybe (TxOut CtxTx)
  mkCommitOutput x =
    case fromData @(DatumType Commit.Commit) $ toPlutusData x of
      Just (_party, _validatorHash, serialisedTxOut) ->
        toTxContext <$> convertTxOut serialisedTxOut
      Nothing -> error "Invalid Commit datum"

-- * Observe Hydra Head transactions

data InitObservation = InitObservation
  { -- | The state machine UTxO produced by the Init transaction
    -- This output should always be present and 'threaded' across all
    -- transactions.
    -- NOTE(SN): The Head's identifier is somewhat encoded in the TxOut's address
    -- XXX(SN): Data and [OnChain.Party] are overlapping
    threadOutput :: (TxIn, TxOut CtxUTxO, ScriptData, [OnChain.Party])
  , initials :: [UTxOWithScript]
  , commits :: [UTxOWithScript]
  , headId :: HeadId
  }
  deriving (Show, Eq)

-- XXX(SN): We should log decisions why a tx is not an initTx etc. instead of
-- only returning a Maybe, i.e. 'Either Reason (OnChainTx tx, OnChainHeadState)'
observeInitTx ::
  NetworkId ->
  Party ->
  Tx ->
  Maybe (OnChainTx Tx, InitObservation)
observeInitTx networkId party tx = do
  -- FIXME: This is affected by "same structure datum attacks", we should be
  -- using the Head script address instead.
  (ix, headOut, headData, Head.Initial cp ps) <- findFirst headOutput indexedOutputs
  let parties = map convertParty ps
  let cperiod = contestationPeriodToDiffTime cp
  guard $ party `elem` parties
  headId <- findStateToken headOut
  pure
    ( OnInitTx cperiod parties
    , InitObservation
        { threadOutput =
            ( mkTxIn tx ix
            , toCtxUTxOTxOut headOut
            , fromLedgerData headData
            , ps
            )
        , initials
        , commits = []
        , headId
        }
    )
 where
  findStateToken txOut =
    flip findFirst (valueToList $ txOutValue txOut) $ \case
      (AssetId pid aname, q)
        | aname == hydraHeadV1AssetName && q == 1 ->
          Just $ mkHeadId pid
      _ ->
        Nothing

  headOutput = \case
    (ix, out@(TxOut _ _ (TxOutDatum d))) ->
      (ix,out,toLedgerData d,) <$> fromData (toPlutusData d)
    _ -> Nothing

  indexedOutputs = zip [0 ..] (txOuts' tx)

  initialOutputs = filter (isInitial . snd) indexedOutputs

  initials =
    mapMaybe
      ( \(i, o) -> do
          dat <- getScriptData o
          pure (mkTxIn tx i, toCtxUTxOTxOut o, dat)
      )
      initialOutputs

  isInitial (TxOut addr _ _) = addr == initialAddress

  initialAddress = mkScriptAddress @PlutusScriptV1 networkId initialScript

  initialScript = fromPlutusScript Initial.validatorScript

convertParty :: OnChain.Party -> Party
convertParty = Party . partyToVerKey

type CommitObservation = UTxOWithScript

-- | Identify a commit tx by:
--
-- - Find which 'initial' tx input is being consumed.
-- - Find the redeemer corresponding to that 'initial', which contains the tx
--   input of the committed utxo.
-- - Find the outputs which pays to the commit validator.
-- - Using the datum of that output, deserialize the comitted output.
-- - Reconstruct the committed UTxO from both values (tx input and output).
observeCommitTx ::
  NetworkId ->
  -- | Known (remaining) initial tx inputs.
  [TxIn] ->
  Tx ->
  Maybe (OnChainTx Tx, CommitObservation)
observeCommitTx networkId initials tx = do
  initialTxIn <- findInitialTxIn
  mCommittedTxIn <- decodeInitialRedeemer initialTxIn

  (commitIn, commitOut) <- findTxOutByAddress commitAddress tx
  dat <- getScriptData commitOut
  -- TODO: This 'party' would be available from the spent 'initial' utxo (PT eventually)
  (party, _, serializedTxOut) <- fromData @(DatumType Commit.Commit) $ toPlutusData dat
  let mCommittedTxOut = convertTxOut serializedTxOut

  comittedUTxO <-
    case (mCommittedTxIn, mCommittedTxOut) of
      (Nothing, Nothing) -> Just mempty
      (Just i, Just o) -> Just $ UTxO.singleton (i, o)
      (Nothing, Just{}) -> error "found commit with no redeemer out ref but with serialized output."
      (Just{}, Nothing) -> error "found commit with redeemer out ref but with no serialized output."

  let onChainTx = OnCommitTx (convertParty party) comittedUTxO
  pure
    ( onChainTx
    , (commitIn, toUTxOContext commitOut, dat)
    )
 where
  findInitialTxIn =
    case filter (`elem` initials) (txIns' tx) of
      [input] -> Just input
      _ -> Nothing

  decodeInitialRedeemer =
    findRedeemerSpending tx >=> \case
      Initial.Abort ->
        Nothing
      Initial.Commit{committedRef} ->
        Just (fromPlutusTxOutRef <$> committedRef)

  commitAddress = mkScriptAddress @PlutusScriptV1 networkId commitScript

  commitScript = fromPlutusScript Commit.validatorScript

convertTxOut :: Maybe Commit.SerializedTxOut -> Maybe (TxOut CtxUTxO)
convertTxOut = \case
  Nothing -> Nothing
  Just (Commit.SerializedTxOut outBytes) ->
    -- XXX(SN): these errors might be more severe and we could throw an
    -- exception here?
    case fromLedgerTxOut <$> decodeFull' (fromBuiltin outBytes) of
      Right result -> Just result
      Left{} -> error "couldn't deserialize serialized output in commit's datum."

-- TODO(SN): obviously the observeCollectComTx/observeAbortTx can be DRYed.. deliberately hold back on it though

data CollectComObservation = CollectComObservation
  { threadOutput :: (TxIn, TxOut CtxUTxO, ScriptData, [OnChain.Party])
  , headId :: HeadId
  }
  deriving (Show, Eq)

-- | Identify a collectCom tx by lookup up the input spending the Head output
-- and decoding its redeemer.
observeCollectComTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe (OnChainTx Tx, CollectComObservation)
observeCollectComTx utxo tx = do
  (headInput, headOutput) <- findScriptOutput @PlutusScriptV1 utxo headScript
  redeemer <- findRedeemerSpending tx headInput
  oldHeadDatum <- lookupScriptData tx headOutput
  datum <- fromData $ toPlutusData oldHeadDatum
  case (datum, redeemer) of
    (Head.Initial{parties}, Head.CollectCom) -> do
      (newHeadInput, newHeadOutput) <- findScriptOutput @PlutusScriptV1 (utxoFromTx tx) headScript
      newHeadDatum <- lookupScriptData tx newHeadOutput
      pure
        ( OnCollectComTx
        , CollectComObservation
            { threadOutput =
                ( newHeadInput
                , newHeadOutput
                , newHeadDatum
                , parties
                )
            , headId = HeadId ""
            }
        )
    _ -> Nothing
 where
  headScript = fromPlutusScript $ Head.validatorScript policyId

data CloseObservation = CloseObservation
  { threadOutput :: (TxIn, TxOut CtxUTxO, ScriptData, [OnChain.Party])
  , headId :: HeadId
  }
  deriving (Show, Eq)

-- | Identify a close tx by lookup up the input spending the Head output and
-- decoding its redeemer.
observeCloseTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe (OnChainTx Tx, CloseObservation)
observeCloseTx utxo tx = do
  (headInput, headOutput) <- findScriptOutput @PlutusScriptV1 utxo headScript
  redeemer <- findRedeemerSpending tx headInput
  oldHeadDatum <- lookupScriptData tx headOutput
  datum <- fromData $ toPlutusData oldHeadDatum
  case (datum, redeemer) of
    (Head.Open{parties}, Head.Close{snapshotNumber = onChainSnapshotNumber}) -> do
      (newHeadInput, newHeadOutput) <- findScriptOutput @PlutusScriptV1 (utxoFromTx tx) headScript
      newHeadDatum <- lookupScriptData tx newHeadOutput
      snapshotNumber <- integerToNatural onChainSnapshotNumber
      pure
        ( OnCloseTx{contestationDeadline, snapshotNumber}
        , CloseObservation
            { threadOutput =
                ( newHeadInput
                , newHeadOutput
                , newHeadDatum
                , parties
                )
            , headId = HeadId ""
            }
        )
    _ -> Nothing
 where
  headScript = fromPlutusScript $ Head.validatorScript policyId

  -- FIXME(SN): store in/read from datum
  contestationDeadline = UTCTime (ModifiedJulianDay 0) 0

type FanoutObservation = ()

-- | Identify a fanout tx by lookup up the input spending the Head output and
-- decoding its redeemer.
--
-- TODO: Ideally, the fanout does not produce any state-machine output. That
-- means, to observe it, we need to look for a transaction with an input spent
-- from a known script (the head state machine script) with a "fanout" redeemer.
observeFanoutTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe (OnChainTx Tx, FanoutObservation)
observeFanoutTx utxo tx = do
  headInput <- fst <$> findScriptOutput @PlutusScriptV1 utxo headScript
  findRedeemerSpending tx headInput
    >>= \case
      Head.Fanout{} -> pure (OnFanoutTx, ())
      _ -> Nothing
 where
  headScript = fromPlutusScript $ Head.validatorScript policyId

type AbortObservation = ()

-- | Identify an abort tx by looking up the input spending the Head output and
-- decoding its redeemer.
observeAbortTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe (OnChainTx Tx, AbortObservation)
observeAbortTx utxo tx = do
  headInput <- fst <$> findScriptOutput @PlutusScriptV1 utxo headScript
  findRedeemerSpending tx headInput >>= \case
    Head.Abort -> pure (OnAbortTx, ())
    _ -> Nothing
 where
  -- FIXME(SN): make sure this is aborting "the right head / your head" by not hard-coding policyId
  headScript = fromPlutusScript $ Head.validatorScript policyId

-- * Functions related to OnChainHeadState

-- | Look for the "initial" which corresponds to given cardano verification key.
ownInitial ::
  VerificationKey PaymentKey ->
  [UTxOWithScript] ->
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

mkHeadId :: PolicyId -> HeadId
mkHeadId =
  HeadId . serialiseToRawBytes

-- * Helpers

-- | Find first occurrence including a transformation.
findFirst :: Foldable t => (a -> Maybe b) -> t a -> Maybe b
findFirst fn = getFirst . foldMap (First . fn)
