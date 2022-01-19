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

import Hydra.Prelude

import Cardano.Api (NetworkId)
import Cardano.Binary (serialize)
import Cardano.Ledger.Address (Addr (Addr))
import Cardano.Ledger.Alonzo (Script)
import Cardano.Ledger.Alonzo.Data (Data, getPlutusData)
import Cardano.Ledger.Alonzo.Language (Language (PlutusV1))
import Cardano.Ledger.Alonzo.Scripts (Script (PlutusScript))
import Cardano.Ledger.Alonzo.Tx (ValidatedTx (..))
import Cardano.Ledger.Alonzo.TxBody (TxBody (..), TxOut (TxOut))
import Cardano.Ledger.Alonzo.TxInfo (transKeyHash)
import Cardano.Ledger.Alonzo.TxWitness (TxWitness (..), unTxDats)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Era (hashScript)
import Cardano.Ledger.Shelley.API (
  Credential (ScriptHashObj),
  Network (Testnet),
  StakeReference (StakeRefNull),
  StrictMaybe (..),
  TxIn,
  hashKey,
 )
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time (Day (ModifiedJulianDay), UTCTime (UTCTime))
import Hydra.Chain (HeadParameters (..), OnChainTx (..))
import Hydra.Chain.Direct.Util (Era)
import qualified Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.Initial as Initial
import Hydra.Data.ContestationPeriod (contestationPeriodFromDiffTime, contestationPeriodToDiffTime)
import Hydra.Data.Party (partyFromVerKey, partyToVerKey)
import qualified Hydra.Data.Party as OnChain
import Hydra.Data.Utxo (fromByteString)
import qualified Hydra.Data.Utxo as OnChain
import Hydra.Ledger.Cardano (
  CardanoTx,
  PaymentKey,
  Utxo,
  Utxo' (Utxo),
  VerificationKey (PaymentVerificationKey),
  fromLedgerTx,
  fromPlutusScript,
  getDatum,
  hashTxOuts,
  mkScriptAddress,
  toAlonzoData,
  toCtxUTxOTxOut,
  toLedgerTxIn,
  toLedgerTxOut,
  toPlutusData,
 )
import qualified Hydra.Ledger.Cardano as Api
import Hydra.Party (MultiSigned, Party (Party), toPlutusSignatures, vkey)
import Hydra.Snapshot (Snapshot (..))
import Ledger.Value (AssetClass (..), currencyMPSHash)
import Plutus.V1.Ledger.Api (MintingPolicyHash, PubKeyHash (..), fromData)
import qualified Plutus.V1.Ledger.Api as Plutus
import Plutus.V1.Ledger.Value (assetClass, currencySymbol, tokenName)
import Plutus.V2.Ledger.Api (toBuiltin)

-- FIXME: parameterize
network :: Network
network = Testnet

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
        threadOutput :: (TxIn StandardCrypto, TxOut Era, Data Era, [OnChain.Party])
      , initials :: [(TxIn StandardCrypto, TxOut Era, Data Era)]
      , commits :: [(TxIn StandardCrypto, TxOut Era, Data Era)]
      }
  | OpenOrClosed
      { -- | The state machine UTxO produced by the Init transaction
        -- This output should always be present and 'threaded' across all
        -- transactions.
        -- NOTE(SN): The Head's identifier is somewhat encoded in the TxOut's address
        -- XXX(SN): Data and [OnChain.Party] are overlapping
        threadOutput :: (TxIn StandardCrypto, TxOut Era, Data Era, [OnChain.Party])
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
--
-- TODO: Remove all the qualified 'Api' once the refactoring is over and there's
-- no more import clash with 'cardano-ledger'.
--
-- TODO: Get rid of Ledger types in the signature and fully rely on Cardano.Api
initTx ::
  NetworkId ->
  -- | Participant's cardano public keys.
  [VerificationKey PaymentKey] ->
  HeadParameters ->
  TxIn StandardCrypto ->
  ValidatedTx Era
initTx networkId cardanoKeys HeadParameters{contestationPeriod, parties} txIn =
  Api.toLedgerTx $
    Api.unsafeBuildTransaction $
      Api.emptyTxBody
        & Api.addVkInputs [Api.fromLedgerTxIn txIn]
        & Api.addOutputs (headOutput : map mkInitialOutput cardanoKeys)
 where
  headOutput =
    Api.TxOut headAddress headValue headDatum
  headScript =
    Api.fromPlutusScript $ Head.validatorScript policyId
  headAddress =
    Api.mkScriptAddress @Api.PlutusScriptV1 networkId headScript
  headValue =
    Api.lovelaceToTxOutValue $ Api.Lovelace 2_000_000
  headDatum =
    Api.mkTxOutDatum $
      Head.Initial
        (contestationPeriodFromDiffTime contestationPeriod)
        (map (partyFromVerKey . vkey) parties)

  mkInitialOutput (Api.toPlutusKeyHash . Api.verificationKeyHash -> vkh) =
    Api.TxOut initialAddress initialValue (mkInitialDatum vkh)
  initialScript =
    Api.fromPlutusScript Initial.validatorScript
  -- FIXME: should really be the minted PTs plus some ADA to make the ledger happy
  initialValue =
    Api.lovelaceToTxOutValue $ Api.Lovelace 2_000_000
  initialAddress =
    Api.mkScriptAddress @Api.PlutusScriptV1 networkId initialScript
  mkInitialDatum =
    Api.mkTxOutDatum . Initial.datum

pubKeyHash :: VerificationKey PaymentKey -> PubKeyHash
pubKeyHash (PaymentVerificationKey vkey) = transKeyHash $ hashKey @StandardCrypto $ vkey

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
  Maybe (Api.TxIn, Api.TxOut Api.CtxUTxO Api.Era) ->
  -- | The inital output (sent to each party) which should contain the PT and is
  -- locked by initial script
  (TxIn StandardCrypto, PubKeyHash) ->
  ValidatedTx Era
commitTx networkId party utxo (initialInput, vkh) =
  Api.toLedgerTx $
    Api.unsafeBuildTransaction $
      Api.emptyTxBody
        & Api.addInputs [(Api.fromLedgerTxIn initialInput, initialWitness)]
        & Api.addVkInputs [commit | Just (commit, _) <- [utxo]]
        & Api.addOutputs [commitOutput]
 where
  initialWitness =
    Api.BuildTxWith $ Api.mkScriptWitness initialScript initialDatum initialRedeemer
  initialScript =
    Api.fromPlutusScript Initial.validatorScript
  initialDatum =
    Api.mkDatumForTxIn $ Initial.datum vkh
  initialRedeemer =
    Api.mkRedeemerForTxIn $ Initial.redeemer ()

  commitOutput =
    Api.TxOut commitAddress commitValue commitDatum
  commitScript =
    Api.fromPlutusScript Commit.validatorScript
  commitAddress =
    Api.mkScriptAddress @Api.PlutusScriptV1 networkId commitScript
  -- FIXME: We should add the value from the initialIn too because it contains the PTs
  commitValue =
    Api.mkTxOutValue $
      Api.lovelaceToValue 2_000_000 <> maybe mempty (Api.txOutValue . snd) utxo
  commitDatum =
    Api.mkTxOutDatum $ mkCommitDatum party utxo

mkCommitDatum :: Party -> Maybe (Api.TxIn, Api.TxOut Api.CtxUTxO Api.Era) -> Plutus.Datum
mkCommitDatum (partyFromVerKey . vkey -> party) utxo =
  Commit.datum (party, commitUtxo)
 where
  commitUtxo = fromByteString $
    toStrict $
      Aeson.encode $ case utxo of
        Nothing -> mempty
        Just (i, o) -> Utxo $ Map.singleton i o

-- | Create a transaction collecting all "committed" utxo and opening a Head,
-- i.e. driving the Head script state.
collectComTx ::
  NetworkId ->
  -- | Committed UTxO to become U0 in the Head ledger state.
  -- This is only used as a datum passed to the Head state machine script.
  Utxo ->
  -- | Everything needed to spend the Head state-machine output.
  -- FIXME(SN): should also contain some Head identifier/address and stored Value (maybe the TxOut + Data?)
  (TxIn StandardCrypto, Data Era, [OnChain.Party]) ->
  -- | Data needed to spend the commit output produced by each party.
  -- Should contain the PT and is locked by @ν_commit@ script.
  Map (TxIn StandardCrypto) (TxOut Era, Data Era) ->
  ValidatedTx Era
-- TODO(SN): utxo unused means other participants would not "see" the opened
-- utxo when observing. Right now, they would be trusting the OCV checks this
-- and construct their "world view" from observed commit txs in the HeadLogic
collectComTx networkId utxo (Api.fromLedgerTxIn -> headInput, Api.fromLedgerData -> headDatumBefore, parties) commits =
  Api.toLedgerTx $
    Api.unsafeBuildTransaction $
      Api.emptyTxBody
        & Api.addInputs ((headInput, headWitness) : (mkCommit <$> Map.toList commits))
        & Api.addOutputs [headOutput]
 where
  headWitness =
    Api.BuildTxWith $ Api.mkScriptWitness headScript headDatumBefore headRedeemer
  headScript =
    Api.fromPlutusScript $ Head.validatorScript policyId
  headRedeemer =
    Api.mkRedeemerForTxIn $
      Head.CollectCom{utxoHash}
  utxoHash = toBuiltin $ hashTxOuts $ toList utxo
  headOutput =
    Api.TxOut
      (Api.mkScriptAddress @Api.PlutusScriptV1 networkId headScript)
      (Api.mkTxOutValue $ Api.lovelaceToValue 2_000_000 <> commitValue)
      headDatumAfter
  headDatumAfter =
    Api.mkTxOutDatum Head.Open{Head.parties = parties, utxoHash}
  mkCommit (commitInput, (_commitOutput, commitDatum)) =
    ( Api.fromLedgerTxIn commitInput
    , mkCommitWitness commitDatum
    )
  mkCommitWitness (Api.fromLedgerData -> commitDatum) =
    Api.BuildTxWith $ Api.mkScriptWitness commitScript commitDatum commitRedeemer
  commitValue =
    mconcat $ Api.txOutValue . Api.fromLedgerTxOut . fst <$> Map.elems commits
  commitScript =
    Api.fromPlutusScript Commit.validatorScript
  commitRedeemer =
    Api.mkRedeemerForTxIn $ Commit.redeemer Commit.Collect

-- | Create a transaction closing a head with given snapshot number and utxo.
closeTx ::
  Snapshot CardanoTx ->
  -- | Multi-signature of the whole snapshot
  MultiSigned (Snapshot CardanoTx) ->
  -- | Everything needed to spend the Head state-machine output.
  -- FIXME(SN): should also contain some Head identifier/address and stored Value (maybe the TxOut + Data?)
  (TxIn StandardCrypto, TxOut Era, Data Era) ->
  ValidatedTx Era
closeTx Snapshot{number, utxo} sig (Api.fromLedgerTxIn -> headInput, Api.fromLedgerTxOut -> headOutputBefore, Api.fromLedgerData -> headDatumBefore) =
  Api.toLedgerTx $
    Api.unsafeBuildTransaction $
      Api.emptyTxBody
        & Api.addInputs [(headInput, headWitness)]
        & Api.addOutputs [headOutputAfter]
 where
  headWitness =
    Api.BuildTxWith $ Api.mkScriptWitness headScript headDatumBefore headRedeemer
  headScript =
    Api.fromPlutusScript $ Head.validatorScript policyId
  headRedeemer =
    Api.mkRedeemerForTxIn
      Head.Close
        { snapshotNumber = toInteger number
        , signature = toPlutusSignatures sig
        , utxoHash
        }
  headOutputAfter =
    Api.modifyTxOutDatum (const headDatumAfter) headOutputBefore
  headDatumAfter =
    Api.mkTxOutDatum
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
  (TxIn StandardCrypto, Data Era) ->
  ValidatedTx Era
fanoutTx utxo (Api.fromLedgerTxIn -> headInput, Api.fromLedgerData -> headDatumBefore) =
  Api.toLedgerTx $
    Api.unsafeBuildTransaction $
      Api.emptyTxBody
        & Api.addInputs [(headInput, headWitness)]
        & Api.addOutputs fanoutOutputs
 where
  headWitness =
    Api.BuildTxWith $ Api.mkScriptWitness headScript headDatumBefore headRedeemer
  headScript =
    Api.fromPlutusScript $ Head.validatorScript policyId
  headRedeemer =
    Api.mkRedeemerForTxIn (Head.Fanout $ fromIntegral $ length utxo)

  fanoutOutputs =
    foldr ((:) . Api.toTxContext) [] utxo

data AbortTxError = OverlappingInputs
  deriving (Show)

-- | Create transaction which aborts a head by spending the Head output and all
-- other "initial" outputs.
abortTx ::
  -- | Network identifier for address discrimination
  NetworkId ->
  -- | Everything needed to spend the Head state-machine output.
  (TxIn StandardCrypto, Data Era) ->
  -- | Data needed to spend the inital output sent to each party to the Head
  -- which should contain the PT and is locked by initial script.
  Map (TxIn StandardCrypto) (Data Era) ->
  Either AbortTxError (ValidatedTx Era)
abortTx networkId (Api.fromLedgerTxIn -> headInput, Api.fromLedgerData -> headDatumBefore) initialInputs
  | isJust (lookup (Api.toLedgerTxIn headInput) initialInputs) =
    Left OverlappingInputs
  | otherwise =
    Right $
      Api.toLedgerTx $
        Api.unsafeBuildTransaction $
          Api.emptyTxBody
            & Api.addInputs ((headInput, headWitness) : (mkAbort <$> Map.toList initialInputs))
            & Api.addOutputs [headOutput]
 where
  headWitness =
    Api.BuildTxWith $ Api.mkScriptWitness headScript headDatumBefore headRedeemer
  headScript =
    Api.fromPlutusScript $ Head.validatorScript policyId
  headRedeemer =
    Api.mkRedeemerForTxIn Head.Abort

  -- FIXME:
  -- (a) Abort need to reimburse participants that have committed!
  -- (b) There's in principle no need to output any SM output here, it's over.
  headOutput =
    Api.TxOut
      (Api.mkScriptAddress @Api.PlutusScriptV1 networkId headScript)
      (Api.mkTxOutValue $ Api.lovelaceToValue 2_000_000)
      headDatumAfter
  headDatumAfter =
    Api.mkTxOutDatum Head.Final

  -- NOTE: Abort datums contain the datum of the spent state-machine input, but
  -- also, the datum of the created output which is necessary for the
  -- state-machine on-chain validator to control the correctness of the
  -- transition.
  mkAbort (Api.fromLedgerTxIn -> initialInput, Api.fromLedgerData -> initialDatum) =
    (initialInput, mkAbortWitness initialDatum)
  mkAbortWitness initialDatum =
    Api.BuildTxWith $ Api.mkScriptWitness initialScript initialDatum initialRedeemer
  initialScript =
    Api.fromPlutusScript Initial.validatorScript
  initialRedeemer =
    Api.mkRedeemerForTxIn $ Initial.redeemer ()

-- * Observe Hydra Head transactions

-- XXX(SN): We should log decisions why a tx is not an initTx etc. instead of
-- only returning a Maybe, i.e. 'Either Reason (OnChainTx tx, OnChainHeadState)'
observeInitTx ::
  Api.NetworkId ->
  Party ->
  ValidatedTx Era ->
  Maybe (OnChainTx CardanoTx, OnChainHeadState)
observeInitTx networkId party (Api.getTxBody . fromLedgerTx -> txBody) = do
  (ix, headOut, headData, Head.Initial cp ps) <- findFirst headOutput indexedOutputs
  let parties = map convertParty ps
  let cperiod = contestationPeriodToDiffTime cp
  guard $ party `elem` parties
  pure
    ( OnInitTx cperiod parties
    , Initial
        { threadOutput =
            ( toLedgerTxIn $ Api.mkTxIn txBody ix
            , toLedgerTxOut $ toCtxUTxOTxOut headOut
            , headData
            , ps
            )
        , initials
        , commits = []
        }
    )
 where
  Api.TxBody Api.TxBodyContent{txOuts} = txBody

  headOutput = \case
    (ix, out@(Api.TxOut _ _ (Api.TxOutDatum _ d))) ->
      (ix,out,Api.toAlonzoData d,) <$> fromData (Api.toPlutusData d)
    _ -> Nothing

  indexedOutputs = zip [0 ..] txOuts

  initialOutputs = filter (isInitial . snd) indexedOutputs

  initials =
    mapMaybe
      ( \(i, o) -> do
          dat <- toAlonzoData <$> getDatum o
          pure (toLedgerTxIn $ Api.mkTxIn txBody i, toLedgerTxOut $ toCtxUTxOTxOut o, dat)
      )
      initialOutputs

  isInitial (Api.TxOut addr _ _) = addr == initialAddress

  initialAddress = mkScriptAddress @Api.PlutusScriptV1 networkId initialScript

  initialScript = fromPlutusScript Initial.validatorScript

convertParty :: OnChain.Party -> Party
convertParty = Party . partyToVerKey

-- | Identify a commit tx by looking for an output which pays to v_commit.
observeCommitTx ::
  Api.NetworkId ->
  ValidatedTx Era ->
  Maybe (OnChainTx CardanoTx, (TxIn StandardCrypto, TxOut Era, Data Era))
observeCommitTx networkId (Api.getTxBody . fromLedgerTx -> txBody) = do
  (commitIn, commitOut) <- Api.findTxOutByAddress commitAddress txBody
  dat <- getDatum commitOut
  (party, utxo) <- fromData $ toPlutusData dat
  onChainTx <- OnCommitTx (convertParty party) <$> convertUtxo utxo
  pure
    ( onChainTx
    ,
      ( toLedgerTxIn commitIn
      , toLedgerTxOut $ toCtxUTxOTxOut commitOut
      , toAlonzoData dat
      )
    )
 where
  convertUtxo = Aeson.decodeStrict' . OnChain.toByteString

  commitAddress = mkScriptAddress @Api.PlutusScriptV1 networkId commitScript

  commitScript = fromPlutusScript Commit.validatorScript

-- REVIEW(SN): Is this really specific to commit only, or wouldn't we be able to
-- filter all 'knownUtxo' after observing any protocol tx?
observeCommit ::
  Api.NetworkId ->
  ValidatedTx Era ->
  OnChainHeadState ->
  Maybe (OnChainTx CardanoTx, OnChainHeadState)
observeCommit networkId tx = \case
  Initial{threadOutput, initials, commits} -> do
    (onChainTx, commitTriple) <- observeCommitTx networkId tx
    -- NOTE(SN): A commit tx has been observed and thus we can remove all it's
    -- inputs from our tracked initials
    let commitIns = inputs $ body tx
    let initials' = filter (\(i, _, _) -> i `Set.notMember` commitIns) initials
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
  UTxO Era ->
  ValidatedTx Era ->
  Maybe (OnChainTx CardanoTx, OnChainHeadState)
observeCollectComTx utxo tx = do
  (headInput, headOutput) <- Api.findScriptOutput @Api.PlutusScriptV1 (Api.fromLedgerUtxo utxo) headScript
  redeemer <-
    Api.findRedeemerSpending
      (Api.getTxBody $ Api.fromLedgerTx tx)
      headInput
  oldHeadDatum <- lookupDatum (wits tx) (Api.toLedgerTxOut headOutput)
  datum <- fromData $ getPlutusData oldHeadDatum
  case (datum, redeemer) of
    (Head.Initial{parties}, Head.CollectCom{}) -> do
      (newHeadInput, newHeadOutput) <- Api.findScriptOutput @Api.PlutusScriptV1 (Api.utxoFromTx $ Api.fromLedgerTx tx) headScript
      newHeadDatum <- lookupDatum (wits tx) (Api.toLedgerTxOut newHeadOutput)
      pure
        ( OnCollectComTx
        , OpenOrClosed
            { threadOutput =
                ( Api.toLedgerTxIn newHeadInput
                , Api.toLedgerTxOut newHeadOutput
                , newHeadDatum
                , parties
                )
            }
        )
    _ -> Nothing
 where
  headScript = Api.fromPlutusScript $ Head.validatorScript policyId

-- | Identify a close tx by lookup up the input spending the Head output and
-- decoding its redeemer.
observeCloseTx ::
  -- | A Utxo set to lookup tx inputs
  UTxO Era ->
  ValidatedTx Era ->
  Maybe (OnChainTx CardanoTx, OnChainHeadState)
observeCloseTx utxo tx = do
  (headInput, headOutput) <- Api.findScriptOutput @Api.PlutusScriptV1 (Api.fromLedgerUtxo utxo) headScript
  redeemer <-
    Api.findRedeemerSpending
      (Api.getTxBody $ Api.fromLedgerTx tx)
      headInput
  oldHeadDatum <- lookupDatum (wits tx) (Api.toLedgerTxOut headOutput)
  datum <- fromData $ getPlutusData oldHeadDatum
  case (datum, redeemer) of
    (Head.Open{parties}, Head.Close{snapshotNumber = onChainSnapshotNumber}) -> do
      (newHeadInput, newHeadOutput) <- Api.findScriptOutput @Api.PlutusScriptV1 (Api.utxoFromTx $ Api.fromLedgerTx tx) headScript
      newHeadDatum <- lookupDatum (wits tx) (Api.toLedgerTxOut newHeadOutput)
      snapshotNumber <- integerToNatural onChainSnapshotNumber
      pure
        ( OnCloseTx{contestationDeadline, snapshotNumber}
        , OpenOrClosed
            { threadOutput =
                ( Api.toLedgerTxIn newHeadInput
                , Api.toLedgerTxOut newHeadOutput
                , newHeadDatum
                , parties
                )
            }
        )
    _ -> Nothing
 where
  headScript = Api.fromPlutusScript $ Head.validatorScript policyId

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
  UTxO Era ->
  ValidatedTx Era ->
  Maybe (OnChainTx CardanoTx, OnChainHeadState)
observeFanoutTx utxo tx = do
  headInput <- fst <$> Api.findScriptOutput @Api.PlutusScriptV1 (Api.fromLedgerUtxo utxo) headScript
  Api.findRedeemerSpending
    (Api.getTxBody $ Api.fromLedgerTx tx)
    headInput
    >>= \case
      Head.Fanout{} -> pure (OnFanoutTx, Final)
      _ -> Nothing
 where
  headScript = Api.fromPlutusScript $ Head.validatorScript policyId

-- | Identify an abort tx by looking up the input spending the Head output and
-- decoding its redeemer.
observeAbortTx ::
  -- | A Utxo set to lookup tx inputs
  UTxO Era ->
  ValidatedTx Era ->
  Maybe (OnChainTx CardanoTx, OnChainHeadState)
observeAbortTx utxo tx = do
  headInput <- fst <$> Api.findScriptOutput @Api.PlutusScriptV1 (Api.fromLedgerUtxo utxo) headScript
  Api.findRedeemerSpending
    (Api.getTxBody $ Api.fromLedgerTx tx)
    headInput
    >>= \case
      Head.Abort -> pure (OnAbortTx, Final)
      _ -> Nothing
 where
  -- FIXME(SN): make sure this is aborting "the right head / your head" by not hard-coding policyId
  headScript = Api.fromPlutusScript $ Head.validatorScript policyId

-- * Functions related to OnChainHeadState

-- | Provide a UTXO map for given OnChainHeadState. Used by the TinyWallet and
-- the direct chain component to lookup inputs for balancing / constructing txs.
-- XXX(SN): This is a hint that we might want to track the Utxo directly?
knownUtxo :: OnChainHeadState -> Map (TxIn StandardCrypto) (TxOut Era)
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
ownInitial :: VerificationKey PaymentKey -> [(TxIn StandardCrypto, TxOut Era, Data Era)] -> Maybe (TxIn StandardCrypto, PubKeyHash)
ownInitial vkey =
  foldl' go Nothing
 where
  go (Just x) _ = Just x
  go Nothing (i, _, dat) = do
    pkh <- fromData (getPlutusData dat)
    guard $ pkh == pubKeyHash vkey
    pure (i, pkh)

-- * Helpers

-- | Get the ledger address for a given plutus script.
scriptAddr :: Script Era -> Addr StandardCrypto
scriptAddr script =
  Addr
    network
    (ScriptHashObj $ hashScript @Era script)
    -- REVIEW(SN): stake head funds?
    StakeRefNull

plutusScript :: Plutus.Script -> Script Era
plutusScript = PlutusScript PlutusV1 . toShort . fromLazy . serialize

-- | Lookup included datum of given 'TxOut'.
lookupDatum :: TxWitness Era -> TxOut Era -> Maybe (Data Era)
lookupDatum wits = \case
  (TxOut _ _ (SJust datumHash)) -> Map.lookup datumHash . unTxDats $ txdats wits
  _ -> Nothing

-- | Find first occurrence including a transformation.
findFirst :: Foldable t => (a -> Maybe b) -> t a -> Maybe b
findFirst fn = getFirst . foldMap (First . fn)
