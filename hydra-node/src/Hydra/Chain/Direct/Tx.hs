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
import Cardano.Ledger.Alonzo.Data (Data, DataHash, getPlutusData, hashData)
import Cardano.Ledger.Alonzo.Language (Language (PlutusV1))
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Script (PlutusScript), Tag (Spend))
import Cardano.Ledger.Alonzo.Tx (IsValid (IsValid), ScriptPurpose (Spending), ValidatedTx (..), rdptr)
import Cardano.Ledger.Alonzo.TxBody (TxBody (..), TxOut (TxOut))
import Cardano.Ledger.Alonzo.TxInfo (transKeyHash)
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr (RdmrPtr), Redeemers (..), TxDats (..), TxWitness (..), unRedeemers, unTxDats)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Era (hashScript)
import qualified Cardano.Ledger.SafeHash as SafeHash
import Cardano.Ledger.Shelley.API (
  Coin (..),
  Credential (ScriptHashObj),
  Network (Testnet),
  ScriptHash,
  StakeReference (StakeRefNull),
  StrictMaybe (..),
  TxId (TxId),
  TxIn (TxIn),
  Wdrl (Wdrl),
  hashKey,
 )
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Time (Day (ModifiedJulianDay), UTCTime (UTCTime))
import Hydra.Chain (HeadParameters (..), OnChainTx (..))
import Hydra.Chain.Direct.Util (Era)
import qualified Hydra.Contract.MockCommit as MockCommit
import qualified Hydra.Contract.MockHead as MockHead
import qualified Hydra.Contract.MockInitial as MockInitial
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
 )
import qualified Hydra.Ledger.Cardano as Api
import Hydra.Party (Party (Party), vkey)
import Hydra.Snapshot (SnapshotNumber)
import Ledger.Value (AssetClass (..), currencyMPSHash)
import Plutus.V1.Ledger.Api (FromData, MintingPolicyHash, PubKeyHash (..), fromData)
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Plutus.V1.Ledger.Crypto as Plutus
import Plutus.V1.Ledger.Value (assetClass, currencySymbol, tokenName)

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
        threadOutput :: (TxIn StandardCrypto, TxOut Era, Data Era)
      , initials :: [(TxIn StandardCrypto, TxOut Era, Data Era)]
      , commits :: [(TxIn StandardCrypto, TxOut Era, Data Era)]
      }
  | OpenOrClosed
      { -- | The state machine UTxO produced by the Init transaction
        -- This output should always be present and 'threaded' across all
        -- transactions.
        -- NOTE(SN): The Head's identifier is somewhat encoded in the TxOut's address
        threadOutput :: (TxIn StandardCrypto, TxOut Era, Data Era)
      }
  | Final
  deriving (Eq, Show, Generic)

-- FIXME: should not be hardcoded, for testing purposes only
threadToken :: AssetClass
threadToken = assetClass (currencySymbol "hydra") (tokenName "token")

-- FIXME: should not be hardcoded
policyId :: MintingPolicyHash
(policyId, _) = first currencyMPSHash (unAssetClass threadToken)

emptyTx :: ValidatedTx Era
emptyTx =
  ValidatedTx
    { body = emptyTxBody
    , wits =
        TxWitness
          { txwitsVKey = mempty
          , txwitsBoot = mempty
          , txscripts = mempty
          , txdats = mempty
          , txrdmrs = Redeemers Map.empty
          }
    , isValid = IsValid True -- REVIEW(SN): no idea of the semantics of this
    , auxiliaryData = SNothing
    }

withBody :: TxBody Era -> ValidatedTx Era -> ValidatedTx Era
withBody body tx =
  tx{body}

withDatums :: [Data Era] -> ValidatedTx Era -> ValidatedTx Era
withDatums datums tx =
  tx{wits = (wits tx){txdats = datumsFromList datums}}

withRedeemers :: [(TxIn StandardCrypto, Data Era)] -> ValidatedTx Era -> ValidatedTx Era
withRedeemers redeemers tx =
  tx{wits = (wits tx){txrdmrs = redeemersFromList $ mkRedeemer <$> redeemers}}
 where
  mkRedeemer (txin, redeemer) = (rdptr (body tx) (Spending txin), (redeemer, ExUnits 0 0))

withScripts :: Map (ScriptHash StandardCrypto) (Script Era) -> ValidatedTx Era -> ValidatedTx Era
withScripts scripts tx =
  tx{wits = (wits tx){txscripts = scripts}}

emptyTxBody :: TxBody Era
emptyTxBody =
  TxBody
    { inputs = mempty
    , collateral = mempty
    , outputs = mempty
    , txcerts = mempty
    , txwdrls = Wdrl mempty
    , txfee = Coin 0
    , txvldt = ValidityInterval SNothing SNothing
    , txUpdates = SNothing
    , reqSignerHashes = mempty
    , mint = mempty
    , scriptIntegrityHash = SNothing
    , adHash = SNothing
    , txnetworkid = SNothing
    }

-- | Adds the given 'inputs' to the existing transaction body's existing inputs.
withInputs :: [TxIn StandardCrypto] -> TxBody Era -> TxBody Era
withInputs newInputs txbody =
  txbody{inputs = inputs txbody <> Set.fromList newInputs}

-- | Appends the given 'newOutputs' to the transaction body's existing outputs.
withOutputs :: [TxOut Era] -> TxBody Era -> TxBody Era
withOutputs newOutputs txbody =
  txbody{outputs = outputs txbody <> StrictSeq.fromList newOutputs}

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
    Api.fromPlutusScript $ MockHead.validatorScript policyId
  headAddress =
    Api.mkScriptAddress @Api.PlutusScriptV1 networkId headScript
  headValue =
    Api.lovelaceToTxOutValue $ Api.Lovelace 2_000_000
  headDatum =
    Api.mkTxOutDatum $
      MockHead.Initial
        (contestationPeriodFromDiffTime contestationPeriod)
        (map (partyFromVerKey . vkey) parties)

  mkInitialOutput (Api.toPlutusKeyHash . Api.verificationKeyHash -> vkh) =
    Api.TxOut initialAddress initialValue (mkInitialDatum vkh)
  initialScript =
    Api.fromPlutusScript MockInitial.validatorScript
  -- FIXME: should really be the minted PTs plus some ADA to make the ledger happy
  initialValue =
    Api.lovelaceToTxOutValue $ Api.Lovelace 2_000_000
  initialAddress =
    Api.mkScriptAddress @Api.PlutusScriptV1 networkId initialScript
  mkInitialDatum =
    Api.mkTxOutDatum . MockInitial.datum

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
    Api.fromPlutusScript MockInitial.validatorScript
  initialDatum =
    Api.mkDatumForTxIn $ MockInitial.datum vkh
  initialRedeemer =
    Api.mkRedeemerForTxIn $ MockInitial.redeemer ()

  commitOutput =
    Api.TxOut commitAddress commitValue commitDatum
  commitScript =
    Api.fromPlutusScript MockCommit.validatorScript
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
  MockCommit.datum (party, commitUtxo)
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
  (TxIn StandardCrypto, Data Era) ->
  -- | Data needed to spend the commit output produced by each party.
  -- Should contain the PT and is locked by @ν_commit@ script.
  Map (TxIn StandardCrypto) (TxOut Era, Data Era) ->
  ValidatedTx Era
-- TODO(SN): utxo unused means other participants would not "see" the opened
-- utxo when observing. Right now, they would be trusting the OCV checks this
-- and construct their "world view" from observed commit txs in the HeadLogic
collectComTx networkId _utxo (Api.fromLedgerTxIn -> headInput, Api.fromLedgerData -> headDatumBefore) commits =
  Api.toLedgerTx $
    Api.unsafeBuildTransaction $
      Api.emptyTxBody
        & Api.addInputs ((headInput, headWitness) : (mkCommit <$> Map.toList commits))
        & Api.addOutputs [headOutput]
 where
  headWitness =
    Api.BuildTxWith $ Api.mkScriptWitness headScript headDatumBefore headRedeemer
  headScript =
    Api.fromPlutusScript $ MockHead.validatorScript policyId
  headRedeemer =
    Api.mkRedeemerForTxIn $ MockHead.CollectCom $ Api.toPlutusValue commitValue

  headOutput =
    Api.TxOut
      (Api.mkScriptAddress @Api.PlutusScriptV1 networkId headScript)
      (Api.mkTxOutValue $ Api.lovelaceToValue 2_000_000 <> commitValue)
      headDatumAfter
  headDatumAfter =
    Api.mkTxOutDatum MockHead.Open

  mkCommit (commitInput, (_commitOutput, commitDatum)) =
    ( Api.fromLedgerTxIn commitInput
    , mkCommitWitness commitDatum
    )
  mkCommitWitness (Api.fromLedgerData -> commitDatum) =
    Api.BuildTxWith $ Api.mkScriptWitness commitScript commitDatum commitRedeemer
  commitValue =
    mconcat $ Api.txOutValue . Api.fromLedgerTxOut . fst <$> Map.elems commits
  commitScript =
    Api.fromPlutusScript MockCommit.validatorScript
  commitRedeemer =
    Api.mkRedeemerForTxIn MockCommit.redeemer

-- | Create a transaction closing a head with given snapshot number and utxo.
closeTx ::
  SnapshotNumber ->
  -- | Snapshotted Utxo to close the Head with.
  Utxo ->
  -- | Everything needed to spend the Head state-machine output.
  -- FIXME(SN): should also contain some Head identifier/address and stored Value (maybe the TxOut + Data?)
  (TxIn StandardCrypto, TxOut Era, Data Era) ->
  ValidatedTx Era
closeTx snapshotNumber _utxo (Api.fromLedgerTxIn -> headInput, Api.fromLedgerTxOut -> headOutputBefore, Api.fromLedgerData -> headDatumBefore) =
  Api.toLedgerTx $
    Api.unsafeBuildTransaction $
      Api.emptyTxBody
        & Api.addInputs [(headInput, headWitness)]
        & Api.addOutputs [headOutputAfter]
 where
  headWitness =
    Api.BuildTxWith $ Api.mkScriptWitness headScript headDatumBefore headRedeemer
  headScript =
    Api.fromPlutusScript $ MockHead.validatorScript policyId
  headRedeemer =
    Api.mkRedeemerForTxIn $ closeRedeemer snapshotNumber undefined

  headOutputAfter =
    Api.modifyTxOutDatum (const headDatumAfter) headOutputBefore
  headDatumAfter =
    Api.mkTxOutDatum MockHead.Closed

closeRedeemer :: SnapshotNumber -> ByteString -> MockHead.Input
closeRedeemer snapshotNumber sigBytes =
  MockHead.Close
    { snapshotNumber = onChainSnapshotNumber
    , signature = onChainSignature
    }
 where
  onChainSnapshotNumber = fromIntegral snapshotNumber

  onChainSignature = Plutus.Signature undefined

fanoutTx ::
  -- | Network identifier for address discrimination
  NetworkId ->
  -- | Snapshotted Utxo to fanout on layer 1
  Utxo ->
  -- | Everything needed to spend the Head state-machine output.
  -- FIXME(SN): should also contain some Head identifier/address and stored Value (maybe the TxOut + Data?)
  (TxIn StandardCrypto, Data Era) ->
  ValidatedTx Era
fanoutTx networkId utxo (Api.fromLedgerTxIn -> headInput, Api.fromLedgerData -> headDatumBefore) =
  Api.toLedgerTx $
    Api.unsafeBuildTransaction $
      Api.emptyTxBody
        & Api.addInputs [(headInput, headWitness)]
        & Api.addOutputs (headOutput : fanoutOutputs)
 where
  headWitness =
    Api.BuildTxWith $ Api.mkScriptWitness headScript headDatumBefore headRedeemer
  headScript =
    Api.fromPlutusScript $ MockHead.validatorScript policyId
  headRedeemer =
    Api.mkRedeemerForTxIn MockHead.Fanout

  -- TODO: we probably don't need an output for the head SM which we don't use anyway
  headOutput =
    Api.TxOut
      (Api.mkScriptAddress @Api.PlutusScriptV1 networkId headScript)
      (Api.mkTxOutValue $ Api.lovelaceToValue 2_000_000)
      headDatumAfter
  headDatumAfter =
    Api.mkTxOutDatum MockHead.Final

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
    Api.fromPlutusScript $ MockHead.validatorScript policyId
  headRedeemer =
    Api.mkRedeemerForTxIn MockHead.Abort

  -- FIXME:
  -- (a) Abort need to reimburse participants that have committed!
  -- (b) There's in principle no need to output any SM output here, it's over.
  headOutput =
    Api.TxOut
      (Api.mkScriptAddress @Api.PlutusScriptV1 networkId headScript)
      (Api.mkTxOutValue $ Api.lovelaceToValue 2_000_000)
      headDatumAfter
  headDatumAfter =
    Api.mkTxOutDatum MockHead.Final

  -- NOTE: Abort datums contain the datum of the spent state-machine input, but
  -- also, the datum of the created output which is necessary for the
  -- state-machine on-chain validator to control the correctness of the
  -- transition.
  mkAbort (Api.fromLedgerTxIn -> initialInput, Api.fromLedgerData -> initialDatum) =
    (initialInput, mkAbortWitness initialDatum)
  mkAbortWitness initialDatum =
    Api.BuildTxWith $ Api.mkScriptWitness initialScript initialDatum initialRedeemer
  initialScript =
    Api.fromPlutusScript MockInitial.validatorScript
  initialRedeemer =
    Api.mkRedeemerForTxIn $ MockInitial.redeemer ()

-- * Observe Hydra Head transactions

-- XXX(SN): We should log decisions why a tx is not an initTx etc. instead of
-- only returning a Maybe, i.e. 'Either Reason (OnChainTx tx, OnChainHeadState)'
observeInitTx :: Party -> ValidatedTx Era -> Maybe (OnChainTx CardanoTx, OnChainHeadState)
observeInitTx party ValidatedTx{wits, body} = do
  (dh, headDatum, MockHead.Initial cp ps) <- getFirst $ foldMap (First . decodeHeadDatum) datumsList
  let parties = map convertParty ps
  let cperiod = contestationPeriodToDiffTime cp
  guard $ party `elem` parties
  (i, o) <- getFirst $ foldMap (First . findSmOutput dh) indexedOutputs
  pure
    ( OnInitTx cperiod parties
    , Initial
        { threadOutput = (i, o, headDatum)
        , initials
        , commits = mempty
        }
    )
 where
  decodeHeadDatum (dh, d) =
    (dh,d,) <$> fromData (getPlutusData d)

  findSmOutput dh (ix, o@(TxOut _ _ dh')) =
    guard (SJust dh == dh') $> (i, o)
   where
    i = TxIn (TxId $ SafeHash.hashAnnotated body) ix

  datumsList = Map.toList datums

  datums = unTxDats $ txdats wits

  indexedOutputs =
    zip [0 ..] (toList (outputs body))

  initials =
    let initialOutputs = filter (isInitial . snd) indexedOutputs
     in mapMaybe mkInitial initialOutputs

  mkInitial (ix, txOut) =
    (mkTxIn ix,txOut,) <$> lookupDatum wits txOut

  mkTxIn ix = TxIn (TxId $ SafeHash.hashAnnotated body) ix

  isInitial (TxOut addr _ _) =
    addr == scriptAddr initialScript

  initialScript = plutusScript MockInitial.validatorScript

convertParty :: OnChain.Party -> Party
convertParty = Party . partyToVerKey

-- | Identify a commit tx by looking for an output which pays to v_commit.
observeCommitTx :: ValidatedTx Era -> Maybe (OnChainTx CardanoTx, (TxIn StandardCrypto, TxOut Era, Data Era))
observeCommitTx tx@ValidatedTx{wits} = do
  (txIn, txOut) <- findScriptOutput (utxoFromTx tx) commitScript
  dat <- lookupDatum wits txOut
  (party, utxo) <- fromData $ getPlutusData dat
  onChainTx <- OnCommitTx (convertParty party) <$> convertUtxo utxo
  pure (onChainTx, (txIn, txOut, dat))
 where
  commitScript = plutusScript MockCommit.validatorScript

  convertUtxo = Aeson.decodeStrict' . OnChain.toByteString

observeCommit ::
  ValidatedTx Era ->
  OnChainHeadState ->
  Maybe (OnChainTx CardanoTx, OnChainHeadState)
observeCommit tx = \case
  Initial{threadOutput, initials, commits} -> do
    (onChainTx, commitTriple) <- observeCommitTx tx
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
  Map (TxIn StandardCrypto) (TxOut Era) ->
  ValidatedTx Era ->
  Maybe (OnChainTx CardanoTx, OnChainHeadState)
observeCollectComTx utxo tx = do
  headInput <- fst <$> findScriptOutput utxo headScript
  redeemer <- getRedeemerSpending tx headInput
  case redeemer of
    MockHead.CollectCom _ -> do
      (newHeadInput, newHeadOutput) <- findScriptOutput (utxoFromTx tx) headScript
      newHeadDatum <- lookupDatum (wits tx) newHeadOutput
      pure
        ( OnCollectComTx
        , OpenOrClosed{threadOutput = (newHeadInput, newHeadOutput, newHeadDatum)}
        )
    _ -> Nothing
 where
  headScript = plutusScript $ MockHead.validatorScript policyId

-- | Identify a close tx by lookup up the input spending the Head output and
-- decoding its redeemer.
observeCloseTx ::
  -- | A Utxo set to lookup tx inputs
  Map (TxIn StandardCrypto) (TxOut Era) ->
  ValidatedTx Era ->
  Maybe (OnChainTx CardanoTx, OnChainHeadState)
observeCloseTx utxo tx = do
  headInput <- fst <$> findScriptOutput utxo headScript
  redeemer <- getRedeemerSpending tx headInput
  case redeemer of
    MockHead.Close{snapshotNumber} -> do
      (newHeadInput, newHeadOutput) <- findScriptOutput (utxoFromTx tx) headScript
      newHeadDatum <- lookupDatum (wits tx) newHeadOutput
      pure
        ( OnCloseTx{contestationDeadline, snapshotNumber = fromIntegral snapshotNumber}
        , OpenOrClosed{threadOutput = (newHeadInput, newHeadOutput, newHeadDatum)}
        )
    _ -> Nothing
 where
  headScript = plutusScript $ MockHead.validatorScript policyId

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
  Map (TxIn StandardCrypto) (TxOut Era) ->
  ValidatedTx Era ->
  Maybe (OnChainTx CardanoTx, OnChainHeadState)
observeFanoutTx utxo tx = do
  headInput <- fst <$> findScriptOutput utxo headScript
  getRedeemerSpending tx headInput >>= \case
    MockHead.Fanout -> pure (OnFanoutTx, Final)
    _ -> Nothing
 where
  headScript = plutusScript $ MockHead.validatorScript policyId

-- | Identify an abort tx by looking up the input spending the Head output and
-- decoding its redeemer.
observeAbortTx ::
  -- | A Utxo set to lookup tx inputs
  Map (TxIn StandardCrypto) (TxOut Era) ->
  ValidatedTx Era ->
  Maybe (OnChainTx CardanoTx, OnChainHeadState)
observeAbortTx utxo tx = do
  headInput <- fst <$> findScriptOutput utxo headScript
  getRedeemerSpending tx headInput >>= \case
    MockHead.Abort -> pure (OnAbortTx, Final)
    _ -> Nothing
 where
  -- FIXME(SN): make sure this is aborting "the right head / your head" by not hard-coding policyId
  headScript = plutusScript $ MockHead.validatorScript policyId

-- * Functions related to OnChainHeadState

-- | Provide a UTXO map for given OnChainHeadState. Used by the TinyWallet and
-- the direct chain component to lookup inputs for balancing / constructing txs.
-- XXX(SN): This is a hint that we might want to track the Utxo directly?
knownUtxo :: OnChainHeadState -> Map (TxIn StandardCrypto) (TxOut Era)
knownUtxo = \case
  Initial{threadOutput, initials, commits} ->
    Map.fromList . map onlyUtxo $ (threadOutput : initials <> commits)
  OpenOrClosed{threadOutput = (i, o, _)} ->
    Map.singleton i o
  _ ->
    mempty
 where
  onlyUtxo (i, o, _) = (i, o)

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

withDataHash :: Data Era -> (DataHash StandardCrypto, Data Era)
withDataHash d = (hashData d, d)

withScriptHash :: Script Era -> (ScriptHash StandardCrypto, Script Era)
withScriptHash s = (hashScript @Era s, s)

datumsFromList :: [Data Era] -> TxDats Era
datumsFromList = TxDats . Map.fromList . fmap withDataHash

-- | Slightly unsafe, as it drops `SNothing` values from the list silently.
redeemersFromList ::
  [(StrictMaybe RdmrPtr, (Data Era, ExUnits))] ->
  Redeemers Era
redeemersFromList =
  Redeemers . Map.fromList . foldl' hasRdmrPtr []
 where
  hasRdmrPtr acc = \case
    (SNothing, _) -> acc
    (SJust v, ex) -> (v, ex) : acc

-- | Lookup included datum of given 'TxOut'.
lookupDatum :: TxWitness Era -> TxOut Era -> Maybe (Data Era)
lookupDatum wits = \case
  (TxOut _ _ (SJust datumHash)) -> Map.lookup datumHash . unTxDats $ txdats wits
  _ -> Nothing

-- | Lookup and decode redeemer which is spending a given 'TxIn'.
getRedeemerSpending :: FromData a => ValidatedTx Era -> TxIn StandardCrypto -> Maybe a
getRedeemerSpending ValidatedTx{body, wits} txIn = do
  idx <- Set.lookupIndex txIn (inputs body)
  (d, _exUnits) <- Map.lookup (RdmrPtr Spend $ fromIntegral idx) redeemers
  fromData $ getPlutusData d
 where
  redeemers = unRedeemers $ txrdmrs wits

findScriptOutput ::
  Map (TxIn StandardCrypto) (TxOut Era) ->
  Script Era ->
  Maybe (TxIn StandardCrypto, TxOut Era)
findScriptOutput utxo script =
  find go $ Map.toList utxo
 where
  go (_, TxOut addr _ _) = addr == scriptAddr script

-- | Get the Utxo set created by given transaction.
-- TODO(SN): DRY with Hydra.Ledger.Cardano.utxoFromTx
utxoFromTx :: ValidatedTx Era -> Map (TxIn StandardCrypto) (TxOut Era)
utxoFromTx ValidatedTx{body} =
  Map.fromList $ zip (map mkTxIn [0 ..]) . toList $ outputs body
 where
  mkTxIn = TxIn (TxId $ SafeHash.hashAnnotated body)
