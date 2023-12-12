{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Smart constructors for creating Hydra protocol transactions to be used in
-- the 'Hydra.Chain.Direct' way of talking to the main-chain.
--
-- This module also encapsulates the transaction format used when talking to the
-- cardano-node, which is currently different from the 'Hydra.Ledger.Cardano',
-- thus we have not yet "reached" 'isomorphism'.
module Hydra.Chain.Direct.Tx where

import Hydra.Cardano.Api
import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Alonzo.Core (AsIxItem (..))
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData (..))
import Cardano.Ledger.Api (
  AlonzoPlutusPurpose (..),
  AsIx (..),
  EraTxAuxData (hashTxAuxData),
  Redeemers (..),
  auxDataHashTxBodyL,
  auxDataTxL,
  bodyTxL,
  inputsTxBodyL,
  mintTxBodyL,
  outputsTxBodyL,
  rdmrsTxWitsL,
  referenceInputsTxBodyL,
  reqSignerHashesTxBodyL,
  unRedeemers,
  witsTxL,
 )
import Cardano.Ledger.Babbage.Core (redeemerPointerInverse)
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Control.Lens ((.~), (<>~), (^.))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Map qualified as Map
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set
import Hydra.Cardano.Api.Network (networkIdToNetwork)
import Hydra.Chain (CommitBlueprintTx (..), HeadParameters (..))
import Hydra.Chain.Direct.ScriptRegistry (ScriptRegistry (..))
import Hydra.Chain.Direct.TimeHandle (PointInTime)
import Hydra.ContestationPeriod (ContestationPeriod, fromChain, toChain)
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokens qualified as HeadTokens
import Hydra.Contract.Initial qualified as Initial
import Hydra.Contract.MintAction (MintAction (Burn, Mint))
import Hydra.Contract.Util (hydraHeadV1)
import Hydra.Crypto (MultiSignature, toPlutusSignatures)
import Hydra.Data.ContestationPeriod (addContestationPeriod)
import Hydra.Data.ContestationPeriod qualified as OnChain
import Hydra.Data.Party qualified as OnChain
import Hydra.HeadId (HeadId (..), HeadSeed (..))
import Hydra.Ledger (IsTx (hashUTxO))
import Hydra.Ledger.Cardano (addReferenceInputs)
import Hydra.Ledger.Cardano.Builder (
  addExtraRequiredSigners,
  addInputs,
  addOutputs,
  addVkInputs,
  burnTokens,
  emptyTxBody,
  mintTokens,
  setValidityLowerBound,
  setValidityUpperBound,
  unsafeBuildTransaction,
 )
import Hydra.OnChainId (OnChainId (..))
import Hydra.Party (Party, partyFromChain, partyToChain)
import Hydra.Plutus.Extras (posixFromUTCTime, posixToUTCTime)
import Hydra.Plutus.Orphans ()
import Hydra.Snapshot (Snapshot (..), SnapshotNumber, fromChainSnapshot)
import PlutusLedgerApi.V2 (CurrencySymbol (CurrencySymbol), fromBuiltin, getPubKeyHash, toBuiltin)
import PlutusLedgerApi.V2 qualified as Plutus
import Test.QuickCheck (vectorOf)

-- | Needed on-chain data to create Head transactions.
type UTxOWithScript = (TxIn, TxOut CtxUTxO, HashableScriptData)

newtype UTxOHash = UTxOHash ByteString
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid)

instance ToJSON UTxOHash where
  toJSON (UTxOHash bytes) =
    Aeson.String . decodeUtf8 $ Base16.encode bytes

instance FromJSON UTxOHash where
  parseJSON = Aeson.withText "UTxOHash" $ \cborText ->
    case Base16.decode $ encodeUtf8 cborText of
      Left e -> fail e
      Right bs -> pure $ UTxOHash bs

instance Arbitrary UTxOHash where
  arbitrary = UTxOHash . BS.pack <$> vectorOf 32 arbitrary

-- | Representation of the Head output after an Init transaction.
data InitialThreadOutput = InitialThreadOutput
  { initialThreadUTxO :: (TxIn, TxOut CtxUTxO)
  , initialContestationPeriod :: OnChain.ContestationPeriod
  , initialParties :: [OnChain.Party]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary InitialThreadOutput where
  arbitrary = genericArbitrary
  shrink = genericShrink

-- | Representation of the Head output after a CollectCom transaction.
data OpenThreadOutput = OpenThreadOutput
  { openThreadUTxO :: (TxIn, TxOut CtxUTxO)
  , openContestationPeriod :: OnChain.ContestationPeriod
  , openParties :: [OnChain.Party]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary OpenThreadOutput where
  arbitrary = genericArbitrary
  shrink = genericShrink

data ClosedThreadOutput = ClosedThreadOutput
  { closedThreadUTxO :: (TxIn, TxOut CtxUTxO)
  , closedParties :: [OnChain.Party]
  , closedContestationDeadline :: Plutus.POSIXTime
  , closedContesters :: [Plutus.PubKeyHash]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary ClosedThreadOutput where
  arbitrary = genericArbitrary
  shrink = genericShrink

hydraHeadV1AssetName :: AssetName
hydraHeadV1AssetName = AssetName (fromBuiltin hydraHeadV1)

-- | The metadata label used for identifying Hydra protocol transactions. As
-- suggested by a friendly large language model: The number most commonly
-- associated with "Hydra" is 5, as in the mythological creature Hydra, which
-- had multiple heads, and the number 5 often symbolizes multiplicity or
-- diversity. However, there is no specific numerical association for Hydra
-- smaller than 10000 beyond this mythological reference.
hydraMetadataLabel :: Word64
hydraMetadataLabel = 55555

-- | Create a transaction metadata entry to identify Hydra transactions (for
-- informational purposes).
mkHydraHeadV1TxName :: Text -> TxMetadata
mkHydraHeadV1TxName name =
  TxMetadata $ Map.fromList [(hydraMetadataLabel, TxMetaText $ "HydraV1/" <> name)]

-- | Get the metadata entry to identify Hydra transactions (for informational
-- purposes).
getHydraHeadV1TxName :: Tx -> Maybe Text
getHydraHeadV1TxName =
  lookupName . txMetadata . getTxBodyContent . getTxBody
 where
  lookupName = \case
    TxMetadataNone -> Nothing
    TxMetadataInEra (TxMetadata m) ->
      case Map.lookup hydraMetadataLabel m of
        Just (TxMetaText name) -> Just name
        _ -> Nothing

-- * Create Hydra Head transactions

-- | Create the init transaction from some 'HeadParameters' and a single TxIn
-- which will be used as unique parameter for minting NFTs.
initTx ::
  NetworkId ->
  -- | Seed input.
  TxIn ->
  -- | Verification key hashes of all participants.
  [OnChainId] ->
  HeadParameters ->
  Tx
initTx networkId seedTxIn participants parameters =
  unsafeBuildTransaction $
    emptyTxBody
      & addVkInputs [seedTxIn]
      & addOutputs
        ( mkHeadOutputInitial networkId seedTxIn parameters
            : map (mkInitialOutput networkId seedTxIn) participants
        )
      & mintTokens (HeadTokens.mkHeadTokenScript seedTxIn) Mint ((hydraHeadV1AssetName, 1) : participationTokens)
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV1TxName "InitTx")
 where
  participationTokens =
    [(onChainIdToAssetName oid, 1) | oid <- participants]

mkHeadOutput :: NetworkId -> PolicyId -> TxOutDatum ctx -> TxOut ctx
mkHeadOutput networkId tokenPolicyId datum =
  TxOut
    (mkScriptAddress @PlutusScriptV2 networkId headScript)
    (valueFromList [(AssetId tokenPolicyId hydraHeadV1AssetName, 1)])
    datum
    ReferenceScriptNone
 where
  headScript = fromPlutusScript Head.validatorScript

mkHeadOutputInitial :: NetworkId -> TxIn -> HeadParameters -> TxOut CtxTx
mkHeadOutputInitial networkId seedTxIn HeadParameters{contestationPeriod, parties} =
  mkHeadOutput networkId tokenPolicyId headDatum
 where
  tokenPolicyId = HeadTokens.headPolicyId seedTxIn
  headDatum =
    mkTxOutDatumInline $
      Head.Initial
        { contestationPeriod = toChain contestationPeriod
        , parties = map partyToChain parties
        , headId = toPlutusCurrencySymbol tokenPolicyId
        , seed = toPlutusTxOutRef seedTxIn
        }

mkInitialOutput :: NetworkId -> TxIn -> OnChainId -> TxOut CtxTx
mkInitialOutput networkId seedTxIn participant =
  TxOut initialAddress initialValue initialDatum ReferenceScriptNone
 where
  tokenPolicyId = HeadTokens.headPolicyId seedTxIn
  initialValue =
    valueFromList [(AssetId tokenPolicyId (onChainIdToAssetName participant), 1)]
  initialAddress =
    mkScriptAddress @PlutusScriptV2 networkId initialScript
  initialScript =
    fromPlutusScript Initial.validatorScript
  initialDatum =
    mkTxOutDatumInline $ Initial.datum (toPlutusCurrencySymbol tokenPolicyId)

-- | Craft a commit transaction which includes the "committed" utxo as a datum.
commitTx ::
  NetworkId ->
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  HeadId ->
  Party ->
  CommitBlueprintTx Tx ->
  -- | The initial output (sent to each party) which should contain the PT and is
  -- locked by initial script
  (TxIn, TxOut CtxUTxO, Hash PaymentKey) ->
  Tx
commitTx networkId scriptRegistry headId party commitBlueprintTx (initialInput, out, vkh) =
  -- NOTE: We use the cardano-ledger-api functions here such that we can use the
  -- blueprint transaction as a starting point (cardano-api does not allow
  -- convenient transaction modifications).
  fromLedgerTx $
    toLedgerTx blueprintTx
      & spendFromInitial
      & bodyTxL . outputsTxBodyL .~ StrictSeq.singleton (toLedgerTxOut commitOutput)
      & bodyTxL . reqSignerHashesTxBodyL <>~ Set.singleton (toLedgerKeyHash vkh)
      & bodyTxL . mintTxBodyL .~ mempty
      & addMetadata (mkHydraHeadV1TxName "CommitTx")
 where
  addMetadata (TxMetadata newMetadata) tx =
    let
      newMetadataMap = toShelleyMetadata newMetadata
      newAuxData =
        case toLedgerTx blueprintTx ^. auxDataTxL of
          SNothing -> AlonzoTxAuxData newMetadataMap mempty mempty
          SJust (AlonzoTxAuxData metadata timeLocks languageMap) ->
            AlonzoTxAuxData (Map.union metadata newMetadataMap) timeLocks languageMap
     in
      tx
        & auxDataTxL .~ SJust newAuxData
        & bodyTxL . auxDataHashTxBodyL .~ SJust (hashTxAuxData newAuxData)

  spendFromInitial tx =
    let newRedeemers =
          resolveSpendingRedeemers tx
            & Map.insert (toLedgerTxIn initialInput) (toLedgerData @LedgerEra initialRedeemer)
        newInputs = tx ^. bodyTxL . inputsTxBodyL <> Set.singleton (toLedgerTxIn initialInput)
     in tx
          & bodyTxL . inputsTxBodyL .~ newInputs
          & bodyTxL . referenceInputsTxBodyL <>~ Set.singleton (toLedgerTxIn initialScriptRef)
          & witsTxL . rdmrsTxWitsL .~ mkRedeemers newRedeemers newInputs

  -- Make redeemers (with zeroed units) from a TxIn -> Data map and a set of transaction inputs
  mkRedeemers resolved inputs =
    Redeemers . Map.fromList $
      foldl'
        ( \newRedeemerData txin ->
            let ix = fromIntegral $ Set.findIndex txin inputs
             in case Map.lookup txin resolved of
                  Nothing -> newRedeemerData
                  Just d ->
                    (AlonzoSpending (AsIx ix), (d, ExUnits 0 0)) : newRedeemerData
        )
        []
        inputs

  -- Create a TxIn -> Data map of all spending redeemers
  resolveSpendingRedeemers tx =
    Map.foldMapWithKey
      ( \p (d, _ex) ->
          -- XXX: Should soon be available through cardano-ledger-api again
          case redeemerPointerInverse (tx ^. bodyTxL) p of
            SJust (AlonzoSpending (AsIxItem _ txIn)) -> Map.singleton txIn d
            _ -> mempty
      )
      (unRedeemers $ tx ^. witsTxL . rdmrsTxWitsL)

  initialScriptRef =
    fst (initialReference scriptRegistry)

  initialRedeemer =
    toScriptData . Initial.redeemer $
      Initial.ViaCommit (toPlutusTxOutRef <$> committedTxIns)

  committedTxIns = txIns' blueprintTx

  commitOutput =
    TxOut commitAddress commitValue commitDatum ReferenceScriptNone

  commitScript =
    fromPlutusScript Commit.validatorScript

  commitAddress =
    mkScriptAddress @PlutusScriptV2 networkId commitScript

  utxoToCommit =
    UTxO.fromPairs $ mapMaybe (\txin -> (txin,) <$> UTxO.resolve txin lookupUTxO) committedTxIns

  commitValue =
    txOutValue out <> foldMap txOutValue utxoToCommit

  commitDatum =
    mkTxOutDatumInline $ mkCommitDatum party utxoToCommit (headIdToCurrencySymbol headId)

  CommitBlueprintTx{lookupUTxO, blueprintTx} = commitBlueprintTx

mkCommitDatum :: Party -> UTxO -> CurrencySymbol -> Plutus.Datum
mkCommitDatum party utxo headId =
  Commit.datum (partyToChain party, commits, headId)
 where
  commits =
    mapMaybe Commit.serializeCommit $ UTxO.pairs utxo

-- | Create a transaction collecting all "committed" utxo and opening a Head,
-- i.e. driving the Head script state.
collectComTx ::
  NetworkId ->
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | Party who's authorizing this transaction
  VerificationKey PaymentKey ->
  -- | Head identifier
  HeadId ->
  -- | Parameters of the head to collect .
  HeadParameters ->
  -- | Everything needed to spend the Head state-machine output.
  (TxIn, TxOut CtxUTxO) ->
  -- | Data needed to spend the commit output produced by each party.
  -- Should contain the PT and is locked by @ν_commit@ script.
  Map TxIn (TxOut CtxUTxO) ->
  -- | UTxO to be used to collect.
  -- Should match whatever is recorded in the commit inputs.
  UTxO ->
  Tx
collectComTx networkId scriptRegistry vk headId headParameters (headInput, initialHeadOutput) commits utxoToCollect =
  unsafeBuildTransaction $
    emptyTxBody
      & addInputs ((headInput, headWitness) : (mkCommit <$> Map.keys commits))
      & addReferenceInputs [commitScriptRef, headScriptRef]
      & addOutputs [headOutput]
      & addExtraRequiredSigners [verificationKeyHash vk]
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV1TxName "CollectComTx")
 where
  HeadParameters{parties, contestationPeriod} = headParameters

  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef headScript InlineScriptDatum headRedeemer
  headScript = fromPlutusScript @PlutusScriptV2 Head.validatorScript
  headScriptRef = fst (headReference scriptRegistry)
  headRedeemer = toScriptData Head.CollectCom
  headOutput =
    TxOut
      (mkScriptAddress @PlutusScriptV2 networkId headScript)
      (txOutValue initialHeadOutput <> commitValue)
      headDatumAfter
      ReferenceScriptNone
  headDatumAfter =
    mkTxOutDatumInline
      Head.Open
        { Head.parties = partyToChain <$> parties
        , utxoHash
        , contestationPeriod = toChain contestationPeriod
        , headId = headIdToCurrencySymbol headId
        }

  utxoHash = toBuiltin $ hashUTxO @Tx utxoToCollect

  mkCommit commitTxIn = (commitTxIn, commitWitness)
  commitWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference commitScriptRef commitScript InlineScriptDatum commitRedeemer
  commitScriptRef =
    fst (commitReference scriptRegistry)
  commitValue =
    mconcat $ txOutValue <$> Map.elems commits
  commitScript =
    fromPlutusScript @PlutusScriptV2 Commit.validatorScript
  commitRedeemer =
    toScriptData $ Commit.redeemer Commit.ViaCollectCom

-- | Construct a _decrement_ transaction which takes as input some 'UTxO' present
-- in the L2 ledger state and makes it available on L1.
decrementTx ::
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | Party who's authorizing this transaction
  VerificationKey PaymentKey ->
  -- | Head identifier
  HeadId ->
  -- | Parameters of the head.
  HeadParameters ->
  -- | Everything needed to spend the Head state-machine output.
  (TxIn, TxOut CtxUTxO) ->
  -- | Confirmed Snapshot
  Snapshot Tx ->
  MultiSignature (Snapshot Tx) ->
  Tx
decrementTx scriptRegistry vk headId headParameters (headInput, headOutput) snapshot signatures =
  unsafeBuildTransaction $
    emptyTxBody
      & addInputs [(headInput, headWitness)]
      & addReferenceInputs [headScriptRef]
      -- NOTE: at this point 'utxoToDecommit' is populated
      & addOutputs (headOutput' : map toTxContext (maybe [] (fmap snd . UTxO.pairs) utxoToDecommit))
      & addExtraRequiredSigners [verificationKeyHash vk]
 where
  headRedeemer = toScriptData $ Head.Decrement (toPlutusSignatures signatures)
  utxoHash = toBuiltin $ hashUTxO @Tx utxo

  HeadParameters{parties, contestationPeriod} = headParameters

  headOutput' =
    modifyTxOutDatum (const headDatumAfter) headOutput

  headScript = fromPlutusScript @PlutusScriptV2 Head.validatorScript

  headScriptRef = fst (headReference scriptRegistry)

  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef headScript InlineScriptDatum headRedeemer

  headDatumAfter =
    mkTxOutDatumInline
      Head.Open
        { Head.parties = partyToChain <$> parties
        , utxoHash
        , contestationPeriod = toChain contestationPeriod
        , headId = headIdToCurrencySymbol headId
        }
  Snapshot{utxo, utxoToDecommit} = snapshot

-- | Low-level data type of a snapshot to close the head with. This is different
-- to the 'ConfirmedSnasphot', which is provided to `CloseTx` as it also
-- contains relevant chain state like the 'openUtxoHash'.
data ClosingSnapshot
  = CloseWithInitialSnapshot {openUtxoHash :: UTxOHash}
  | CloseWithConfirmedSnapshot
      { snapshotNumber :: SnapshotNumber
      , closeUtxoHash :: UTxOHash
      , closeUtxoToDecommitHash :: UTxOHash
      , -- XXX: This is a bit of a wart and stems from the fact that our
        -- SignableRepresentation of 'Snapshot' is in fact the snapshotNumber
        -- and the closeUtxoHash as also included above
        signatures :: MultiSignature (Snapshot Tx)
      }

data CloseTxError
  = InvalidHeadIdInClose {headId :: HeadId}
  | CannotFindHeadOutputToClose
  deriving stock (Show)

-- | Create a transaction closing a head with either the initial snapshot or
-- with a multi-signed confirmed snapshot.
closeTx ::
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | Party who's authorizing this transaction
  VerificationKey PaymentKey ->
  -- | The snapshot to close with, can be either initial or confirmed one.
  ClosingSnapshot ->
  -- | Lower validity slot number, usually a current or quite recent slot number.
  SlotNo ->
  -- | Upper validity slot and UTC time to compute the contestation deadline time.
  PointInTime ->
  -- | Everything needed to spend the Head state-machine output.
  OpenThreadOutput ->
  -- | Head identifier
  HeadId ->
  Tx
closeTx scriptRegistry vk closing startSlotNo (endSlotNo, utcTime) openThreadOutput headId =
  unsafeBuildTransaction $
    emptyTxBody
      & addInputs [(headInput, headWitness)]
      & addReferenceInputs [headScriptRef]
      & addOutputs [headOutputAfter]
      & addExtraRequiredSigners [verificationKeyHash vk]
      & setValidityLowerBound startSlotNo
      & setValidityUpperBound endSlotNo
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV1TxName "CloseTx")
 where
  OpenThreadOutput
    { openThreadUTxO = (headInput, headOutputBefore)
    , openContestationPeriod
    , openParties
    } = openThreadOutput

  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef headScript InlineScriptDatum headRedeemer

  headScriptRef =
    fst (headReference scriptRegistry)

  headScript =
    fromPlutusScript @PlutusScriptV2 Head.validatorScript

  headRedeemer =
    toScriptData
      Head.Close
        { signature
        }

  headOutputAfter =
    modifyTxOutDatum (const headDatumAfter) headOutputBefore

  headDatumAfter =
    mkTxOutDatumInline
      Head.Closed
        { snapshotNumber
        , utxoHash = toBuiltin utxoHashBytes
        , -- TODO: find a way to introduce this value
          utxoToDecommitHash = toBuiltin decommitUTxOHashBytes
        , parties = openParties
        , contestationDeadline
        , contestationPeriod = openContestationPeriod
        , headId = headIdToCurrencySymbol headId
        , contesters = []
        }

  snapshotNumber = toInteger $ case closing of
    CloseWithInitialSnapshot{} -> 0
    CloseWithConfirmedSnapshot{snapshotNumber = sn} -> sn

  (UTxOHash utxoHashBytes, UTxOHash decommitUTxOHashBytes) = case closing of
    CloseWithInitialSnapshot{openUtxoHash} -> (openUtxoHash, mempty)
    CloseWithConfirmedSnapshot{closeUtxoHash, closeUtxoToDecommitHash} -> (closeUtxoHash, closeUtxoToDecommitHash)

  signature = case closing of
    CloseWithInitialSnapshot{} -> mempty
    CloseWithConfirmedSnapshot{signatures = s} -> toPlutusSignatures s

  contestationDeadline =
    addContestationPeriod (posixFromUTCTime utcTime) openContestationPeriod

data ContestTxError
  = InvalidHeadIdInContest {headId :: HeadId}
  | CannotFindHeadOutputToContest
  | MissingHeadDatumInContest
  | MissingHeadRedeemerInContest
  | WrongDatumInContest
  | FailedToConvertFromScriptDataInContest
  deriving stock (Show)

-- XXX: This function is VERY similar to the 'closeTx' function (only notable
-- difference being the redeemer, which is in itself also the same structure as
-- the close's one. We could potentially refactor this to avoid repetition or do
-- something more principled at the protocol level itself and "merge" close and
-- contest as one operation.
contestTx ::
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | Party who's authorizing this transaction
  VerificationKey PaymentKey ->
  -- | Contested snapshot number (i.e. the one we contest to)
  Snapshot Tx ->
  -- | Multi-signature of the whole snapshot
  MultiSignature (Snapshot Tx) ->
  -- | Current slot and posix time to be used as the contestation time.
  PointInTime ->
  -- | Everything needed to spend the Head state-machine output.
  ClosedThreadOutput ->
  HeadId ->
  ContestationPeriod ->
  Tx
contestTx scriptRegistry vk Snapshot{number, utxo, utxoToDecommit} sig (slotNo, _) closedThreadOutput headId contestationPeriod =
  unsafeBuildTransaction $
    emptyTxBody
      & addInputs [(headInput, headWitness)]
      & addReferenceInputs [headScriptRef]
      & addOutputs [headOutputAfter]
      & addExtraRequiredSigners [verificationKeyHash vk]
      & setValidityUpperBound slotNo
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV1TxName "ContestTx")
 where
  ClosedThreadOutput
    { closedThreadUTxO = (headInput, headOutputBefore)
    , closedParties
    , closedContestationDeadline
    , closedContesters
    } = closedThreadOutput

  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef headScript InlineScriptDatum headRedeemer
  headScriptRef =
    fst (headReference scriptRegistry)
  headScript =
    fromPlutusScript @PlutusScriptV2 Head.validatorScript
  headRedeemer =
    toScriptData
      Head.Contest
        { signature = toPlutusSignatures sig
        }
  headOutputAfter =
    modifyTxOutDatum (const headDatumAfter) headOutputBefore

  contester = toPlutusKeyHash (verificationKeyHash vk)

  onChainConstestationPeriod = toChain contestationPeriod

  newContestationDeadline =
    if length (contester : closedContesters) == length closedParties
      then closedContestationDeadline
      else addContestationPeriod closedContestationDeadline onChainConstestationPeriod

  headDatumAfter =
    mkTxOutDatumInline
      Head.Closed
        { snapshotNumber = toInteger number
        , utxoHash
        , utxoToDecommitHash
        , parties = closedParties
        , contestationDeadline = newContestationDeadline
        , contestationPeriod = onChainConstestationPeriod
        , headId = headIdToCurrencySymbol headId
        , contesters = contester : closedContesters
        }
  utxoHash = toBuiltin $ hashUTxO @Tx utxo

  utxoToDecommitHash = toBuiltin $ hashUTxO @Tx (fromMaybe mempty utxoToDecommit)

data FanoutTxError
  = CannotFindHeadOutputToFanout
  | MissingHeadDatumInFanout
  | WrongDatumInFanout
  | FailedToConvertFromScriptDataInFanout
  deriving stock (Show)

-- | Create the fanout transaction, which distributes the closed state
-- accordingly. The head validator allows fanout only > deadline, so we need
-- to set the lower bound to be deadline + 1 slot.
fanoutTx ::
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | Snapshotted UTxO to fanout on layer 1
  UTxO ->
  -- | Everything needed to spend the Head state-machine output.
  (TxIn, TxOut CtxUTxO) ->
  -- | Contestation deadline as SlotNo, used to set lower tx validity bound.
  SlotNo ->
  -- | Minting Policy script, made from initial seed
  PlutusScript ->
  Tx
fanoutTx scriptRegistry utxo (headInput, headOutput) deadlineSlotNo headTokenScript =
  unsafeBuildTransaction $
    emptyTxBody
      & addInputs [(headInput, headWitness)]
      & addReferenceInputs [headScriptRef]
      & addOutputs orderedTxOutsToFanout
      & burnTokens headTokenScript Burn headTokens
      & setValidityLowerBound (deadlineSlotNo + 1)
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV1TxName "FanoutTx")
 where
  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef headScript InlineScriptDatum headRedeemer
  headScriptRef =
    fst (headReference scriptRegistry)
  headScript =
    fromPlutusScript @PlutusScriptV2 Head.validatorScript
  headRedeemer =
    toScriptData (Head.Fanout $ fromIntegral $ length utxo)

  headTokens =
    headTokensFromValue headTokenScript (txOutValue headOutput)

  orderedTxOutsToFanout =
    toTxContext <$> toList utxo

data AbortTxError
  = OverlappingInputs
  | CannotFindHeadOutputToAbort
  deriving stock (Show)

-- | Create transaction which aborts a head by spending the Head output and all
-- other "initial" outputs.
abortTx ::
  -- | Committed UTxOs to reimburse.
  UTxO ->
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | Party who's authorizing this transaction
  VerificationKey PaymentKey ->
  -- | Everything needed to spend the Head state-machine output.
  (TxIn, TxOut CtxUTxO) ->
  -- | Script for monetary policy to burn tokens
  PlutusScript ->
  -- | Data needed to spend the initial output sent to each party to the Head.
  -- Should contain the PT and is locked by initial script.
  Map TxIn (TxOut CtxUTxO) ->
  -- | Data needed to spend commit outputs.
  -- Should contain the PT and is locked by commit script.
  Map TxIn (TxOut CtxUTxO) ->
  Either AbortTxError Tx
abortTx committedUTxO scriptRegistry vk (headInput, initialHeadOutput) headTokenScript initialsToAbort commitsToAbort
  | isJust (lookup headInput initialsToAbort) =
      Left OverlappingInputs
  | otherwise =
      Right $
        unsafeBuildTransaction $
          emptyTxBody
            & addInputs ((headInput, headWitness) : initialInputs <> commitInputs)
            & addReferenceInputs [initialScriptRef, commitScriptRef, headScriptRef]
            & addOutputs reimbursedOutputs
            & burnTokens headTokenScript Burn headTokens
            & addExtraRequiredSigners [verificationKeyHash vk]
 where
  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef headScript InlineScriptDatum headRedeemer
  headScriptRef =
    fst (headReference scriptRegistry)
  headScript =
    fromPlutusScript @PlutusScriptV2 Head.validatorScript
  headRedeemer =
    toScriptData Head.Abort

  initialInputs = mkAbortInitial <$> Map.keys initialsToAbort

  commitInputs = mkAbortCommit <$> Map.keys commitsToAbort

  headTokens =
    headTokensFromValue headTokenScript $
      mconcat
        [ txOutValue initialHeadOutput
        , foldMap txOutValue initialsToAbort
        , foldMap txOutValue commitsToAbort
        ]

  mkAbortInitial initialInput = (initialInput, abortInitialWitness)
  abortInitialWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference initialScriptRef initialScript InlineScriptDatum initialRedeemer
  initialScriptRef =
    fst (initialReference scriptRegistry)
  initialScript =
    fromPlutusScript @PlutusScriptV2 Initial.validatorScript
  initialRedeemer =
    toScriptData $ Initial.redeemer Initial.ViaAbort

  mkAbortCommit commitInput = (commitInput, abortCommitWitness)
  abortCommitWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference commitScriptRef commitScript InlineScriptDatum commitRedeemer
  commitScriptRef =
    fst (commitReference scriptRegistry)
  commitScript =
    fromPlutusScript @PlutusScriptV2 Commit.validatorScript
  commitRedeemer =
    toScriptData (Commit.redeemer Commit.ViaAbort)

  reimbursedOutputs = toTxContext . snd <$> UTxO.pairs committedUTxO

-- * Observe Hydra Head transactions

-- | Generalised type for arbitrary Head observations on-chain.
data HeadObservation
  = NoHeadTx
  | Init InitObservation
  | Abort AbortObservation
  | Commit CommitObservation
  | CollectCom CollectComObservation
  | Decrement DecrementObservation
  | Close CloseObservation
  | Contest ContestObservation
  | Fanout FanoutObservation
  deriving stock (Eq, Show, Generic)

instance Arbitrary HeadObservation where
  arbitrary = genericArbitrary

-- | Observe any Hydra head transaction.
observeHeadTx :: NetworkId -> UTxO -> Tx -> HeadObservation
observeHeadTx networkId utxo tx =
  fromMaybe NoHeadTx $
    either (const Nothing) (Just . Init) (observeInitTx tx)
      <|> Abort <$> observeAbortTx utxo tx
      <|> Commit <$> observeCommitTx networkId utxo tx
      <|> CollectCom <$> observeCollectComTx utxo tx
      <|> Decrement <$> observeDecrementTx utxo tx
      <|> Close <$> observeCloseTx utxo tx
      <|> Contest <$> observeContestTx utxo tx
      <|> Fanout <$> observeFanoutTx utxo tx

-- | Data which can be observed from an `initTx`.
data InitObservation = InitObservation
  { initialThreadUTxO :: (TxIn, TxOut CtxUTxO)
  , initials :: [(TxIn, TxOut CtxUTxO)]
  , headId :: HeadId
  , -- XXX: This is cardano-specific, while headId, parties and
    -- contestationPeriod are already generic here. Check which is more
    -- convenient and consistent!
    seedTxIn :: TxIn
  , contestationPeriod :: ContestationPeriod
  , parties :: [Party]
  , -- XXX: Improve naming
    participants :: [OnChainId]
  }
  deriving stock (Show, Eq, Generic)

instance Arbitrary InitObservation where
  arbitrary = genericArbitrary

data NotAnInitReason
  = NoHeadOutput
  | NotAHeadDatum
  | NoSTFound
  | NotAHeadPolicy
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary NotAnInitReason where
  arbitrary = genericArbitrary

-- | Identify a init tx by checking the output value for holding tokens that are
-- valid head tokens (checked by seed + policy).
observeInitTx ::
  Tx ->
  Either NotAnInitReason InitObservation
observeInitTx tx = do
  -- XXX: Lots of redundant information here
  (ix, headOut, headState) <-
    maybeLeft NoHeadOutput $
      findFirst matchHeadOutput indexedOutputs

  -- check that we have a proper head
  (pid, contestationPeriod, onChainParties, seedTxIn) <- case headState of
    (Head.Initial cp ps cid outRef) -> do
      pid <- fromPlutusCurrencySymbol cid ?> NotAHeadPolicy
      pure (pid, fromChain cp, ps, fromPlutusTxOutRef outRef)
    _ -> Left NotAHeadDatum

  let stQuantity = selectAsset (txOutValue headOut) (AssetId pid hydraHeadV1AssetName)

  -- check that ST is present in the head output
  unless (stQuantity == 1) $
    Left NoSTFound

  -- check that we are using the same seed and headId matches
  unless (pid == HeadTokens.headPolicyId seedTxIn) $
    Left NotAHeadPolicy

  pure $
    InitObservation
      { headId = mkHeadId pid
      , seedTxIn
      , initialThreadUTxO = (mkTxIn tx ix, toCtxUTxOTxOut headOut)
      , initials
      , contestationPeriod
      , parties = mapMaybe partyFromChain onChainParties
      , participants = assetNameToOnChainId <$> mintedTokenNames pid
      }
 where
  maybeLeft e = maybe (Left e) Right

  matchHeadOutput (ix, out) = do
    guard $ isScriptTxOut headScript out
    (ix,out,) <$> (fromScriptData =<< txOutScriptData out)

  headScript = fromPlutusScript @PlutusScriptV2 Head.validatorScript

  indexedOutputs = zip [0 ..] (txOuts' tx)

  initialOutputs = filter (isInitial . snd) indexedOutputs

  initials =
    map
      (bimap (mkTxIn tx) toCtxUTxOTxOut)
      initialOutputs

  isInitial = isScriptTxOut initialScript

  initialScript = fromPlutusScript @PlutusScriptV2 Initial.validatorScript

  mintedTokenNames pid =
    [ assetName
    | (AssetId policyId assetName, q) <- txMintAssets tx
    , q == 1 -- NOTE: Only consider unique tokens
    , policyId == pid
    , assetName /= hydraHeadV1AssetName
    ]

-- | Full observation of a commit transaction.
data CommitObservation = CommitObservation
  { commitOutput :: (TxIn, TxOut CtxUTxO)
  , party :: Party
  -- ^ Hydra participant who committed the UTxO.
  , committed :: UTxO
  , headId :: HeadId
  }
  deriving stock (Eq, Show, Generic)

instance Arbitrary CommitObservation where
  arbitrary = genericArbitrary

-- | Identify a commit tx by:
--
-- - Check that its spending from the init validator,
-- - Find the outputs which pays to the commit validator,
-- - Using the datum of that output, deserialize the committed output,
-- - Reconstruct the committed UTxO from both values (tx input and output).
observeCommitTx ::
  NetworkId ->
  -- | A UTxO set to lookup tx inputs. Should at least contain the input
  -- spending from νInitial.
  UTxO ->
  Tx ->
  Maybe CommitObservation
observeCommitTx networkId utxo tx = do
  -- NOTE: Instead checking to spend from initial we could be looking at the
  -- seed:
  --
  --  - We must check that participation token in output satisfies
  --      policyId = hash(mu_head(seed))
  --
  --  - This allows us to assume (by induction) the output datum at the commit
  --    script is legit
  --
  --  - Further, we need to assert / assume that only one script is spent = onle
  --    one redeemer matches the InitialRedeemer, as we do not have information
  --    which of the inputs is spending from the initial script otherwise.
  --
  --  Right now we only have the headId in the datum, so we use that in place of
  --  the seed -> THIS CAN NOT BE TRUSTED.
  guard isSpendingFromInitial

  (commitIn, commitOut) <- findTxOutByAddress commitAddress tx
  dat <- txOutScriptData commitOut
  (onChainParty, onChainCommits, headId) :: Commit.DatumType <- fromScriptData dat
  party <- partyFromChain onChainParty

  -- NOTE: If we have the resolved inputs (utxo) then we could avoid putting
  -- the commit into the datum (+ changing the hashing strategy of
  -- collect/fanout)
  committed <- do
    committedUTxO <- traverse (Commit.deserializeCommit (networkIdToNetwork networkId)) onChainCommits
    pure . UTxO.fromPairs $ committedUTxO

  policyId <- fromPlutusCurrencySymbol headId
  pure
    CommitObservation
      { commitOutput = (commitIn, toUTxOContext commitOut)
      , party
      , committed
      , headId = mkHeadId policyId
      }
 where
  isSpendingFromInitial :: Bool
  isSpendingFromInitial =
    any (\o -> txOutAddress o == initialAddress) (resolveInputsUTxO utxo tx)

  initialAddress = mkScriptAddress @PlutusScriptV2 networkId initialScript

  initialScript = fromPlutusScript Initial.validatorScript

  commitAddress = mkScriptAddress @PlutusScriptV2 networkId commitScript

  commitScript = fromPlutusScript Commit.validatorScript

data CollectComObservation = CollectComObservation
  { threadOutput :: OpenThreadOutput
  , headId :: HeadId
  , utxoHash :: UTxOHash
  }
  deriving stock (Show, Eq, Generic)

instance Arbitrary CollectComObservation where
  arbitrary = genericArbitrary

-- | Identify a collectCom tx by lookup up the input spending the Head output
-- and decoding its redeemer.
observeCollectComTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe CollectComObservation
observeCollectComTx utxo tx = do
  let inputUTxO = resolveInputsUTxO utxo tx
  (headInput, headOutput) <- findTxOutByScript @PlutusScriptV2 inputUTxO headScript
  redeemer <- findRedeemerSpending tx headInput
  oldHeadDatum <- txOutScriptData $ toTxContext headOutput
  datum <- fromScriptData oldHeadDatum
  headId <- findStateToken headOutput
  case (datum, redeemer) of
    (Head.Initial{parties, contestationPeriod}, Head.CollectCom) -> do
      (newHeadInput, newHeadOutput) <- findTxOutByScript @PlutusScriptV2 (utxoFromTx tx) headScript
      newHeadDatum <- txOutScriptData $ toTxContext newHeadOutput
      utxoHash <- UTxOHash <$> decodeUtxoHash newHeadDatum
      pure
        CollectComObservation
          { threadOutput =
              OpenThreadOutput
                { openThreadUTxO = (newHeadInput, newHeadOutput)
                , openParties = parties
                , openContestationPeriod = contestationPeriod
                }
          , headId
          , utxoHash
          }
    _ -> Nothing
 where
  headScript = fromPlutusScript Head.validatorScript
  decodeUtxoHash datum =
    case fromScriptData datum of
      Just Head.Open{utxoHash} -> Just $ fromBuiltin utxoHash
      _ -> Nothing

newtype DecrementObservation = DecrementObservation
  { headId :: HeadId
  }
  deriving stock (Show, Eq, Generic)

instance Arbitrary DecrementObservation where
  arbitrary = genericArbitrary

observeDecrementTx ::
  UTxO ->
  Tx ->
  Maybe DecrementObservation
observeDecrementTx utxo tx = do
  let inputUTxO = resolveInputsUTxO utxo tx
  (headInput, headOutput) <- findTxOutByScript @PlutusScriptV2 inputUTxO headScript
  redeemer <- findRedeemerSpending tx headInput
  oldHeadDatum <- txOutScriptData $ toTxContext headOutput
  datum <- fromScriptData oldHeadDatum
  headId <- findStateToken headOutput
  case (datum, redeemer) of
    (Head.Open{}, Head.Decrement{}) -> do
      (_, newHeadOutput) <- findTxOutByScript @PlutusScriptV2 (utxoFromTx tx) headScript
      newHeadDatum <- txOutScriptData $ toTxContext newHeadOutput
      case fromScriptData newHeadDatum of
        Just Head.Open{} ->
          pure
            DecrementObservation
              { headId
              }
        _ -> Nothing
    _ -> Nothing
 where
  headScript = fromPlutusScript Head.validatorScript

data CloseObservation = CloseObservation
  { threadOutput :: ClosedThreadOutput
  , headId :: HeadId
  , snapshotNumber :: SnapshotNumber
  }
  deriving stock (Show, Eq, Generic)

instance Arbitrary CloseObservation where
  arbitrary = genericArbitrary

-- | Identify a close tx by lookup up the input spending the Head output and
-- decoding its redeemer.
observeCloseTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe CloseObservation
observeCloseTx utxo tx = do
  let inputUTxO = resolveInputsUTxO utxo tx
  (headInput, headOutput) <- findTxOutByScript @PlutusScriptV2 inputUTxO headScript
  redeemer <- findRedeemerSpending tx headInput
  oldHeadDatum <- txOutScriptData $ toTxContext headOutput
  datum <- fromScriptData oldHeadDatum
  headId <- findStateToken headOutput
  case (datum, redeemer) of
    (Head.Open{parties}, Head.Close{}) -> do
      (newHeadInput, newHeadOutput) <- findTxOutByScript @PlutusScriptV2 (utxoFromTx tx) headScript
      newHeadDatum <- txOutScriptData $ toTxContext newHeadOutput
      (closeContestationDeadline, onChainSnapshotNumber) <- case fromScriptData newHeadDatum of
        Just Head.Closed{contestationDeadline, snapshotNumber} -> pure (contestationDeadline, snapshotNumber)
        _ -> Nothing
      pure
        CloseObservation
          { threadOutput =
              ClosedThreadOutput
                { closedThreadUTxO = (newHeadInput, newHeadOutput)
                , closedParties = parties
                , closedContestationDeadline = closeContestationDeadline
                , closedContesters = []
                }
          , headId
          , snapshotNumber = fromChainSnapshot onChainSnapshotNumber
          }
    _ -> Nothing
 where
  headScript = fromPlutusScript Head.validatorScript

data ContestObservation = ContestObservation
  { contestedThreadOutput :: (TxIn, TxOut CtxUTxO)
  , headId :: HeadId
  , snapshotNumber :: SnapshotNumber
  , contestationDeadline :: UTCTime
  , contesters :: [Plutus.PubKeyHash]
  }
  deriving stock (Show, Eq, Generic)

instance Arbitrary ContestObservation where
  arbitrary = genericArbitrary

-- | Identify a close tx by lookup up the input spending the Head output and
-- decoding its redeemer.
observeContestTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe ContestObservation
observeContestTx utxo tx = do
  let inputUTxO = resolveInputsUTxO utxo tx
  (headInput, headOutput) <- findTxOutByScript @PlutusScriptV2 inputUTxO headScript
  redeemer <- findRedeemerSpending tx headInput
  oldHeadDatum <- txOutScriptData $ toTxContext headOutput
  datum <- fromScriptData oldHeadDatum
  headId <- findStateToken headOutput
  case (datum, redeemer) of
    (Head.Closed{}, Head.Contest{}) -> do
      (newHeadInput, newHeadOutput) <- findTxOutByScript @PlutusScriptV2 (utxoFromTx tx) headScript
      newHeadDatum <- txOutScriptData $ toTxContext newHeadOutput
      let (onChainSnapshotNumber, contestationDeadline, contesters) = decodeDatum newHeadDatum
      pure
        ContestObservation
          { contestedThreadOutput = (newHeadInput, newHeadOutput)
          , headId
          , snapshotNumber = fromChainSnapshot onChainSnapshotNumber
          , contestationDeadline = posixToUTCTime contestationDeadline
          , contesters
          }
    _ -> Nothing
 where
  headScript = fromPlutusScript Head.validatorScript

  decodeDatum headDatum =
    case fromScriptData headDatum of
      Just Head.Closed{snapshotNumber, contestationDeadline, contesters} -> (snapshotNumber, contestationDeadline, contesters)
      _ -> error "wrong state in output datum"

newtype FanoutObservation = FanoutObservation {headId :: HeadId} deriving stock (Eq, Show, Generic)

instance Arbitrary FanoutObservation where
  arbitrary = genericArbitrary

-- | Identify a fanout tx by lookup up the input spending the Head output and
-- decoding its redeemer.
observeFanoutTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe FanoutObservation
observeFanoutTx utxo tx = do
  let inputUTxO = resolveInputsUTxO utxo tx
  (headInput, headOutput) <- findTxOutByScript @PlutusScriptV2 inputUTxO headScript
  headId <- findStateToken headOutput
  findRedeemerSpending tx headInput
    >>= \case
      Head.Fanout{} -> pure FanoutObservation{headId}
      _ -> Nothing
 where
  headScript = fromPlutusScript Head.validatorScript

newtype AbortObservation = AbortObservation {headId :: HeadId} deriving stock (Eq, Show, Generic)

instance Arbitrary AbortObservation where
  arbitrary = genericArbitrary

-- | Identify an abort tx by looking up the input spending the Head output and
-- decoding its redeemer.
observeAbortTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe AbortObservation
observeAbortTx utxo tx = do
  let inputUTxO = resolveInputsUTxO utxo tx
  (headInput, headOutput) <- findTxOutByScript @PlutusScriptV2 inputUTxO headScript
  headId <- findStateToken headOutput
  findRedeemerSpending tx headInput >>= \case
    Head.Abort -> pure $ AbortObservation headId
    _ -> Nothing
 where
  headScript = fromPlutusScript Head.validatorScript

-- * Cardano specific identifiers

mkHeadId :: PolicyId -> HeadId
mkHeadId = UnsafeHeadId . serialiseToRawBytes

headIdToCurrencySymbol :: HeadId -> CurrencySymbol
headIdToCurrencySymbol (UnsafeHeadId headId) = CurrencySymbol (toBuiltin headId)

currencySymbolToHeadId :: MonadFail m => CurrencySymbol -> m HeadId
currencySymbolToHeadId = fmap mkHeadId . fromPlutusCurrencySymbol

headIdToPolicyId :: MonadFail m => HeadId -> m PolicyId
headIdToPolicyId = fromPlutusCurrencySymbol . headIdToCurrencySymbol

headSeedToTxIn :: MonadFail m => HeadSeed -> m TxIn
headSeedToTxIn (UnsafeHeadSeed bytes) =
  case Aeson.decodeStrict bytes of
    Nothing -> fail $ "Failed to decode HeadSeed " <> show bytes
    Just txIn -> pure txIn

txInToHeadSeed :: TxIn -> HeadSeed
txInToHeadSeed txin = UnsafeHeadSeed $ toStrict $ Aeson.encode txin

assetNameToOnChainId :: AssetName -> OnChainId
assetNameToOnChainId (AssetName bs) = UnsafeOnChainId bs

onChainIdToAssetName :: OnChainId -> AssetName
onChainIdToAssetName = AssetName . serialiseToRawBytes

-- | Derive the 'OnChainId' from a Cardano 'PaymentKey'. The on-chain identifier
-- is the public key hash as it is also availble to plutus validators.
verificationKeyToOnChainId :: VerificationKey PaymentKey -> OnChainId
verificationKeyToOnChainId =
  UnsafeOnChainId . fromBuiltin . getPubKeyHash . toPlutusKeyHash . verificationKeyHash

-- * Helpers

headTokensFromValue :: PlutusScript -> Value -> [(AssetName, Quantity)]
headTokensFromValue headTokenScript v =
  [ (assetName, q)
  | (AssetId pid assetName, q) <- valueToList v
  , pid == scriptPolicyId (PlutusScript headTokenScript)
  ]

-- | Find first occurrence including a transformation.
findFirst :: Foldable t => (a -> Maybe b) -> t a -> Maybe b
findFirst fn = getFirst . foldMap (First . fn)

findHeadAssetId :: TxOut ctx -> Maybe (PolicyId, AssetName)
findHeadAssetId txOut =
  flip findFirst (valueToList $ txOutValue txOut) $ \case
    (AssetId pid aname, q)
      | aname == hydraHeadV1AssetName && q == 1 ->
          Just (pid, aname)
    _ ->
      Nothing

-- | Find (if it exists) the head identifier contained in given `TxOut`.
findStateToken :: TxOut ctx -> Maybe HeadId
findStateToken =
  fmap (mkHeadId . fst) . findHeadAssetId
