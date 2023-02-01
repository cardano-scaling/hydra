{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.Contract.Close where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Cardano.Api.UTxO as UTxO
import Data.Maybe (fromJust)
import Hydra.Chain.Direct.Contract.Gen (genForParty, genHash, genMintedOrBurnedValue)
import Hydra.Chain.Direct.Contract.Mutation (Mutation (..), SomeMutation (..), addParticipationTokens, changeHeadOutputDatum, changeMintedTokens, replaceContestationDeadline, replaceContesters, replaceHeadId, replaceParties, replacePolicyIdWith, replaceSnapshotNumber, replaceUtxoHash)
import Hydra.Chain.Direct.Fixture (testNetworkId)
import qualified Hydra.Chain.Direct.Fixture as Fixture
import Hydra.Chain.Direct.TimeHandle (PointInTime)
import Hydra.Chain.Direct.Tx (ClosingSnapshot (..), OpenThreadOutput (..), UTxOHash (UTxOHash), closeTx, mkHeadId, mkHeadOutput)
import Hydra.ContestationPeriod (fromChain)
import qualified Hydra.Contract.HeadState as Head
import Hydra.Contract.HeadTokens (headPolicyId)
import Hydra.Crypto (HydraKey, MultiSignature, aggregate, sign, toPlutusSignatures)
import Hydra.Data.ContestationPeriod (posixFromUTCTime)
import qualified Hydra.Data.ContestationPeriod as OnChain
import qualified Hydra.Data.Party as OnChain
import Hydra.Ledger (hashUTxO)
import Hydra.Ledger.Cardano (genOneUTxOFor, genVerificationKey)
import Hydra.Ledger.Cardano.Evaluate (genValidityBoundsFromContestationPeriod)
import Hydra.Party (Party, deriveParty, partyToChain)
import Hydra.Snapshot (Snapshot (..), SnapshotNumber)
import Plutus.Orphans ()
import Plutus.V1.Ledger.Time (DiffMilliSeconds (..), fromMilliSeconds)
import Plutus.V2.Ledger.Api (BuiltinByteString, POSIXTime, PubKeyHash (PubKeyHash), toBuiltin, toData)
import Test.Hydra.Fixture (aliceSk, bobSk, carolSk)
import Test.QuickCheck (arbitrarySizedNatural, choose, elements, oneof, suchThat, vectorOf)
import Test.QuickCheck.Instances ()

--
-- CloseTx
--

-- | Healthy close transaction for the generic case were we close a head
--   after one or more snapshot have been agreed upon between the members.
healthyCloseTx :: (Tx, UTxO)
healthyCloseTx =
  (tx, lookupUTxO)
 where
  tx =
    closeTx
      somePartyCardanoVerificationKey
      closingSnapshot
      healthyCloseLowerBoundSlot
      healthyCloseUpperBoundPointInTime
      openThreadOutput
      (mkHeadId Fixture.testPolicyId)

  lookupUTxO = UTxO.singleton (healthyOpenHeadTxIn, healthyOpenHeadTxOut)

  headDatum = fromPlutusData $ toData healthyOpenHeadDatum

  openThreadOutput =
    OpenThreadOutput
      { openThreadUTxO = (healthyOpenHeadTxIn, healthyOpenHeadTxOut, headDatum)
      , openParties = healthyOnChainParties
      , openContestationPeriod = healthyContestationPeriod
      }
  closingSnapshot :: ClosingSnapshot
  closingSnapshot =
    CloseWithConfirmedSnapshot
      { snapshotNumber = healthySnapshotNumber
      , closeUtxoHash = UTxOHash $ hashUTxO @Tx healthyCloseUTxO
      , signatures = healthySignature healthySnapshotNumber
      }

-- | Healthy close transaction for the specific case were we close a head
--   with the initial UtxO, that is, no snapshot have been agreed upon and
--   signed by the head members yet.
healthyCloseInitialTx :: (Tx, UTxO)
healthyCloseInitialTx =
  (tx, lookupUTxO)
 where
  tx =
    closeTx
      somePartyCardanoVerificationKey
      closingSnapshot
      healthyCloseLowerBoundSlot
      healthyCloseUpperBoundPointInTime
      openThreadOutput
      (mkHeadId Fixture.testPolicyId)

  lookupUTxO = UTxO.singleton (healthyOpenHeadTxIn, healthyOpenHeadTxOut)

  headDatum = fromPlutusData $ toData healthyOpenHeadDatum

  openThreadOutput =
    OpenThreadOutput
      { openThreadUTxO = (healthyOpenHeadTxIn, healthyOpenHeadTxOut, headDatum)
      , openParties = healthyOnChainParties
      , openContestationPeriod = healthyContestationPeriod
      }
  closingSnapshot :: ClosingSnapshot
  closingSnapshot =
    CloseWithInitialSnapshot
      { openUtxoHash = UTxOHash $ hashUTxO @Tx healthyUTxO
      }

-- NOTE: We need to use the contestation period when generating start/end tx
-- validity slots/time since if tx validity bound difference is bigger than
-- contestation period our close validator will fail
healthyCloseLowerBoundSlot :: SlotNo
healthyCloseUpperBoundPointInTime :: PointInTime
(healthyCloseLowerBoundSlot, healthyCloseUpperBoundPointInTime) =
  genValidityBoundsFromContestationPeriod (fromChain healthyContestationPeriod) `generateWith` 42

healthyOpenHeadTxIn :: TxIn
healthyOpenHeadTxIn = generateWith arbitrary 42

healthyOpenHeadTxOut :: TxOut CtxUTxO
healthyOpenHeadTxOut =
  mkHeadOutput testNetworkId Fixture.testPolicyId headTxOutDatum
    & addParticipationTokens healthyParties
 where
  headTxOutDatum = toUTxOContext (mkTxOutDatum healthyOpenHeadDatum)

healthySnapshot :: Snapshot Tx
healthySnapshot =
  Snapshot
    { number = healthySnapshotNumber
    , utxo = healthyCloseUTxO
    , confirmed = []
    }

healthyCloseUTxO :: UTxO
healthyCloseUTxO =
  (genOneUTxOFor somePartyCardanoVerificationKey `suchThat` (/= healthyUTxO))
    `generateWith` 42

healthySnapshotNumber :: SnapshotNumber
healthySnapshotNumber = 1

healthyOpenHeadDatum :: Head.State
healthyOpenHeadDatum =
  Head.Open
    { parties = healthyOnChainParties
    , utxoHash = toBuiltin $ hashUTxO @Tx healthyUTxO
    , contestationPeriod = healthyContestationPeriod
    , headId = toPlutusCurrencySymbol Fixture.testPolicyId
    }

healthyContestationPeriod :: OnChain.ContestationPeriod
healthyContestationPeriod = OnChain.contestationPeriodFromDiffTime $ fromInteger healthyContestationPeriodSeconds

healthyContestationPeriodSeconds :: Integer
healthyContestationPeriodSeconds = 10

healthyUTxO :: UTxO
healthyUTxO = genOneUTxOFor somePartyCardanoVerificationKey `generateWith` 42

somePartyCardanoVerificationKey :: VerificationKey PaymentKey
somePartyCardanoVerificationKey = flip generateWith 42 $ do
  genForParty genVerificationKey <$> elements healthyParties

healthySigningKeys :: [SigningKey HydraKey]
healthySigningKeys = [aliceSk, bobSk, carolSk]

healthyParties :: [Party]
healthyParties = deriveParty <$> healthySigningKeys

healthyOnChainParties :: [OnChain.Party]
healthyOnChainParties = partyToChain <$> healthyParties

healthySignature :: SnapshotNumber -> MultiSignature (Snapshot Tx)
healthySignature number = aggregate [sign sk snapshot | sk <- healthySigningKeys]
 where
  snapshot = healthySnapshot{number}

healthyContestationDeadline :: UTCTime
healthyContestationDeadline =
  addUTCTime
    (fromInteger healthyContestationPeriodSeconds)
    (snd healthyCloseUpperBoundPointInTime)

healthyClosedUTxOHash :: BuiltinByteString
healthyClosedUTxOHash =
  toBuiltin $ hashUTxO @Tx healthyClosedUTxO

healthyClosedUTxO :: UTxO
healthyClosedUTxO =
  genOneUTxOFor somePartyCardanoVerificationKey `generateWith` 42

data CloseMutation
  = MutateSignatureButNotSnapshotNumber
  | -- | Change the resulting snapshot number, this should make the signature
    -- invalid.
    MutateSnapshotNumberButNotSignature
  | -- | This test the case when we have a non-initial utxo hash but the snapshot number is less than or equal to 0
    MutateSnapshotNumberToLessThanZero
  | MutateParties
  | MutateRequiredSigner
  | MutateCloseUTxOHash
  | MutatePartiesInOutput
  | MutateHeadIdInOutput
  | InfiniteLowerBound
  | InfiniteUpperBound
  | -- | See spec: 5.5 rule 4 -> contestationDeadline = upperBound + contestationPeriod
    MutateContestationDeadline
  | -- | See spec: 5.5. rule 5 -> upperBound - lowerBound <= contestationPeriod
    MutateValidityInterval
  | MutateHeadId
  | -- | Minting or burning of the tokens should not be possible in v_head apart from 'checkAbort' or 'checkFanout'
    MutateTokenMintingOrBurning
  | MutateContesters
  deriving (Generic, Show, Enum, Bounded)

genCloseMutation :: (Tx, UTxO) -> Gen SomeMutation
genCloseMutation (tx, _utxo) =
  oneof
    [ SomeMutation (Just "invalid snapshot signature") MutateSignatureButNotSnapshotNumber . ChangeHeadRedeemer <$> do
        Head.Close . toPlutusSignatures <$> (arbitrary :: Gen (MultiSignature (Snapshot Tx)))
    , SomeMutation (Just "closed with non-initial hash") MutateSnapshotNumberToLessThanZero <$> do
        mutatedSnapshotNumber <- arbitrary `suchThat` (<= 0)
        pure $ ChangeOutput 0 $ changeHeadOutputDatum (replaceSnapshotNumber mutatedSnapshotNumber) headTxOut
    , SomeMutation (Just "invalid snapshot signature") MutateSnapshotNumberButNotSignature <$> do
        mutatedSnapshotNumber <- arbitrarySizedNatural `suchThat` (\n -> n /= healthySnapshotNumber && n > 0)
        pure $ ChangeOutput 0 $ changeHeadOutputDatum (replaceSnapshotNumber $ toInteger mutatedSnapshotNumber) headTxOut
    , SomeMutation Nothing MutateParties . ChangeInputHeadDatum <$> do
        mutatedParties <- arbitrary `suchThat` (/= healthyOnChainParties)
        pure $
          Head.Open
            { parties = mutatedParties
            , utxoHash = ""
            , contestationPeriod = healthyContestationPeriod
            , headId = toPlutusCurrencySymbol Fixture.testPolicyId
            }
    , SomeMutation (Just "changed parameters") MutatePartiesInOutput <$> do
        mutatedParties <- arbitrary `suchThat` (/= healthyOnChainParties)
        pure $ ChangeOutput 0 $ changeHeadOutputDatum (replaceParties mutatedParties) headTxOut
    , SomeMutation (Just "changed parameters") MutateHeadIdInOutput <$> do
        otherHeadId <- toPlutusCurrencySymbol . headPolicyId <$> arbitrary `suchThat` (/= Fixture.testSeedInput)
        pure $ ChangeOutput 0 $ changeHeadOutputDatum (replaceHeadId otherHeadId) headTxOut
    , SomeMutation Nothing MutateRequiredSigner <$> do
        newSigner <- verificationKeyHash <$> genVerificationKey
        pure $ ChangeRequiredSigners [newSigner]
    , SomeMutation Nothing MutateCloseUTxOHash . ChangeOutput 0 <$> mutateCloseUTxOHash
    , SomeMutation (Just "incorrect closed contestation deadline") MutateContestationDeadline <$> do
        mutatedDeadline <- genMutatedDeadline
        pure $ ChangeOutput 0 $ changeHeadOutputDatum (replaceContestationDeadline mutatedDeadline) headTxOut
    , SomeMutation (Just "infinite lower bound") InfiniteLowerBound . ChangeValidityLowerBound <$> do
        pure TxValidityNoLowerBound
    , SomeMutation (Just "infinite upper bound") InfiniteUpperBound . ChangeValidityUpperBound <$> do
        pure TxValidityNoUpperBound
    , SomeMutation (Just "hasBoundedValidity check failed") MutateValidityInterval <$> do
        (lowerSlotNo, upperSlotNo, adjustedContestationDeadline) <- genOversizedTransactionValidity
        pure $
          Changes
            [ ChangeValidityInterval (TxValidityLowerBound lowerSlotNo, TxValidityUpperBound upperSlotNo)
            , ChangeOutput 0 $ changeHeadOutputDatum (replaceContestationDeadline adjustedContestationDeadline) headTxOut
            ]
    , SomeMutation Nothing MutateHeadId <$> do
        otherHeadId <- headPolicyId <$> arbitrary `suchThat` (/= Fixture.testSeedInput)
        pure $
          Changes
            [ ChangeOutput 0 (replacePolicyIdWith Fixture.testPolicyId otherHeadId headTxOut)
            , ChangeInput
                healthyOpenHeadTxIn
                (replacePolicyIdWith Fixture.testPolicyId otherHeadId healthyOpenHeadTxOut)
                (Just $ toScriptData healthyOpenHeadDatum)
            ]
    , SomeMutation (Just "minting or burning is forbidden") MutateTokenMintingOrBurning
        <$> (changeMintedTokens tx =<< genMintedOrBurnedValue)
    , SomeMutation (Just "changed parameters") MutateContesters . ChangeOutput 0 <$> mutateClosedContesters
    ]
 where
  genOversizedTransactionValidity = do
    -- Implicit hypotheses: the slot length is and has always been 1 seconds so we can add slot with seconds
    lowerValidityBound <- arbitrary :: Gen Word64
    upperValidityBound <- choose (lowerValidityBound + fromIntegral healthyContestationPeriodSeconds, maxBound)
    let adjustedContestationDeadline =
          fromMilliSeconds . DiffMilliSeconds $ (healthyContestationPeriodSeconds + fromIntegral upperValidityBound) * 1000
    pure (SlotNo lowerValidityBound, SlotNo upperValidityBound, adjustedContestationDeadline)

  headTxOut = fromJust $ txOuts' tx !!? 0

  mutateCloseUTxOHash :: Gen (TxOut CtxTx)
  mutateCloseUTxOHash = do
    mutatedUTxOHash <- genHash
    pure $ changeHeadOutputDatum (replaceUtxoHash $ toBuiltin mutatedUTxOHash) headTxOut

  mutateClosedContesters :: Gen (TxOut CtxTx)
  mutateClosedContesters = do
    n <- elements [1, 3]
    hashes <- vectorOf n genHash
    let mutatedContesters = PubKeyHash . toBuiltin <$> hashes
    pure $ changeHeadOutputDatum (replaceContesters mutatedContesters) headTxOut

data CloseInitialMutation
  = MutateCloseContestationDeadline'
  deriving (Generic, Show, Enum, Bounded)

-- | Mutations for the specific case of closing with the intial state.
-- We should probably validate all the mutation to this initial state but at
-- least we keep this regression test as we stumbled upon problems with the following case.
-- The nice thing to do would probably to generate either "normal" healthy close or
-- or initial healthy close and apply all the mutation to it but we did'nt manage to do that
-- rightaway.
genCloseInitialMutation :: (Tx, UTxO) -> Gen SomeMutation
genCloseInitialMutation (tx, _utxo) =
  oneof
    [ SomeMutation (Just "incorrect closed contestation deadline") MutateCloseContestationDeadline' <$> do
        mutatedDeadline <- genMutatedDeadline
        pure $ ChangeOutput 0 $ changeHeadOutputDatum (replaceContestationDeadline mutatedDeadline) headTxOut
    ]
 where
  headTxOut = fromJust $ txOuts' tx !!? 0

-- | Generate not acceptable, but interesting deadlines.
genMutatedDeadline :: Gen POSIXTime
genMutatedDeadline = do
  oneof
    [ valuesAroundZero
    , valuesAroundDeadline
    ]
 where
  valuesAroundZero = arbitrary `suchThat` (/= deadline)

  valuesAroundDeadline = arbitrary `suchThat` (/= 0) <&> (+ deadline)

  deadline = posixFromUTCTime healthyContestationDeadline
