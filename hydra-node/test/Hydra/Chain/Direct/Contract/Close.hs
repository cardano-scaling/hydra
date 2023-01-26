{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.Contract.Close where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Cardano.Api.UTxO as UTxO
import Data.Maybe (fromJust)
import Hydra.Chain.Direct.Contract.Mutation (Mutation (..), SomeMutation (..), addParticipationTokens, changeHeadOutputDatum, genHash, replaceContestationDeadline, replacePolicyIdWith, replaceSnapshotNumber, replaceUtxoHash)
import Hydra.Chain.Direct.Fixture (genForParty, testNetworkId)
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
import Hydra.Ledger.Cardano.Evaluate (genValidityBoundsFromContestationPeriod, slotNoToUTCTime)
import Hydra.Party (Party, deriveParty, partyToChain)
import Hydra.Snapshot (Snapshot (..), SnapshotNumber)
import Plutus.Orphans ()
import Plutus.V2.Ledger.Api (BuiltinByteString, toBuiltin, toData)
import Test.Hydra.Fixture (aliceSk, bobSk, carolSk)
import Test.QuickCheck (arbitrarySizedNatural, choose, elements, oneof, suchThat)
import Test.QuickCheck.Instances ()

--
-- CloseTx
--

healthyCloseTx :: (Tx, UTxO)
healthyCloseTx =
  (tx, lookupUTxO)
 where
  tx =
    closeTx
      somePartyCardanoVerificationKey
      healthyClosingSnapshot
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
  healthyClosingSnapshot :: ClosingSnapshot
  healthyClosingSnapshot =
    CloseWithConfirmedSnapshot
      { snapshotNumber = healthySnapshotNumber
      , closeUtxoHash = UTxOHash $ hashUTxO @Tx healthyCloseUTxO
      , signatures = healthySignature healthySnapshotNumber
      }

healthyCloseInitialTx :: (Tx, UTxO)
healthyCloseInitialTx =
  (tx, lookupUTxO)
 where
  tx =
    closeTx
      somePartyCardanoVerificationKey
      healthyClosingSnapshot
      startSlot
      pointInTime
      openThreadOutput
      (mkHeadId Fixture.testPolicyId)

  -- here we need to pass in contestation period when generating start/end tx validity slots/time
  -- since if tx validity bound difference is bigger than contestation period our close validator
  -- will fail
  (startSlot, pointInTime) =
    genValidityBoundsFromContestationPeriod (fromChain healthyContestationPeriod) `generateWith` 42

  lookupUTxO = UTxO.singleton (healthyOpenHeadTxIn, healthyOpenHeadTxOut)

  headDatum = fromPlutusData $ toData healthyOpenHeadDatum

  openThreadOutput =
    OpenThreadOutput
      { openThreadUTxO = (healthyOpenHeadTxIn, healthyOpenHeadTxOut, headDatum)
      , openParties = healthyOnChainParties
      , openContestationPeriod = healthyContestationPeriod
      }
  healthyClosingSnapshot :: ClosingSnapshot
  healthyClosingSnapshot =
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

-- FIXME: This is not a healthy value anyhow related to the 'healthyCloseTx' above
brokenSlotNo :: SlotNo
brokenSlotNo = arbitrary `generateWith` 42

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
  | MutateValidityInterval
  | MutateCloseContestationDeadline
  | MutateCloseContestationDeadlineWithZero
  | MutateHeadId
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
    , SomeMutation Nothing MutateRequiredSigner <$> do
        newSigner <- verificationKeyHash <$> genVerificationKey
        pure $ ChangeRequiredSigners [newSigner]
    , SomeMutation Nothing MutateCloseUTxOHash . ChangeOutput 0 <$> mutateCloseUTxOHash
    , SomeMutation (Just "incorrect closed contestation deadline") MutateCloseContestationDeadline . ChangeOutput 0
        <$> (mutateClosedContestationDeadline headTxOut =<< arbitrary @Integer `suchThat` (/= healthyContestationPeriodSeconds))
    , SomeMutation Nothing MutateCloseContestationDeadlineWithZero . ChangeOutput 0 <$> mutateClosedContestationDeadline headTxOut 0
    , SomeMutation Nothing MutateValidityInterval . ChangeValidityInterval <$> do
        lb <- arbitrary
        ub <- arbitrary `suchThat` (/= TxValidityUpperBound brokenSlotNo)
        pure (lb, ub)
    , -- try to change a tx so that lower bound is higher than the upper bound
      SomeMutation Nothing MutateValidityInterval . ChangeValidityInterval <$> do
        lb <- arbitrary
        ub <- (lb -) <$> choose (0, lb)
        pure (TxValidityLowerBound (SlotNo lb), TxValidityUpperBound (SlotNo ub))
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
    ]
 where
  headTxOut = fromJust $ txOuts' tx !!? 0

  mutateCloseUTxOHash :: Gen (TxOut CtxTx)
  mutateCloseUTxOHash = do
    mutatedUTxOHash <- genHash
    pure $ changeHeadOutputDatum (replaceUtxoHash $ toBuiltin mutatedUTxOHash) headTxOut

data CloseInitialMutation
  = MutateCloseContestationDeadline'
  | MutateCloseContestationDeadlineWithZero'
  deriving (Generic, Show, Enum, Bounded)

genCloseInitialMutation :: (Tx, UTxO) -> Gen SomeMutation
genCloseInitialMutation (tx, _utxo) =
  oneof
    [ SomeMutation (Just "incorrect closed contestation deadline") MutateCloseContestationDeadline' . ChangeOutput 0
        <$> (mutateClosedContestationDeadline headTxOut =<< arbitrary @Integer `suchThat` (/= healthyContestationPeriodSeconds))
    , SomeMutation (Just "incorrect closed contestation deadline") MutateCloseContestationDeadlineWithZero' . ChangeOutput 0
        <$> mutateClosedContestationDeadline headTxOut 0
    ]
 where
  headTxOut = fromJust $ txOuts' tx !!? 0

-- In case contestation period param is 'Nothing' we will generate arbitrary value
--mutateClosedContestationDeadline :: TxOut CtxTx UTXO.Era -> Integer -> Gen (TxOut CtxTx)
mutateClosedContestationDeadline :: TxOut CtxTx -> Integer -> Gen (TxOut CtxTx)
mutateClosedContestationDeadline headTxOut contestationPeriodSeconds = do
  -- NOTE: we need to be sure the generated contestation period is large enough to have an impact on the on-chain
  -- deadline computation, which means having a resolution of seconds instead of the default picoseconds
  let closingTime = slotNoToUTCTime brokenSlotNo
  let contestationDeadline = posixFromUTCTime $ addUTCTime (fromInteger contestationPeriodSeconds) closingTime
  pure $ changeHeadOutputDatum (replaceContestationDeadline contestationDeadline) headTxOut
