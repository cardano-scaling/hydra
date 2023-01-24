{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Close transaction mutation tests starting at the health case of closing
-- with the initial snapshot.
module Hydra.Chain.Direct.Contract.CloseInitial where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Cardano.Api.UTxO as UTxO
import Data.Maybe (fromJust)
import Hydra.Chain.Direct.Contract.Mutation (Mutation (..), SomeMutation (..), addParticipationTokens, changeHeadOutputDatum, genHash, replacePolicyIdWith)
import Hydra.Chain.Direct.Fixture (genForParty, testNetworkId)
import qualified Hydra.Chain.Direct.Fixture as Fixture
import Hydra.Chain.Direct.Tx (ClosingSnapshot (..), OpenThreadOutput (..), UTxOHash (UTxOHash), closeTx, mkHeadId, mkHeadOutput)
import Hydra.ContestationPeriod (fromChain)
import qualified Hydra.Contract.HeadState as Head
import Hydra.Contract.HeadTokens (headPolicyId)
import Hydra.Crypto (HydraKey, MultiSignature, toPlutusSignatures)
import Hydra.Data.ContestationPeriod (posixFromUTCTime)
import qualified Hydra.Data.ContestationPeriod as OnChain
import qualified Hydra.Data.Party as OnChain
import Hydra.Ledger (hashUTxO)
import Hydra.Ledger.Cardano (genOneUTxOFor, genVerificationKey)
import Hydra.Ledger.Cardano.Evaluate (genValidityBoundsFromContestationPeriod, slotNoToUTCTime)
import Hydra.Party (Party, deriveParty, partyToChain)
import Hydra.Snapshot (Snapshot (..), SnapshotNumber)
import Plutus.Orphans ()
import Plutus.V2.Ledger.Api (toBuiltin, toData)
import Test.Hydra.Fixture (aliceSk, bobSk, carolSk)
import Test.QuickCheck (arbitrarySizedNatural, choose, elements, oneof, suchThat)
import Test.QuickCheck.Instances ()

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

healthyOpenHeadTxIn :: TxIn
healthyOpenHeadTxIn = generateWith arbitrary 42

healthyOpenHeadTxOut :: TxOut CtxUTxO
healthyOpenHeadTxOut =
  mkHeadOutput testNetworkId Fixture.testPolicyId headTxOutDatum
    & addParticipationTokens healthyParties
 where
  headTxOutDatum = toUTxOContext (mkTxOutDatum healthyOpenHeadDatum)

-- FIXME: This is not a healthy value anyhow related to the 'healthyCloseInitialTx' above
healthySlotNo :: SlotNo
healthySlotNo = arbitrary `generateWith` 42

healthyClosingSnapshot :: ClosingSnapshot
healthyClosingSnapshot =
  CloseWithInitialSnapshot
    { openUtxoHash = UTxOHash $ hashUTxO @Tx healthyUTxO
    }

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

healthyContestationDeadline :: UTCTime
healthyContestationDeadline =
  addUTCTime
    (fromInteger healthyContestationPeriodSeconds)
    (slotNoToUTCTime healthySlotNo)

data CloseMutation
  = MutateSignatureButNotSnapshotNumber
  | MutateSnapshotNumberButNotSignature
  | MutateSnapshotToIllFormedValue
  | MutateParties
  | MutateRequiredSigner
  | MutateCloseUTxOHash
  | MutateValidityInterval
  | MutateCloseContestationDeadline
  | MutateCloseContestationDeadlineWithZero
  | MutateHeadId
  deriving (Generic, Show, Enum, Bounded)

genCloseInitialMutation :: (Tx, UTxO) -> Gen SomeMutation
genCloseInitialMutation (tx, _utxo) =
  oneof
    [ SomeMutation Nothing MutateSignatureButNotSnapshotNumber . ChangeHeadRedeemer <$> do
        Head.Close . toPlutusSignatures <$> (arbitrary :: Gen (MultiSignature (Snapshot Tx)))
    , SomeMutation Nothing MutateSnapshotNumberButNotSignature . ChangeInputHeadDatum <$> do
        -- FIXME: This is failing for the wrong reason, we would expect "invalid snapshot signature" here
        mutatedSnapshotNumber <- arbitrarySizedNatural `suchThat` (\n -> n /= healthySnapshotNumber && n > 0)
        pure $
          Head.Closed
            { snapshotNumber = toInteger mutatedSnapshotNumber
            , utxoHash = Head.utxoHash healthyOpenHeadDatum
            , parties = Head.parties healthyOpenHeadDatum
            , contestationDeadline = posixFromUTCTime healthyContestationDeadline
            , headId = Head.headId healthyOpenHeadDatum
            }
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
        <$> (mutateClosedContestationDeadline =<< arbitrary @Integer `suchThat` (/= healthyContestationPeriodSeconds))
    , SomeMutation Nothing MutateCloseContestationDeadlineWithZero . ChangeOutput 0
        <$> mutateClosedContestationDeadline 0
    , SomeMutation Nothing MutateValidityInterval . ChangeValidityInterval <$> do
        lb <- arbitrary
        ub <- arbitrary `suchThat` (/= TxValidityUpperBound healthySlotNo)
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
    pure $ changeHeadOutputDatum (mutateHash mutatedUTxOHash) headTxOut

  mutateHash mutatedUTxOHash = \case
    Head.Closed{snapshotNumber, parties, contestationDeadline, headId} ->
      Head.Closed
        { snapshotNumber
        , utxoHash = toBuiltin mutatedUTxOHash
        , parties
        , contestationDeadline
        , headId
        }
    st -> error $ "unexpected state " <> show st
  -- In case contestation period param is 'Nothing' we will generate arbitrary value
  mutateClosedContestationDeadline :: Integer -> Gen (TxOut CtxTx)
  mutateClosedContestationDeadline contestationPeriodSeconds = do
    -- NOTE: we need to be sure the generated contestation period is large enough to have an impact on the on-chain
    -- deadline computation, which means having a resolution of seconds instead of the default picoseconds
    pure $ changeHeadOutputDatum (mutateContestationDeadline contestationPeriodSeconds) headTxOut

  mutateContestationDeadline contestationPeriod = \case
    Head.Closed{snapshotNumber, utxoHash, parties} ->
      Head.Closed
        { snapshotNumber
        , utxoHash
        , parties
        , contestationDeadline =
            let closingTime = slotNoToUTCTime healthySlotNo
             in posixFromUTCTime $ addUTCTime (fromInteger contestationPeriod) closingTime
        , headId = toPlutusCurrencySymbol Fixture.testPolicyId
        }
    st -> error $ "unexpected state " <> show st
