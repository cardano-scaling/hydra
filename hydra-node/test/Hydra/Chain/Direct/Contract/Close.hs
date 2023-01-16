{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.Contract.Close where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Cardano.Api.UTxO as UTxO
import Cardano.Binary (serialize')
import Data.Maybe (fromJust)
import Hydra.Chain.Direct.Contract.Mutation (Mutation (..), SomeMutation (..), addParticipationTokens, changeHeadOutputDatum, genHash, replacePolicyIdWith)
import Hydra.Chain.Direct.Fixture (genForParty, testNetworkId)
import qualified Hydra.Chain.Direct.Fixture as Fixture
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
import Hydra.Snapshot (Snapshot (..), SnapshotNumber (UnsafeSnapshotNumber))
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

  headDatum = fromPlutusData $ toData healthyCloseDatum

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
  headTxOutDatum = toUTxOContext (mkTxOutDatum healthyCloseDatum)

healthySlotNo :: SlotNo
healthySlotNo = arbitrary `generateWith` 42

healthyClosingSnapshot :: ClosingSnapshot
healthyClosingSnapshot =
  CloseWithConfirmedSnapshot
    { snapshotNumber = healthySnapshotNumber
    , closeUtxoHash = UTxOHash $ hashUTxO @Tx healthyCloseUTxO
    , signatures = healthySignature healthySnapshotNumber
    }

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

healthyCloseDatum :: Head.State
healthyCloseDatum =
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
    (slotNoToUTCTime healthySlotNo)

healthyClosedUTxOHash :: BuiltinByteString
healthyClosedUTxOHash =
  toBuiltin $ hashUTxO @Tx healthyClosedUTxO

healthyClosedUTxO :: UTxO
healthyClosedUTxO =
  genOneUTxOFor somePartyCardanoVerificationKey `generateWith` 42

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

genCloseMutation :: (Tx, UTxO) -> Gen SomeMutation
genCloseMutation (tx, _utxo) =
  -- FIXME: using 'closeRedeemer' here is actually too high-level and reduces
  -- the power of the mutators, we should test at the level of the validator.
  -- That is, using the on-chain types. 'closeRedeemer' is also not used
  -- anywhere after changing this and can be moved into the closeTx
  oneof
    [ SomeMutation Nothing MutateSignatureButNotSnapshotNumber . ChangeHeadRedeemer <$> do
        closeRedeemer <$> (arbitrary :: Gen (MultiSignature (Snapshot Tx)))
    , SomeMutation Nothing MutateSnapshotNumberButNotSignature . ChangeHeadDatum <$> do
        (UnsafeSnapshotNumber mutatedSnapshotNumber) <- arbitrarySizedNatural `suchThat` (\n -> n /= healthySnapshotNumber && n > 0)
        pure $
          Head.Closed
            { snapshotNumber = toInteger mutatedSnapshotNumber
            , utxoHash = healthyClosedUTxOHash
            , parties = healthyOnChainParties
            , contestationDeadline = posixFromUTCTime healthyContestationDeadline
            , headId = toPlutusCurrencySymbol Fixture.testPolicyId
            }
    , SomeMutation Nothing MutateSnapshotToIllFormedValue <$> do
        mutatedSnapshotNumber <- arbitrary `suchThat` (< 0)
        let mutatedSignature =
              aggregate [sign sk $ serialize' mutatedSnapshotNumber | sk <- healthySigningKeys]
        pure $
          Changes
            [ ChangeHeadDatum $
                Head.Closed
                  { snapshotNumber = mutatedSnapshotNumber
                  , utxoHash = healthyClosedUTxOHash
                  , parties = healthyOnChainParties
                  , contestationDeadline = posixFromUTCTime healthyContestationDeadline
                  , headId = toPlutusCurrencySymbol Fixture.testPolicyId
                  }
            , ChangeHeadRedeemer $
                Head.Close
                  { signature = toPlutusSignatures mutatedSignature
                  , utxoHash = ""
                  }
            ]
    , SomeMutation Nothing MutateParties . ChangeHeadDatum <$> do
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
    , SomeMutation Nothing MutateCloseContestationDeadline . ChangeOutput 0
        <$> (mutateClosedContestationDeadline =<< arbitrary @Integer `suchThat` (/= healthyContestationPeriodSeconds))
    , SomeMutation Nothing MutateCloseContestationDeadlineWithZero . ChangeOutput 0 <$> mutateClosedContestationDeadline 0
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
                (Just $ toScriptData healthyCloseDatum)
            ]
    ]
 where
  headTxOut = fromJust $ txOuts' tx !!? 0

  closeRedeemer sig =
    Head.Close
      { signature = toPlutusSignatures sig
      , utxoHash = ""
      }

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
