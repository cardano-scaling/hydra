{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.Contract.Contest where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Data.Maybe (fromJust)

import Cardano.Api.UTxO as UTxO
import Hydra.Chain.Direct.Contract.Mutation (
  Mutation (..),
  SomeMutation (..),
  addParticipationTokens,
  changeHeadOutputDatum,
  genHash,
 )
import Hydra.Chain.Direct.Fixture (genForParty, testNetworkId, testPolicyId)
import Hydra.Chain.Direct.Tx (ClosedThreadOutput (..), contestTx, mkHeadOutput)
import qualified Hydra.Contract.HeadState as Head
import Hydra.Crypto (HydraKey, MultiSignature, aggregate, sign, toPlutusSignatures)
import Hydra.Data.ContestationPeriod (posixFromUTCTime)
import qualified Hydra.Data.Party as OnChain
import Hydra.Ledger (hashUTxO)
import Hydra.Ledger.Cardano (genOneUTxOFor, genVerificationKey)
import Hydra.Ledger.Cardano.Evaluate (slotNoToUTCTime)
import Hydra.Party (Party, deriveParty, partyToChain)
import Hydra.Snapshot (Snapshot (..), SnapshotNumber)
import Plutus.Orphans ()
import Plutus.V2.Ledger.Api (BuiltinByteString, toBuiltin, toData)
import Test.Hydra.Fixture (aliceSk, bobSk, carolSk)
import Test.QuickCheck (elements, oneof, suchThat)
import Test.QuickCheck.Gen (choose)
import Test.QuickCheck.Instances ()

--
-- ContestTx
--

healthyContestTx :: (Tx, UTxO)
healthyContestTx =
  (tx, lookupUTxO)
 where
  tx =
    contestTx
      somePartyCardanoVerificationKey
      healthyContestSnapshot
      (healthySignature healthyContestSnapshotNumber)
      (healthySlotNo, slotNoToUTCTime healthySlotNo)
      closedThreadOutput

  headInput = generateWith arbitrary 42

  headResolvedInput =
    mkHeadOutput testNetworkId testPolicyId headTxOutDatum
      & addParticipationTokens healthyParties

  headTxOutDatum = toUTxOContext (mkTxOutDatum healthyClosedState)

  headDatum = fromPlutusData $ toData healthyClosedState

  lookupUTxO = UTxO.singleton (headInput, headResolvedInput)

  closedThreadOutput =
    ClosedThreadOutput
      { closedThreadUTxO = (headInput, headResolvedInput, headDatum)
      , closedParties =
          healthyOnChainParties
      , closedContestationDeadline = posixFromUTCTime healthyContestationDeadline
      }

healthyContestSnapshot :: Snapshot Tx
healthyContestSnapshot =
  Snapshot
    { number = healthyContestSnapshotNumber
    , utxo = healthyContestUTxO
    , confirmed = []
    }

healthyContestSnapshotNumber :: SnapshotNumber
healthyContestSnapshotNumber = 4

healthyContestUTxO :: UTxO
healthyContestUTxO =
  (genOneUTxOFor somePartyCardanoVerificationKey `suchThat` (/= healthyClosedUTxO))
    `generateWith` 42

healthyContestUTxOHash :: BuiltinByteString
healthyContestUTxOHash =
  toBuiltin $ hashUTxO @Tx healthyContestUTxO

healthyClosedState :: Head.State
healthyClosedState =
  Head.Closed
    { snapshotNumber = fromIntegral healthyClosedSnapshotNumber
    , utxoHash = healthyClosedUTxOHash
    , parties = healthyOnChainParties
    , contestationDeadline = posixFromUTCTime healthyContestationDeadline
    }

healthySlotNo :: SlotNo
healthySlotNo = arbitrary `generateWith` 42

healthyContestationDeadline :: UTCTime
healthyContestationDeadline =
  addUTCTime
    (fromInteger healthyContestationPeriodSeconds)
    (slotNoToUTCTime healthySlotNo)

healthyContestationPeriodSeconds :: Integer
healthyContestationPeriodSeconds = 10

healthyClosedSnapshotNumber :: SnapshotNumber
healthyClosedSnapshotNumber = 3

healthyClosedUTxOHash :: BuiltinByteString
healthyClosedUTxOHash =
  toBuiltin $ hashUTxO @Tx healthyClosedUTxO

healthyClosedUTxO :: UTxO
healthyClosedUTxO =
  genOneUTxOFor somePartyCardanoVerificationKey `generateWith` 42

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
healthySignature number =
  aggregate [sign sk snapshot | sk <- healthySigningKeys]
 where
  snapshot = healthyContestSnapshot{number}

-- FIXME: Should try to mutate the 'closedAt' recorded time to something else
data ContestMutation
  = -- | Ensure signatures are actually checked.
    MutateSignatureButNotSnapshotNumber
  | -- | Ensure too old snapshot are not valid.
    MutateToNonNewerSnapshot
  | -- | Ensure that it's performed by a Head party
    MutateRequiredSigner
  | -- | Ensure output state is consistent with redeemer
    MutateContestUTxOHash
  | -- | Change parties stored in the state, causing multisig to fail
    MutateParties
  | -- | Change the validity interval of the transaction to a value greater
    -- than the contestation deadline
    MutateValidityPastDeadline
  deriving (Generic, Show, Enum, Bounded)

genContestMutation :: (Tx, UTxO) -> Gen SomeMutation
genContestMutation
  ( tx
    , _utxo
    ) =
    oneof
      [ SomeMutation MutateSignatureButNotSnapshotNumber . ChangeHeadRedeemer <$> do
          mutatedSignature <- arbitrary :: Gen (MultiSignature (Snapshot Tx))
          pure $
            Head.Contest
              { snapshotNumber = toInteger healthyContestSnapshotNumber
              , utxoHash = healthyContestUTxOHash
              , signature = toPlutusSignatures mutatedSignature
              }
      , SomeMutation MutateToNonNewerSnapshot . ChangeHeadRedeemer <$> do
          mutatedSnapshotNumber <- choose (0, toInteger healthyClosedSnapshotNumber)
          pure $
            Head.Contest
              { snapshotNumber = mutatedSnapshotNumber
              , utxoHash = healthyContestUTxOHash
              , signature =
                  toPlutusSignatures $
                    healthySignature (fromInteger mutatedSnapshotNumber)
              }
      , SomeMutation MutateRequiredSigner <$> do
          newSigner <- verificationKeyHash <$> genVerificationKey
          pure $ ChangeRequiredSigners [newSigner]
      , SomeMutation MutateContestUTxOHash . ChangeOutput 0 <$> do
          mutateCloseUTxOHash
      , SomeMutation MutateParties . ChangeHeadDatum <$> do
          mutatedParties <- arbitrary `suchThat` (/= healthyOnChainParties)
          pure $
            Head.Closed
              { parties = mutatedParties
              , utxoHash = healthyClosedUTxOHash
              , snapshotNumber = fromIntegral healthyClosedSnapshotNumber
              , contestationDeadline = arbitrary `generateWith` 42
              }
      , SomeMutation MutateValidityPastDeadline . ChangeValidityInterval <$> do
          lb <- arbitrary
          ub <- TxValidityUpperBound <$> arbitrary `suchThat` slotOverContestationDeadline
          pure (lb, ub)
      ]
   where
    headTxOut = fromJust $ txOuts' tx !!? 0

    mutateCloseUTxOHash :: Gen (TxOut CtxTx)
    mutateCloseUTxOHash = do
      mutatedUTxOHash <- genHash `suchThat` ((/= healthyContestUTxOHash) . toBuiltin)
      pure $
        changeHeadOutputDatum
          ( const $
              Head.Closed
                { snapshotNumber = fromIntegral healthyContestSnapshotNumber
                , utxoHash = toBuiltin mutatedUTxOHash
                , parties = healthyOnChainParties
                , contestationDeadline = arbitrary `generateWith` 42
                }
          )
          headTxOut

    slotOverContestationDeadline slotNo =
      slotNoToUTCTime slotNo > healthyContestationDeadline
