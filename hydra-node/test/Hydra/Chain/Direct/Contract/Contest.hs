{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.Contract.Contest where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Data.Maybe (fromJust)

import Cardano.Api.UTxO as UTxO
import Hydra.Chain.Direct.Contract.Gen (genForParty, genHash, genMintedOrBurnedValue)
import Hydra.Chain.Direct.Contract.Mutation (
  Mutation (..),
  SomeMutation (..),
  addParticipationTokens,
  changeHeadOutputDatum,
  changeMintedTokens,
  replaceParties,
  replacePolicyIdWith,
  replaceSnapshotNumber,
  replaceUtxoHash,
 )
import Hydra.Chain.Direct.Fixture (testNetworkId, testPolicyId)
import Hydra.Chain.Direct.Tx (ClosedThreadOutput (..), contestTx, mkHeadId, mkHeadOutput)
import qualified Hydra.Contract.HeadState as Head
import Hydra.Contract.HeadTokens (headPolicyId)
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
      (mkHeadId testPolicyId)

  headDatum = fromPlutusData $ toData healthyClosedState

  lookupUTxO = UTxO.singleton (healthyClosedHeadTxIn, healthyClosedHeadTxOut)

  closedThreadOutput =
    ClosedThreadOutput
      { closedThreadUTxO = (healthyClosedHeadTxIn, healthyClosedHeadTxOut, headDatum)
      , closedParties =
          healthyOnChainParties
      , closedContestationDeadline = posixFromUTCTime healthyContestationDeadline
      , closedContestors = []
      }

healthyClosedHeadTxIn :: TxIn
healthyClosedHeadTxIn = generateWith arbitrary 42

healthyClosedHeadTxOut :: TxOut CtxUTxO
healthyClosedHeadTxOut =
  mkHeadOutput testNetworkId testPolicyId headTxOutDatum
    & addParticipationTokens healthyParties
 where
  headTxOutDatum = toUTxOContext (mkTxOutDatum healthyClosedState)

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
    , headId = toPlutusCurrencySymbol testPolicyId
    , contestors = []
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
  | -- | Change the head policy id to test the head validators
    MutateHeadId
  | -- | Minting or burning of the tokens should not be possible in v_head apart from 'checkAbort' or 'checkFanout'
    MutateTokenMintingOrBurning
  | -- | Change the contestors list to test the head validators
    MutateContestors
  deriving (Generic, Show, Enum, Bounded)

genContestMutation :: (Tx, UTxO) -> Gen SomeMutation
genContestMutation
  ( tx
    , _utxo
    ) =
    oneof
      [ SomeMutation Nothing MutateSignatureButNotSnapshotNumber . ChangeHeadRedeemer <$> do
          mutatedSignature <- arbitrary :: Gen (MultiSignature (Snapshot Tx))
          pure $
            Head.Contest
              { signature = toPlutusSignatures mutatedSignature
              }
      , SomeMutation Nothing MutateToNonNewerSnapshot <$> do
          mutatedSnapshotNumber <- choose (0, toInteger healthyClosedSnapshotNumber)
          pure $
            Changes
              [ ChangeInputHeadDatum $
                  healthyClosedState & replaceSnapshotNumber mutatedSnapshotNumber
              , ChangeHeadRedeemer $
                  Head.Contest
                    { signature =
                        toPlutusSignatures $
                          healthySignature (fromInteger mutatedSnapshotNumber)
                    }
              ]
      , SomeMutation Nothing MutateRequiredSigner <$> do
          newSigner <- verificationKeyHash <$> genVerificationKey
          pure $ ChangeRequiredSigners [newSigner]
      , SomeMutation Nothing MutateContestUTxOHash . ChangeOutput 0 <$> do
          mutatedUTxOHash <- genHash `suchThat` ((/= healthyContestUTxOHash) . toBuiltin)
          pure $
            changeHeadOutputDatum
              (const $ healthyClosedState & replaceUtxoHash (toBuiltin mutatedUTxOHash))
              headTxOut
      , SomeMutation Nothing MutateParties . ChangeInputHeadDatum <$> do
          mutatedParties <- arbitrary `suchThat` (/= healthyOnChainParties)
          pure $
            healthyClosedState & replaceParties mutatedParties
      , SomeMutation Nothing MutateValidityPastDeadline . ChangeValidityInterval <$> do
          lb <- arbitrary
          ub <- TxValidityUpperBound <$> arbitrary `suchThat` slotOverContestationDeadline
          pure (lb, ub)
      , SomeMutation Nothing MutateHeadId <$> do
          otherHeadId <- fmap headPolicyId (arbitrary `suchThat` (/= healthyClosedHeadTxIn))
          pure $
            Changes
              [ ChangeOutput 0 (replacePolicyIdWith testPolicyId otherHeadId headTxOut)
              , ChangeInput
                  healthyClosedHeadTxIn
                  (replacePolicyIdWith testPolicyId otherHeadId healthyClosedHeadTxOut)
                  (Just $ toScriptData healthyClosedState)
              ]
      , SomeMutation (Just "minting or burning is forbidden") MutateTokenMintingOrBurning
          <$> (changeMintedTokens tx =<< genMintedOrBurnedValue)
      , SomeMutation Nothing MutateContestors . ChangeInputHeadDatum <$> do
          let contestor = toPlutusKeyHash (verificationKeyHash somePartyCardanoVerificationKey)
          pure $
            Head.Closed
              { parties = healthyOnChainParties
              , utxoHash = healthyClosedUTxOHash
              , snapshotNumber = fromIntegral healthyClosedSnapshotNumber
              , contestationDeadline = arbitrary `generateWith` 42
              , headId = toPlutusCurrencySymbol testPolicyId
              , contestors = [contestor]
              }
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
                , headId = toPlutusCurrencySymbol testPolicyId
                , contestors = []
                }
          )
          headTxOut

    slotOverContestationDeadline slotNo =
      slotNoToUTCTime slotNo > healthyContestationDeadline
