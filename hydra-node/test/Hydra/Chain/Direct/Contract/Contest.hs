{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.Contract.Contest where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Data.Maybe (fromJust)

import Cardano.Api.UTxO as UTxO
import Hydra.Chain.Direct.Contract.Mutation (
  Mutation (..),
  SomeMutation (..),
  changeHeadOutputDatum,
  genHash,
 )
import Hydra.Chain.Direct.Fixture (genForParty, testNetworkId, testPolicyId)
import Hydra.Chain.Direct.Tx (ClosedThreadOutput (..), assetNameFromVerificationKey, contestTx, mkHeadOutput)
import qualified Hydra.Contract.HeadState as Head
import Hydra.Crypto (aggregate, sign, toPlutusSignatures)
import qualified Hydra.Crypto as Hydra
import qualified Hydra.Data.Party as OnChain
import Hydra.Ledger.Cardano (genOneUTxOFor, genVerificationKey, hashTxOuts)
import Hydra.Party (Party, deriveParty, partyToChain)
import Hydra.Snapshot (Snapshot (..), SnapshotNumber)
import Plutus.Orphans ()
import Plutus.V1.Ledger.Api (BuiltinByteString, POSIXTime, UpperBound, toBuiltin, toData)
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
      , closedAtUpperBound = healthyClosedAt
      }

addParticipationTokens :: [Party] -> TxOut CtxUTxO -> TxOut CtxUTxO
addParticipationTokens parties (TxOut addr val datum) =
  TxOut addr val' datum
 where
  val' =
    val
      <> valueFromList
        [ (AssetId testPolicyId (assetNameFromVerificationKey cardanoVk), 1)
        | cardanoVk <- genForParty genVerificationKey <$> parties
        ]

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
  toBuiltin $ hashTxOuts $ toList healthyContestUTxO

healthyClosedState :: Head.State
healthyClosedState =
  Head.Closed
    { snapshotNumber = fromIntegral healthyClosedSnapshotNumber
    , utxoHash = healthyClosedUTxOHash
    , parties = healthyOnChainParties
    , closedAt = healthyClosedAt
    }

healthyClosedAt :: UpperBound POSIXTime
healthyClosedAt = arbitrary `generateWith` 42

healthyClosedSnapshotNumber :: SnapshotNumber
healthyClosedSnapshotNumber = 3

healthyClosedUTxOHash :: BuiltinByteString
healthyClosedUTxOHash =
  toBuiltin $ hashTxOuts $ toList healthyClosedUTxO

healthyClosedUTxO :: UTxO
healthyClosedUTxO =
  genOneUTxOFor somePartyCardanoVerificationKey `generateWith` 42

somePartyCardanoVerificationKey :: VerificationKey PaymentKey
somePartyCardanoVerificationKey = flip generateWith 42 $ do
  genForParty genVerificationKey <$> elements healthyParties

healthySigningKeys :: [Hydra.SigningKey]
healthySigningKeys = [aliceSk, bobSk, carolSk]

healthyParties :: [Party]
healthyParties = deriveParty <$> healthySigningKeys

healthyOnChainParties :: [OnChain.Party]
healthyOnChainParties = partyToChain <$> healthyParties

healthySignature :: SnapshotNumber -> Hydra.MultiSignature (Snapshot Tx)
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
  deriving (Generic, Show, Enum, Bounded)

genContestMutation :: (Tx, UTxO) -> Gen SomeMutation
genContestMutation
  ( tx
    , _utxo
    ) =
    oneof
      [ SomeMutation MutateSignatureButNotSnapshotNumber . ChangeHeadRedeemer <$> do
          mutatedSignature <- arbitrary :: Gen (Hydra.MultiSignature (Snapshot Tx))
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
              , closedAt = arbitrary `generateWith` 42
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
                , closedAt = arbitrary `generateWith` 42
                }
          )
          headTxOut
