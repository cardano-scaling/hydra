{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Tx.Contract.Contest.Healthy where

import "hydra-cardano-api" Hydra.Cardano.Api
import "hydra-prelude" Hydra.Prelude hiding (label)
import "hydra-test-utils" Test.Hydra.Prelude

import "QuickCheck" Test.QuickCheck (elements, suchThat)
import "cardano-api" Cardano.Api.UTxO qualified as UTxO
import "hydra-plutus" Hydra.Contract.HeadState qualified as Head
import "hydra-plutus" Hydra.Data.ContestationPeriod qualified as OnChain
import "hydra-plutus" Hydra.Data.Party qualified as OnChain
import "hydra-plutus-extras" Hydra.Plutus.Extras (posixFromUTCTime)
import "hydra-plutus-extras" Hydra.Plutus.Orphans ()
import "hydra-tx" Hydra.Ledger.Cardano.Time (slotNoToUTCTime)
import "hydra-tx" Hydra.Tx (registryUTxO)
import "hydra-tx" Hydra.Tx.Contest (ClosedThreadOutput (..), contestTx)
import "hydra-tx" Hydra.Tx.ContestationPeriod (ContestationPeriod, fromChain)
import "hydra-tx" Hydra.Tx.Crypto (HydraKey, MultiSignature, aggregate, sign)
import "hydra-tx" Hydra.Tx.HeadId (mkHeadId)
import "hydra-tx" Hydra.Tx.Init (mkHeadOutput)
import "hydra-tx" Hydra.Tx.IsTx (hashUTxO)
import "hydra-tx" Hydra.Tx.Party (Party, deriveParty, partyToChain)
import "hydra-tx" Hydra.Tx.Snapshot (Snapshot (..), SnapshotNumber, SnapshotVersion)
import "hydra-tx" Hydra.Tx.Utils (
  IncrementalAction (..),
  setIncrementalActionMaybe,
  splitUTxO,
 )
import "hydra-tx" Test.Hydra.Tx.Fixture (aliceSk, bobSk, carolSk, slotLength, systemStart, testNetworkId, testPolicyId)
import "hydra-tx" Test.Hydra.Tx.Gen (
  genForParty,
  genOneUTxOFor,
  genScriptRegistry,
  genVerificationKey,
 )
import "hydra-tx" Test.Hydra.Tx.Mutation (
  addParticipationTokens,
 )
import "plutus-ledger-api" PlutusLedgerApi.V2 (BuiltinByteString, toBuiltin)
import "quickcheck-instances" Test.QuickCheck.Instances ()

--
-- ContestTx
--

-- | Healthy contest tx where the contester is the first one to contest and
-- correctly pushing out the deadline by the contestation period.
healthyContestTx :: (Tx, UTxO)
healthyContestTx =
  (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton healthyClosedHeadTxIn healthyClosedHeadTxOut
      <> registryUTxO scriptRegistry

  tx =
    contestTx
      scriptRegistry
      healthyContesterVerificationKey
      (mkHeadId testPolicyId)
      healthyContestationPeriod
      healthyCloseSnapshotVersion
      healthyContestSnapshot
      (healthySignature healthyContestSnapshotNumber)
      (healthySlotNo, slotNoToUTCTime systemStart slotLength healthySlotNo)
      closedThreadOutput
      incrementalAction

  incrementalAction = fromMaybe NoThing $ setIncrementalActionMaybe (utxoToCommit healthyContestSnapshot) (utxoToDecommit healthyContestSnapshot)
  scriptRegistry = genScriptRegistry `generateWith` 42

  closedThreadOutput =
    ClosedThreadOutput
      { closedThreadUTxO = (healthyClosedHeadTxIn, healthyClosedHeadTxOut)
      , closedParties =
          healthyOnChainParties
      , closedContestationDeadline = posixFromUTCTime healthyContestationDeadline
      , closedContesters = []
      }

healthyContestSnapshotNumber :: SnapshotNumber
healthyContestSnapshotNumber = 4

healthyCloseSnapshotVersion :: SnapshotVersion
healthyCloseSnapshotVersion = 4

healthyClosedUTxO :: UTxO
healthyClosedUTxO =
  genOneUTxOFor healthyContesterVerificationKey `generateWith` 42

healthyContestUTxO :: UTxO
healthyContestUTxO =
  (genOneUTxOFor healthyContesterVerificationKey `suchThat` (/= healthyClosedUTxO))
    `generateWith` 42

splitContestUTxO :: (UTxO, UTxO)
splitContestUTxO = splitUTxO healthyContestUTxO

splitUTxOInHead :: UTxO
splitUTxOInHead = fst splitContestUTxO

splitUTxOToDecommit :: UTxO
splitUTxOToDecommit = snd splitContestUTxO

healthyContestSnapshot :: Snapshot Tx
healthyContestSnapshot =
  Snapshot
    { headId = mkHeadId testPolicyId
    , number = healthyContestSnapshotNumber
    , utxo = splitUTxOInHead
    , confirmed = []
    , utxoToCommit = Nothing
    , utxoToDecommit = Just splitUTxOToDecommit
    , version = healthyCloseSnapshotVersion
    }

healthyClosedState :: Head.State
healthyClosedState =
  Head.Closed
    Head.ClosedDatum
      { snapshotNumber = fromIntegral healthyClosedSnapshotNumber
      , utxoHash = healthyClosedUTxOHash
      , alphaUTxOHash = mempty
      , omegaUTxOHash = mempty
      , parties = healthyOnChainParties
      , contestationDeadline = posixFromUTCTime healthyContestationDeadline
      , contestationPeriod = healthyOnChainContestationPeriod
      , headId = toPlutusCurrencySymbol testPolicyId
      , contesters = []
      , version = toInteger healthyCloseSnapshotVersion
      }

healthyContestUTxOHash :: BuiltinByteString
healthyContestUTxOHash =
  toBuiltin $ hashUTxO @Tx splitUTxOInHead

healthyContestUTxOToDecommitHash :: BuiltinByteString
healthyContestUTxOToDecommitHash =
  toBuiltin $ hashUTxO @Tx splitUTxOToDecommit

healthyClosedUTxOHash :: BuiltinByteString
healthyClosedUTxOHash =
  toBuiltin $ hashUTxO @Tx healthyClosedUTxO

healthyClosedSnapshotNumber :: SnapshotNumber
healthyClosedSnapshotNumber = 3

healthySlotNo :: SlotNo
healthySlotNo = arbitrary `generateWith` 42

healthyClosedHeadTxIn :: TxIn
healthyClosedHeadTxIn = generateWith arbitrary 42

healthyClosedHeadTxOut :: TxOut CtxUTxO
healthyClosedHeadTxOut =
  mkHeadOutput testNetworkId testPolicyId headTxOutDatum
    & addParticipationTokens healthyParticipants
 where
  headTxOutDatum :: TxOutDatum CtxUTxO
  headTxOutDatum = mkTxOutDatumInline healthyClosedState

healthyOnChainContestationPeriod :: OnChain.ContestationPeriod
healthyOnChainContestationPeriod = OnChain.contestationPeriodFromDiffTime $ fromInteger healthyContestationPeriodSeconds

healthyContestationPeriod :: ContestationPeriod
healthyContestationPeriod = fromChain healthyOnChainContestationPeriod

healthyContestationPeriodSeconds :: Integer
healthyContestationPeriodSeconds = 10

healthyParticipants :: [VerificationKey PaymentKey]
healthyParticipants =
  genForParty genVerificationKey <$> healthyParties

healthyContesterVerificationKey :: VerificationKey PaymentKey
healthyContesterVerificationKey =
  elements healthyParticipants `generateWith` 42

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

healthyContestationDeadline :: UTCTime
healthyContestationDeadline =
  addUTCTime
    (fromInteger healthyContestationPeriodSeconds)
    (slotNoToUTCTime systemStart slotLength healthySlotNo)
