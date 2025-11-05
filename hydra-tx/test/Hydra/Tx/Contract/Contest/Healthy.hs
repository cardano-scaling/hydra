{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Tx.Contract.Contest.Healthy where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Cardano.Api.UTxO qualified as UTxO
import Hydra.Contract.HeadState qualified as Head
import Hydra.Data.ContestationPeriod qualified as OnChain
import Hydra.Data.Party qualified as OnChain
import Hydra.Ledger.Cardano.Time (slotNoToUTCTime)
import Hydra.Plutus.Extras (posixFromUTCTime)
import Hydra.Plutus.Orphans ()
import Hydra.Tx (registryUTxO)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Contest (ClosedThreadOutput (..), contestTx)
import Hydra.Tx.ContestationPeriod (ContestationPeriod, fromChain)
import Hydra.Tx.Crypto (HydraKey, MultiSignature, aggregate, sign)
import Hydra.Tx.HeadId (mkHeadId)
import Hydra.Tx.Init (mkHeadOutput)
import Hydra.Tx.IsTx (hashUTxO)
import Hydra.Tx.Party (Party, deriveParty, partyToChain)
import Hydra.Tx.Snapshot (Snapshot (..), SnapshotNumber, SnapshotVersion)
import Hydra.Tx.Utils (
  IncrementalAction (..),
  setIncrementalActionMaybe,
  splitUTxO,
 )
import PlutusLedgerApi.V2 (BuiltinByteString, toBuiltin)
import Test.Hydra.Tx.Fixture (aliceSk, bobSk, carolSk, slotLength, systemStart, testNetworkId, testPolicyId)
import Test.Hydra.Tx.Gen (
  genForParty,
  genOneUTxOFor,
  genScriptRegistry,
  genVerificationKey,
 )
import Test.Hydra.Tx.Mutation (
  addParticipationTokens,
 )
import Test.QuickCheck (elements, suchThat)
import Test.QuickCheck.Instances ()

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
  let utxoHash = hashUTxO splitUTxOInHead
   in Snapshot
        { headId = mkHeadId testPolicyId
        , number = healthyContestSnapshotNumber
        , utxo = splitUTxOInHead
        , utxoHash
        , confirmed = []
        , utxoToCommit = Nothing
        , utxoToDecommit = Just splitUTxOToDecommit
        , version = healthyCloseSnapshotVersion
        , accumulator = Accumulator.build [utxoHash, hashUTxO @Tx mempty, hashUTxO splitUTxOToDecommit]
        , crs = ""
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
      , accumulatorHash = toBuiltin ("" :: ByteString) -- TODO: Proper accumulator hash
      , crs = toBuiltin ("" :: ByteString) -- TODO: Proper CRS
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
