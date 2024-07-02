{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.Contract.Close.Healthy where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Hydra.Chain.Direct.Contract.Mutation (
  addParticipationTokens,
 )
import Hydra.Chain.Direct.Fixture qualified as Fixture
import Hydra.Chain.Direct.State (splitUTxO)
import Hydra.Chain.Direct.TimeHandle (PointInTime)
import Hydra.Chain.Direct.Tx (mkHeadId, mkHeadOutput, ClosingSnapshot (..), UTxOHash (..), OpenThreadOutput (..), closeTx)
import Hydra.ContestationPeriod (fromChain)
import Hydra.Contract.HeadState qualified as Head
import Hydra.Crypto (HydraKey, MultiSignature, aggregate, sign)
import Hydra.Data.ContestationPeriod qualified as OnChain
import Hydra.Data.Party qualified as OnChain
import Hydra.Ledger (hashUTxO)
import Hydra.Ledger.Cardano (genOneUTxOFor, genVerificationKey)
import Hydra.Ledger.Cardano.Evaluate (genValidityBoundsFromContestationPeriod)
import Hydra.Party (Party, deriveParty, partyToChain)
import Hydra.Plutus.Orphans ()
import Hydra.Snapshot as Snapshot (Snapshot (..), SnapshotNumber, SnapshotVersion)
import PlutusLedgerApi.V2 (toBuiltin, BuiltinByteString)
import Test.Hydra.Fixture (aliceSk, bobSk, carolSk, genForParty)
import Test.QuickCheck (elements)
import Test.QuickCheck.Instances ()
import qualified Cardano.Api.UTxO as UTxO
import Hydra.Chain.Direct.ScriptRegistry (registryUTxO, genScriptRegistry, ScriptRegistry)

healthySeed :: Int
healthySeed = 42

healthyUTxO :: UTxO
healthyUTxO = genOneUTxOFor somePartyCardanoVerificationKey `generateWith` healthySeed

healthySplitUTxOInHead :: UTxO
healthySplitUTxOToDecommit :: UTxO
(healthySplitUTxOInHead, healthySplitUTxOToDecommit) = splitUTxO healthyUTxO

-- NOTE: We need to use the contestation period when generating start/end tx
-- validity slots/time since if tx validity bound difference is bigger than
-- contestation period our close validator will fail
healthyCloseLowerBoundSlot :: SlotNo
healthyCloseUpperBoundPointInTime :: PointInTime
(healthyCloseLowerBoundSlot, healthyCloseUpperBoundPointInTime) =
  genValidityBoundsFromContestationPeriod (fromChain healthyContestationPeriod) `generateWith` healthySeed

healthyOpenHeadTxIn :: TxIn
healthyOpenHeadTxIn = generateWith arbitrary healthySeed

healthyOpenHeadTxOut :: TxOutDatum CtxUTxO -> TxOut CtxUTxO
healthyOpenHeadTxOut headTxOutDatum =
  mkHeadOutput Fixture.testNetworkId Fixture.testPolicyId headTxOutDatum
    & addParticipationTokens healthyParticipants

healthyContestationPeriodSeconds :: Integer
healthyContestationPeriodSeconds = 10

healthyContestationPeriod :: OnChain.ContestationPeriod
healthyContestationPeriod = OnChain.contestationPeriodFromDiffTime $ fromInteger healthyContestationPeriodSeconds

healthyContestationDeadline :: UTCTime
healthyContestationDeadline =
  addUTCTime
    (fromInteger healthyContestationPeriodSeconds)
    (snd healthyCloseUpperBoundPointInTime)

healthyCloseUTxOHash :: BuiltinByteString
healthyCloseUTxOHash =
  toBuiltin $ hashUTxO @Tx healthySplitUTxOInHead

healthyParticipants :: [VerificationKey PaymentKey]
healthyParticipants =
  genForParty genVerificationKey <$> healthyParties

somePartyCardanoVerificationKey :: VerificationKey PaymentKey
somePartyCardanoVerificationKey =
  elements healthyParticipants `generateWith` healthySeed

healthySigningKeys :: [SigningKey HydraKey]
healthySigningKeys = [aliceSk, bobSk, carolSk]

healthyParties :: [Party]
healthyParties = deriveParty <$> healthySigningKeys

healthyOnChainParties :: [OnChain.Party]
healthyOnChainParties = partyToChain <$> healthyParties

healthySignature :: Snapshot Tx -> MultiSignature (Snapshot Tx)
healthySignature snapshot = aggregate [sign sk snapshot | sk <- healthySigningKeys]

healthySnapshot :: SnapshotNumber -> SnapshotVersion -> Snapshot Tx
healthySnapshot number version =
  Snapshot
    { headId = mkHeadId Fixture.testPolicyId
    , number
    , utxo = healthySplitUTxOInHead
    , confirmed = []
    , -- XXX even after observing a decrement tx,
      -- the snapshot still contains something to decommit.
      utxoToDecommit = Just healthySplitUTxOToDecommit
    , version
    }

healthyOpenDatum :: Snapshot Tx -> Head.State
healthyOpenDatum Snapshot{version, number} =
  Head.Open
    { parties = healthyOnChainParties
    , utxoHash = toBuiltin $ hashUTxO @Tx healthySplitUTxOInHead
    , snapshotNumber = toInteger number
    , contestationPeriod = healthyContestationPeriod
    , headId = toPlutusCurrencySymbol Fixture.testPolicyId
    , version = toInteger version
    }

healthyConfirmedClosingSnapshot :: Snapshot Tx -> ClosingSnapshot
healthyConfirmedClosingSnapshot snapshot =
  CloseWithConfirmedSnapshot
    { snapshotNumber = number snapshot
    , closeUtxoHash = UTxOHash $ hashUTxO @Tx $ utxo snapshot
    , closeUtxoToDecommitHash = UTxOHash $ hashUTxO @Tx $ fromMaybe mempty $ utxoToDecommit snapshot
    , signatures = healthySignature snapshot
    , version = Snapshot.version snapshot
    }

healthyConfirmedClosingSnapshotTx :: Snapshot Tx -> (Tx, UTxO)
healthyConfirmedClosingSnapshotTx snapshot@Snapshot{version} =
  (tx, lookupUTxO)
 where
  tx :: Tx
  tx =
    closeTx
      scriptRegistry
      somePartyCardanoVerificationKey
      (healthyConfirmedClosingSnapshot snapshot)
      healthyCloseLowerBoundSlot
      healthyCloseUpperBoundPointInTime
      openThreadOutput
      (mkHeadId Fixture.testPolicyId)
      version

  datum :: TxOutDatum CtxUTxO
  datum = toUTxOContext $ mkTxOutDatumInline $ healthyOpenDatum snapshot

  lookupUTxO :: UTxO' (TxOut CtxUTxO)
  lookupUTxO =
    UTxO.singleton (healthyOpenHeadTxIn, healthyOpenHeadTxOut datum)
      <> registryUTxO scriptRegistry

  scriptRegistry :: ScriptRegistry
  scriptRegistry = genScriptRegistry `generateWith` healthySeed

  openThreadOutput :: OpenThreadOutput
  openThreadOutput =
    OpenThreadOutput
      { openThreadUTxO = (healthyOpenHeadTxIn, healthyOpenHeadTxOut datum)
      , openParties = healthyOnChainParties
      , openContestationPeriod = healthyContestationPeriod
      }
