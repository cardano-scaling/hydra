{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.Contract.Close.Healthy where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Hydra.Chain.Direct.Contract.Mutation (addParticipationTokens)
import Hydra.Chain.Direct.Fixture qualified as Fixture
import Hydra.Chain.Direct.State (splitUTxO)
import Hydra.Chain.Direct.TimeHandle (PointInTime)
import Hydra.Chain.Direct.Tx (mkHeadOutput)
import Hydra.ContestationPeriod (fromChain)
import Hydra.Crypto (HydraKey, MultiSignature, aggregate, sign)
import Hydra.Data.ContestationPeriod qualified as OnChain
import Hydra.Data.Party qualified as OnChain
import Hydra.Ledger (hashUTxO)
import Hydra.Ledger.Cardano (genOneUTxOFor, genVerificationKey)
import Hydra.Ledger.Cardano.Evaluate (genValidityBoundsFromContestationPeriod)
import Hydra.Party (Party, deriveParty, partyToChain)
import Hydra.Plutus.Orphans ()
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot)
import PlutusLedgerApi.V2 (BuiltinByteString, toBuiltin)
import Test.Hydra.Fixture (aliceSk, bobSk, carolSk, genForParty)
import Test.QuickCheck (elements)
import Test.QuickCheck.Instances ()

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

healthyConfirmedSnapshot :: Snapshot Tx -> ConfirmedSnapshot Tx
healthyConfirmedSnapshot snapshot =
  ConfirmedSnapshot
    { snapshot
    , signatures = healthySignature snapshot
    }
