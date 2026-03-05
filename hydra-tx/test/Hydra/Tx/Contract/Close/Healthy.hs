{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Tx.Contract.Close.Healthy where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Hydra.Data.ContestationPeriod qualified as OnChain
import Hydra.Data.Party qualified as OnChain
import Hydra.Plutus.Orphans ()
import Hydra.Tx (
  ConfirmedSnapshot (..),
  Party,
  Snapshot,
  deriveParty,
  hashUTxO,
  partyToChain,
 )
import Hydra.Tx.Close (PointInTime)
import Hydra.Tx.ContestationPeriod (fromChain)
import Hydra.Tx.Crypto (HydraKey, MultiSignature, aggregate, sign)
import Hydra.Tx.Init (mkHeadOutput)
import Hydra.Tx.Utils (splitUTxO)
import PlutusLedgerApi.V3 (BuiltinByteString, toBuiltin)
import Test.Hydra.Prelude
import Test.Hydra.Tx.Fixture (aliceSk, bobSk, carolSk)
import Test.Hydra.Tx.Fixture qualified as Fixture
import Test.Hydra.Tx.Gen (genForParty, genOneUTxOFor, genValidityBoundsFromContestationPeriod, genVerificationKey)
import Test.Hydra.Tx.Mutation (addParticipationTokens)
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
