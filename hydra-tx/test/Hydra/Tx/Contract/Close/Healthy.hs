{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Tx.Contract.Close.Healthy where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Cardano.Api.UTxO qualified as UTxO
import Hydra.Data.ContestationPeriod qualified as OnChain
import Hydra.Data.Party qualified as OnChain
import Hydra.Plutus.Orphans ()
import Hydra.Tx (
  ConfirmedSnapshot (..),
  Party,
  Snapshot,
  deriveParty,
  partyToChain,
 )
import Hydra.Tx.Close (PointInTime)
import Hydra.Tx.ContestationPeriod (fromChain)
import Hydra.Tx.Crypto (HydraKey, MultiSignature, aggregate, sign)
import Hydra.Tx.Init (mkHeadOutput)
import Hydra.Tx.Secret (Secret)
import Hydra.Tx.Utils (splitUTxO, verificationKeyToOnChainId)

import Test.Hydra.Prelude
import Test.Hydra.Tx.Fixture (aliceSk, bobSk, carolSk)
import Test.Hydra.Tx.Fixture qualified as Fixture
import Test.Hydra.Tx.Gen (genForParty, genOneUTxOFor, genValidityBoundsFromContestationPeriod, genVerificationKey)
import Test.QuickCheck (elements)
import Test.QuickCheck.Instances ()

healthySeed :: Int
healthySeed = 42

healthyUTxO :: UTxO
healthyUTxO = genOneUTxOFor somePartyCardanoVerificationKey `generateWith` healthySeed

-- | The headAdaOverhead for healthy close tests. The test head output has only
-- tokens (0 lovelace), so this equals the negation of the total UTxO lovelace.
healthyHeadAdaOverhead :: Integer
healthyHeadAdaOverhead =
  let Coin n = selectLovelace (UTxO.totalValue healthySplitUTxOInHead)
   in negate n

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
healthyOpenHeadTxOut =
  mkHeadOutput
    Fixture.testNetworkId
    Fixture.testPolicyId
    (verificationKeyToOnChainId <$> healthyParticipants)

healthyContestationPeriodSeconds :: Integer
healthyContestationPeriodSeconds = 10

healthyContestationPeriod :: OnChain.ContestationPeriod
healthyContestationPeriod = OnChain.contestationPeriodFromDiffTime $ fromInteger healthyContestationPeriodSeconds

healthyContestationDeadline :: UTCTime
healthyContestationDeadline =
  addUTCTime
    (fromInteger healthyContestationPeriodSeconds)
    (snd healthyCloseUpperBoundPointInTime)

healthyParticipants :: [VerificationKey PaymentKey]
healthyParticipants =
  genForParty genVerificationKey <$> healthyParties

somePartyCardanoVerificationKey :: VerificationKey PaymentKey
somePartyCardanoVerificationKey =
  elements healthyParticipants `generateWith` healthySeed

healthySigningKeys :: [Secret (SigningKey HydraKey)]
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
