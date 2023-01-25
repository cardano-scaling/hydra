{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Close transaction mutation tests starting at the health case of closing
-- with the initial snapshot.
module Hydra.Chain.Direct.Contract.CloseInitial where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Cardano.Api.UTxO as UTxO
import Data.Maybe (fromJust)
import Hydra.Chain.Direct.Contract.Mutation (Mutation (..), SomeMutation (..), addParticipationTokens, changeHeadOutputDatum)
import Hydra.Chain.Direct.Fixture (genForParty, testNetworkId)
import qualified Hydra.Chain.Direct.Fixture as Fixture
import Hydra.Chain.Direct.Tx (ClosingSnapshot (..), OpenThreadOutput (..), UTxOHash (UTxOHash), closeTx, mkHeadId, mkHeadOutput)
import Hydra.ContestationPeriod (fromChain)
import qualified Hydra.Contract.HeadState as Head
import Hydra.Crypto (HydraKey)
import Hydra.Data.ContestationPeriod (posixFromUTCTime)
import qualified Hydra.Data.ContestationPeriod as OnChain
import qualified Hydra.Data.Party as OnChain
import Hydra.Ledger (hashUTxO)
import Hydra.Ledger.Cardano (genOneUTxOFor, genVerificationKey)
import Hydra.Ledger.Cardano.Evaluate (genValidityBoundsFromContestationPeriod, slotNoToUTCTime)
import Hydra.Party (Party, deriveParty, partyToChain)
import Plutus.Orphans ()
import Plutus.V2.Ledger.Api (toBuiltin, toData)
import Test.Hydra.Fixture (aliceSk, bobSk, carolSk)
import Test.QuickCheck (elements, oneof, suchThat)
import Test.QuickCheck.Instances ()

healthyCloseInitialTx :: (Tx, UTxO)
healthyCloseInitialTx =
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

  headDatum = fromPlutusData $ toData healthyOpenHeadDatum

  openThreadOutput =
    OpenThreadOutput
      { openThreadUTxO = (healthyOpenHeadTxIn, healthyOpenHeadTxOut, headDatum)
      , openParties = healthyOnChainParties
      , openContestationPeriod = healthyContestationPeriod
      }

-- TODO: INLINE or throw away all thse healthy things when not used

healthyOpenHeadTxIn :: TxIn
healthyOpenHeadTxIn = generateWith arbitrary 42

healthyOpenHeadTxOut :: TxOut CtxUTxO
healthyOpenHeadTxOut =
  mkHeadOutput testNetworkId Fixture.testPolicyId headTxOutDatum
    & addParticipationTokens healthyParties
 where
  headTxOutDatum = toUTxOContext (mkTxOutDatum healthyOpenHeadDatum)

-- FIXME: This is not a healthy value anyhow related to the 'healthyCloseInitialTx' above
healthySlotNo :: SlotNo
healthySlotNo = arbitrary `generateWith` 42

healthyClosingSnapshot :: ClosingSnapshot
healthyClosingSnapshot =
  CloseWithInitialSnapshot
    { openUtxoHash = UTxOHash $ hashUTxO @Tx healthyUTxO
    }

healthyOpenHeadDatum :: Head.State
healthyOpenHeadDatum =
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

healthyContestationDeadline :: UTCTime
healthyContestationDeadline =
  addUTCTime
    (fromInteger healthyContestationPeriodSeconds)
    (slotNoToUTCTime healthySlotNo)

data CloseMutation
  = MutateCloseContestationDeadline
  | MutateCloseContestationDeadlineWithZero
  deriving (Generic, Show, Enum, Bounded)

-- TODO: THESE SHOULD ACTUALLY BE PART OF Close.hs in a generic enough way to
-- not duplicate these mutations.
genCloseInitialMutation :: (Tx, UTxO) -> Gen SomeMutation
genCloseInitialMutation (tx, _utxo) =
  oneof
    [ SomeMutation (Just "incorrect closed contestation deadline") MutateCloseContestationDeadline . ChangeOutput 0
        <$> (mutateClosedContestationDeadline =<< arbitrary @Integer `suchThat` (/= healthyContestationPeriodSeconds))
    , SomeMutation (Just "incorrect closed contestation deadline") MutateCloseContestationDeadlineWithZero . ChangeOutput 0
        <$> mutateClosedContestationDeadline 0
    ]
 where
  headTxOut = fromJust $ txOuts' tx !!? 0

  -- In case contestation period param is 'Nothing' we will generate arbitrary value
  mutateClosedContestationDeadline :: Integer -> Gen (TxOut CtxTx)
  mutateClosedContestationDeadline contestationPeriodSeconds = do
    -- NOTE: we need to be sure the generated contestation period is large enough to have an impact on the on-chain
    -- deadline computation, which means having a resolution of seconds instead of the default picoseconds
    pure $ changeHeadOutputDatum (mutateContestationDeadline contestationPeriodSeconds) headTxOut

  mutateContestationDeadline contestationPeriod = \case
    Head.Closed{snapshotNumber, utxoHash, parties, headId} ->
      Head.Closed
        { snapshotNumber
        , utxoHash
        , parties
        , contestationDeadline =
            let closingTime = slotNoToUTCTime healthySlotNo
             in posixFromUTCTime $ addUTCTime (fromInteger contestationPeriod) closingTime
        , headId
        }
    st -> error $ "unexpected state " <> show st
