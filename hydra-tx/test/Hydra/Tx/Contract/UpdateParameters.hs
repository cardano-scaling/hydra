{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Tx.Contract.UpdateParameters where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokens qualified as HeadTokens
import Hydra.Data.Party qualified as OnChain
import Hydra.Tx.ContestationPeriod (ContestationPeriod, toChain)
import Hydra.Tx.Crypto (HydraKey, MultiSignature (..), aggregate, sign)
import Hydra.Tx.HeadId (mkHeadId)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Init (mkHeadOutput)
import Hydra.Tx.IsTx (IsTx (hashUTxO))
import Hydra.Tx.OnChainId (OnChainId)
import Hydra.Tx.ParameterUpdate (ParameterUpdate (..))
import Hydra.Tx.Party (Party, deriveParty, partyToChain)
import Hydra.Tx.ScriptRegistry (registryUTxO)
import Hydra.Tx.Snapshot (Snapshot (..), SnapshotNumber, SnapshotVersion)
import Hydra.Tx.UpdateParameters (updateParametersTx)
import Hydra.Tx.Utils (adaOnly, verificationKeyToOnChainId)
import PlutusTx.Builtins (toBuiltin)
import Test.Hydra.Tx.Fixture (aliceSk, bobSk, carolSk, testNetworkId, testPolicyId, testSeedInput)
import Test.Hydra.Tx.Gen (
  genForParty,
  genScriptRegistry,
  genUTxOSized,
  genVerificationKey,
 )

-- * Healthy fixture

healthyUpdateParametersTx :: (Tx, UTxO)
healthyUpdateParametersTx =
  (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton headInput headOutput
      <> registryUTxO scriptRegistry

  tx =
    updateParametersTx
      scriptRegistry
      somePartyCardanoVerificationKey
      (testSeedInput, mkHeadId testPolicyId)
      parameters
      (headInput, headOutput)
      healthySnapshot
      healthySignature
      healthyParameterUpdate
      (HeadTokens.mkHeadTokenScript testSeedInput)

  parameters =
    HeadParameters
      { parties = healthyParties
      , contestationPeriod = healthyContestationPeriod
      }

  scriptRegistry = genScriptRegistry `generateWith` 42

  headInput = generateWith arbitrary 42

  -- All participation tokens for the current parties stay attached to the head
  -- output. The 'updateParametersTx' builder is what removes the leaving
  -- party's PT via a burn in the mint value.
  headOutput =
    mkHeadOutput @CtxUTxO
      testNetworkId
      testPolicyId
      (verificationKeyToOnChainId <$> healthyParticipants)
      (mkTxOutDatumInline healthyDatum)

somePartyCardanoVerificationKey :: VerificationKey PaymentKey
somePartyCardanoVerificationKey =
  -- We pick the first party (who stays). The leaving party's vk would also
  -- work but the leaver is not required to be online when the L1 tx is posted.
  case healthyParticipants of
    (vk : _) -> vk
    [] -> error "healthyParticipants is empty"

healthySigningKeys :: [SigningKey HydraKey]
healthySigningKeys = [aliceSk, bobSk, carolSk]

healthyParticipants :: [VerificationKey PaymentKey]
healthyParticipants =
  genForParty genVerificationKey <$> healthyParties

healthyParties :: [Party]
healthyParties = deriveParty <$> healthySigningKeys

healthyOnChainParties :: [OnChain.Party]
healthyOnChainParties = partyToChain <$> healthyParties

-- The party leaving is the last one in the list (Carol).
healthyLeavingParty :: Party
healthyLeavingParty = lastOfList healthyParties

healthyLeavingOnChainId :: OnChainId
healthyLeavingOnChainId =
  verificationKeyToOnChainId (lastOfList healthyParticipants)

-- Internal helper since 'last' is forbidden by the prelude. Errors on empty
-- input; only used over the hard-coded healthy fixture which always has 3
-- elements.
lastOfList :: [a] -> a
lastOfList = \case
  [] -> error "lastOfList: empty"
  [x] -> x
  (_ : xs) -> lastOfList xs

healthyParameterUpdate :: ParameterUpdate
healthyParameterUpdate =
  RemoveParty
    { leavingParty = healthyLeavingParty
    , leavingOnChainId = healthyLeavingOnChainId
    }

healthySnapshot :: Snapshot Tx
healthySnapshot =
  Snapshot
    { headId = mkHeadId testPolicyId
    , version = healthySnapshotVersion
    , number = succ healthySnapshotNumber
    , confirmed = []
    , utxo = healthyUTxO
    , utxoToCommit = Nothing
    , utxoToDecommit = Nothing
    , parameterUpdate = Just healthyParameterUpdate
    }

-- All parties (including the leaver) must sign the snapshot that authorizes
-- their own departure.
healthySignature :: MultiSignature (Snapshot Tx)
healthySignature = aggregate [sign sk healthySnapshot | sk <- healthySigningKeys]

healthySnapshotNumber :: SnapshotNumber
healthySnapshotNumber = 1

healthySnapshotVersion :: SnapshotVersion
healthySnapshotVersion = 1

healthyContestationPeriod :: ContestationPeriod
healthyContestationPeriod =
  arbitrary `generateWith` 42

healthyUTxO :: UTxO
healthyUTxO = UTxO.map adaOnly $ generateWith (genUTxOSized 3) 42

healthyDatum :: Head.State
healthyDatum =
  Head.Open
    Head.OpenDatum
      { headSeed = toPlutusTxOutRef testSeedInput
      , headId = toPlutusCurrencySymbol testPolicyId
      , utxoHash = toBuiltin $ hashUTxO @Tx healthyUTxO
      , parties = healthyOnChainParties
      , contestationPeriod = toChain healthyContestationPeriod
      , version = toInteger healthySnapshotVersion
      }
