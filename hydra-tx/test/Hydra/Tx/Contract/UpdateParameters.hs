{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Tx.Contract.UpdateParameters where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokens qualified as HeadTokens
import Hydra.Data.Party qualified as OnChain
import Hydra.Tx.Accumulator qualified as Accumulator
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
    , accumulator = healthyAccumulator
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

-- | Accumulator built over the snapshot UTxO. 'UpdateParameters' does not
-- change the snapshotted UTxO set so this is reused on both sides of the
-- transition for the on-chain 'accumulatorHash'.
healthyAccumulator :: Accumulator.HydraAccumulator
healthyAccumulator = Accumulator.buildFromSnapshotUTxOs healthyUTxO Nothing Nothing

-- * Add-party healthy fixture

--
-- A symmetric fixture for the AddParty branch: an initial two-party head
-- (alice + bob), with a third party (carol) joining. The validator must
-- mint exactly carol's PT, append carol to 'parties', and grow the head
-- output value by the new PT.

healthyAddPartyTx :: (Tx, UTxO)
healthyAddPartyTx =
  (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton headInput headOutput
      <> registryUTxO scriptRegistry

  tx =
    updateParametersTx
      scriptRegistry
      somePartyCardanoVerificationKeyExisting
      (testSeedInput, mkHeadId testPolicyId)
      parametersBefore
      (headInput, headOutput)
      healthyAddSnapshot
      healthyAddSignature
      healthyAddParameterUpdate
      (HeadTokens.mkHeadTokenScript testSeedInput)

  parametersBefore =
    HeadParameters
      { parties = existingParties
      , contestationPeriod = healthyContestationPeriod
      }

  scriptRegistry = genScriptRegistry `generateWith` 43

  headInput = generateWith arbitrary 43

  -- The pre-Add head output carries only the existing parties' PTs.
  headOutput =
    mkHeadOutput @CtxUTxO
      testNetworkId
      testPolicyId
      (verificationKeyToOnChainId <$> existingParticipantsVKs)
      (mkTxOutDatumInline healthyAddDatumBefore)

somePartyCardanoVerificationKeyExisting :: VerificationKey PaymentKey
somePartyCardanoVerificationKeyExisting =
  case existingParticipantsVKs of
    (vk : _) -> vk
    [] -> error "existingParticipantsVKs is empty"

existingSigningKeys :: [SigningKey HydraKey]
existingSigningKeys = [aliceSk, bobSk]

existingParties :: [Party]
existingParties = deriveParty <$> existingSigningKeys

existingParticipantsVKs :: [VerificationKey PaymentKey]
existingParticipantsVKs = genForParty genVerificationKey <$> existingParties

healthyJoiningSk :: SigningKey HydraKey
healthyJoiningSk = carolSk

healthyJoiningParty :: Party
healthyJoiningParty = deriveParty healthyJoiningSk

healthyJoiningOnChainId :: OnChainId
healthyJoiningOnChainId = verificationKeyToOnChainId (genForParty genVerificationKey healthyJoiningParty)

healthyAddParameterUpdate :: ParameterUpdate
healthyAddParameterUpdate =
  AddParty
    { joiningParty = healthyJoiningParty
    , joiningOnChainId = healthyJoiningOnChainId
    , joiningHost = "127.0.0.1:5003"
    }

healthyAddSnapshot :: Snapshot Tx
healthyAddSnapshot =
  Snapshot
    { headId = mkHeadId testPolicyId
    , version = healthySnapshotVersion
    , number = succ healthySnapshotNumber
    , confirmed = []
    , utxo = healthyUTxO
    , utxoToCommit = Nothing
    , utxoToDecommit = Nothing
    , accumulator = healthyAccumulator
    , parameterUpdate = Just healthyAddParameterUpdate
    }

-- Only the existing parties sign the snapshot that authorizes the join;
-- the joining party's consent is captured out-of-band (they have to start
-- participating in the head after the L1 tx is observed). See the design
-- note above 'checkUpdateParameters'.
healthyAddSignature :: MultiSignature (Snapshot Tx)
healthyAddSignature =
  aggregate [sign sk healthyAddSnapshot | sk <- existingSigningKeys]

healthyAddDatumBefore :: Head.State
healthyAddDatumBefore =
  Head.Open
    Head.OpenDatum
      { headSeed = toPlutusTxOutRef testSeedInput
      , headId = toPlutusCurrencySymbol testPolicyId
      , utxoHash = toBuiltin $ hashUTxO @Tx healthyUTxO
      , accumulatorHash = toBuiltin $ Accumulator.getAccumulatorHash healthyAccumulator
      , parties = partyToChain <$> existingParties
      , contestationPeriod = toChain healthyContestationPeriod
      , version = toInteger healthySnapshotVersion
      }

healthyDatum :: Head.State
healthyDatum =
  Head.Open
    Head.OpenDatum
      { headSeed = toPlutusTxOutRef testSeedInput
      , headId = toPlutusCurrencySymbol testPolicyId
      , utxoHash = toBuiltin $ hashUTxO @Tx healthyUTxO
      , accumulatorHash = toBuiltin $ Accumulator.getAccumulatorHash healthyAccumulator
      , parties = healthyOnChainParties
      , contestationPeriod = toChain healthyContestationPeriod
      , version = toInteger healthySnapshotVersion
      }
