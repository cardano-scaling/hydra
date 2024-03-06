{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Chain.Direct.Contract.Decrement where

import Hydra.Cardano.Api
import Hydra.Chain.Direct.Contract.Mutation (
  Mutation (..),
  SomeMutation (..),
  modifyInlineDatum,
  replaceParties,
  replaceSnapshotNumberInOpen,
 )
import Hydra.Prelude hiding (label)

import Cardano.Api.UTxO as UTxO
import Data.Maybe (fromJust)
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct.Fixture (testNetworkId, testPolicyId)
import Hydra.Chain.Direct.ScriptRegistry (genScriptRegistry, registryUTxO)
import Hydra.Chain.Direct.Tx (
  decrementTx,
  mkHeadId,
  mkHeadOutput,
 )
import Hydra.ContestationPeriod (ContestationPeriod, toChain)
import Hydra.Contract.Error (ToErrorCode (..))
import Hydra.Contract.HeadError (HeadError (..))
import Hydra.Contract.HeadState qualified as Head
import Hydra.Crypto (MultiSignature (..))
import Hydra.Data.Party qualified as OnChain
import Hydra.Ledger (IsTx (hashUTxO, withoutUTxO))
import Hydra.Ledger.Cardano (
  adaOnly,
  genTxOut,
  genVerificationKey,
 )
import Hydra.Party (Party, partyToChain, vkey)
import Hydra.Plutus.Orphans ()
import Hydra.Snapshot (Snapshot (..), SnapshotNumber)
import PlutusTx.Builtins (toBuiltin)
import Test.Hydra.Fixture (genForParty)
import Test.QuickCheck (arbitrarySizedNatural, elements, oneof)
import Test.QuickCheck.Gen (suchThat)
import Test.QuickCheck.Instances ()

healthyDecrementTx :: (Tx, UTxO)
healthyDecrementTx =
  (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton (headInput, headOutput)
      <> registryUTxO scriptRegistry

  tx =
    decrementTx
      scriptRegistry
      somePartyCardanoVerificationKey
      (mkHeadId testPolicyId)
      parameters
      (headInput, headOutput)
      healthySnapshot
      multisig

  multisig = HydraMultiSignature $ arbitrary `generateWith` 42

  parameters =
    HeadParameters
      { parties = healthyParties
      , contestationPeriod = healthyContestationPeriod
      }

  somePartyCardanoVerificationKey =
    elements healthyParticipants `generateWith` 42

  scriptRegistry = genScriptRegistry `generateWith` 42

  headInput = generateWith arbitrary 42

  headOutput' =
    mkHeadOutput testNetworkId testPolicyId (toUTxOContext $ mkTxOutDatumInline healthyDatum)

  headOutput = modifyTxOutValue (<> participationTokens) headOutput'

  participationTokens =
    valueFromList $
      map
        ( \party ->
            (AssetId testPolicyId (AssetName . serialiseToRawBytes . verificationKeyHash . vkey $ party), 1)
        )
        healthyParties

healthySnapshotNumber :: SnapshotNumber
healthySnapshotNumber = 1

healthySnapshot :: Snapshot Tx
healthySnapshot =
  let (utxoToDecommit', utxo) = splitDecommitUTxO healthyUTxO
   in Snapshot
        { headId = mkHeadId testPolicyId
        , number = succ healthySnapshotNumber
        , utxo
        , confirmed = []
        , utxoToDecommit = Just utxoToDecommit'
        }

splitDecommitUTxO :: UTxO -> (UTxO, UTxO)
splitDecommitUTxO utxo =
  case UTxO.pairs utxo of
    [] -> error "empty utxo in splitDecommitUTxO"
    (decommit : _rest) ->
      let decommitUTxO' = UTxO.fromPairs [decommit]
       in (utxo `withoutUTxO` decommitUTxO', decommitUTxO')

healthyContestationPeriod :: ContestationPeriod
healthyContestationPeriod =
  arbitrary `generateWith` 42

healthyParticipants :: [VerificationKey PaymentKey]
healthyParticipants =
  genForParty genVerificationKey <$> healthyParties

healthyUTxO :: UTxO
healthyUTxO =
  adaOnly
    <$> generateWith
      (UTxO.fromPairs . (: []) <$> ((,) <$> genTxIn <*> genTxOut))
      42

healthyDatum :: Head.State
healthyDatum =
  Head.Open
    { utxoHash = toBuiltin $ hashUTxO @Tx healthyUTxO
    , parties = healthyOnChainParties
    , contestationPeriod = toChain healthyContestationPeriod
    , snapshotNumber = toInteger healthySnapshotNumber
    , headId = toPlutusCurrencySymbol testPolicyId
    }

healthyParties :: [Party]
healthyParties =
  [ generateWith arbitrary i | i <- [1 .. 3]
  ]

healthyOnChainParties :: [OnChain.Party]
healthyOnChainParties = partyToChain <$> healthyParties

data DecrementMutation
  = -- | Ensures parties do not change between head input datum and head output
    --  datum.
    MutatePartiesInOutput
  | -- | Invalidates the tx by changing the snapshot number in resulting head
    -- output.
    --
    -- Ensures the snapshot number is aligned.
    MutateSnapshotNumber
  deriving stock (Generic, Show, Enum, Bounded)

genDecrementMutation :: (Tx, UTxO) -> Gen SomeMutation
genDecrementMutation (tx, _utxo) =
  oneof
    [ SomeMutation (Just $ toErrorCode ChangedParameters) MutatePartiesInOutput <$> do
        mutatedParties <- arbitrary `suchThat` (/= healthyOnChainParties)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceParties mutatedParties) headTxOut
    , SomeMutation (Just $ toErrorCode SnapshotNumberMismatch) MutateSnapshotNumber <$> do
        mutatedSnapshotNumber <- arbitrarySizedNatural `suchThat` (< healthySnapshotNumber)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotNumberInOpen $ toInteger mutatedSnapshotNumber) headTxOut
    ]
 where
  headTxOut = fromJust $ txOuts' tx !!? 0
