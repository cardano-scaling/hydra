{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Chain.Direct.Contract.Decrement where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Cardano.Api.UTxO as UTxO
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct.Fixture (testNetworkId, testPolicyId)
import Hydra.Chain.Direct.ScriptRegistry (genScriptRegistry, registryUTxO)
import Hydra.Chain.Direct.Tx (
  decrementTx,
  mkHeadId,
  mkHeadOutput,
 )
import Hydra.ContestationPeriod (ContestationPeriod)
import Hydra.Contract.HeadState qualified as Head
import Hydra.Crypto (MultiSignature (..))
import Hydra.Ledger (IsTx (hashUTxO, withoutUTxO))
import Hydra.Ledger.Cardano (
  adaOnly,
  genTxOut,
  genVerificationKey,
 )
import Hydra.Party (Party, partyToChain, vkey)
import Hydra.Plutus.Orphans ()
import Hydra.Snapshot (Snapshot (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Hydra.Fixture (genForParty)
import Test.QuickCheck (elements)
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

healthySnapshot :: Snapshot Tx
healthySnapshot =
  let (utxoToDecommit', utxo) = splitDecommitUTxO healthyUTxO
   in Snapshot
        { headId = mkHeadId testPolicyId
        , number = 1
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
    , parties =
        partyToChain <$> healthyParties
    , contestationPeriod = 10
    , headId = toPlutusCurrencySymbol testPolicyId
    }

healthyParties :: [Party]
healthyParties =
  [ generateWith arbitrary i | i <- [1 .. 3]
  ]
