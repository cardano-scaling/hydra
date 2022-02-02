{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.Contract.CollectCom where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import qualified Cardano.Api.UTxO as UTxO
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Hydra.Chain.Direct.Contract.Mutation (
  Mutation (..),
  SomeMutation (..),
  anyPayToPubKeyTxOut,
  genHash,
  headTxIn,
 )
import qualified Hydra.Chain.Direct.Fixture as Fixture
import Hydra.Chain.Direct.Tx (
  collectComTx,
  headValue,
  mkCommitDatum,
  mkHeadOutput,
  policyId,
 )
import qualified Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.HeadState as Head
import qualified Hydra.Data.Party as OnChain
import qualified Hydra.Data.Party as Party
import Hydra.Ledger.Cardano (CardanoTx, genAdaOnlyUTxO, genValue)
import Hydra.Party (Party, vkey)
import Plutus.Orphans ()
import Plutus.V1.Ledger.Api (fromData, toBuiltin, toData)
import Test.QuickCheck (oneof, suchThat)
import Test.QuickCheck.Instances ()
import qualified Prelude

--
-- CollectComTx
--

healthyCollectComTx :: (CardanoTx, UTxO)
healthyCollectComTx =
  (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton (headInput, headResolvedInput) <> UTxO (fst <$> commits)

  tx =
    collectComTx
      Fixture.testNetworkId
      (headInput, headDatum, healthyCollectComOnChainParties)
      commits

  committedUTxO =
    generateWith
      (replicateM (length healthyCollectComParties) genCommittableTxOut)
      42

  commits =
    (uncurry healthyCommitOutput <$> zip healthyCollectComParties committedUTxO)
      & Map.fromList

  headInput = generateWith arbitrary 42
  headResolvedInput = mkHeadOutput Fixture.testNetworkId (toUTxOContext $ mkTxOutDatum healthyCollectComInitialDatum)
  headDatum = fromPlutusData $ toData healthyCollectComInitialDatum

healthyCollectComInitialDatum :: Head.State
healthyCollectComInitialDatum =
  Head.Initial
    { contestationPeriod = generateWith arbitrary 42
    , parties = healthyCollectComOnChainParties
    }

healthyCollectComOnChainParties :: [OnChain.Party]
healthyCollectComOnChainParties =
  Party.partyFromVerKey . vkey <$> healthyCollectComParties

healthyCollectComParties :: [Party]
healthyCollectComParties = flip generateWith 42 $ do
  alice <- arbitrary
  bob <- arbitrary
  carol <- arbitrary
  pure [alice, bob, carol]

genCommittableTxOut :: Gen (TxIn, TxOut CtxUTxO AlonzoEra)
genCommittableTxOut =
  Prelude.head . UTxO.pairs <$> (genAdaOnlyUTxO `suchThat` (\u -> length u > 1))

healthyCommitOutput ::
  Party ->
  (TxIn, TxOut CtxUTxO AlonzoEra) ->
  (TxIn, (TxOut CtxUTxO AlonzoEra, ScriptData))
healthyCommitOutput party committed =
  ( generateWith arbitrary seed
  ,
    ( toCtxUTxOTxOut (TxOut commitAddress commitValue (mkTxOutDatum commitDatum))
    , fromPlutusData (toData commitDatum)
    )
  )
 where
  Party.UnsafeParty (fromIntegral -> seed) = Party.partyFromVerKey (vkey party)

  commitScript =
    fromPlutusScript Commit.validatorScript
  commitAddress =
    mkScriptAddress @PlutusScriptV1 Fixture.testNetworkId commitScript
  commitValue =
    mkTxOutValue $
      headValue <> (txOutValue . snd) committed
  commitDatum =
    mkCommitDatum party (Head.validatorHash policyId) (Just committed)

data CollectComMutation
  = MutateOpenOutputValue
  | MutateOpenUTxOHash
  | MutateHeadScriptInput
  | MutateHeadTransition
  deriving (Generic, Show, Enum, Bounded)

genCollectComMutation :: (CardanoTx, UTxO) -> Gen SomeMutation
genCollectComMutation (tx, utxo) =
  oneof
    [ SomeMutation MutateOpenOutputValue . ChangeOutput 0 <$> do
        mutatedValue <- (mkTxOutValue <$> genValue) `suchThat` (/= collectComOutputValue)
        pure $ TxOut collectComOutputAddress mutatedValue collectComOutputDatum
    , SomeMutation MutateOpenUTxOHash . ChangeOutput 0 <$> mutateUTxOHash
    , SomeMutation MutateHeadScriptInput . ChangeInput (headTxIn utxo) <$> anyPayToPubKeyTxOut
    , SomeMutation MutateHeadTransition <$> do
        changeRedeemer <- ChangeHeadRedeemer <$> (Head.Close 0 . toBuiltin <$> genHash <*> arbitrary)
        changeDatum <- ChangeHeadDatum <$> (Head.Open <$> arbitrary <*> (toBuiltin <$> genHash))
        pure $ Changes [changeRedeemer, changeDatum]
    ]
 where
  TxOut collectComOutputAddress collectComOutputValue collectComOutputDatum =
    fromJust $ txOuts' tx !!? 0

  mutateUTxOHash = do
    mutatedUTxOHash <- genHash
    -- NOTE / TODO:
    --
    -- This should probably be defined a 'Mutation', but it is different
    -- from 'ChangeHeadDatum'. The latter modifies the _input_ datum given
    -- to the script, whereas this one modifies the resulting head datum in
    -- the output.
    case collectComOutputDatum of
      TxOutDatumNone ->
        error "Unexpected empty head datum"
      (TxOutDatumHash _sdsie _ha) ->
        error "Unexpected hash-only datum"
      (TxOutDatum _sdsie sd) ->
        case fromData $ toPlutusData sd of
          (Just Head.Open{parties}) ->
            pure $
              TxOut
                collectComOutputAddress
                collectComOutputValue
                (mkTxOutDatum Head.Open{parties, Head.utxoHash = toBuiltin mutatedUTxOHash})
          (Just st) ->
            error $ "Unexpected state " <> show st
          Nothing ->
            error "Invalid data"
