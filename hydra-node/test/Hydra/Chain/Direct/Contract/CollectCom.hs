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
import Hydra.Chain.Direct.Fixture (testNetworkId, testPolicyId)
import Hydra.Chain.Direct.Tx (
  collectComTx,
  headValue,
  mkCommitDatum,
  mkHeadOutput,
 )
import qualified Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.HeadState as Head
import qualified Hydra.Data.Party as OnChain
import qualified Hydra.Data.Party as Party
import Hydra.Ledger.Cardano (genAdaOnlyUTxO, genValue)
import Hydra.Party (Party, vkey)
import Plutus.Orphans ()
import Plutus.V1.Ledger.Api (fromData, toBuiltin, toData)
import Test.QuickCheck (elements, oneof, suchThat)
import Test.QuickCheck.Instances ()
import qualified Prelude

--
-- CollectComTx
--

healthyCollectComTx :: (Tx, UTxO)
healthyCollectComTx =
  (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton (headInput, headResolvedInput) <> UTxO (fst <$> commits)

  tx =
    collectComTx
      testNetworkId
      (headInput, headResolvedInput, headDatum, healthyCollectComOnChainParties)
      commits

  committedUTxO =
    generateWith
      (replicateM (length healthyCollectComParties) genCommittableTxOut)
      42

  commits =
    (uncurry healthyCommitOutput <$> zip healthyCollectComParties committedUTxO)
      & Map.fromList

  headInput = generateWith arbitrary 42
  headResolvedInput = mkHeadOutput testNetworkId testPolicyId (toUTxOContext $ mkTxOutDatum healthyCollectComInitialDatum)
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

genCommittableTxOut :: Gen (TxIn, TxOut CtxUTxO)
genCommittableTxOut =
  Prelude.head . UTxO.pairs <$> (genAdaOnlyUTxO `suchThat` (\u -> length u > 1))

healthyCommitOutput ::
  Party ->
  (TxIn, TxOut CtxUTxO) ->
  (TxIn, (TxOut CtxUTxO, ScriptData))
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
    mkScriptAddress @PlutusScriptV1 testNetworkId commitScript
  commitValue =
    headValue <> (txOutValue . snd) committed
  commitDatum =
    mkCommitDatum party Head.validatorHash (Just committed)

data CollectComMutation
  = MutateOpenOutputValue
  | MutateOpenUTxOHash
  | MutateHeadScriptInput
  | MutateHeadTransition
  | MutateDropOneCommit
  deriving (Generic, Show, Enum, Bounded)

genCollectComMutation :: (Tx, UTxO) -> Gen SomeMutation
genCollectComMutation (tx, utxo) =
  oneof
    [ SomeMutation MutateOpenOutputValue . ChangeOutput 0 <$> do
        mutatedValue <- genValue `suchThat` (/= collectComOutputValue)
        pure $ TxOut collectComOutputAddress mutatedValue collectComOutputDatum
    , SomeMutation MutateOpenUTxOHash . ChangeOutput 0 <$> mutateUTxOHash
    , SomeMutation MutateHeadScriptInput . ChangeInput (headTxIn utxo) <$> anyPayToPubKeyTxOut
    , SomeMutation MutateHeadTransition <$> do
        changeRedeemer <- ChangeHeadRedeemer <$> (Head.Close 0 . toBuiltin <$> genHash <*> arbitrary)
        changeDatum <- ChangeHeadDatum <$> (Head.Open <$> arbitrary <*> (toBuiltin <$> genHash))
        pure $ Changes [changeRedeemer, changeDatum]
    , SomeMutation MutateDropOneCommit <$> do
        (commitTxIn, commitTxOut) <- findSomeCommitUTxO
        let headOutput = Prelude.head $ txOuts' tx
        let out' = modifyTxOutValue (\v -> v <> negateValue (txOutValue commitTxOut)) headOutput
        pure $
          Changes
            [ RemoveInput commitTxIn
            , ChangeOutput 0 out'
            ]
    ]
 where
  TxOut collectComOutputAddress collectComOutputValue collectComOutputDatum =
    fromJust $ txOuts' tx !!? 0

  findSomeCommitUTxO :: Gen (TxIn, TxOut CtxUTxO)
  findSomeCommitUTxO = do
    commitTxIn <- elements (txIns' tx)
    let commitTxOut = fromJust $ UTxO.resolve commitTxIn utxo
    pure (commitTxIn, commitTxOut)
      `suchThat` (\(_, o) -> txOutAddress o == commitAddress)

  commitAddress = undefined

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
      (TxOutDatumHash _ha) ->
        error "Unexpected hash-only datum"
      (TxOutDatum sd) ->
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
