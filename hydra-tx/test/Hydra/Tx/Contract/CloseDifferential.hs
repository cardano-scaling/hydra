{-# LANGUAGE OverloadedRecordDot #-}

-- | Differential test (Tier 2): run the Agda-extracted decidable reference checker
-- ('Hydra.Agda.Reference.checkClose') and the real Plutus validator on the same close
-- transactions, and assert they agree on the decidable layer.
--
-- Property: for the healthy close tx and every generated close mutation,
-- @reference-rejects ⇒ validator-rejects@ (the reference accepts everything spec-valid, proved
-- in spec/src/Hydra/Protocol/ReferenceBridge.agda, so a reference reject means a spec-level
-- decidable check failed, hence the validator must reject too). Crypto/value/deadline checks are
-- mocked (@const True@) on the reference side, so the converse (validator-rejects-while-reference-
-- accepts, e.g. a bad signature) is expected and not asserted.
module Hydra.Tx.Contract.CloseDifferential (spec) where

import Hydra.Prelude

import Hydra.Agda.Reference qualified as Ref
import Hydra.Cardano.Api (
  Tx,
  UTxO,
  fromCtxUTxOTxOut,
  txOuts',
 )
import Hydra.Cardano.Api.ScriptData (fromScriptData, txOutScriptData)
import Hydra.Cardano.Api.TxBody (findRedeemerSpending)
import Hydra.Cardano.Api.TxOut (findTxOutByScript)
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as HS
import Hydra.Data.ContestationPeriod (milliseconds)
import Hydra.Tx.Contract.Close.CloseUnused (genCloseCurrentMutation, healthyCloseCurrentTx)
import Test.Hydra.Ledger.Cardano.Fixtures (evaluateTx)
import Test.Hydra.Prelude
import Test.Hydra.Tx.Mutation (SomeMutation (..), applyMutation)
import Test.QuickCheck (Property, forAll, property, (===))

-- | Does the real Plutus validator accept @(tx, utxo)@ (phase-2 success, no script error)?
validatorAccepts :: (Tx, UTxO) -> Bool
validatorAccepts (tx, utxo) =
  case evaluateTx tx utxo of
    Right report -> all isRight report
    Left _ -> False

-- | The extracted reference verdict on a close @(tx, utxo)@, or 'Nothing' if the head
-- datums/redeemer could not be read — in which case the reference abstains and the differential
-- property imposes no constraint.
closeRefVerdict :: (Tx, UTxO) -> Maybe Bool
closeRefVerdict (tx, utxo) = do
  (headIn, headInOut) <- findTxOutByScript utxo Head.validatorScript
  inSt <- fromScriptData =<< txOutScriptData (fromCtxUTxOTxOut headInOut)
  HS.Open od <- Just (inSt :: HS.State)
  headOut <- txOuts' tx !!? 0
  outSt <- fromScriptData =<< txOutScriptData headOut
  HS.Closed cd <- Just (outSt :: HS.State)
  HS.Close cr <- findRedeemerSpending tx headIn :: Maybe HS.Input
  let hsOpen =
        Ref.MkOpen
          od.version
          (toInteger (milliseconds od.contestationPeriod))
      hsClosed =
        Ref.MkClosed
          cd.version
          (toInteger (milliseconds cd.contestationPeriod))
          cd.snapshotNumber
          (fromIntegral (length cd.contesters))
  pure (Ref.checkClose (Ref.mkOps (\_ _ _ -> True)) hsOpen hsClosed (tagOf cr))
 where
  tagOf = \case
    HS.CloseInitial -> Ref.CloseInitialT
    HS.CloseAny{} -> Ref.CloseAnyT
    HS.CloseUnused{} -> Ref.CloseUnusedT
    HS.CloseUsed{} -> Ref.CloseUsedT

spec :: Spec
spec = parallel $ do
  prop "reference accepts the healthy close tx" $
    closeRefVerdict healthyCloseCurrentTx === Just True

  prop "validator accepts the healthy close tx" $
    validatorAccepts healthyCloseCurrentTx === True

  prop "differential: reference-reject ⇒ validator-reject (CloseUnused mutations)" $
    forAll (genCloseCurrentMutation healthyCloseCurrentTx) $ \SomeMutation{mutation} ->
      let m = applyMutation mutation healthyCloseCurrentTx
       in differential m
 where
  differential :: (Tx, UTxO) -> Property
  differential m =
    case closeRefVerdict m of
      Just False -> validatorAccepts m === False
      _ -> property True
