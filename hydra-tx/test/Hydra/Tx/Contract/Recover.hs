module Hydra.Tx.Contract.Recover where

import Hydra.Cardano.Api
import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.Fixed (Milli)
import Data.List qualified as List
import Data.Time.Clock.POSIX qualified as POSIX
import Hydra.Contract.Commit (Commit)
import Hydra.Contract.Deposit (DepositRedeemer (Recover))
import Hydra.Contract.DepositError (DepositError (..))
import Hydra.Contract.Error (toErrorCode)
import Hydra.Ledger.Cardano.Evaluate (slotLength, systemStart)
import Hydra.Ledger.Cardano.Time (slotNoToUTCTime)
import Hydra.Plutus.Extras (posixFromUTCTime)
import Hydra.Tx.Deposit (mkDepositOutput)
import Hydra.Tx.Recover (recoverTx)
import PlutusLedgerApi.V3 (CurrencySymbol, POSIXTime)
import Test.Hydra.Tx.Fixture (testHeadId, testNetworkId)
import Test.Hydra.Tx.Gen (genUTxOAdaOnlyOfSize, genValue)
import Test.Hydra.Tx.Mutation (
  Mutation (ChangeInput, ChangeOutput, ChangeValidityLowerBound),
  SomeMutation (..),
  modifyInlineDatum,
 )
import Test.QuickCheck (elements, oneof, suchThat)

healthyRecoverTx :: (Tx, UTxO)
healthyRecoverTx =
  (tx, utxo)
 where
  tx =
    recoverTx
      depositTxId
      deposited
      recoverSlotNo

  utxo =
    UTxO.singleton (TxIn depositTxId $ TxIx 0) depositTxOut

  depositTxId = arbitrary `generateWith` 42

  depositTxOut :: TxOut CtxUTxO
  depositTxOut = mkDepositOutput testNetworkId testHeadId deposited depositDeadline

  deposited = genUTxOAdaOnlyOfSize 1 `generateWith` 42

recoverSlotNo :: SlotNo
recoverSlotNo = SlotNo $ arbitrary `generateWith` 42

depositDeadline :: UTCTime
depositDeadline =
  slotNoToUTCTime systemStart slotLength (recoverSlotNo - SlotNo 1)

data RecoverMutation
  = -- | Move the deposit deadline further so that the recover lower bound is
    -- not after the deadline
    MutateDepositPeriod
  | -- | Change the recover output so that the datum commit hash does not match
    MutateRecoverOutput
  | -- | Remove the lower bound from the recover transaction
    RemoveTxValidityLowerBound
  deriving stock (Generic, Show, Enum, Bounded)

genRecoverMutation :: (Tx, UTxO) -> Gen SomeMutation
genRecoverMutation (tx, utxo) =
  oneof
    [ SomeMutation (pure $ toErrorCode DepositPeriodNotReached) MutateDepositPeriod <$> do
        -- Could also use depositTxIn/Out directly but this way we can be sure that the depositTxIn/Out are in the UTxO
        let (depositIn, depositOut@(TxOut addr val _ rscript)) = List.head $ UTxO.toList (resolveInputsUTxO utxo tx)
        let n = POSIX.posixSecondsToUTCTime $ realToFrac $ (arbitrary :: Gen Milli) `generateWith` 42
        let datum =
              txOutDatum $
                flip modifyInlineDatum (fromCtxUTxOTxOut depositOut) $ \case
                  ((headCS', depositDatumDeadline, commits) :: (CurrencySymbol, POSIXTime, [Commit])) ->
                    (headCS', depositDatumDeadline + posixFromUTCTime n, commits)
        let newOutput = toCtxUTxOTxOut $ TxOut addr val datum rscript
        pure $ ChangeInput depositIn newOutput (Just $ toScriptData $ Recover 1)
    , SomeMutation (pure $ toErrorCode IncorrectDepositHash) MutateRecoverOutput <$> do
        let outs = txOuts' tx
        (ix :: Int, out) <- elements (zip [0 ..] outs)
        value' <- genValue `suchThat` (/= txOutValue out)
        pure $ ChangeOutput (fromIntegral ix) (modifyTxOutValue (const value') out)
    , SomeMutation (pure $ toErrorCode DepositNoLowerBoundDefined) RemoveTxValidityLowerBound . ChangeValidityLowerBound <$> do
        pure TxValidityNoLowerBound
    ]
