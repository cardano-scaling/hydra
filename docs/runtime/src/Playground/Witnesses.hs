module Playground.Witnesses (checks) where

import Hydra.Prelude

import Data.Aeson (Value, object, (.=))
import Data.Set qualified as Set
import Hydra.HeadLogic.Interpreter (SnapshotSigner (..))
import Hydra.Ledger.Simple (SimpleTx, SimpleTxOut (..))
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Crypto (aggregate, verifyMultiSignature)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (InitialSnapshot), Snapshot (..), getSnapshot)
import Playground.Engine (signerFor)
import Playground.Engine qualified as Engine

checks :: Value
checks =
  object
    [ "valid" .= verify [0] [signature 0 original] original
    , "bothSigners" .= verify [0, 1] [signature 0 original, signature 1 original] original
    , "wrongSigner" .= verify [1] [signature 0 original] original
    , "missingSignature" .= verify [0, 1] [signature 0 original] original
    , "extraSignature" .= verify [0] [signature 0 original, signature 1 original] original
    , "reorderedSignatures" .= verify [0, 1] [signature 1 original, signature 0 original] original
    , "changedNumber" .= verify [0] [signature 0 original] original{number = 2}
    , "changedVersion" .= verify [0] [signature 0 original] original{version = 2}
    , "changedVersionAndNumber" .= verify [0] [signature 0 original{version = 1, number = 23}] original{version = 12, number = 3}
    , "changedOutputs" .= verify [0] [signature 0 original] (withDeposit 41 8)
    , "changedDeposit" .= verify [0] [signature 0 original] (withDeposit 42 7)
    , "accumulatorOrderIndependent" .= (commitment ["a", "b"] == commitment ["b", "a"])
    , "accumulatorBindsMultiplicity" .= (commitment ["a", "a"] /= commitment ["a"])
    , "accumulatorBindsContents" .= (commitment ["a"] /= commitment ["b"])
    ]
 where
  original = withDeposit 41 7
  commitment = Accumulator.getAccumulatorHash . Accumulator.build
  signature i = signOne $ signerFor i
  verify indices signatures snapshot = isRight $ verifyMultiSignature (map (signingIdentity . signerFor) indices) (aggregate signatures) snapshot
  withDeposit :: Integer -> Integer -> Snapshot SimpleTx
  withDeposit depositId output =
    let commit = Just $ Set.singleton $ SimpleTxOut output
        (base, applied) = Accumulator.buildFromSnapshotUTxOs @SimpleTx mempty commit Nothing
     in (getSnapshot $ InitialSnapshot Engine.headId){number = 1, utxoToCommit = commit, depositTxId = Just depositId, accumulator = base, appliedAccumulator = applied}
