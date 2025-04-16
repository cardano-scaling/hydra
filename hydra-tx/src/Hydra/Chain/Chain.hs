{-# LANGUAGE UndecidableInstances #-}

module Hydra.Chain.Chain where

import Hydra.Prelude
import Hydra.Tx (ArbitraryIsTx, HeadId, HeadParameters, HeadSeed, IsTx, Party, SnapshotNumber, SnapshotVersion, TxIdType, UTxOType)
import Hydra.Tx.OnChainId (OnChainId)
import Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary (..))

-- | Describes transactions as seen on chain. Holds as minimal information as
-- possible to simplify observing the chain.
data OnChainTx tx
  = OnInitTx
      { headId :: HeadId
      , headSeed :: HeadSeed
      , headParameters :: HeadParameters
      , participants :: [OnChainId]
      }
  | OnCommitTx
      { headId :: HeadId
      , party :: Party
      , committed :: UTxOType tx
      }
  | OnAbortTx {headId :: HeadId}
  | OnCollectComTx {headId :: HeadId}
  | OnDepositTx
      { headId :: HeadId
      , deposited :: UTxOType tx
      , depositTxId :: TxIdType tx
      , deadline :: UTCTime
      }
  | OnRecoverTx
      { headId :: HeadId
      , recoveredTxId :: TxIdType tx
      , recoveredUTxO :: UTxOType tx
      }
  | OnIncrementTx
      { headId :: HeadId
      , newVersion :: SnapshotVersion
      , depositTxId :: TxIdType tx
      }
  | OnDecrementTx
      { headId :: HeadId
      , newVersion :: SnapshotVersion
      , distributedUTxO :: UTxOType tx
      }
  | OnCloseTx
      { headId :: HeadId
      , snapshotNumber :: SnapshotNumber
      , contestationDeadline :: UTCTime
      }
  | OnContestTx
      { headId :: HeadId
      , snapshotNumber :: SnapshotNumber
      , contestationDeadline :: UTCTime
      }
  | OnFanoutTx {headId :: HeadId, fanoutUTxO :: UTxOType tx}
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (OnChainTx tx)
deriving stock instance IsTx tx => Show (OnChainTx tx)
deriving anyclass instance IsTx tx => ToJSON (OnChainTx tx)
deriving anyclass instance IsTx tx => FromJSON (OnChainTx tx)

instance ArbitraryIsTx tx => Arbitrary (OnChainTx tx) where
  arbitrary = genericArbitrary

instance ArbitraryIsTx tx => ToADTArbitrary (OnChainTx tx)
