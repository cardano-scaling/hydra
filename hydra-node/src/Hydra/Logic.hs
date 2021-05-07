{-# LANGUAGE UndecidableInstances #-}

module Hydra.Logic where

import Cardano.Prelude

import Hydra.Ledger (Ledger (Ledger, canApply), ValidationError, ValidationResult (Invalid, Valid), initLedgerState)
import qualified Hydra.Logic.SimpleHead as SimpleHead

data Event tx
  = ClientEvent (ClientRequest tx)
  | NetworkEvent HydraMessage
  | OnChainEvent OnChainTx
  deriving (Eq, Show)

data Effect tx
  = ClientEffect ClientResponse
  | NetworkEffect HydraMessage
  | OnChainEffect OnChainTx
  | -- | Wait effect should be interpreted as a non-blocking interruption which
    -- retries on every state changes until the continuation returns Just{}.
    Wait (HeadState tx -> Maybe (HeadState tx, [Effect tx]))

data ClientRequest tx
  = Init
  | Commit
  | NewTx tx
  | Close
  | Contest
  deriving (Eq, Show)

data ClientResponse
  = ReadyToCommit
  | AcceptingTx
  deriving (Eq, Show)

data HydraMessage
  = ReqTx
  | AckTx
  | ConfTx
  | ReqSn
  | AckSn
  | ConfSn
  deriving (Eq, Show)

data OnChainTx
  = InitTx
  | CommitTx
  | CollectComTx
  | CloseTx
  | ContestTx
  | FanoutTx
  deriving (Eq, Show)

data HeadState tx
  = InitState
  | OpenState (SimpleHead.State tx)
  | ClosedState

deriving instance Eq (SimpleHead.State tx) => Eq (HeadState tx)
deriving instance Show (SimpleHead.State tx) => Show (HeadState tx)

-- | Verification used to authenticate main chain transactions that are
-- restricted to members of the Head protocol instance, i.e. the commit
-- transaction. This key is named k_i in the paper and for Cardano, this is
-- currently a Ed25519 verification key
data OnChainVerificationKey

-- | Verification key to the signing key used for signing / acking transactions
-- off chain. This key is named K_i in the paper and can be aggregated with
-- other party member's 'HydraVerificationKey' to K_agg.
data HydraVerificationKey

-- | Identifes a party in a Hydra head.
type Party = (OnChainVerificationKey, HydraVerificationKey)

-- | Contains at least the contestation period and other things.
data HeadParameters = HeadParameters

-- | Decides when, how often and who is in charge of creating snapshots.
data SnapshotStrategy = SnapshotStrategy

-- | Assume: We know the party members and their verification keys. These need
-- to be exchanged somehow, eventually.
createHeadState :: [Party] -> HeadParameters -> SnapshotStrategy -> HeadState tx
createHeadState _ _ _ = InitState

data LogicError tx
  = InvalidEvent (Event tx) (HeadState tx)
  | InvalidState (HeadState tx)
  | LedgerError ValidationError

deriving instance (Eq (HeadState tx), Eq (Event tx)) => Eq (LogicError tx)
deriving instance (Show (HeadState tx), Show (Event tx)) => Show (LogicError tx)

data Outcome tx
  = NewState (HeadState tx) [Effect tx]
  | Error (LogicError tx)

-- | The heart of the Hydra head logic, a handler of all kinds of 'Event' in the
-- Hydra head. This may also be split into multiple handlers, i.e. one for hydra
-- network events, one for client events and one for main chain events, or by
-- sub-'State'.
update :: Ledger tx -> HeadState tx -> Event tx -> Outcome tx
update Ledger{canApply, initLedgerState} st ev = case (st, ev) of
  (InitState, ClientEvent Init) ->
    NewState InitState [OnChainEffect InitTx]
  (InitState, OnChainEvent InitTx) ->
    let initSt = SimpleHead.mkState initLedgerState
     in NewState (OpenState initSt) [ClientEffect ReadyToCommit]
  (OpenState st', ClientEvent (NewTx tx)) ->
    let ls = SimpleHead.confirmedLedger st'
     in case canApply ls tx of
          Valid -> NewState st [NetworkEffect ReqTx]
          Invalid err -> Error (LedgerError err)
  _ -> Error (InvalidEvent ev st)

-- NOTE: This three things needs to be polymorphic in the output eventually, likely a
-- type-class with data-families for each sub-modules.

mapState :: HeadState tx -> Maybe (SimpleHead.State tx)
mapState = \case
  OpenState st' -> Just st'
  _ -> Nothing

mapEffect :: SimpleHead.Effect tx -> Effect tx
mapEffect = \case
  SimpleHead.MulticastReqTx -> NetworkEffect ReqTx
  SimpleHead.MulticastReqSn -> NetworkEffect ReqSn
  SimpleHead.MulticastConfTx -> NetworkEffect ConfTx
  SimpleHead.SendAckTx -> NetworkEffect AckTx
  SimpleHead.Wait continue ->
    Wait $ mapState >=> fmap (bimap OpenState (map mapEffect)) . continue
