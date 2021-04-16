module Hydra.Logic where

import Cardano.Prelude

import qualified Hydra.Logic.SimpleHead as SimpleHead

data Event
  = ClientEvent ClientCommand
  | NetworkEvent HydraMessage
  | OnChainEvent OnChainTx

data Effect
  = ClientEffect ClientInstruction
  | NetworkEffect HydraMessage
  | OnChainEffect OnChainTx
  -- | Wait effect should be interpreted as a non-blocking interruption which
  -- retries on every state changes until the continuation returns Just{}.
  | Wait (State -> Maybe (State, [Effect]))

data ClientCommand
  = Init
  | Commit
  | NewTx
  | Close
  | Contest

data ClientInstruction

data HydraMessage
  = ReqTx
  | AckTx
  | ConfTx
  | ReqSn
  | AckSn

data OnChainTx
  = InitTx
  | CommitTx
  | CollectComTx
  | CloseTx
  | ContestTx
  | FanoutTx

data State
  = Init
  | Open SimpleHead.State
  | Closed

update :: State -> Event -> (State, [Effect])
update st e =
  case st of
    Init ->
      (st, [])
    Open st' ->
      case mapEvent e of
        Nothing -> (st, [])
        Just e' -> bimap Open mapEffect $ SimpleHead.update st' e'
    Closed ->
      (st, [])


-- | NOTE: This needs to be polymorphic in the output eventually, likely a
-- type-class with data-families for each sub-modules.
mapEvent :: Event -> Maybe SimpleHead.Event
mapEvent = \case
  NetworkEvent ReqTx -> Just ReqTxFromPeer
  NetworkEvent ReqSn -> Just ReqSnFromPeer
  ClientEvent NewTx ->  Just NewTxFromClient
  _ -> Nothing

-- | NOTE: This needs to be polymorphic in the input eventually
mapEffect :: SimpleHead.Effect -> Effect
mapEffect = \case
  MulticastReqTx -> NetworkEffect ReqTx
  MulticastReqSn -> NetworkEffect ReqSn
  MulticastConfTx -> NetworkEffect ConfTx
  SendAckTx -> NetworkEffect AckTx
  Wait continue -> Wait $ \case
    Open st' -> bimap Open mapEffect <$> continue st'
