module Hydra.Logic where

import Cardano.Prelude hiding (State)

import qualified Hydra.Logic.SimpleHead as SimpleHead

data Event
  = ClientEvent ClientCommand
  | NetworkEvent HydraMessage
  | OnChainEvent OnChainTx

data Effect
  = ClientEffect ClientInstruction
  | NetworkEffect HydraMessage
  | OnChainEffect OnChainTx
  | -- | Wait effect should be interpreted as a non-blocking interruption which
    -- retries on every state changes until the continuation returns Just{}.
    Wait (State -> Maybe (State, [Effect]))

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
  = InitState
  | OpenState SimpleHead.State
  | ClosedState

update :: State -> Event -> (State, [Effect])
update st e =
  fromMaybe (st, []) $ -- TODO this is likely an error
    asum
      [ init st e
      , close st e
      , do
          st' <- mapState st
          e' <- mapEvent e
          pure $ bimap OpenState (map mapEffect) $ SimpleHead.update st' e'
      ]

init :: State -> Event -> Maybe (State, [Effect])
init InitState{} (ClientEvent Init) = Just (OpenState SimpleHead.mkState, [])
init _ _ = Nothing

close :: State -> Event -> Maybe (State, [Effect])
close OpenState{} (ClientEvent Close) = Just (ClosedState, [])
close _ _ = Nothing

-- NOTE: This three things needs to be polymorphic in the output eventually, likely a
-- type-class with data-families for each sub-modules.

mapState :: State -> Maybe SimpleHead.State
mapState = \case
  OpenState st' -> Just st'
  _ -> Nothing

mapEvent :: Event -> Maybe SimpleHead.Event
mapEvent = \case
  NetworkEvent ReqTx -> Just SimpleHead.ReqTxFromPeer
  NetworkEvent ReqSn -> Just SimpleHead.ReqSnFromPeer
  ClientEvent NewTx -> Just SimpleHead.NewTxFromClient
  _ -> Nothing

-- | NOTE: This needs to be polymorphic in the input eventually
mapEffect :: SimpleHead.Effect -> Effect
mapEffect = \case
  SimpleHead.MulticastReqTx -> NetworkEffect ReqTx
  SimpleHead.MulticastReqSn -> NetworkEffect ReqSn
  SimpleHead.MulticastConfTx -> NetworkEffect ConfTx
  SimpleHead.SendAckTx -> NetworkEffect AckTx
  SimpleHead.Wait continue -> Wait $ mapState >>= fmap (bimap OpenState mapEffect) . continue
