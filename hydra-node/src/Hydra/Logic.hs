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

data Env = Env

update :: Env -> State -> Event -> (State, [Effect])
update _env st ev = case (st, ev) of
  (InitState{}, ClientEvent Init) -> init
  (OpenState st', ClientEvent Close) -> close st'
  (OpenState st', NetworkEvent ReqTx) ->
    bimap OpenState (map mapEffect) $ SimpleHead.update st' SimpleHead.ReqTxFromPeer

-- | NOTE: This is definitely not directly Open!
init :: (State, [Effect])
init = (OpenState SimpleHead.mkState, [])

close :: SimpleHead.State -> (State, [Effect])
close _ = (ClosedState, [])

-- NOTE: This three things needs to be polymorphic in the output eventually, likely a
-- type-class with data-families for each sub-modules.

mapState :: State -> Maybe SimpleHead.State
mapState = \case
  OpenState st' -> Just st'
  _ -> Nothing

mapEffect :: SimpleHead.Effect -> Effect
mapEffect = \case
  SimpleHead.MulticastReqTx -> NetworkEffect ReqTx
  SimpleHead.MulticastReqSn -> NetworkEffect ReqSn
  SimpleHead.MulticastConfTx -> NetworkEffect ConfTx
  SimpleHead.SendAckTx -> NetworkEffect AckTx
  SimpleHead.Wait continue -> Wait $ mapState >=> fmap (bimap OpenState (map mapEffect)) . continue
