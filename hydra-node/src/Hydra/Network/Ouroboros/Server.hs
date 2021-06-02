module Hydra.Network.Ouroboros.Server where

import Cardano.Prelude
import Network.TypedProtocol (Peer (Await, Done, Effect), PeerHasAgency (ClientAgency), PeerRole (AsServer))
import Hydra.Network.Ouroboros.Type (ClientHasAgency (TokIdle), FireForget (StIdle), Message (MsgDone, MsgSend), NobodyHasAgency (TokDone))

data FireForgetServer msg m a = FireForgetServer
  { -- | The client sent us a message.
    -- There is no response and we must have effects.
    recvMsg :: msg -> m (FireForgetServer msg m a)
  , -- | The client terminated. Here we have a pure return value, but we
    -- could have done another action in 'm' if we wanted to.
    recvMsgDone :: m a
  }

fireForgetServerPeer ::
  Monad m =>
  FireForgetServer msg m a ->
  Peer (FireForget msg) 'AsServer 'StIdle m a
fireForgetServerPeer FireForgetServer{recvMsg, recvMsgDone} =
  -- In the 'StIdle' the server is awaiting a request message
  Await (ClientAgency TokIdle) $ \msg ->
    -- The client got to choose between two messages and we have to handle
    -- either of them
    case msg of
      MsgSend payload -> Effect $ fireForgetServerPeer <$> recvMsg payload
      MsgDone -> Effect $ Done TokDone <$> recvMsgDone
