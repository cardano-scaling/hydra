module Hydra.Network.Ouroboros.Server where

import Hydra.Prelude

import Hydra.Network.Ouroboros.Type (
  FireForget (StIdle),
  Message (MsgDone, MsgSend),
 )
import Network.TypedProtocol (
  IsPipelined (..),
  PeerRole (AsServer),
  ReflRelativeAgency (..),
 )

import Network.TypedProtocol.Peer (
  Peer (Await, Done, Effect),
 )

data FireForgetServer msg m a = FireForgetServer
  { recvMsg :: msg -> m (FireForgetServer msg m a)
  -- ^ The client sent us a message.
  -- There is no response and we must have effects.
  , recvMsgDone :: m a
  -- ^ The client terminated. Here we have a pure return value, but we
  -- could have done another action in 'm' if we wanted to.
  }

fireForgetServerPeer ::
  Monad m =>
  FireForgetServer msg m a ->
  Peer (FireForget msg) 'AsServer 'NonPipelined 'StIdle m a
fireForgetServerPeer FireForgetServer{recvMsg, recvMsgDone} =
  -- In the 'StIdle' the server is awaiting a request message
  Await ReflClientAgency $ \case
    -- The client got to choose between two messages and we have to handle
    -- either of them
    MsgSend payload -> Effect $ fireForgetServerPeer <$> recvMsg payload
    MsgDone -> Effect $ Done ReflNobodyAgency <$> recvMsgDone
