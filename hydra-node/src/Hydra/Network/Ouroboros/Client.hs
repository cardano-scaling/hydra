module Hydra.Network.Ouroboros.Client where

import Hydra.Prelude

import Hydra.Network.Ouroboros.Type (
  FireForget (..),
  Message (MsgDone, MsgSend),
 )
import Network.TypedProtocol.Core (
  IsPipelined (..),
  PeerRole (..),
  ReflRelativeAgency (..),
 )
import Network.TypedProtocol.Peer (
  Peer (..),
 )

data FireForgetClient msg m a where
  Idle :: m (FireForgetClient msg m a) -> FireForgetClient msg m a
  SendMsg :: msg -> m (FireForgetClient msg m a) -> FireForgetClient msg m a
  SendDone :: m a -> FireForgetClient msg m a

fireForgetClientPeer ::
  Monad m =>
  FireForgetClient msg m a ->
  Peer (FireForget msg) 'AsClient 'NonPipelined 'StIdle m a
fireForgetClientPeer = \case
  Idle next ->
    Effect $ fireForgetClientPeer <$> next
  SendMsg msg next ->
    Yield ReflClientAgency (MsgSend msg) $
      Effect $
        fireForgetClientPeer <$> next
  SendDone action ->
    Effect $ Yield ReflClientAgency MsgDone . Done ReflNobodyAgency <$> action
