module Hydra.Network.Ouroboros.Client where

import Cardano.Prelude

import Network.TypedProtocol.Core (
  Peer (..),
  PeerHasAgency (..),
  PeerRole (..),
 )
import Hydra.Network.Ouroboros.Type (
  ClientHasAgency (TokIdle),
  FireForget (..),
  Message (MsgDone, MsgSend),
  NobodyHasAgency (TokDone),
 )

data FireForgetClient msg m a where
  Idle :: m (FireForgetClient msg m a) -> FireForgetClient msg m a
  SendMsg :: msg -> m (FireForgetClient msg m a) -> FireForgetClient msg m a
  SendDone :: m a -> FireForgetClient msg m a

fireForgetClientPeer ::
  Monad m =>
  FireForgetClient msg m a ->
  Peer (FireForget msg) 'AsClient 'StIdle m a
fireForgetClientPeer = \case
  Idle next ->
    Effect $ fireForgetClientPeer <$> next
  SendMsg msg next ->
    Yield (ClientAgency TokIdle) (MsgSend msg) $
      Effect $ fireForgetClientPeer <$> next
  SendDone action ->
    Effect $ Yield (ClientAgency TokIdle) MsgDone . Done TokDone <$> action
