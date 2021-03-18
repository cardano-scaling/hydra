{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE PolyKinds #-}

module Network.TypedProtocol.FireForget.Type where

import Cardano.Prelude (Eq)
import GHC.Show (Show (show))
import Network.TypedProtocol (Protocol (..))

-- | TODO explain Protocol
--
-- It is used both as a type level tag for the protocol and as the kind of the
-- types of the states in the protocol state machine. That is @FireForget@ is a
-- kind, and @StIdle@ is a type of that kind.
data FireForget msg where
  StIdle :: FireForget msg
  StDone :: FireForget msg

instance ShowProxy (FireForget msg) where
  showProxy _ = "FireForget"

instance Protocol (FireForget msg) where
  -- The actual messages in our protocol.
  --
  -- These involve transitions between different states within the 'PingPong'
  -- states. A ping request goes from idle to busy, and a pong response go from
  -- busy to idle.
  data Message (FireForget msg) from to where
    MsgSend :: msg -> Message (FireForget msg) 'StIdle 'StIdle
    MsgDone :: Message (FireForget msg) 'StIdle 'StDone

  -- We have to explain to the framework what our states mean, in terms of
  -- who is expected to send and receive in the different states.
  --
  -- Idle states are where it is for the client to send a message.
  data ClientHasAgency st where
    TokIdle :: ClientHasAgency 'StIdle

  -- In our protocol the server is always receiving, thus in no state the server
  -- has agency.
  data ServerHasAgency st

  -- In the done state neither client nor server can send messages.
  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency 'StDone

  exclusionLemma_ClientAndServerHaveAgency TokIdle tok = case tok of
  exclusionLemma_NobodyAndClientHaveAgency TokDone tok = case tok of
  exclusionLemma_NobodyAndServerHaveAgency TokDone tok = case tok of

deriving instance (Show msg) => Show (Message (FireForget msg) from to)

deriving instance (Eq msg) => Eq (Message (FireForget msg) from to)

instance Show (ClientHasAgency (st :: FireForget msg)) where
  show TokIdle = "TokIdle"

instance Show (ServerHasAgency (st :: FireForget msg)) where
  show _ = panic "absurd"
