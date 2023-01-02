{-# OPTIONS_GHC -Wwarn #-}

-- | An experimental network component for Hydra node that wraps several heads.
--
-- All messages are encapsulated in an `Enveloped` that identifies the `Head` to which the message
-- pertains. Sender attaches the id to the message which is decoded and correctly dispatched by
-- the receiver.
module Hydra.Network.MultiHead where

import Hydra.Prelude

import Control.Monad.Class.MonadAsync (MonadAsync)
import Control.Monad.Class.MonadSTM (MonadSTM (readTVarIO))
import Hydra.Chain (HeadId)
import Hydra.Network (Network (..), NetworkCallback, NetworkComponent)

data Enveloped msg = Enveloped {headId :: HeadId, message :: msg}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToCBOR, FromCBOR)

withMultiHead ::
  (MonadAsync m) =>
  -- | The headId for this network instance.
  -- This variable will be set externally when the `Init` transaction is posted and
  -- observed on-chain. When unset, no communication happens and all messages will
  -- be simply dropped.
  TVar m (Maybe HeadId) ->
  NetworkComponent m (Enveloped msg) a ->
  NetworkComponent m msg a
withMultiHead headId withNetwork callback action = do
  withNetwork (messageDispatcher headId callback) $ \network ->
    action (decorateMessage headId network)

decorateMessage ::
  MonadSTM m =>
  TVar m (Maybe HeadId) ->
  Network m (Enveloped msg) ->
  Network m msg
decorateMessage headId Network{broadcast} =
  Network $ \msg ->
    readTVarIO headId >>= \case
      Nothing -> pure ()
      Just hid -> broadcast (Enveloped hid msg)

messageDispatcher ::
  MonadSTM m =>
  TVar m (Maybe HeadId) ->
  NetworkCallback msg m ->
  NetworkCallback (Enveloped msg) m
messageDispatcher headId callback (Enveloped hid msg) =
  readTVarIO headId >>= \case
    Just hid' | hid' == hid -> callback msg
    _ -> pure ()
