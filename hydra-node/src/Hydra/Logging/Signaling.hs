-- | A `Tracer` "middleware" to capture some logs and dispatch them to a `Server`.
module Hydra.Logging.Signaling where

import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (newTVarIO, readTVarIO, writeTVar)
import qualified Data.ByteString.Base16 as Hex
import Hydra.API.Server (Server (..))
import Hydra.API.ServerOutput (ServerOutput (SomeHeadInitializing))
import Hydra.Cardano.Api (AssetName (..))
import Hydra.Chain.Direct.Handlers (DirectChainLog (SomeHeadObserved))
import Hydra.Logging (Tracer (..))
import Hydra.Logging.Messages (HydraLog (DirectChain))

withSignaling ::
  (MonadSTM m) =>
  Tracer m (HydraLog tx net) ->
  (Tracer m (HydraLog tx net) -> TVar m (Maybe (Server tx m)) -> m a) ->
  m a
withSignaling (Tracer tracer) k = do
  var <- newTVarIO Nothing
  let tracer' = Tracer $ decorate var tracer
  k tracer' var

installSignal :: MonadSTM m => TVar m (Maybe (Server tx m)) -> Server tx m -> m ()
installSignal var server =
  atomically $ writeTVar var (Just server)

decorate ::
  (MonadSTM m) =>
  TVar m (Maybe (Server tx m)) ->
  (HydraLog tx net -> m ()) ->
  HydraLog tx net ->
  m ()
decorate var tracer log = do
  case signal log of
    Just out ->
      readTVarIO var >>= \case
        Just Server{sendOutput} -> sendOutput out
        Nothing -> pure ()
    Nothing -> pure ()
  tracer log

signal :: HydraLog tx net -> Maybe (ServerOutput tx)
signal = \case
  DirectChain (SomeHeadObserved headId pkhs) -> Just (SomeHeadInitializing headId (asHexText <$> pkhs))
  _ -> Nothing
 where
  asHexText :: AssetName -> Text
  asHexText (AssetName bytes) = decodeUtf8 $ Hex.encode bytes
