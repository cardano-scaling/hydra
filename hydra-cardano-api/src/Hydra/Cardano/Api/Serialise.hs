module Hydra.Cardano.Api.Serialise where

import Control.Monad.Class.MonadThrow (Exception, MonadThrow (..))
import Hydra.Cardano.Api.Prelude

data SerialiseFromRawBytesException = SerialiseFromRawBytesException ByteString String
  deriving stock (Show)

instance Exception SerialiseFromRawBytesException

deserialiseFromRawBytesThrow :: forall a m. SerialiseAsRawBytes a => MonadThrow m => ByteString -> m a
deserialiseFromRawBytesThrow bs = case deserialiseFromRawBytes (proxyToAsType (Proxy @a)) bs of
  Left e -> throwIO $ SerialiseFromRawBytesException bs (show e)
  Right x -> pure x
