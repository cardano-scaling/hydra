module Playground.Runtime (evaluateJSON, newEvaluator) where

import Data.Aeson (Result (..), Value (..), eitherDecode, encode, fromJSON, object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy (ByteString)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Playground.Engine qualified as Engine
import Playground.View qualified as View
import Playground.Witnesses qualified as Witnesses
import Prelude

evaluateJSON :: ByteString -> ByteString
evaluateJSON = fst . evaluateWith Nothing

newEvaluator :: IO (ByteString -> IO ByteString)
newEvaluator = do
  cached <- newIORef Nothing
  pure $ \bytes -> do
    previous <- readIORef cached
    let (result, next) = evaluateWith previous bytes
    writeIORef cached next
    pure result

evaluateWith :: Maybe (Engine.Request, Engine.World) -> ByteString -> (ByteString, Maybe (Engine.Request, Engine.World))
evaluateWith cached bytes = case eitherDecode bytes of
  Left err -> (failure err, cached)
  Right (Object fields) | KeyMap.lookup "diagnostic" fields == Just (String "witnesses") -> (encode Witnesses.checks, cached)
  Right value -> case fromJSON value of
    Error err -> (failure err, cached)
    Success request -> case Engine.replayFrom cached request of
      Left err -> (failure err, cached)
      Right world -> (encode $ View.render (Engine.audit request) world, Just (request, world))
 where
  failure err = encode $ object ["status" .= ("error" :: Text), "reason" .= err]
