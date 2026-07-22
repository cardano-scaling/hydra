{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

-- | Lenient parser for a hydra-node's structured JSON trace log. The event
-- store (hydra.db) only records committed 'StateChanged's, so it cannot show
-- /why/ a node is parked. The trace log can: each @LogicOutcome@ entry carries
-- the 'Outcome' the head logic produced, including @Wait@ reasons
-- (e.g. @WaitOnTxs@ with the missing tx ids) and @Error@s. We also pick up
-- reliability-layer "failed to find message" lines. This is what the
-- stuck-head post-mortems (#1374 / #1415) read by hand.
module HydraVis.Trace (
  TraceEntry (..),
  TraceKind (..),
  loadTraceLog,
) where

import Hydra.Prelude

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Char8 qualified as BS8
import Data.Text qualified as T

-- | What a single trace line tells us, when it is one we care about.
data TraceKind
  = TContinue
  | -- | The wait reason (e.g. @"WaitOnTxs ..."@).
    TWait Text
  | TError Text
  | TReliabilityFail
  deriving stock (Eq, Show)

data TraceEntry = TraceEntry
  { teTime :: UTCTime
  , teKind :: TraceKind
  }
  deriving stock (Eq, Show)

-- | Parse a hydra-node JSON log (one JSON envelope per line). Lines we do not
-- recognise are skipped, so this tolerates schema drift and mixed logs.
loadTraceLog :: FilePath -> IO [TraceEntry]
loadTraceLog path = do
  content <- readFileBS path
  pure (mapMaybe parseLine (BS8.lines content))

parseLine :: ByteString -> Maybe TraceEntry
parseLine bs = do
  v <- Aeson.decodeStrict bs
  obj <- asObject v
  t <- KM.lookup "timestamp" obj >>= time'
  kind <- classify obj (decodeUtf8 bs)
  pure (TraceEntry t kind)
 where
  time' tv = case Aeson.fromJSON tv of
    Aeson.Success (u :: UTCTime) -> Just u
    _ -> Nothing

classify :: Aeson.Object -> Text -> Maybe TraceKind
classify envelope raw =
  case KM.lookup "message" envelope >>= asObject of
    Just m
      | tagIs "Node" m
      , Just node <- KM.lookup "node" m >>= asObject
      , tagIs "LogicOutcome" node
      , Just oc <- KM.lookup "outcome" node >>= asObject ->
          case KM.lookup "tag" oc of
            Just (Aeson.String "Continue") -> Just TContinue
            Just (Aeson.String "Wait") -> Just (TWait (reasonText (KM.lookup "reason" oc)))
            Just (Aeson.String "Error") -> Just (TError (reasonText (KM.lookup "error" oc)))
            _ -> reliability
    _ -> reliability
 where
  reliability
    | "FailedToFindMsg" `T.isInfixOf` raw = Just TReliabilityFail
    | otherwise = Nothing

tagIs :: Text -> Aeson.Object -> Bool
tagIs t o = KM.lookup "tag" o == Just (Aeson.String t)

-- | A short human label for a wait/error payload: its @tag@ plus a truncated
-- dump of the remaining fields (e.g. the missing tx ids).
reasonText :: Maybe Aeson.Value -> Text
reasonText = \case
  Just (Aeson.Object o) -> case KM.lookup "tag" o of
    Just (Aeson.String t) -> t <> rest (KM.delete "tag" o)
    _ -> compact (Aeson.Object o)
  Just v -> compact v
  Nothing -> "?"
 where
  rest o
    | KM.null o = ""
    | otherwise = " " <> T.take 160 (compact (Aeson.Object o))
  compact = decodeUtf8 . toStrict . Aeson.encode

asObject :: Aeson.Value -> Maybe Aeson.Object
asObject = \case Aeson.Object o -> Just o; _ -> Nothing
