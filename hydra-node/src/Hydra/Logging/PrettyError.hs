{-# OPTIONS_GHC -Wno-orphans #-}

-- | The 'PrettyError' class lets each log-message type declare its own
-- 'Severity' and a line-based rendering ('showPretty') used by the
-- 'LogPretty' formatter in "Hydra.Logging".
--
-- The class has no default for 'showPretty' — every instance must commit to
-- a rendering. Most instances will simply set @showPretty = genericFlatten@
-- to get the standard one-key-per-line layout; instances that want a
-- hand-tuned format for some of their constructors should pattern-match
-- and use 'prettyKv' / 'prettyKvHeader' to keep the visual style consistent.
module Hydra.Logging.PrettyError (
  -- * Class & severity
  Severity (..),
  PrettyError (..),

  -- * Standard generic rendering, exposed for instances to call
  genericFlatten,

  -- * Helpers for hand-rolled @showPretty@ output
  prettyKv,
  prettyKvHeader,

  -- * Lower-level building blocks (used by 'genericFlatten' and exposed in

  -- case a custom rendering wants to splice a JSON value in directly)
  renderField,
  renderScalar,
  objFields,
  arrFields,
  indent,

  -- * ANSI primitives (re-used by "Hydra.Logging")
  ansi,
  dim,
) where

import Hydra.Prelude

import Data.Aeson (Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text

-- | Three-level classification used by the pretty renderer to pick a colour.
data Severity
  = Info
  | Warning
  | Error
  deriving stock (Eq, Ord, Show, Generic)

-- | A log-message type carries a 'Severity' and a line-based pretty
-- rendering. Both must be implemented; there is no fallback. Use
-- 'genericFlatten' to inherit the standard "JSON-flatten" layout for
-- constructors where a hand-rolled rendering wouldn't add value.
class PrettyError a where
  -- | How this value should be classified by the renderer. Default 'Info'.
  severity :: a -> Severity
  severity _ = Info

  -- | The body lines printed under the path header. Each line must already
  -- include its leading indent and any ANSI styling (see 'prettyKv').
  showPretty :: a -> [Text]

-- | Default rendering for any 'ToJSON' value: walk through nested
-- tag-only wrappers, then emit one indented @key=value@ line per leaf
-- field. Mirrors the layout produced by the 'LogPretty' formatter when
-- given an unannotated message.
genericFlatten :: ToJSON a => a -> [Text]
genericFlatten a =
  let residual = leafObject (Aeson.toJSON a)
   in concatMap (renderField 1) (objFields residual)
 where
  -- Strip the outer single-tag-wrapper chain and return the remaining
  -- fields of the leaf object. (Same idea as 'Hydra.Logging.pathTags',
  -- duplicated here to avoid pulling that module into the class file.)
  -- A non-tagged record (no "tag" key) is its own leaf — return its fields
  -- as-is so that types like 'Hydra.Options.RunOptions' still render.
  leafObject :: Value -> KeyMap Value
  leafObject = \case
    Object km
      | Just (String _) <- KeyMap.lookup "tag" km ->
          let rest = KeyMap.delete "tag" km
           in case KeyMap.toList rest of
                [(_, inner@(Object km'))] | KeyMap.member "tag" km' -> leafObject inner
                _ -> rest
      | otherwise -> km
    _ -> mempty

-- | Render one (synthetic-key, value) at the given indent level. Null values
-- are dropped. Objects flatten by emitting @key=@ then each child one level
-- deeper. Arrays flatten the same way, using @[N]@ as the synthetic key for
-- each element. Scalars render inline on the same line as the key.
renderField :: Int -> (Text, Value) -> [Text]
renderField level (k, v) =
  let header = indent level <> ansi dim (k <> "=")
   in case v of
        Null -> []
        Object km ->
          header : concatMap (renderField (level + 1)) (objFields km)
        Array xs ->
          header : concatMap (renderField (level + 1)) (arrFields xs)
        scalar ->
          [header <> renderScalar scalar]

-- | Turn a JSON object into a list of @(name, value)@ pairs.
objFields :: KeyMap Value -> [(Text, Value)]
objFields = map (first Key.toText) . KeyMap.toList

-- | Turn a JSON array into a list of @([N], value)@ pairs.
arrFields :: Foldable t => t Value -> [(Text, Value)]
arrFields xs =
  zipWith (\i v -> ("[" <> show i <> "]", v)) [0 :: Int ..] (toList xs)

-- | Render a JSON scalar. Strings emit bare (no surrounding quotes);
-- numbers, booleans and null go through Aeson's compact encoding.
renderScalar :: Value -> Text
renderScalar = \case
  String s -> s
  Bool True -> "true"
  Bool False -> "false"
  Null -> "null"
  v -> decodeUtf8 (LBS.toStrict (Aeson.encode v))

-- | Two-space-per-level indentation.
indent :: Int -> Text
indent n = Text.replicate (2 * n) " "

-- | @"  key=value"@ with the key wrapped in ANSI dim.
prettyKv :: Text -> Text -> Text
prettyKv k v = "  " <> ansi dim (k <> "=") <> v

-- | @"  key="@ — for a key whose value continues on the following indented
-- lines. Pair with 'indentLines' on the children to splice them in.
prettyKvHeader :: Text -> Text
prettyKvHeader k = "  " <> ansi dim (k <> "=")

-- | Wrap text in an ANSI SGR sequence; the sequence resets at the end.
ansi :: Text -> Text -> Text
ansi code s = "\ESC[" <> code <> "m" <> s <> "\ESC[0m"

-- | ANSI dim (faint) SGR parameter.
dim :: Text
dim = "2"

-- | Orphan: a generic 'Value' falls back to the standard flatten so the
-- bench (@hydra-node\/bench\/logging\/Main.hs@) and 'Value'-using tests
-- compile without per-call instances.
instance PrettyError Value where
  showPretty = genericFlatten
