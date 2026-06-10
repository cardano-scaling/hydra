{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

-- | Small shared view helpers used across the panels: layout primitives, JSON
-- rendering and short-text formatting.
module HydraVis.UI.Widgets (
  panelStyle,
  row,
  italic,
  mInt,
  renderJson,
  jsonText,
  jsonStringy,
  shortText,
  shortParty,
) where

import Hydra.Prelude

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Hydra.Tx.Party (Party)
import HydraVis.UI.Model (Action)
import Miso (View)
import Miso.Html (code_, div_, pre_, span_, text)
import Miso.Html.Property (styleInline_)
import Miso.String (MisoString, ms)

panelStyle :: MisoString
panelStyle =
  "border: 1px solid #ddd; border-radius: 4px; padding: 0.75rem 1rem; \
  \margin-bottom: 1rem; background: #fafafa;"

row :: MisoString -> Text -> View (Action tx)
row k v =
  div_
    [styleInline_ "display: flex; gap: 0.75rem; margin: 0.15rem 0;"]
    [ span_ [styleInline_ "min-width: 12rem; color: #666;"] [text k]
    , span_ [styleInline_ "font-family: ui-monospace, monospace;"] [text (ms v)]
    ]

italic :: MisoString -> View (Action tx)
italic t = div_ [styleInline_ "color: #777; font-style: italic;"] [text t]

mInt :: Int -> MisoString
mInt n = ms (show n :: Text)

renderJson :: ToJSON a => a -> View (Action tx)
renderJson x =
  pre_
    [ styleInline_
        "background: white; border: 1px solid #eee; border-radius: 3px; \
        \padding: 0.5rem; overflow-x: auto; font-size: 0.85rem; \
        \max-height: 30rem; overflow-y: auto;"
    ]
    [ code_ [] [text (ms (jsonText x))]
    ]

jsonText :: ToJSON a => a -> Text
jsonText = TE.decodeUtf8 . BSL.toStrict . encodePretty

-- | Render a value whose JSON is a plain string (e.g. 'HeadId', which @show@s
-- as raw escaped bytes) via that string. Falls back to compact JSON otherwise.
jsonStringy :: ToJSON a => a -> Text
jsonStringy x = case Aeson.toJSON x of
  Aeson.String t -> t
  other -> TE.decodeUtf8 (BSL.toStrict (Aeson.encode other))

shortText :: Text -> Text
shortText t
  | T.length t <= 28 = t
  | otherwise = T.take 24 t <> "..."

shortParty :: Party -> Text
shortParty p = T.take 16 (T.drop (T.length ("Party {vkey = \"" :: Text)) (show p))
