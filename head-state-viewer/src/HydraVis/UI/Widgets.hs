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
  fmtTime,
  shortParty,
  shortAddr,
  UtxoSummary (..),
  summariseUtxo,
  datumText,
  lovelaceText,
  adaText,
  commas,
) where

import Hydra.Prelude

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
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

-- | A fixed-width time rendering @YYYY-MM-DD HH:MM:SS UTC@: drops the
-- variable-length fractional seconds from 'show' so the readout does not
-- jump width as the cursor moves.
fmtTime :: UTCTime -> Text
fmtTime t =
  let s = show t
   in if T.isInfixOf "." s then T.takeWhile (/= '.') s <> " UTC" else s

shortParty :: Party -> Text
shortParty p = T.take 16 (T.drop (T.length ("Party {vkey = \"" :: Text)) (show p))

shortAddr :: Text -> Text
shortAddr a
  | T.length a <= 24 = a
  | otherwise = T.take 14 a <> "..." <> T.takeEnd 6 a

-- * UTxO / lovelace helpers (shared by the snapshot panel and the compare view)

-- | Per-output datum presence, to spot inline datums being dropped at fanout
-- (see #2569 / #1598).
data DatumKind = KInline | KHash | KNone
  deriving stock (Eq)

-- | A breakdown of a UTxO-ish value. Derived by walking its JSON so it works
-- for both a Cardano UTxO (object keyed by input, per-output @address@ /
-- @value.lovelace@ / @inlineDatum@ / @datumhash@) and the output-set arrays in
-- fanout events. A value-less ledger (e.g. SimpleTx, an array of ints) yields
-- 'usValued' False and only an entry count.
data UtxoSummary = UtxoSummary
  { usEntries :: Int
  , usValued :: Bool
  , usTotalLovelace :: Integer
  , usByAddress :: [(Text, Integer)]
  , usInline :: Int
  , usDatumHash :: Int
  , usPlain :: Int
  }

summariseUtxo :: ToJSON a => a -> UtxoSummary
summariseUtxo = summariseJson . Aeson.toJSON

summariseJson :: Aeson.Value -> UtxoSummary
summariseJson v =
  let entries = case v of
        Aeson.Object o -> KM.elems o
        Aeson.Array a -> V.toList a
        _ -> []
      outs = [o | Aeson.Object o <- entries]
      perAddr = [pair | o <- outs, Just pair <- [addrLovelace o]]
      byAddr = Map.toList (Map.fromListWith (+) perAddr)
      kinds = map datumKind outs
      kount k = length (filter (== k) kinds)
   in UtxoSummary
        { usEntries = length entries
        , usValued = not (null outs)
        , usTotalLovelace = sum (snd <$> perAddr)
        , usByAddress = sortOn (negate . snd) byAddr
        , usInline = kount KInline
        , usDatumHash = kount KHash
        , usPlain = kount KNone
        }
 where
  addrLovelace o = do
    addr <- KM.lookup "address" o >>= asText
    let love = fromMaybe 0 (KM.lookup "value" o >>= asObject >>= KM.lookup "lovelace" >>= asInteger)
    pure (addr, love)
  datumKind o
    | present "inlineDatum" o = KInline
    | present "datumhash" o = KHash
    | otherwise = KNone
  present k o = case KM.lookup k o of
    Just Aeson.Null -> False
    Just _ -> True
    Nothing -> False
  asText = \case Aeson.String t -> Just t; _ -> Nothing
  asObject = \case Aeson.Object o -> Just o; _ -> Nothing
  asInteger = \case Aeson.Number n -> Just (round n); _ -> Nothing

-- | Per-output datum tally, e.g. @"2 inline, 0 hash, 3 none"@.
datumText :: UtxoSummary -> Text
datumText s =
  show (usInline s) <> " inline, " <> show (usDatumHash s) <> " hash, " <> show (usPlain s) <> " none"

-- | Render lovelace with thousands separators alongside the ADA value.
lovelaceText :: Integer -> Text
lovelaceText n = commas n <> " lovelace (" <> adaText n <> " ADA)"

adaText :: Integer -> Text
adaText n =
  let (q, r) = abs n `divMod` 1_000_000
      frac = T.dropWhileEnd (== '0') (T.justifyRight 6 '0' (show r))
      frac' = if T.null frac then "0" else frac
   in (if n < 0 then "-" else "") <> commas q <> "." <> frac'

commas :: Integer -> Text
commas n =
  let digits = show (abs n) :: Text
      chunks = map T.reverse (chunk3 (T.reverse digits))
   in (if n < 0 then "-" else "") <> T.intercalate "," (reverse chunks)
 where
  chunk3 t
    | T.null t = []
    | otherwise = T.take 3 t : chunk3 (T.drop 3 t)
