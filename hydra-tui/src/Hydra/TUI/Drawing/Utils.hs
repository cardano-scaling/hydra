{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.TUI.Drawing.Utils where

import Hydra.Prelude

import Brick (Padding (..), Widget, padLeft, str, txt, vBox, withAttr, (<+>))
import Cardano.Api.UTxO qualified as UTxO
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime, utctDayTime)
import Data.Time.Format (FormatTime)
import Hydra.Cardano.Api hiding (Active)
import Hydra.TUI.Style (infoA, own, positive)
import Hydra.Tx (HeadId)

-- | Render a 'HeadId' as its hex-encoded bytes (matches the Main tab display).
prettyHeadId :: HeadId -> Text
prettyHeadId = serialiseToRawBytesHexText

-- | Render a 'TxId' as its hex-encoded bytes (no quotes, no wrapper type).
prettyTxId :: TxId -> Text
prettyTxId = serialiseToRawBytesHexText

drawHex :: SerialiseAsRawBytes a => a -> Widget n
drawHex = txt . ("⚬ " <>) . serialiseToRawBytesHexText

drawShow :: forall a n. Show a => a -> Widget n
drawShow = txt . show

-- | Render a full address as text.
drawAddress :: AddressInEra -> Widget n
drawAddress addr = txt (serialiseAddress addr)

-- | Highlight the given address if it matches the own address.
highlightOwnAddress :: AddressInEra -> AddressInEra -> Widget n
highlightOwnAddress ownAddress a =
  withAttr (if a == ownAddress then own else mempty) $ drawAddress a

-- | Render a UTxO map grouped by address, with ADA amounts.
drawUTxO :: (AddressInEra -> Widget n) -> UTxO -> Widget n
drawUTxO f utxo =
  let byAddress =
        Map.foldrWithKey
          (\k v@TxOut{txOutAddress = addr} -> Map.unionWith (++) (Map.singleton addr [(k, v)]))
          mempty
          $ UTxO.toMap utxo
   in vBox
        [ vBox
          [ f addr
          , padLeft (Pad 2) $ vBox (drawUTxOEntryAda <$> u)
          ]
        | (addr, u) <- Map.toList byAddress
        ]

-- | Render a single UTxO entry: last part of the TxIn plus its ADA value.
drawUTxOEntryAda :: (TxIn, TxOut CtxUTxO) -> Widget n
drawUTxOEntryAda (txin, TxOut _ val _ _) =
  txt (T.drop 54 (renderTxIn txin) <> "  ") <+> withAttr infoA (txt (renderAda val))

-- | Render a lovelace value as ADA with the ₳ symbol.
renderAda :: Value -> Text
renderAda v =
  let Coin l = selectLovelace v
      (ada, frac) = abs l `divMod` 1_000_000
      fracStr = show frac
      padded = replicate (6 - length fracStr) '0' <> fracStr
      sign = if l < 0 then "-" else ""
   in sign <> "₳ " <> show ada <> "." <> toText padded

-- | Format a time duration as "Xd Xh Xm Xs".
renderTime :: (Ord t, Num t, FormatTime t) => t -> String
renderTime r
  | r < 0 = "-" <> renderTime (negate r)
  | otherwise = formatTime defaultTimeLocale "%dd %Hh %Mm %Ss" r

-- | Animated spinner character cycling with wall-clock time.
spinnerFrame :: UTCTime -> Text
spinnerFrame now =
  let frames = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"] :: [Text]
      s = floor (realToFrac (utctDayTime now) :: Double) `mod` (10 :: Int)
   in case drop s frames of
        (f : _) -> f
        [] -> "⠋"

-- | Remaining contestation time, or a "ready to fan out" notice.
-- Used by both the Main tab (head state panel) and the Funds tab (closed state).
drawRemainingContestationPeriod :: UTCTime -> UTCTime -> Widget n
drawRemainingContestationPeriod deadline now =
  let remaining = diffUTCTime deadline now
   in if remaining > 0
        then txt "Remaining time to contest: " <+> str (renderTime remaining)
        else withAttr positive $ txt "Contestation period passed — ready to fan out."
