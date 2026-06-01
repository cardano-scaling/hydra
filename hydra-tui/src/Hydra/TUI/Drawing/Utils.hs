{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.TUI.Drawing.Utils where

import Hydra.Prelude

import Brick (Padding (..), ViewportType (Vertical), Widget, clickable, padLeft, str, txt, vBox, viewport, withAttr, (<+>))
import Cardano.Api.UTxO qualified as UTxO
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime, utctDayTime)
import Data.Time.Format (FormatTime)
import Hydra.Cardano.Api hiding (Active)
import Hydra.TUI.Style (infoA, own, sectionHeaderA)
import Hydra.Tx (HeadId, IsTx (..))

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

-- | Render a UTxO map grouped by address, with ADA amounts and global 1-based
-- numbering across all entries (so users can confirm scrollable views are scrolling).
drawUTxO :: (AddressInEra -> Widget n) -> UTxO -> Widget n
drawUTxO f utxo =
  let byAddress =
        Map.foldrWithKey
          (\k v@TxOut{txOutAddress = addr} -> Map.unionWith (++) (Map.singleton addr [(k, v)]))
          mempty
          $ UTxO.toMap utxo
      indexedGroups =
        snd $
          mapAccumL
            ( \nextIdx (addr, entries) ->
                let nEntries = length entries
                 in (nextIdx + nEntries, (addr, zip [nextIdx ..] entries))
            )
            (1 :: Int)
            (Map.toList byAddress)
   in vBox
        [ vBox
          [ f addr
          , padLeft (Pad 2) $ vBox (uncurry drawUTxOEntryAda <$> u)
          ]
        | (addr, u) <- indexedGroups
        ]

-- | Render a single UTxO entry: index, last part of the TxIn, and ADA value.
-- The TxIn is shortened to the last 10 characters of the hash plus its
-- '#index' suffix, derived structurally so it stays correct if the rendering
-- of 'TxIn' ever changes width.
drawUTxOEntryAda :: Int -> (TxIn, TxOut CtxUTxO) -> Widget n
drawUTxOEntryAda idx (txin, TxOut _ val _ _) =
  let (hashHex, idxPart) = T.breakOn "#" (renderTxIn txin)
      shortened = T.takeEnd 10 hashHex <> idxPart
   in txt (show idx <> ". " <> shortened <> "  ") <+> withAttr infoA (txt (renderAda val))

-- | Wrap a widget in a clickable, vertically scrollable viewport using a single name
-- for both. The shared name lets the mouse-wheel handler (matching MouseDown on any
-- clickable) target the corresponding viewport, and lets keyboard handlers scroll it
-- by name too.
scrollableViewport :: (Ord n, Show n) => n -> Widget n -> Widget n
scrollableViewport name = clickable name . viewport name Vertical

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
        then withAttr sectionHeaderA $ txt "Remaining time to contest: " <+> str (renderTime remaining)
        else drawFanoutPossibleMessage

-- | Status message shown when the contestation period has passed and a
-- fanout transaction can be submitted. Shared between the Main and Funds
-- tabs so the wording matches.
drawFanoutPossibleMessage :: Widget n
drawFanoutPossibleMessage =
  withAttr sectionHeaderA $ txt "Contestation period passed — ready to fan out."

-- | Status message shown when the head has been finalized, including the
-- total ADA value of the distributed UTxO. Shared between the Main and
-- Funds tabs so the wording matches.
drawHeadFinalizedMessage :: UTxO -> Widget n
drawHeadFinalizedMessage utxo =
  vBox
    [ withAttr sectionHeaderA $ txt "Head finalized."
    , txt ("Distributed UTxO — total: " <> renderAda (balance @Tx utxo))
    ]
