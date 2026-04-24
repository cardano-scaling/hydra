module Hydra.TUI.Drawing.EventHistoryTab where

import Hydra.Prelude

import Brick
import Brick.Widgets.Border (borderWithLabel, hBorderWithLabel)
import Brick.Widgets.List qualified as BrickList
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (TimeZone, utcToLocalTime)
import Hydra.TUI.Logging.Types (LogMessage (..), Severity (..))
import Hydra.TUI.Model
import Hydra.TUI.Style hiding (style)
import Lens.Micro ((^.))

drawEventHistoryTab :: RootState -> Widget Name
drawEventHistoryTab s =
  borderWithLabel (withAttr neutral $ txt " Event History ") $
    vBox
      [ vLimitPercent 50 $ BrickList.renderList (drawEventListItem tz) True (s ^. eventHistoryListL)
      , hBorderWithLabel (withAttr neutral $ txt detailLabel)
      , viewport "event-detail" Vertical $
          padLeftRight 1 $
            drawEventDetail tz rawView (BrickList.listSelectedElement (s ^. eventHistoryListL))
      ]
 where
  tz = s ^. timeZoneL
  rawView = s ^. eventDetailRawL
  detailLabel = if rawView then " Detail (raw)  d:summary " else " Detail  d:raw "

-- | Render a single log entry for display in event lists.
-- Also used by 'drawMainTab' for the recent-events strip.
drawEventListItem :: TimeZone -> Bool -> LogMessage -> Widget Name
drawEventListItem tz selected (LogMessage{message, severity, time}) =
  let ts = formatTime defaultTimeLocale "%b %d %H:%M:%S" (utcToLocalTime tz time)
      line = str ts <+> txt "  " <+> txt severityIcon <+> txt "  " <+> txt message
      styled = case severity of
        Success | not selected -> withAttr infoA line
        _ -> line
   in if selected then withAttr BrickList.listSelectedAttr styled else styled
 where
  severityIcon = case severity of
    Success -> "✓"
    Info -> "·"
    Error -> "✗"

drawEventDetail :: TimeZone -> Bool -> Maybe (Int, LogMessage) -> Widget Name
drawEventDetail _ _ Nothing = withAttr neutral $ txt "No event selected."
drawEventDetail tz rawView (Just (_, LogMessage{detail, severity, time, rawJson})) =
  vBox
    [ hBox
        [ withAttr (severityToAttr severity) $ txt (severityLabel severity)
        , txt "  "
        , str $ formatTime defaultTimeLocale "%b %d %Y  %H:%M:%S" (utcToLocalTime tz time)
        ]
    , txt " "
    , body
    ]
 where
  severityLabel = \case
    Success -> "Success"
    Info -> "Info"
    Error -> "Error"
  body
    | rawView = txtWrap rawJson
    | otherwise = txtWrap detail
