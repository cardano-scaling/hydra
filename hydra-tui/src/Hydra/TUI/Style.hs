module Hydra.TUI.Style where

import Brick (
  AttrMap,
  AttrName,
  attrMap,
  attrName,
  fg,
  on,
 )
import Brick.Forms (focusedFormInputAttr, invalidFormInputAttr)
import Brick.Widgets.Border (borderAttr)
import Brick.Widgets.List (listSelectedAttr)
import Graphics.Vty (
  black,
  blue,
  bold,
  brightBlack,
  brightWhite,
  cyan,
  defAttr,
  green,
  italic,
  magenta,
  red,
  withForeColor,
  withStyle,
  yellow,
 )
import Hydra.TUI.Logging.Types (Severity (..))

severityToAttr :: Severity -> AttrName
severityToAttr = \case
  Success -> positive
  Info -> infoA
  Error -> negative

infoA :: AttrName
infoA = attrName "info"

positive :: AttrName
positive = attrName "positive"

negative :: AttrName
negative = attrName "negative"

neutral :: AttrName
neutral = attrName "neutral"

own :: AttrName
own = attrName "own"

activeTabA :: AttrName
activeTabA = attrName "activeTab"

keyA :: AttrName
keyA = attrName "key"

pendingA :: AttrName
pendingA = attrName "pending"

headStateA :: AttrName
headStateA = attrName "headState"

actionDescA :: AttrName
actionDescA = attrName "actionDesc"

-- | Dark theme — optimised for dark terminal backgrounds.
darkStyle :: s -> AttrMap
darkStyle _ =
  attrMap
    defAttr
    [ (infoA, fg blue)
    , (negative, fg red)
    , (positive, fg green)
    , (neutral, fg brightBlack)
    , (own, fg yellow)
    , (activeTabA, brightWhite `on` blue)
    , (keyA, withStyle (withStyle defAttr bold) italic)
    , (pendingA, fg magenta)
    , (headStateA, withStyle (fg magenta) bold)
    , (actionDescA, withStyle defAttr italic)
    , (listSelectedAttr, brightWhite `on` blue)
    , (borderAttr, fg brightBlack)
    , (focusedFormInputAttr, fg blue)
    , (invalidFormInputAttr, fg red)
    ]

-- | Light theme — optimised for light terminal backgrounds.
lightStyle :: s -> AttrMap
lightStyle _ =
  attrMap
    (withForeColor defAttr black)
    [ (infoA, fg blue)
    , (negative, fg red)
    , (positive, fg green)
    , (neutral, fg black)
    , (own, fg cyan)
    , (activeTabA, brightWhite `on` blue)
    , (keyA, withStyle (withStyle defAttr bold) italic)
    , (pendingA, fg magenta)
    , (headStateA, withStyle (fg blue) bold)
    , (actionDescA, withStyle defAttr italic)
    , (listSelectedAttr, brightWhite `on` blue)
    , (borderAttr, fg black)
    , (focusedFormInputAttr, fg blue)
    , (invalidFormInputAttr, fg red)
    ]

-- | Backwards-compatible alias; defaults to the dark theme.
style :: s -> AttrMap
style = darkStyle
