module Hydra.TUI.Style where

import "brick" Brick (
  AttrMap,
  AttrName,
  attrMap,
  attrName,
  fg,
 )
import "brick" Brick.Forms (focusedFormInputAttr, invalidFormInputAttr)
import "vty" Graphics.Vty (

import Hydra.TUI.Logging.Types (Severity (..))
  brightBlack,
  brightBlue,
  defAttr,
  green,
  red,
  yellow,
 )

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

style :: s -> AttrMap
style _ =
  attrMap
    defAttr
    [ (infoA, fg brightBlue)
    , (negative, fg red)
    , (positive, fg green)
    , (neutral, fg brightBlack)
    , (own, fg yellow)
    , -- Brick forms
      (focusedFormInputAttr, fg brightBlue)
    , (invalidFormInputAttr, fg red)
    ]
