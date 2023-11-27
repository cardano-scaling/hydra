#!/usr/bin/env runhaskell
{-# LANGUAGE LambdaCase #-}
import Text.Pandoc.JSON
import Text.Pandoc.Walk
import Debug.Trace

main :: IO ()
main = toJSONFilter fixFigures

fixFigures :: Block -> Block
fixFigures = \case
  Figure attr caption content -> Figure attr caption $ walk dropImageAltText content
  x -> x

dropImageAltText :: Inline -> Inline
dropImageAltText = \case
  Image attr _alt target -> Image attr [] target
  x -> x
