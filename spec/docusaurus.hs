#!/usr/bin/env runhaskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Debug.Trace
import Text.Pandoc.JSON
import Text.Pandoc.Walk

-- | Various tweaks to make a document better suited for Docusaurus markdown
-- rendering.
main :: IO ()
main =
  toJSONFilter $
    removeRawTex .
      walk (fixFigures . demoteHeaders)

-- | Remove all raw tex blocks. Even after resolving latex macros those are
-- still included in the AST.
removeRawTex :: [Block] -> [Block]
removeRawTex = filter $ \case
  RawBlock format _ -> format /= Format "tex"
  _ -> True

-- | Demote headers by one.
demoteHeaders :: Block -> Block
demoteHeaders = \case
  Header level attr content -> Header (level + 1) attr content
  x -> x

-- | Remove alt text on images in figures (captions are enough).
fixFigures :: Block -> Block
fixFigures = \case
  Figure attr caption content -> Figure attr caption $ walk dropImageAltText content
  x -> x

dropImageAltText :: Inline -> Inline
dropImageAltText = \case
  Image attr _alt target -> Image attr [] target
  x -> x
