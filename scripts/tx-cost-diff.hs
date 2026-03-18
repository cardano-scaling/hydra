{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.List (intercalate)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Shh
import System.Environment
import Text.Pandoc
import Text.Pandoc.Definition

-- | A table as a list of rows, each a list of plain-text cells.
type MdTable = [[Text]]

-- | Extract the plain text from a list of Inline elements.
inlineText :: [Inline] -> Text
inlineText = T.concat . map go
 where
  go (Str t) = t
  go Space = " "
  go SoftBreak = " "
  go (Emph is) = inlineText is
  go (Strong is) = inlineText is
  go (Code _ t) = t
  go LineBreak = " "
  go _ = ""

-- | Extract plain text from a table Cell.
cellText :: Cell -> Text
cellText (Cell _ _ _ _ blocks) = T.strip $ T.concat $ map blockText blocks
 where
  blockText (Plain is) = inlineText is
  blockText (Para is) = inlineText is
  blockText _ = ""

-- | Convert a pandoc Row to a list of cell texts.
rowTexts :: Row -> [Text]
rowTexts (Row _ cells) = map cellText cells

-- | Parse a markdown document into [(section heading, table rows)].
-- Pairs each level-2 Header with the first Table block following it.
parseSections :: Text -> IO [(Text, MdTable)]
parseSections md = do
  Pandoc _ blocks <- runIOorExplode $ readMarkdown def md
  return $ go blocks Nothing []
 where
  go [] _ acc = reverse acc
  go (b : bs) cur acc = case b of
    Header 2 _ inlines ->
      go bs (Just (inlineText inlines)) acc
    Table _ _ _ (TableHead _ headRows) bodies _ ->
      case cur of
        Nothing -> go bs cur acc
        Just heading ->
          let cols = concatMap rowTexts headRows
              dataRows = concatMap (\(TableBody _ _ _ rows) -> map rowTexts rows) bodies
           in go bs Nothing ((heading, cols : dataRows) : acc)
    _ -> go bs cur acc

-- | Parse a numeric cell value, returning Nothing if not a number.
parseNum :: Text -> Maybe Double
parseNum t = case reads (T.unpack t) of
  [(n, "")] -> Just n
  [(n, s)] | all (`elem` (" \t" :: String)) s -> Just n
  _ -> Nothing

-- | Diff two tables: header row + data rows indexed by first column.
-- Returns Just (colHeaders, [(index, [diff])]) or Nothing if nothing changed.
diffTables :: MdTable -> MdTable -> Maybe (MdTable, [(Text, [Double])])
diffTables [] _ = Nothing
diffTables _ [] = Nothing
diffTables (oldHdr : oldRows) (newHdr : newRows)
  | null changed = Nothing
  | otherwise = Just (newHdr : [], changed)
 where
  -- Build lookup from first-column key to remaining cells
  oldMap = [(head r, tail r) | r <- oldRows, not (null r)]
  cols = drop 1 newHdr

  changed = mapMaybe diffRow newRows

  diffRow newRow
    | null newRow = Nothing
    | otherwise =
        let idx = head newRow
            newVals = tail newRow
         in case lookup idx oldMap of
              Nothing -> Nothing
              Just oldVals ->
                let diffs =
                      [ nv - ov
                      | (mnv, mov) <- zip (map parseNum newVals) (map parseNum oldVals)
                      , let nv = fromMaybe 0 mnv
                      , let ov = fromMaybe 0 mov
                      ]
                 in if all (== 0) diffs
                      then Nothing
                      else Just (idx, diffs)

-- | Format a numeric diff value with colour for improvements.
formatDiff :: Double -> Text
formatDiff d
  | d == 0 = "-"
  | d > 0 = "+" <> T.pack (showFixed d)
  | otherwise = "$$\\color{green}" <> T.pack (showFixed d) <> "$$"
 where
  showFixed x =
    let rounded = fromIntegral (round (x * 100) :: Int) / 100.0 :: Double
     in show rounded

-- | Render a diff result as a Markdown table.
renderDiff :: Text -> [Text] -> [(Text, [Double])] -> Text
renderDiff heading colHeaders rows =
  T.unlines $
    [ ""
    , "## " <> heading
    , ""
    , headerLine
    , sepLine
    ]
      ++ map dataLine rows
 where
  allCols = colHeaders
  w = 14

  pad t =
    let s = T.unpack t
     in T.pack $ s ++ replicate (max 0 (w - length s)) ' '

  headerLine = "| " <> T.intercalate " | " (pad "Row" : map pad allCols) <> " |"
  sepLine = "| " <> T.intercalate " | " (replicate (1 + length allCols) (T.replicate w "-")) <> " |"
  dataLine (idx, diffs) =
    "| " <> T.intercalate " | " (pad idx : map (pad . formatDiff) diffs) <> " |"

runRemoteTxCost :: ByteString -> Integer -> IO ByteString
runRemoteTxCost revision seed =
  exe
    "nix"
    "run"
    ("git+https://github.com/cardano-scaling/hydra?" <> revision <> "#tx-cost")
    "--"
    "--seed"
    seed
    |> capture

runLocalTxCost :: Integer -> IO ByteString
runLocalTxCost seed =
  exe
    "nix"
    "run"
    ".#tx-cost"
    "--"
    "--seed"
    seed
    |> capture

main :: IO ()
main = do
  args <- getArgs
  let revision =
        case args of
          [x] -> "rev=" <> x
          _ -> "ref=master"

  newMd <- T.pack . TL.unpack . decodeUtf8 <$> runLocalTxCost 0
  oldMd <- T.pack . TL.unpack . decodeUtf8 <$> runRemoteTxCost (fromString revision) 0

  newSections <- parseSections newMd
  oldSections <- parseSections oldMd

  -- Diff sections by position; new headings win (master may have different names)
  let diffs = flip mapMaybe (zip newSections oldSections) $
        \((heading, newTable), (_, oldTable)) ->
          case diffTables oldTable newTable of
            Nothing -> Nothing
            Just (newHdr, rows) ->
              Just (heading, drop 1 (head newHdr), rows)

  let header = "# Transaction cost differences\n\n"

  let body
        | null diffs = "No cost or size differences found.\n"
        | otherwise = T.concat [renderDiff h cs rs | (h, cs, rs) <- diffs]

  LBS.writeFile "diff.md" $ encodeUtf8 $ TL.pack $ T.unpack $ header <> body
