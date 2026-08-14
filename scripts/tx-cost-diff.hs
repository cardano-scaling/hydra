{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- \| Compare the on-chain cost measurements of this working tree against another
-- revision and render the difference as Markdown.
--
-- Both sides are produced by @tx-cost --json@, which emits the same numbers as
-- the Markdown report in a structured form: a list of tables, each carrying the
-- names of its index columns (which identify a row) and of its value columns.
-- Rows are matched across revisions by index, so a row that only exists on one
-- side is dropped rather than silently compared against the wrong one.
import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.:))
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.List (intercalate, isInfixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Numeric (showFFloat)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), die)
import System.IO (IOMode (WriteMode), hPutStr, hSetEncoding, stdout, utf8, withFile)
import System.Process.Typed (proc, readProcess)

-- | Why a side of the comparison could not be obtained.
data RunError
  = -- | The revision predates @tx-cost --json@; expected on the change that adds it.
    NoJsonSupport String
  | Failed String Int String
  | Undecodable String String

newtype Report = Report {tables :: [Table]}

instance FromJSON Report where
  parseJSON = withObject "Report" $ \o -> Report <$> o .: "tables"

data Table = Table
  { title :: String
  , index :: [String]
  , columns :: [String]
  , rows :: [Row]
  }

instance FromJSON Table where
  parseJSON = withObject "Table" $ \o ->
    Table <$> o .: "title" <*> o .: "index" <*> o .: "columns" <*> o .: "rows"

data Row = Row {key :: [String], values :: [Double]}

instance FromJSON Row where
  parseJSON = withObject "Row" $ \o -> Row <$> o .: "key" <*> o .: "values"

main :: IO ()
main = do
  revision <-
    getArgs >>= \case
      [rev] -> pure ("rev=" <> rev)
      _ -> pure "ref=master"
  new <- run ".#tx-cost" >>= either (die . describe) pure
  -- The baseline is built from another revision, which may predate `--json`.
  -- Only THAT is tolerated: any other failure (nix eval, network, a crash) is
  -- fatal, so an infrastructure problem cannot masquerade as "no differences".
  run ("git+https://github.com/cardano-scaling/hydra?" <> revision <> "#tx-cost") >>= \case
    Right old -> report (render (skipped old new) (diff old new))
    Left err@(NoJsonSupport _) -> report (unsupportedBaseline (describe err))
    Left err -> die (describe err)
 where
  -- NOTE: .github/workflows/tx-cost-diff.yaml uploads diff.md as an artifact
  -- and uses it verbatim as the PR comment body, so the file must always be
  -- written. Also echoed to stdout so the job log shows what was posted.
  -- NOTE: the report always contains non-ASCII (νHead, μHead, "Min fee ₳"), so
  -- the encoding is pinned rather than inherited from the locale: under a
  -- non-UTF-8 locale the default would die mid-write and leave a truncated
  -- diff.md, which the workflow's if-no-files-found guard would not catch.
  report markdown = do
    withFile "diff.md" WriteMode $ \h -> do
      hSetEncoding h utf8
      hPutStr h markdown
    hSetEncoding stdout utf8
    putStrLn markdown

  run flakeRef = do
    (code, out, err) <- readProcess (proc "nix" ["run", flakeRef, "--", "--json", "--seed", "0"])
    pure $ case code of
      -- optparse-applicative exits 1 and says so on stderr when the baseline
      -- predates the flag; anything else is a genuine failure.
      ExitFailure _
        | "--json" `isInfixOf` LBS8.unpack err -> Left (NoJsonSupport flakeRef)
      ExitFailure n -> Left (Failed flakeRef n (LBS8.unpack err))
      ExitSuccess -> first (Undecodable flakeRef) (eitherDecode out)

  describe = \case
    NoJsonSupport ref -> ref <> " does not support --json"
    Failed ref n e -> ref <> " exited with code " <> show n <> "\n" <> e
    Undecodable ref e -> "could not decode the report from " <> ref <> ": " <> e

  unsupportedBaseline err =
    unlines
      [ "# Transaction cost differences"
      , ""
      , "No comparison available: the baseline revision does not report costs as"
      , "JSON, so there is nothing to diff against. This is expected on the change"
      , "that introduces `tx-cost --json`, and resolves once it is on master."
      , ""
      , "<details><summary>Details</summary>"
      , ""
      , "```"
      , err
      , "```"
      , ""
      , "</details>"
      ]

-- | The precision the report is rendered at. Deltas are rounded to this before
-- being compared against zero, so "changed" and "displayed as changed" agree.
roundTo2 :: Double -> Double
roundTo2 x = fromIntegral (round (x * 100) :: Integer) / 100

-- | Tables that could not be compared at all, with the reason. Reported rather
-- than dropped: "no differences" and "not compared" look identical to a reader
-- otherwise, which is how a renamed or reshaped table hides a real cost change.
skipped :: Report -> Report -> [String]
skipped old new =
  [ title t <> reason
  | t <- tables new
  , reason <- case Map.lookup (title t) oldTables of
      Nothing -> [" (not present in the baseline; renamed or new?)"]
      Just before
        | columns before /= columns t -> [" (columns differ: " <> render' (columns before) <> " -> " <> render' (columns t) <> ")"]
        | otherwise -> []
  ]
 where
  oldTables = Map.fromList [(title t, t) | t <- tables old]
  render' = intercalate ", "

-- | Per-table differences, keeping only rows present on both sides.
diff :: Report -> Report -> [(Table, [(Row, [Double])])]
diff old new =
  [ (t, changed t)
  | t <- tables new
  , not (null (changed t))
  ]
 where
  oldTables = Map.fromList [(title t, t) | t <- tables old]

  changed t =
    case Map.lookup (title t) oldTables of
      Nothing -> []
      Just before
        -- Compare like with like. Zipping positionally would subtract, say, the
        -- new "Deposit size" from the old "Tx size" the moment a column is
        -- added or reordered, and report the nonsense as a real cost change.
        | columns before /= columns t -> []
        | otherwise ->
            let previous = Map.fromListWith (flip (++)) [(key r, [values r]) | r <- rows before]
             in concatMap (rowDelta previous) (rows t)

  -- NOTE: index columns are not guaranteed unique (two rows of a table can
  -- share a key), so rows are matched pairwise in order within a key rather
  -- than through a Map that would silently keep only the last of each.
  rowDelta previous r =
    case Map.lookup (key r) previous of
      Just (was : _) ->
        -- Round before deciding whether anything changed, so a delta that is
        -- only noise below the reported precision is dropped rather than
        -- rendered as "-0.00".
        let deltas = zipWith (\n o -> roundTo2 (n - o)) (values r) was
         in [(r, deltas) | not (all (== 0) deltas)]
      _ -> []

render :: [String] -> [(Table, [(Row, [Double])])] -> String
render notes ds =
  intercalate "\n" $
    ["# Transaction cost differences"]
      <> (if null ds then ["", "No cost or size differences found"] else concatMap table ds)
      <> notCompared
 where
  notCompared
    | null notes = []
    | otherwise =
        ["", "> Not compared:"] <> map ("> - " <>) notes

  table (t, rs) =
    [ ""
    , "## " <> title t
    , ""
    , cells (index t <> columns t)
    , cells (map (const "---") (index t) <> map (const "---:") (columns t))
    ]
      <> map (\(r, deltas) -> cells (key r <> map colour deltas)) rs

  cells xs = "| " <> intercalate " | " xs <> " |"

  -- Improvements (a smaller number) are green; regressions keep a leading '+'
  -- so the direction is unambiguous in the rendered comment.
  colour x
    | x == 0 = "-"
    | x > 0 = "+" <> showRounded x
    | otherwise = "$${\\color{green}" <> showRounded x <> "}$$"

  -- NOTE: 'show' on a Double switches to scientific notation for small
  -- magnitudes (0.01 renders as "1.0e-2"), which is unreadable in the PR
  -- comment this ends up in. Fix the decimal places instead, as the pandas
  -- implementation this replaced did.
  showRounded x = showFFloat (Just 2) x ""
