{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Lens (asIndex, to, (&), (^.), (^?))
import Data.Aeson
import qualified Data.Aeson.Key as Key
import Data.Aeson.Lens
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromJust)
import Data.String
import Data.Text (Text)
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy.Encoding (decodeLatin1)
import Shh
import System.Environment

runRemoteTxCost :: ByteString -> Integer -> IO ByteString
runRemoteTxCost revision seed = do
  exe
    "nix"
    "run"
    ("git+https://github.com/cardano-scaling/hydra?" <> revision <> "#tx-cost")
    "--"
    "--seed"
    seed
    |> capture

runLocalTxCost :: Integer -> IO ByteString
runLocalTxCost seed = do
  exe
    "nix"
    "run"
    ".#tx-cost"
    "--"
    "--seed"
    seed
    |> capture

runPandoc :: FilePath -> FilePath -> IO ByteString
runPandoc i o = do
  exe
    "pandoc"
    "-i"
    i
    "-o"
    o
    |> capture

extractHeaders :: ByteString -> IO ByteString
extractHeaders md = do
  exe "echo" md |> exe "grep" "##" |> capture

runPandasDiff :: FilePath -> FilePath -> FilePath -> IO ByteString
runPandasDiff headers old new = do
  exe
    "./scripts/diff.py"
    headers
    old
    new
    |> capture

main :: IO ()
main = do
  args <- getArgs
  let revision =
        case args of
          [x] -> "rev=" <> x
          _ -> "ref=master"
  a <- runLocalTxCost 0
  b <- runRemoteTxCost (fromString revision) 0
  LBS.writeFile "new.md" a
  LBS.writeFile "old.md" b
  runPandoc "new.md" "new.html"
  runPandoc "old.md" "old.html"
  x <- extractHeaders a
  y <- extractHeaders b
  LBS.writeFile "new-headers.txt" x
  k <- runPandasDiff "new-headers.txt" "old.html" "new.html"
  LBS.writeFile "diff.md" k
