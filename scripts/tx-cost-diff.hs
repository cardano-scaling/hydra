{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

import "aeson" Data.Aeson
import "aeson" Data.Aeson.Key qualified as Key
import "base" Data.Maybe (fromJust)
import "base" Data.String
import "base" System.Environment
import "bytestring" Data.ByteString.Lazy (ByteString)
import "bytestring" Data.ByteString.Lazy qualified as LBS
import "lens" Control.Lens (asIndex, to, (&), (^.), (^?))
import "lens-aeson" Data.Aeson.Lens
import "shh" Shh
import "text" Data.Text (Text)
import "text" Data.Text.Lazy qualified as Text
import "text" Data.Text.Lazy.Encoding (decodeLatin1)

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
