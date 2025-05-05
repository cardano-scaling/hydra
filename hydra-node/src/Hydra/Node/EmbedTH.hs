-- | Template haskell expression to embed executables.
module Hydra.Node.EmbedTH where

import Hydra.Prelude

import Data.FileEmbed (embedFile)
import Language.Haskell.TH (Exp, Q, runIO)
import System.Directory (findExecutable)

-- | Template haskell expression to find and embed an executable with given name.
embedExecutable :: String -> Q Exp
embedExecutable exe = do
  fp <- runIO $ do
    findExecutable exe >>= \case
      Nothing -> fail $ exe <> " not found, ensure it is in PATH when compiling (and do a cabal clean)"
      Just fp -> do
        putStrLn $ "Embedding " <> exe <> " from: " <> fp
        pure fp
  embedFile fp
