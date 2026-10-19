-- | Template haskell expression to embed executables.
module Hydra.Node.EmbedTH where

import Hydra.Prelude

import Data.FileEmbed (embedFile)
import Language.Haskell.TH (Exp, Q, runIO)
import System.Directory (findExecutable)
import System.Environment qualified as Env

-- | Embed an executable selected by an environment variable, falling back to
-- PATH for native development builds. Cross builds supply the target executable
-- explicitly, independently of the tools running on the build machine.
embedExecutable :: String -> String -> Q Exp
embedExecutable pathVariable exe = do
  fp <- runIO $ do
    configured <- Env.lookupEnv pathVariable
    candidate <- maybe (findExecutable exe) (pure . Just) configured
    case candidate of
      Nothing -> fail $ exe <> " not found, ensure it is in PATH when compiling (and do a cabal clean)"
      Just fp -> do
        putStrLn $ "Embedding " <> exe <> " from: " <> fp
        pure fp
  embedFile fp
