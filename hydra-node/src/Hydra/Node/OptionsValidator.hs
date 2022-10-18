module Hydra.Node.OptionsValidator where

import Hydra.Prelude

import Hydra.Options (RunOptions (..))

newtype CannotStartHydraNode = CannotStartHydraNode Text deriving (Eq, Show)
instance Exception CannotStartHydraNode

-- | Validate cmd line arguments for hydra-node
-- and check if they make sense before actually running the node.
validateArguments :: MonadThrow m => RunOptions -> m ()
validateArguments RunOptions{peers, hydraVerificationKeys} = do
  let numberOfPeers = 4
  when (length peers > numberOfPeers) $
    throwIO $ CannotStartHydraNode $ "Maximum number of peers is currently " <> show numberOfPeers <> "."
  when (length peers /= length hydraVerificationKeys) $
    throwIO $ CannotStartHydraNode "Number of loaded cardano and hydra keys needs to match."
  pure ()
