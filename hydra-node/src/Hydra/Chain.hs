{-# LANGUAGE DeriveAnyClass #-}

module Hydra.Chain where

import Cardano.Prelude
import Control.Monad.Class.MonadThrow (MonadThrow)
import Hydra.HeadLogic (OnChainTx)

data ChainError = ChainError
  deriving (Exception, Show)

-- | Handle to interface with the main chain network
newtype Chain tx m = Chain
  { -- | Construct and send a transaction to the main chain corresponding to the
    -- given 'OnChainTx' event.
    -- Does at least throw 'ChainError'.
    postTx :: MonadThrow m => OnChainTx tx -> m ()
  }
