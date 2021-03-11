-- | Top-level module to run a single Hydra node
{-# LANGUAGE DerivingStrategies #-}
module Hydra.Node where

import Cardano.Prelude

runHydra :: IO ()
runHydra = panic "Not implemented"

nodeStatus :: HydraNode -> IO NodeStatus
nodeStatus = panic "not implemented"

stopHydraNode :: HydraNode -> IO ()
stopHydraNode = panic "not implemented"

startHydraNode :: IO HydraNode
startHydraNode = panic "not implemented"

data HydraNode = HydraNode

data NodeStatus = NotReady | Ready
  deriving stock (Eq, Show)
