{-# LANGUAGE DerivingStrategies #-}

-- | Top-level module to run a single Hydra node
module Hydra.Node where

import Cardano.Prelude

runHydra :: IO ()
runHydra = do
  node <- startHydraNode
  putStrLn ("Hydra started" :: Text)
  void $ stopHydraNode node

-- | Current state of a Hydra node
newtype HydraNode = HydraNode {nodeThread :: Maybe (Async ())}

-- | High-level lifecycle states for a Hydra node
data NodeStatus = NotReady | Ready
  deriving stock (Eq, Show)

stopHydraNode :: HydraNode -> IO HydraNode
stopHydraNode node =
  case nodeThread node of
    Just thread -> cancel thread >> pure (HydraNode Nothing)
    Nothing -> pure node

startHydraNode :: IO HydraNode
startHydraNode = do
  signal <- newEmptyMVar
  thread <- async $ takeMVar signal
  pure $ HydraNode (Just thread)

nodeStatus :: HydraNode -> IO NodeStatus
nodeStatus node =
  case nodeThread node of
    Just{} -> pure Ready
    Nothing -> pure NotReady
