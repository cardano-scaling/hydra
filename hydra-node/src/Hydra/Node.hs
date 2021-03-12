{-# LANGUAGE DerivingStrategies #-}

-- | Top-level module to run a single Hydra node
module Hydra.Node where

import Cardano.Prelude

runHydra :: IO ()
runHydra = do
  node <- startNode
  putStrLn ("Hydra started" :: Text)
  waitForInput
  void $ stopNode node
 where
  waitForInput = void getLine

-- | Current state of a Hydra node
newtype HydraNode = HydraNode {nodeThread :: Maybe (Async ())}

-- | High-level lifecycle states for a Hydra node
data NodeStatus = NotReady | Ready
  deriving stock (Eq, Show)

stopNode :: HydraNode -> IO HydraNode
stopNode node =
  case nodeThread node of
    Just thread -> cancel thread >> pure (HydraNode Nothing)
    Nothing -> pure node

startNode :: IO HydraNode
startNode = do
  signal <- newEmptyMVar
  thread <- async $ takeMVar signal
  pure $ HydraNode (Just thread)

status :: HydraNode -> IO NodeStatus
status node =
  case nodeThread node of
    Just{} -> pure Ready
    Nothing -> pure NotReady
