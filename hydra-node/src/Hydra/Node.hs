{-# LANGUAGE DerivingStrategies #-}

-- | Top-level module to run a single Hydra node
module Hydra.Node where

import Cardano.Prelude

runHydra :: IO ()
runHydra =
  startStopNode $
    const $ do
      putStrLn ("Hydra started" :: Text)
      waitForInput
 where
  waitForInput = void getLine

-- | Current state of a Hydra node
newtype HydraNode = HydraNode {nodeThread :: Maybe (Async ())}

-- | High-level lifecycle states for a Hydra node
data NodeStatus = NotReady | Ready
  deriving stock (Eq, Show)

startNode :: IO ()
startNode = forever $ threadDelay oneSecond

oneSecond :: Int
oneSecond = 1_000_000

stopNode :: HydraNode -> IO HydraNode
stopNode node =
  case nodeThread node of
    Just thread -> cancel thread >> pure (HydraNode Nothing)
    Nothing -> pure node

startStopNode :: (HydraNode -> IO ()) -> IO ()
startStopNode act = withAsync startNode $ \thread -> do
  let node = HydraNode (Just thread)
  res <- act node
  void $ stopNode node
  pure res

status :: HydraNode -> IO NodeStatus
status node =
  case nodeThread node of
    Just{} -> pure Ready
    Nothing -> pure NotReady

buildInitialTransaction :: HydraNode -> HeadParameters -> Transaction
buildInitialTransaction = panic "not implemented"

data Transaction = Transaction

numberOfOutputs :: Transaction -> Int
numberOfOutputs = panic "not implemented"

data HeadParameters = HeadParameters
  { verificationKeys :: [VerificationKey]
  }

newtype VerificationKey = VerificationKey {unverificationKey :: ByteString}
