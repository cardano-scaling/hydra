{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Top-level module to run a single Hydra node
module Hydra.Node where

import Cardano.Prelude
import Data.Map.Strict as Map

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

buildInitialTransaction :: HydraNode -> HeadParameters -> IO (Transaction, AssetId)
buildInitialTransaction _ HeadParameters{verificationKeys} = do
  let policyId = PolicyId "MyPolicyId"
  let assetName = AssetName ""
  let mkOutput =
        TransactionOutput
          ( Value
              (Quantity 0)
              (Map.fromList [(policyId, Map.fromList [(assetName, Quantity 1)])])
          )
  let stateMachineOutput = TransactionOutput (Value 0 mempty)
  let outputs = stateMachineOutput : replicate (length verificationKeys) mkOutput
  return (Transaction{outputs}, (policyId, assetName))

data Transaction = Transaction
  { outputs :: [TransactionOutput]
  }

data TransactionOutput = TransactionOutput
  { value :: Value
  }

data Value = Value Quantity (Map PolicyId (Map AssetName Quantity))

newtype Quantity = Quantity Natural
  deriving newtype (Eq, Num)

data HeadParameters = HeadParameters
  { verificationKeys :: [VerificationKey]
  }

newtype VerificationKey = VerificationKey
  { unverificationKey :: ByteString
  }
  deriving (Show)

data MonetaryScript
  = AnyOf [VerificationKey]

type AssetId = (PolicyId, AssetName)

-- Other constructors (e.g. AllOf, MOfN are left aside for now).

-- FIXME:
--
-- newtype PolicyId = Hash MonetaryScript Blake2b_224
--
-- newtype Hash (what :: Type) (alg :: Type) = Digest alg
newtype PolicyId = PolicyId ByteString deriving (Eq, Ord)

newtype AssetName = AssetName ByteString deriving (Eq, Ord)
