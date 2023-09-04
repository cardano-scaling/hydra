{-# LANGUAGE UndecidableInstances #-}

module Hydra.Network.ReliableNetworkSpec where

import Hydra.Prelude hiding (Any, label)

import qualified Data.Map as Data
import Test.QuickCheck.StateModel (Action, StateModel)
import Test.QuickCheck.StateModel.Variables

type Message = Int

data NetworkStack = NetworkStack
  { received :: [Message]
  , sent :: [Message]
  }
  deriving (Eq, Show)

newtype Network = Network
  { peers :: Data.Map Peer NetworkStack
  }
  deriving (Eq, Show)

data Peer = Alice | Bob | Carol
  deriving (Eq, Show)

instance HasVariables (Action Network a) where
  getAllVariables _ = mempty

instance HasVariables Network where
  getAllVariables _ = mempty

instance StateModel Network where
  data Action Network a where
    Send :: {source :: Peer, destination :: Peer, message :: Message} -> Action Network ()

  initialState = Network{peers = fromList [(Alice, mempty), (Bob, mempty), (Carol, mempty)]}

deriving instance Show (Action Network a)
deriving instance Eq (Action Network a)
