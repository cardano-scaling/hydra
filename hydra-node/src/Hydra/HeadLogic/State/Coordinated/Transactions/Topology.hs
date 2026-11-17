{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | An approved decommit must settle before a new pending one can replace it.
module Hydra.HeadLogic.State.Coordinated.Transactions.Topology where

import Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import Crem.Topology (Topology (Topology))
import Data.Singletons.Base.TH (genSingletons)
import Hydra.Prelude

data DecommitPhase = NoDecommit | DecommitPending | AwaitingDecommitSettlement

$(genSingletons [''DecommitPhase])

type DecommitTopology =
  'Topology
    '[ '( 'NoDecommit, '[ 'DecommitPending, 'AwaitingDecommitSettlement])
     , '( 'DecommitPending, '[ 'NoDecommit, 'AwaitingDecommitSettlement])
     , '( 'AwaitingDecommitSettlement, '[ 'NoDecommit])
     ]

deriving stock instance Eq DecommitPhase
deriving stock instance Show DecommitPhase
deriving stock instance Enum DecommitPhase
deriving stock instance Bounded DecommitPhase
deriving via AllVertices DecommitPhase instance RenderableVertices DecommitPhase

-- | A certificate can establish a decommit before the local ReqDec arrives.
transitionLabels :: [((DecommitPhase, DecommitPhase), String)]
transitionLabels =
  [ ((NoDecommit, DecommitPending), "Record pending request")
  , ((NoDecommit, AwaitingDecommitSettlement), "Confirm / adopt decommit")
  , ((DecommitPending, AwaitingDecommitSettlement), "Confirm / adopt decommit")
  , ((DecommitPending, NoDecommit), "Release / settle / administrative reset")
  , ((AwaitingDecommitSettlement, NoDecommit), "Settle / administrative reset")
  ]
