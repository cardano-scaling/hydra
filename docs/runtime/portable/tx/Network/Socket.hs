module Network.Socket (PortNumber) where

import Hydra.Prelude

newtype PortNumber = PortNumber Word16
  deriving newtype (Eq, Ord, Show, Num, Real, Integral, Enum, Bounded)
