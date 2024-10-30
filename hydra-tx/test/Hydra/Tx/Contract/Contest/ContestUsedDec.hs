{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Tx.Contract.Contest.ContestUsedDec where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Hydra.Tx.Contract.Contest.Healthy ()
import Test.Hydra.Tx.Mutation (
  SomeMutation (..),
 )
import Test.QuickCheck (oneof)
import Test.QuickCheck.Instances ()

data ContestUsedDecMutation = ContestUsedDecMutation
  deriving stock (Generic, Show, Enum, Bounded)

genContestUsedDecMutation :: (Tx, UTxO) -> Gen SomeMutation
genContestUsedDecMutation (_tx, _utxo) =
  oneof []
