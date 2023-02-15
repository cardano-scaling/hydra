{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.ClientInput where

import Prelude

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)
import Generic.Random (genericArbitrary, uniform)
import Hydra.API.Ledger (IsTx, UTxOType)
import Test.QuickCheck (Arbitrary (..))

data ClientInput tx
  = Init
  | Abort
  | Commit {utxo :: UTxOType tx}
  | NewTx {transaction :: tx}
  | GetUTxO
  | Close
  | Contest
  | Fanout
  deriving (Generic)

deriving instance IsTx tx => Eq (ClientInput tx)
deriving instance IsTx tx => Show (ClientInput tx)
deriving instance IsTx tx => ToJSON (ClientInput tx)
deriving instance IsTx tx => FromJSON (ClientInput tx)

instance (Arbitrary tx, Arbitrary (UTxOType tx)) => Arbitrary (ClientInput tx) where
  arbitrary = genericArbitrary uniform

  -- NOTE: Somehow, can't use 'genericShrink' here as GHC is complaining about
  -- Overlapping instances with 'UTxOType tx' even though for a fixed `tx`, there
  -- should be only one 'UTxOType tx'
  shrink = \case
    Init -> []
    Abort -> []
    Commit xs -> Commit <$> shrink xs
    NewTx tx -> NewTx <$> shrink tx
    GetUTxO -> []
    Close -> []
    Contest -> []
    Fanout -> []
