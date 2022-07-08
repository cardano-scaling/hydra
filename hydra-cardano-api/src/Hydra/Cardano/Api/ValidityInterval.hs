{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.ValidityInterval where

import Hydra.Cardano.Api.Prelude

import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import qualified Cardano.Ledger.ShelleyMA.Timelocks as Ledger
import Test.QuickCheck (oneof)

toLedgerValidityInterval ::
  (TxValidityLowerBound era, TxValidityUpperBound era) ->
  Ledger.ValidityInterval
toLedgerValidityInterval (lowerBound, upperBound) =
  Ledger.ValidityInterval
    { Ledger.invalidBefore =
        case lowerBound of
          TxValidityNoLowerBound -> SNothing
          TxValidityLowerBound _ s -> SJust s
    , Ledger.invalidHereafter =
        case upperBound of
          TxValidityNoUpperBound _ -> SNothing
          TxValidityUpperBound _ s -> SJust s
    }

instance Arbitrary (TxValidityLowerBound Era) where
  arbitrary =
    oneof
      [ pure TxValidityNoLowerBound
      , TxValidityLowerBound ValidityLowerBoundInBabbageEra . SlotNo <$> arbitrary
      ]

instance Arbitrary (TxValidityUpperBound Era) where
  arbitrary =
    oneof
      [ pure $ TxValidityNoUpperBound ValidityNoUpperBoundInBabbageEra
      , TxValidityUpperBound ValidityUpperBoundInBabbageEra . SlotNo <$> arbitrary
      ]
