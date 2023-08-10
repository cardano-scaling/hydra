{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.ValidityInterval where

import Hydra.Cardano.Api.Prelude

import qualified Cardano.Ledger.Allegra.Scripts as Ledger
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
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
fromLedgerValidityInterval ::
  Ledger.ValidityInterval ->
  (TxValidityLowerBound Era, TxValidityUpperBound Era)
fromLedgerValidityInterval validityInterval =
  let Ledger.ValidityInterval{Ledger.invalidBefore = invalidBefore, Ledger.invalidHereafter = invalidHereAfter} = validityInterval
      lowerBound = case invalidBefore of
        SNothing -> TxValidityNoLowerBound
        SJust s -> TxValidityLowerBound ValidityLowerBoundInBabbageEra s
      upperBound = case invalidHereAfter of
        SNothing -> TxValidityNoUpperBound ValidityNoUpperBoundInBabbageEra
        SJust s -> TxValidityUpperBound ValidityUpperBoundInBabbageEra s
   in (lowerBound, upperBound)

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
