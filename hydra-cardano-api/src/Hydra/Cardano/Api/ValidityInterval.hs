{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.ValidityInterval where

import Hydra.Cardano.Api.Prelude

import Cardano.Api.Shelley (TxValidityLowerBound (..), TxValidityUpperBound (..), allegraBasedEra, cardanoEra, shelleyBasedEra)
import Cardano.Ledger.Allegra.Scripts qualified as Ledger
import Cardano.Ledger.BaseTypes (StrictMaybe (..), maybeToStrictMaybe)
import Test.Gen.Cardano.Api.Typed (genTxValidityLowerBound, genTxValidityUpperBound)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Hedgehog (hedgehog)

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
          TxValidityUpperBound _ s -> maybeToStrictMaybe s
    }
fromLedgerValidityInterval ::
  Ledger.ValidityInterval ->
  (TxValidityLowerBound Era, TxValidityUpperBound Era)
fromLedgerValidityInterval validityInterval =
  let Ledger.ValidityInterval{Ledger.invalidBefore = invalidBefore, Ledger.invalidHereafter = invalidHereAfter} = validityInterval
      lowerBound = case invalidBefore of
        SNothing -> TxValidityNoLowerBound
        SJust s -> TxValidityLowerBound allegraBasedEra s
      upperBound = case invalidHereAfter of
        SNothing -> TxValidityUpperBound shelleyBasedEra Nothing
        SJust s -> TxValidityUpperBound shelleyBasedEra (Just s)
   in (lowerBound, upperBound)

instance Arbitrary (TxValidityLowerBound Era) where
  arbitrary = hedgehog $ genTxValidityLowerBound cardanoEra

instance Arbitrary (TxValidityUpperBound Era) where
  arbitrary = hedgehog $ genTxValidityUpperBound shelleyBasedEra
