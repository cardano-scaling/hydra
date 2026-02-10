{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.ValidityInterval where

import "hydra-cardano-api" Hydra.Cardano.Api.Prelude

import "cardano-ledger-allegra" Cardano.Ledger.Allegra.Scripts qualified as Ledger
import "cardano-ledger-core" Cardano.Ledger.BaseTypes (StrictMaybe (..), maybeToStrictMaybe)

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
