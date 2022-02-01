module Hydra.Cardano.Api.Lovelace where

import Cardano.Api

import qualified Cardano.Ledger.Coin as Ledger

-- * Type Conversions

-- | Convert a cardano-ledger's 'Coin' into a cardano-api 'Lovelace'.
fromLedgerCoin :: Ledger.Coin -> Lovelace
fromLedgerCoin (Ledger.Coin n) = Lovelace n

-- | Convert a cardano-api's 'Lovelace' into a cardano-ledger's 'Coin'.
toLedgerCoin :: Lovelace -> Ledger.Coin
toLedgerCoin (Lovelace n) = Ledger.Coin n
