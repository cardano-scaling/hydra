module Hydra.Cardano.Api.RdmrTag where

import Hydra.Cardano.Api.Prelude

import qualified Cardano.Ledger.Alonzo.Scripts as Ledger

-- * Extras

-- | A redeemer tag used to qualify the purpose of a redeemer.
data RdmrTag
  = -- | Validates spending a script-locked UTxO
    Spend
  | -- | Validates minting new tokens
    Mint
  | -- | Validates certificate transactions
    Cert
  | -- | Validates withdrawl from a reward account
    Rewrd
  deriving (Eq, Generic, Ord, Show, Enum, Bounded)

-- * Type Conversions

-- | Convert a cardano-ledger's 'Tag' into a cardano-api's 'RdmrTag'
fromLedgerTag :: Ledger.Tag -> RdmrTag
fromLedgerTag = \case
  Ledger.Spend -> Spend
  Ledger.Mint -> Mint
  Ledger.Cert -> Cert
  Ledger.Rewrd -> Rewrd

toLedgerTag :: RdmrTag -> Ledger.Tag
toLedgerTag = \case
  Spend -> Ledger.Spend
  Mint -> Ledger.Mint
  Cert -> Ledger.Cert
  Rewrd -> Ledger.Rewrd
