{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Chain.Wallet where

import Hydra.Prelude

import Cardano.Api.UTxO (UTxO)
import Cardano.Ledger.Address qualified as Ledger
import Cardano.Ledger.Alonzo.Plutus.Context (ContextError)
import Cardano.Ledger.Api (
  Conway,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (
  TxUpgradeError,
 )
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.API qualified as Ledger
import Cardano.Slotting.Time (SystemStart (..))
import Hydra.Cardano.Api (
  BlockHeader,
  ChainPoint,
  LedgerEra,
  ShelleyAddr,
 )
import Hydra.Cardano.Api qualified as Api
import Hydra.Chain.CardanoClient (QueryPoint (..))
import Hydra.Ledger.Cardano ()

type Address = Ledger.Addr StandardCrypto
type TxIn = Ledger.TxIn StandardCrypto
type TxOut = Ledger.TxOut LedgerEra

-- | A 'TinyWallet' is a small abstraction of a wallet with basic UTXO
-- management. The wallet is assumed to have only one address, and only one UTXO
-- at that address. It can sign transactions and keeps track of its UTXO behind
-- the scene.
--
-- The wallet is connecting to the node initially and when asked to 'reset'.
-- Otherwise it can be fed blocks via 'update' as the chain rolls forward.
data TinyWallet m = TinyWallet
  { getUTxO :: STM m (Map TxIn TxOut)
  -- ^ Return all known UTxO addressed to this wallet.
  , getSeedInput :: STM m (Maybe Api.TxIn)
  -- ^ Returns the /seed input/
  -- This is the special input needed by `Direct` chain component to initialise
  -- a head
  , sign :: Api.Tx -> Api.Tx
  , coverFee ::
      UTxO ->
      Api.Tx ->
      m (Either ErrCoverFee Api.Tx)
  , reset :: m ()
  -- ^ Re-initializ wallet against the latest tip of the node and start to
  -- ignore 'update' calls until reaching that tip.
  , update :: BlockHeader -> [Api.Tx] -> m ()
  -- ^ Update the wallet state given a block and list of txs. May be ignored if
  -- wallet is still initializing.
  }

data WalletInfoOnChain = WalletInfoOnChain
  { walletUTxO :: Map TxIn TxOut
  , systemStart :: SystemStart
  , tip :: ChainPoint
  -- ^ Latest point on chain the wallet knows of.
  }

type ChainQuery m = QueryPoint -> Api.Address ShelleyAddr -> m WalletInfoOnChain

-- | This are all the error that can happen during coverFee.
data ErrCoverFee
  = ErrNotEnoughFunds ChangeError
  | ErrNoFuelUTxOFound
  | ErrUnknownInput {input :: TxIn}
  | ErrScriptExecutionFailed {redeemerPointer :: Text, scriptFailure :: Text}
  | ErrTranslationError (ContextError LedgerEra)
  | ErrConwayUpgradeError (TxUpgradeError Conway)
  deriving stock (Show)

data ChangeError = ChangeError {inputBalance :: Coin, outputBalance :: Coin}
  deriving stock (Show)

--
-- Logs
--

data TinyWalletLog
  = BeginInitialize
  | EndInitialize {initialUTxO :: Api.UTxO, tip :: ChainPoint}
  | BeginUpdate {point :: ChainPoint}
  | EndUpdate {newUTxO :: Api.UTxO}
  | SkipUpdate {point :: ChainPoint}
  deriving stock (Eq, Generic, Show)

deriving anyclass instance ToJSON TinyWalletLog

instance Arbitrary TinyWalletLog where
  arbitrary = genericArbitrary
  shrink = genericShrink
