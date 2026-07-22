module Hydra.Cardano.Api.Tx (
  -- * Extras
  module Hydra.Cardano.Api.Tx,

  -- * Re-export normal Tx (any era)
  Tx,
)
where

import Hydra.Cardano.Api.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Api (
  EraTx (mkBasicTx),
  inputsTxBodyL,
  mkBasicTxBody,
 )
import Cardano.Ledger.Api qualified as Ledger
import Cardano.Ledger.Core (TxLevel (..))
import Control.Lens ((&), (.~))
import Data.Bifunctor (bimap)
import Data.Functor ((<&>))
import Data.Set qualified as Set
import Hydra.Cardano.Api.TxIn (mkTxIn, toLedgerTxIn)

-- * Extras

-- | A signing key that can be either a normal 'PaymentKey' or an extended
-- 'PaymentExtendedKey' (BIP32-Ed25519, as produced by HD wallets like Daedalus
-- or hardware wallets).
data CardanoSigningKey
  = CardanoSigningKey (SigningKey PaymentKey)
  | CardanoExtendedSigningKey (SigningKey PaymentExtendedKey)

-- | Convert a 'CardanoSigningKey' to a 'ShelleyWitnessSigningKey' for use with
-- 'makeShelleyKeyWitness'.
toWitness :: CardanoSigningKey -> ShelleyWitnessSigningKey
toWitness (CardanoSigningKey sk) = WitnessPaymentKey sk
toWitness (CardanoExtendedSigningKey sk) = WitnessPaymentExtendedKey sk

-- | Derive the 'VerificationKey PaymentKey' from a 'CardanoSigningKey'.
-- For extended keys, this derives the extended verification key and then casts
-- it to a normal verification key (which preserves the key hash).
getCardanoPaymentVerificationKey :: CardanoSigningKey -> VerificationKey PaymentKey
getCardanoPaymentVerificationKey (CardanoSigningKey sk) = getVerificationKey sk
getCardanoPaymentVerificationKey (CardanoExtendedSigningKey sk) = castVerificationKey $ getVerificationKey sk

-- | Sign transaction using the provided secret key
-- It only works for tx not containing scripts.
-- You can't sign a script utxo with this.
signTx ::
  IsShelleyBasedEra era =>
  SigningKey PaymentKey ->
  Tx era ->
  Tx era
signTx signingKey (Tx body wits) =
  makeSignedTransaction (witness : wits) body
 where
  witness = makeShelleyKeyWitness shelleyBasedEra body (WitnessPaymentKey signingKey)

-- | Like 'signTx' but accepts a 'CardanoSigningKey', supporting both normal
-- and extended (HD wallet) payment keys.
signTxWith ::
  IsShelleyBasedEra era =>
  CardanoSigningKey ->
  Tx era ->
  Tx era
signTxWith csk (Tx body wits) =
  makeSignedTransaction (witness : wits) body
 where
  witness = makeShelleyKeyWitness shelleyBasedEra body (toWitness csk)

-- | Create a transaction spending all given `UTxO`.
txSpendingUTxO :: UTxO Era -> Tx Era
txSpendingUTxO utxo =
  fromLedgerTx $
    mkBasicTx
      ( mkBasicTxBody
          & inputsTxBodyL .~ (toLedgerTxIn `Set.map` inputs)
      )
 where
  inputs = UTxO.inputSet utxo

-- | Get the UTxO that are produced by some transaction.
-- XXX: Defined here to avoid cyclic module dependency
utxoProducedByTx :: Tx Era -> UTxO Era
utxoProducedByTx tx =
  UTxO.fromList $
    zip [0 ..] (txOuts body)
      <&> bimap (mkTxIn tx) toCtxUTxOTxOut
 where
  body = getTxBodyContent $ getTxBody tx

-- * Type Conversions

-- | Convert a cardano-api 'Tx' into a matching cardano-ledger 'Tx'.
toLedgerTx ::
  Tx era ->
  Ledger.Tx TopTx (ShelleyLedgerEra era)
toLedgerTx (ShelleyTx _era tx) = tx

-- | Convert a cardano-ledger's 'Tx' in the Babbage era into a cardano-api 'Tx'.
fromLedgerTx ::
  IsShelleyBasedEra era =>
  Ledger.Tx TopTx (ShelleyLedgerEra era) ->
  Tx era
fromLedgerTx =
  ShelleyTx shelleyBasedEra
