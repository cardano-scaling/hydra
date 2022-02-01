module Hydra.Cardano.Api.KeyWitness where

import Hydra.Cardano.Api.Prelude

import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Shelley.Address.Bootstrap as Ledger
import qualified Cardano.Ledger.Shelley.Tx as Ledger
import qualified Data.Set as Set

-- * Extras

-- | Construct a 'KeyWitness' from a transaction id and credentials.
--
-- TODO: The verification key can be inferred from the signing key and is
-- therefore redundant in the signature.
signWith ::
  forall era.
  (IsShelleyBasedEra era) =>
  TxId ->
  (VerificationKey PaymentKey, SigningKey PaymentKey) ->
  KeyWitness era
signWith (TxId h) (PaymentVerificationKey vk, PaymentSigningKey sk) =
  ShelleyKeyWitness (shelleyBasedEra @era) $
    Ledger.WitVKey
      (Ledger.asWitness vk)
      (Ledger.signedDSIGN @StandardCrypto sk h)

-- * Type Conversions

-- | Convert a 'List' of cardano-api's 'KeyWitness' into a 'Set' of
-- cardano-ledger's 'WitVKey'.
--
-- NOTE: 'KeyWitness' is a bigger type than 'WitVKey' witness, this function
-- does not only the type conversion but also the selection of the right
-- underlying constructors. That means the size of the resulting set may be
-- smaller than the size of the list (but never bigger).
toLedgerKeyWitness ::
  [KeyWitness era] ->
  Set (Ledger.WitVKey 'Ledger.Witness StandardCrypto)
toLedgerKeyWitness vkWits =
  fromList [w | ShelleyKeyWitness _ w <- vkWits]

-- | Convert a 'List' of cardano-api's 'KeyWitness' into a 'Set' of
-- cardano-ledger's 'BootstrapWitness'.
--
-- NOTE: See note on 'toLedgerKeyWitness'.
toLedgerBootstrapWitness ::
  [KeyWitness era] ->
  Set (Ledger.BootstrapWitness StandardCrypto)
toLedgerBootstrapWitness vkWits =
  fromList [w | ShelleyBootstrapWitness _ w <- vkWits]

-- | Convert a cardano-ledger's 'TxWitness' object into a list of cardano-api's
-- 'KeyWitness'.
--
-- NOTE: this only concerns key and bootstrap witnesses. Scripts and auxiliary
-- data are obviously not part of the resulting list.
fromLedgerTxWitness ::
  Ledger.TxWitness LedgerEra ->
  [KeyWitness Era]
fromLedgerTxWitness wits =
  Set.foldr ((:) . ShelleyKeyWitness era) [] (Ledger.txwitsVKey' wits)
    ++ Set.foldr ((:) . ShelleyBootstrapWitness era) [] (Ledger.txwitsBoot' wits)
 where
  era =
    ShelleyBasedEraAlonzo
