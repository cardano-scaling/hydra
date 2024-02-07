module Hydra.Cardano.Api.KeyWitness where

import Hydra.Cardano.Api.Prelude

import Cardano.Ledger.Alonzo.TxWits qualified as Ledger
import Cardano.Ledger.Era qualified as Ledger
import Cardano.Ledger.Keys qualified as Ledger
import Data.Set qualified as Set

-- * Extras

-- | Construct a 'KeyWitness' from a transaction id and credentials.
signWith ::
  forall era.
  IsShelleyBasedEra era =>
  TxId ->
  SigningKey PaymentKey ->
  KeyWitness era
signWith (TxId h) signingKey@(PaymentSigningKey sk) =
  let (PaymentVerificationKey vk) = getVerificationKey signingKey
   in ShelleyKeyWitness (shelleyBasedEra @era) $
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
  Set.fromList [w | ShelleyKeyWitness _ w <- vkWits]

-- | Convert a 'List' of cardano-api's 'KeyWitness' into a 'Set' of
-- cardano-ledger's 'BootstrapWitness'.
--
-- NOTE: See note on 'toLedgerKeyWitness'.
toLedgerBootstrapWitness ::
  [KeyWitness era] ->
  Set (Ledger.BootstrapWitness StandardCrypto)
toLedgerBootstrapWitness vkWits =
  Set.fromList [w | ShelleyBootstrapWitness _ w <- vkWits]

-- | Convert a cardano-ledger's 'TxWitness' object into a list of cardano-api's
-- 'KeyWitness'.
--
-- NOTE: this only concerns key and bootstrap witnesses. Scripts and auxiliary
-- data are obviously not part of the resulting list.
fromLedgerTxWitness ::
  forall era.
  ( IsShelleyBasedEra era
  , UsesStandardCrypto era
  , Ledger.Era (ShelleyLedgerEra era)
  ) =>
  Ledger.AlonzoTxWits (ShelleyLedgerEra era) ->
  [KeyWitness era]
fromLedgerTxWitness wits =
  mconcat
    [ Set.foldr
        ((:) . ShelleyKeyWitness shelleyBasedEra)
        []
        (Ledger.txwitsVKey' wits)
    , Set.foldr
        ((:) . ShelleyBootstrapWitness shelleyBasedEra)
        []
        (Ledger.txwitsBoot' wits)
    ]
