-- | Smart constructors for creating Hydra protocol transactions to be used in
-- the 'Hydra.Chain.Direct' way of talking to the main-chain.
--
-- This module also encapsulates the transaction format used when talking to the
-- cardano-node, which is currently different from the 'Hydra.Ledger.Cardano',
-- thus we have not yet "reached" 'isomorphism'.
module Hydra.Chain.Direct.Tx where

import Hydra.Prelude

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Tx (IsValid (IsValid), ValidatedTx (..))
import Cardano.Ledger.Alonzo.TxBody (TxBody (..))
import Cardano.Ledger.Alonzo.TxWitness (Redeemers (..), TxDats (..), TxWitness (..))
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import qualified Data.Set as Set
import Hydra.Chain (HeadParameters, PostChainTx (InitTx))
import Shelley.Spec.Ledger.API (Coin (..), StrictMaybe (..), TxIn, Wdrl (Wdrl))

-- * Hydra Head transactions

mkUnsignedTx :: TxBody (AlonzoEra StandardCrypto) -> ValidatedTx (AlonzoEra StandardCrypto)
mkUnsignedTx body =
  ValidatedTx
    { body
    , wits =
        TxWitness
          mempty -- txwitsVKey
          mempty -- txwitsBoot
          mempty --txscripts
          (TxDats mempty) -- txdats
          (Redeemers mempty) -- txrdmrs
    , isValid = IsValid True -- REVIEW(SN): no idea of the semantics of this
    , auxiliaryData = SNothing
    }

constructTx :: TxIn StandardCrypto -> PostChainTx tx -> TxBody (AlonzoEra StandardCrypto)
constructTx txIn = \case
  InitTx p -> initTx p txIn
  _ -> error "not implemented"

-- | Create the init transaction from some 'HeadParameters' and a single UTXO
-- which will be used for minting NFTs.
initTx :: HeadParameters -> TxIn StandardCrypto -> TxBody (AlonzoEra StandardCrypto)
initTx _ txIn =
  TxBody
    (Set.singleton txIn) -- inputs
    mempty -- collateral
    mempty -- outputs
    mempty -- txcerts
    (Wdrl mempty) -- txwdrls
    (Coin 0) -- txfee
    (ValidityInterval SNothing SNothing) -- txvldt
    SNothing -- txUpdates
    mempty -- reqSignerHashes
    mempty -- mint
    SNothing -- scriptIntegrityHash
    SNothing -- adHash
    SNothing -- txnetworkid
    -- where
    --  headOut = TxOut headAddress headValue headDatumHash

--  -- TODO(SN): The main Hydra Head script address. Will be parameterized by the
--  -- thread token eventually. For now, this is just some arbitrary address, as
--  -- it is also later quite arbitrary/different per Head.
--  headAddress =
--    makeShelleyAddressInEra
--      networkId
--      (PaymentCredentialByScript $ hashScript headScript)
--      -- REVIEW(SN): stake head funds?
--      NoStakeAddress

--  headScript =
--    PlutusScript PlutusScriptV1 $
--      examplePlutusScriptAlwaysSucceeds WitCtxTxIn

--  -- REVIEW(SN): do we need to consider min utxo value? that would also depend
--  -- on how many assets present in an output
--  headValue = TxOutValue MultiAssetInAlonzoEra $ lovelaceToValue 10

--  headDatumHash = TxOutDatumHash ScriptDataInAlonzoEra $ hashScriptData headDatum

--  -- TODO(SN): how to convert plutus 'Datum' to 'cardano-api' re-/serialize?
--  headDatum = ScriptDataNumber 1337
