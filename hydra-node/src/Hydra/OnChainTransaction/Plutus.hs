-- | Provides an "anticorruption layer" between Hydra types and Plutus types.
--
--While it might be tempting to adopt a /Conformist/ approach and simply reuse types and
--functions from Plutus directly, it seems a better strategy to have a clear separation
--in order to prevent too much adherence inside Hydra to Plutus' concepts and types.
module Hydra.OnChainTransaction.Plutus where

import Cardano.Prelude

-- TODO: Remove dependency on types from contract
import Hydra.ContractStateMachine (VerificationKey (VerificationKey))
import Hydra.OnChainTransaction.Types
import qualified Ledger as Plutus
import qualified Ledger.Value as Value

toCurrencySymbol :: PolicyId -> Plutus.CurrencySymbol
toCurrencySymbol (PolicyId hash) = Value.currencySymbol hash

fromCurrencySymbol :: Plutus.CurrencySymbol -> PolicyId
fromCurrencySymbol = PolicyId . Value.unCurrencySymbol

toPlutusAddress :: Address -> Plutus.Address
toPlutusAddress = \case
  ScriptAddress addr -> Plutus.ScriptAddress $ Plutus.ValidatorHash addr
  PubKeyAddress (VerificationKey k) -> Plutus.PubKeyAddress $ Plutus.PubKeyHash k

fromPlutusAddress :: Plutus.Address -> Address
fromPlutusAddress (Plutus.PubKeyAddress p) = PubKeyAddress $ VerificationKey $ Plutus.getPubKeyHash p
fromPlutusAddress (Plutus.ScriptAddress (Plutus.ValidatorHash b)) = ScriptAddress b

fromPlutusDatumHash :: Plutus.DatumHash -> DatumHash
fromPlutusDatumHash (Plutus.DatumHash b) = DatumHash b

toPlutusDatumHash :: DatumHash -> Plutus.DatumHash
toPlutusDatumHash (DatumHash b) = Plutus.DatumHash b

toTxId :: TxId -> Plutus.TxId
toTxId (TxId hash) = Plutus.TxId hash
