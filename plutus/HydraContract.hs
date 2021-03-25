import           Language.PlutusTx.Prelude
import           Playground.Contract

import           Ledger                    (Address, ValidatorCtx, scriptAddress)
import qualified Language.PlutusTx         as PlutusTx
import qualified Ledger.Typed.Scripts      as Scripts

type Datum = ()
type Redeemer = ()

validate :: Datum -> Redeemer -> ValidatorCtx -> Bool
validate _datum _redeemer _ctx = True

--
-- Boilerplate
--

type Schema = BlockchainActions
mkSchemaDefinitions ''Schema

data Hydra
instance Scripts.ScriptType Hydra where
    type instance RedeemerType Hydra = Redeemer
    type instance DatumType Hydra = Datum

contractInstance :: Scripts.ScriptInstance Hydra
contractInstance = Scripts.validator @Hydra
    $$(PlutusTx.compile [|| validate ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @Datum @Redeemer
