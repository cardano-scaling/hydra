import           Language.PlutusTx.Prelude
import           Playground.Contract
import Numeric.Natural(Natural)
import           Ledger                    (Address, ValidatorCtx, scriptAddress)
import qualified Language.PlutusTx         as PlutusTx
import qualified Ledger.Typed.Scripts      as Scripts


data Datum =
  Open OpenState
    deriving (Generic, PlutusTx.IsData)

data OpenState = OpenState {
  keyAggregate :: MultisigPublicKey,
  eta :: Eta,
  hMT :: MerkleTreeRoot,
  numberOfMembers :: Integer,
  contestationPeriod :: Integer
  }
    deriving Generic

data MultisigPublicKey = MultisigPublicKey

data Eta = Eta

data MerkleTreeRoot = MerkleTreeRoot

PlutusTx.makeLift ''Datum
PlutusTx.makeLift ''OpenState
PlutusTx.makeLift ''MultisigPublicKey
PlutusTx.makeLift ''Eta
PlutusTx.makeLift ''MerkleTreeRoot

data Redeemer = Redeemer Pi Xi
    deriving (PlutusTx.IsData)

data Pi

data Xi


validateClose :: Datum -> Redeemer -> ValidatorCtx -> Bool
validateClose _datum _redeemer _ctx = True

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
    $$(PlutusTx.compile [|| validateClose ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @Datum @Redeemer
