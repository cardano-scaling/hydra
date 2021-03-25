import           Language.PlutusTx.Prelude
import           Playground.Contract
import           Control.Monad(guard)
import           Numeric.Natural(Natural)
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

data UTXO = UTXO

data TransactionBody = TransactionBody

data Transaction = Transaction
  { signature :: MultiSignature
  , body :: TransactionBody
  }

data MultiSignature = MultiSignature

data MerkleTreeRoot = MerkleTreeRoot

data Redeemer = Redeemer Pi Xi
    deriving (PlutusTx.IsData)

data Pi

data Xi =
  Xi { utxos :: UTXO,
       snapshotNumber :: Integer,
       signatures :: MultiSignature,
       confirmedTransactions :: [Transaction]
     }

validateClose :: Datum -> Redeemer -> ValidatorCtx -> Bool
validateClose (Open OpenState{keyAggregate,eta}) (Redeemer _ xi)  _ctx =
  isJust (close keyAggregate eta xi)

-- MultisigPublicKey: Kagg
-- Eta:
--
--
close :: MultisigPublicKey -> Eta -> Xi -> Maybe Eta
close aggregatedKeys eta (Xi u s sigma txs) = do
  guard (all (verifyMultisignature aggregatedKeys) txs)
  undefined

verifyMultisignature :: MultisigPublicKey -> Transaction -> Bool
verifyMultisignature aggregatedKeys Transaction{signature,body} =
  msAVerify aggregatedKeys (hash body) signature


--
-- Crypto Primitives
--

data Digest = Digest

hash :: a -> Digest
hash = const Digest

msAVerify :: MultisigPublicKey -> Digest -> MultiSignature -> Bool
msAVerify _ _ _ = True

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

--
-- Template Haskell
--

PlutusTx.makeLift ''Datum
PlutusTx.makeLift ''OpenState
PlutusTx.makeLift ''MultisigPublicKey
PlutusTx.makeLift ''Eta
PlutusTx.makeLift ''MerkleTreeRoot
PlutusTx.makeLift ''TransactionBody
PlutusTx.makeLift ''Transaction
PlutusTx.makeLift ''UTXO
PlutusTx.makeLift ''MultiSignature
