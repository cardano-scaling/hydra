import           Language.PlutusTx.Prelude
import           Playground.Contract
import           Control.Monad(guard)
import Prelude(Num)
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

data Eta = Eta {
  utxos :: UTXO,  -- u
  snapshotNumber :: SnapshotNumber, -- s
  transactions :: [Transaction] -- morally a Set
  }

newtype SnapshotNumber = SnapshotNumber { getSnapshotNumber :: Integer }
    deriving (Eq, Num)

data UTXO = UTXO

-- | The transaction as handled in the hydra head, i.e. the tx which we have put
-- into Hydra. According to isomorphism property of Hydra, it could also have
-- been put on the main chain.
data Transaction = Transaction

data TransactionObject = TransactionObject
  { sigma :: MultiSignature
  , tx :: Transaction
  }

data MultiSignature = MultiSignature

data MerkleTreeRoot = MerkleTreeRoot

data Redeemer = Redeemer Pi Xi
    deriving (PlutusTx.IsData)

data Pi

data Xi =
  Xi { xiUtxos :: UTXO,
       xiSnapshotNumber :: SnapshotNumber,
       signatures :: MultiSignature,
       confirmedTransactions :: [TransactionObject] -- morally a Set
     }

validateClose :: Datum -> Redeemer -> ValidatorCtx -> Bool
validateClose (Open OpenState{keyAggregate,eta}) (Redeemer _ xi)  _ctx =
  isJust (close keyAggregate eta xi)

-- MultisigPublicKey: Kagg
-- Eta:
--
--
close :: MultisigPublicKey -> Eta -> Xi -> Maybe Eta
close aggregatedKeys eta xi = do
  let (Xi u s sigma txs) = xi
  guard (all (verifyMultisignature aggregatedKeys) txs)
  guard (s /= 0 && verifySnapshot aggregatedKeys u s sigma)
  let realU = if s == 0
              then utxos eta
              else u
      mainchainTxs = map tx txs
  guard (isJust $ applyTransactions realU mainchainTxs)
  pure $ Eta realU s mainchainTxs

verifyMultisignature :: MultisigPublicKey -> TransactionObject -> Bool
verifyMultisignature aggregatedKeys TransactionObject{sigma,tx} =
  msAVerify aggregatedKeys (hash tx) sigma

verifySnapshot :: MultisigPublicKey -> UTXO -> SnapshotNumber -> MultiSignature -> Bool
verifySnapshot kAgg u s sigma =
  msAVerify kAgg (hash u <> hash s) sigma

applyTransactions :: UTXO -> [Transaction] -> Maybe UTXO
applyTransactions = undefined

--
-- Primitives we need
--

hash :: a -> ByteString
hash = const "hashed bytestring"

msAVerify :: MultisigPublicKey -> ByteString -> MultiSignature -> Bool
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
PlutusTx.makeLift ''TransactionObject
PlutusTx.makeLift ''Transaction
PlutusTx.makeLift ''UTXO
PlutusTx.makeLift ''MultiSignature
PlutusTx.makeLift ''SnapshotNumber
