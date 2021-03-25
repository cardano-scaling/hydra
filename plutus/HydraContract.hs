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
  transactions :: [MainchainTransaction]
  }

newtype SnapshotNumber = SnapshotNumber { getSnapshotNumber :: Integer }
    deriving (Eq, Num)

data UTXO = UTXO

data MainchainTransaction = MainchainTransaction

data Transaction = Transaction
  { sigma :: MultiSignature
  , tx :: MainchainTransaction
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
  guard (s /= 0 && verifySnapshot aggregatedKeys u s sigma)
  let realU = if s == 0
              then utxos eta
              else u
      mainchainTxs = map tx txs
  guard (isJust $ applyTransactions realU mainchainTxs)
  pure $ Eta realU s mainchainTxs

verifyMultisignature :: MultisigPublicKey -> Transaction -> Bool
verifyMultisignature aggregatedKeys Transaction{sigma,tx} =
  msAVerify aggregatedKeys (hash tx) sigma

verifySnapshot :: MultisigPublicKey -> UTXO -> SnapshotNumber -> MultiSignature -> Bool
verifySnapshot = undefined

applyTransactions :: UTXO -> [MainchainTransaction] -> Maybe UTXO
applyTransactions = undefined

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
PlutusTx.makeLift ''MainchainTransaction
PlutusTx.makeLift ''Transaction
PlutusTx.makeLift ''UTXO
PlutusTx.makeLift ''MultiSignature
PlutusTx.makeLift ''SnapshotNumber
