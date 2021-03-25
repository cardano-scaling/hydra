import           Ledger                    (Address, Validator, ValidatorCtx, Value, scriptAddress)
import qualified Ledger.Constraints        as Constraints
import qualified Prelude
import           Language.PlutusTx.Prelude
import           Playground.Contract
import           Control.Monad(guard, void)
import Prelude(Num)
import           Numeric.Natural(Natural)
import qualified Language.PlutusTx         as PlutusTx
import qualified Ledger.Typed.Scripts      as Scripts
import           Language.Plutus.Contract

data Datum =
  Open OpenState
    deriving (Generic)

data OpenState = OpenState {
  keyAggregate :: MultisigPublicKey,
  eta :: Eta
  -- hMT :: MerkleTreeRoot,
  -- numberOfMembers :: Integer,
  -- contestationPeriod :: Integer
  }
  deriving Generic

data MultisigPublicKey = MultisigPublicKey

data Eta = Eta {
  utxos :: UTXO,  -- u
  snapshotNumber :: Integer, -- s
  transactions :: [Transaction] -- morally a Set
  }

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

data Redeemer = Redeemer Xi -- Pi

data Pi

data Xi =
  Xi { xiUtxos :: UTXO,
       xiSnapshotNumber :: Integer,
       signatures :: MultiSignature,
       confirmedTransactions :: [TransactionObject] -- morally a Set
     }

validateClose :: Datum -> Redeemer -> ValidatorCtx -> Bool
validateClose (Open OpenState{keyAggregate,eta}) (Redeemer xi)  _ctx =
  isJust (close keyAggregate eta xi)

{-# INLINABLE close #-}
close :: MultisigPublicKey -> Eta -> Xi -> Maybe Eta
close kAgg eta xi = do
  let (Xi u s sigma txs) = xi
  guard (all (verifyMultisignature kAgg) txs)
  guard (s /= 0 && verifySnapshot kAgg u s sigma)
  let realU = if s == 0
              then utxos eta
              else u
      mainchainTxs = map tx txs
  guard (isJust $ applyTransactions realU mainchainTxs)
  pure $ Eta realU s mainchainTxs

{-# INLINABLE verifyMultisignature #-}
verifyMultisignature :: MultisigPublicKey -> TransactionObject -> Bool
verifyMultisignature kAgg TransactionObject{sigma,tx} =
  msAVerify kAgg (hash tx) sigma

{-# INLINABLE verifySnapshot #-}
verifySnapshot :: MultisigPublicKey -> UTXO -> Integer -> MultiSignature -> Bool
verifySnapshot kAgg u s sigma =
  msAVerify kAgg (hash u <> hash s) sigma

--
-- Primitives we need
--

applyTransactions :: UTXO -> [Transaction] -> Maybe UTXO
applyTransactions u _ = Just u -- TODO

hash :: a -> ByteString
hash = const "hashed bytestring" -- TODO

msAVerify :: MultisigPublicKey -> ByteString -> MultiSignature -> Bool
msAVerify _ _ _ = True -- TODO

--
-- Boilerplate
--

data Hydra
instance Scripts.ScriptType Hydra where
    type instance RedeemerType Hydra = Redeemer
    type instance DatumType Hydra = Datum

contractInstance :: Scripts.ScriptInstance Hydra
contractInstance = Scripts.validator @Hydra
    $$(PlutusTx.compile [|| validateClose ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @Datum @Redeemer
        
data CollectComParams = CollectComParams
    { amount     :: Value
    }
    deriving stock (Prelude.Eq, Prelude.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, IotsType, ToSchema, ToArgument)

-- | Our mocked "collectCom" endpoint
collectComEndpoint :: AsContractError e => Contract Schema e ()
collectComEndpoint = do
    CollectComParams amt <- endpoint @"collectCom" @CollectComParams
    let tx         = Constraints.mustPayToTheScript datum amt
    void (submitTxConstraints contractInstance tx)
 where
  datum = Open $ OpenState { keyAggregate = MultisigPublicKey
                           , eta = Eta UTXO 0 []
                           }

-- | Our "close" endpoint to trigger a close
closeEndpoint :: AsContractError e => Contract Schema e ()
closeEndpoint = pure ()
    -- GuessParams theGuess <- endpoint @"guess" @GuessParams
    -- unspentOutputs <- utxoAt gameAddress
    -- let redeemer = clearString theGuess
    --     tx       = collectFromScript unspentOutputs redeemer
    -- void (submitTxConstraintsSpending gameInstance unspentOutputs tx)
        
type Schema = 
    BlockchainActions
        .\/ Endpoint "collectCom" CollectComParams
        .\/ Endpoint "close" ()

endpoints :: AsContractError e => Contract Schema e ()
endpoints = collectComEndpoint `select` closeEndpoint

mkSchemaDefinitions ''Schema

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

PlutusTx.makeIsData ''Datum
PlutusTx.makeIsData ''OpenState
PlutusTx.makeIsData ''MultisigPublicKey
PlutusTx.makeIsData ''Eta
PlutusTx.makeIsData ''MerkleTreeRoot
PlutusTx.makeIsData ''TransactionObject
PlutusTx.makeIsData ''Transaction
PlutusTx.makeIsData ''UTXO
PlutusTx.makeIsData ''MultiSignature
PlutusTx.makeIsData ''Redeemer
PlutusTx.makeIsData ''Xi
