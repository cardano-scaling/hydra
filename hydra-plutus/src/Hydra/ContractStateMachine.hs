{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.ContractStateMachine where

import Control.Monad (guard, void)

import Ledger (Address, Datum (Datum), DatumHash, Validator, Value, datumHash, scriptAddress)
import qualified Ledger.Typed.Scripts as Scripts
import Playground.Contract
import Plutus.Contract
import Plutus.Contract.StateMachine (State (..), Void)
import qualified Plutus.Contract.StateMachine as SM
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude

import Hydra.Contract ()

data HydraState
  = Open OpenState
  | Closed
  deriving stock (Prelude.Eq, Generic)

data OpenState = OpenState
  { keyAggregate :: MultisigPublicKey
  , eta :: Eta
  -- hMT :: MerkleTreeRoot,
  -- numberOfMembers :: Integer,
  -- contestationPeriod :: Integer
  }
  deriving (Prelude.Eq, Generic)

data MultisigPublicKey = MultisigPublicKey [VerificationKey]
  deriving (Prelude.Eq, Generic)

newtype VerificationKey = VerificationKey
  { unverificationKey :: ByteString
  }
  deriving (Prelude.Eq, Show)

data Eta = Eta
  { utxos :: UTXO -- u
  , snapshotNumber :: Integer -- s
  , transactions :: [Transaction] -- morally a Set
  }
  deriving (Prelude.Eq, Generic)

data UTXO = UTXO
  deriving (Prelude.Eq, Generic)

-- | The transaction as handled in the hydra head, i.e. the tx which we have put
-- into Hydra. According to isomorphism property of Hydra, it could also have
-- been put on the main chain.
data Transaction = Transaction
  deriving (Prelude.Eq, Generic)

data TransactionObject = TransactionObject
  { sigma :: MultiSignature
  , tx :: Transaction
  }

data MultiSignature = MultiSignature

data MerkleTreeRoot = MerkleTreeRoot

data HydraInput = HydraInput Xi -- Pi

data Pi

data Xi = Xi
  { xiUtxos :: UTXO
  , xiSnapshotNumber :: Integer
  , signatures :: MultiSignature
  , confirmedTransactions :: [TransactionObject] -- morally a Set
  }

{-# INLINEABLE close #-}
close :: MultisigPublicKey -> Eta -> Xi -> Maybe Eta
close kAgg eta xi = do
  let (Xi u s sigma txs) = xi
  guard (all (verifyMultisignature kAgg) txs)
  guard (s == 0 || verifySnapshot kAgg u s sigma)
  let realU =
        if s == 0
          then utxos eta
          else u
      mainchainTxs = map tx txs
  guard (isJust $ applyTransactions realU mainchainTxs)
  pure $ Eta realU s mainchainTxs

{-# INLINEABLE verifyMultisignature #-}
verifyMultisignature :: MultisigPublicKey -> TransactionObject -> Bool
verifyMultisignature kAgg TransactionObject{sigma, tx} =
  msAVerify kAgg (hash $ serialize tx) sigma

{-# INLINEABLE verifySnapshot #-}
verifySnapshot :: MultisigPublicKey -> UTXO -> Integer -> MultiSignature -> Bool
verifySnapshot kAgg u s sigma =
  msAVerify kAgg (hash $ serialize u <> serialize s) sigma

-- | This is only about folding the transactions onto a UTXO and no evaluation
-- whatsoever.
applyTransactions :: UTXO -> [Transaction] -> Maybe UTXO
applyTransactions u _ = Just u -- TODO

--
-- Primitives we need
--

serialize :: a -> ByteString
serialize = const "reuse plutus tx's isData stuff" -- TODO

hash :: ByteString -> ByteString
hash = const "hashed bytestring" -- TODO

msAVerify :: MultisigPublicKey -> ByteString -> MultiSignature -> Bool
msAVerify _ _ _ = True -- TODO

{-# INLINEABLE transition #-}
transition ::
  State HydraState ->
  HydraInput ->
  Maybe (SM.TxConstraints Void Void, State HydraState)
transition state@State{stateData = Open OpenState{eta, keyAggregate}} (HydraInput xi)
  | isJust (close keyAggregate eta xi) = Just (mempty, state{stateData = Closed})
transition _ _ = Nothing

{-# INLINEABLE machine #-}
machine :: SM.StateMachine HydraState HydraInput
machine = SM.mkStateMachine transition isFinal where isFinal _ = False

{-# INLINEABLE validatorSM #-}
validatorSM :: Scripts.ValidatorType (SM.StateMachine HydraState HydraInput)
validatorSM = SM.mkValidator machine

{- ORMOLU_DISABLE -}
contractInstance
  :: Scripts.ScriptInstance (SM.StateMachine HydraState HydraInput)
contractInstance = Scripts.validator @(SM.StateMachine HydraState HydraInput)
    $$(PlutusTx.compile [|| validatorSM ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @HydraState @HydraInput
{- ORMOLU_ENABLE -}

-- | The 'SM.StateMachineInstance' of the hydra state machine contract. It uses
-- the functions in 'PlutusTx.StateMachine'.
machineInstance :: SM.StateMachineInstance HydraState HydraInput
machineInstance = SM.StateMachineInstance machine contractInstance

client :: SM.StateMachineClient HydraState HydraInput
client = SM.mkStateMachineClient machineInstance

-- | The validator script of the contract.
contractValidator :: Validator
contractValidator = Scripts.validatorScript contractInstance

-- | The address of the contract (the hash of its validator script)
contractAddress :: Address
contractAddress = Ledger.scriptAddress contractValidator

data CollectComParams = CollectComParams
  { amount :: Value
  }
  deriving stock (Prelude.Eq, Prelude.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

-- | Our mocked "collectCom" endpoint
collectComEndpoint :: (AsContractError e, SM.AsSMContractError e) => Contract () Schema e ()
collectComEndpoint = do
  CollectComParams amt <- endpoint @"collectCom" @CollectComParams
  logInfo @String $ "collectComEndpoint"
  void $ SM.runInitialise client initialState amt
 where
  initialState =
    Open $ OpenState{keyAggregate = MultisigPublicKey [], eta = Eta UTXO 0 []}

-- | Our "close" endpoint to trigger a close
closeEndpoint :: (AsContractError e, SM.AsSMContractError e) => Contract () Schema e ()
closeEndpoint = do
  endpoint @"close" @()
  logInfo @String $ "closeEndpoint"
  void $ SM.runStep client input
 where
  input = HydraInput $ Xi UTXO 0 MultiSignature []

type Schema =
  BlockchainActions
    .\/ Endpoint "collectCom" CollectComParams
    .\/ Endpoint "close" ()

hydraHead :: (AsContractError e, SM.AsSMContractError e) => Contract () Schema e ()
hydraHead = collectComEndpoint `select` closeEndpoint -- TODO loop here?

toDatumHash :: PlutusTx.IsData a => a -> DatumHash
toDatumHash = datumHash . Datum . PlutusTx.toData

--
-- Template Haskell
--

PlutusTx.makeLift ''HydraState
PlutusTx.makeLift ''OpenState
PlutusTx.makeLift ''MultisigPublicKey
PlutusTx.makeLift ''VerificationKey
PlutusTx.makeLift ''Eta
PlutusTx.makeLift ''MerkleTreeRoot
PlutusTx.makeLift ''TransactionObject
PlutusTx.makeLift ''Transaction
PlutusTx.makeLift ''UTXO
PlutusTx.makeLift ''MultiSignature

PlutusTx.unstableMakeIsData ''HydraState
PlutusTx.unstableMakeIsData ''OpenState
PlutusTx.unstableMakeIsData ''MultisigPublicKey
PlutusTx.unstableMakeIsData ''VerificationKey
PlutusTx.unstableMakeIsData ''Eta
PlutusTx.unstableMakeIsData ''MerkleTreeRoot
PlutusTx.unstableMakeIsData ''TransactionObject
PlutusTx.unstableMakeIsData ''Transaction
PlutusTx.unstableMakeIsData ''UTXO
PlutusTx.unstableMakeIsData ''MultiSignature
PlutusTx.unstableMakeIsData ''HydraInput
PlutusTx.unstableMakeIsData ''Xi
