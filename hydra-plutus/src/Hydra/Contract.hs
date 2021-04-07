{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract where

import Control.Monad (guard, void)
import qualified Data.Map as Map
import Hydra.Contract.Types
import Ledger (Address, Validator, ValidatorCtx, Value, scriptAddress)
import qualified Ledger.Constraints as Constraints
import Ledger.Tx (TxOut (..), TxOutTx (..))
import qualified Ledger.Typed.Scripts as Scripts
import Playground.Contract
import Plutus.Contract
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude

{-# INLINEABLE validate #-}
validate :: HydraState -> HydraInput -> ValidatorCtx -> Bool
validate Collecting _input _ctx = False
validate (Open OpenState{keyAggregate, eta}) (Close xi) _ctx =
  case close keyAggregate eta xi of
    Just{} -> True
    Nothing -> False
validate _ _ _ = False

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

--
-- Boilerplate
--

data Hydra
instance Scripts.ScriptType Hydra where
  type DatumType Hydra = HydraState
  type RedeemerType Hydra = HydraInput

{- ORMOLU_DISABLE -}
contractInstance :: Scripts.ScriptInstance Hydra
contractInstance = Scripts.validator @Hydra
    $$(PlutusTx.compile [|| validate ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @HydraState @HydraInput
{- ORMOLU_ENABLE -}

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
collectComEndpoint :: AsContractError e => Contract () Schema e ()
collectComEndpoint = do
  CollectComParams amt <- endpoint @"collectCom" @CollectComParams
  logInfo @String $ "collectComEndpoint"
  let tx = Constraints.mustPayToTheScript datum amt
  void (submitTxConstraints contractInstance tx)
 where
  datum =
    Open $
      OpenState
        { keyAggregate = MultisigPublicKey []
        , eta = Eta UTXO 0 []
        }

-- | Our "close" endpoint to trigger a close
closeEndpoint :: AsContractError e => Contract () Schema e ()
closeEndpoint = do
  endpoint @"close" @()
  logInfo @String $ "closeEndpoint"
  -- Querying ledger from application backend
  utxoMap <- utxoAt contractAddress
  let balance = foldMap (txOutValue . txOutTxOut . snd) $ Map.toList utxoMap
  logInfo @String $ "CONTRACT BALANCE: " ++ show balance
  let tx =
        collectFromScript utxoMap redeemer
          <> Constraints.mustPayToTheScript datum balance
  void (submitTxConstraintsSpending contractInstance utxoMap tx)
 where
  datum = Closed -- TODO add more things
  redeemer = Close $ Xi UTXO 0 MultiSignature []

type Schema =
  BlockchainActions
    .\/ Endpoint "collectCom" CollectComParams
    .\/ Endpoint "close" ()

hydraHead :: AsContractError e => Contract () Schema e ()
hydraHead = collectComEndpoint `select` closeEndpoint
