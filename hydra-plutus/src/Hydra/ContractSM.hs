{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | Simplified SM-based contract for the purpose of developing the interface
-- between Node and Chain
module Hydra.ContractSM where

import Control.Lens (makeClassyPrisms)
import Control.Monad (forever)
import Data.Aeson (FromJSON, ToJSON)
import Data.Either (rights)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Hydra.Contract.Party (Party)
import Hydra.Prelude (Eq, Last (..), Show, String, show, uncurry, void)
import Ledger (CurrencySymbol, PubKeyHash (..), TxOut (txOutValue), TxOutTx (txOutTxOut), Value, pubKeyAddress, pubKeyHash)
import Ledger.AddressMap (outputsMapFromTxForAddress)
import Ledger.Constraints (mustPayToPubKey)
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Typed.Tx (tyTxOutData, typeScriptTxOut)
import Ledger.Value (AssetClass, TokenName (..), assetClass, flattenValue, singleton)
import Plutus.Contract (
  AsContractError (..),
  Contract,
  ContractError (..),
  Empty,
  Endpoint,
  currentSlot,
  endpoint,
  logInfo,
  nextTransactionsAt,
  ownPubKey,
  tell,
  throwError,
 )
import Plutus.Contract.StateMachine (StateMachine, StateMachineClient, WaitingResult (..))
import qualified Plutus.Contract.StateMachine as SM
import qualified Plutus.Contracts.Currency as Currency
import qualified PlutusTx
import PlutusTx.Prelude hiding (Eq)

data State
  = Setup
  | Initial [Party]
  | Open
  | Final
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeLift ''State
PlutusTx.unstableMakeIsData ''State

data Input
  = Init [(PubKeyHash, Value)] [Party]
  | CollectCom
  | Abort
  deriving (Generic, Show)

PlutusTx.makeLift ''Input
PlutusTx.unstableMakeIsData ''Input

data HydraPlutusError
  = -- | State machine operation failed
    SMError SM.SMContractError
  | -- | Endpoint, coin selection, etc. failed
    PlutusError ContractError
  | -- | Thread token could not be created
    ThreadTokenError Currency.CurrencyError
  | -- | Arbitrary error
    HydraError String
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''HydraPlutusError

instance AsContractError HydraPlutusError where
  _ContractError = _PlutusError

instance SM.AsSMContractError HydraPlutusError where
  _SMContractError = _SMError

instance Currency.AsCurrencyError HydraPlutusError where
  _CurrencyError = _ThreadTokenError

{-# INLINEABLE hydraStateMachine #-}
hydraStateMachine :: AssetClass -> StateMachine State Input
hydraStateMachine _threadToken =
  -- XXX(SN): This should actually be '(Just threadToken)' as we wan't to have
  -- "contract continuity" as described in the EUTXO paper. Unfortunately we did
  -- not get this yet to work with 'runStep', or at least we were expecting the
  -- statemachine library would handle it for us.
  SM.mkStateMachine Nothing hydraTransition isFinal
 where
  isFinal Final{} = True
  isFinal _ = False

{-# INLINEABLE hydraTransition #-}
hydraTransition :: SM.State State -> Input -> Maybe (SM.TxConstraints SM.Void SM.Void, SM.State State)
hydraTransition oldState input =
  case (SM.stateData oldState, input) of
    (Setup, Init participationTokens parties) ->
      Just (constraints, oldState{SM.stateData = Initial parties})
     where
      constraints = foldMap (uncurry mustPayToPubKey) participationTokens
    _ -> Nothing

-- | The script instance of the auction state machine. It contains the state
-- machine compiled to a Plutus core validator script. The 'AssetClass' serves
-- two roles here:
--
--   1. Parameterizing the script, such that we get a unique address and allow
--   for multiple instances of it
--
--   2. Identify the 'state thread token', which should be passed in
--   transactions transitioning the state machine and provide "contract
--   continuity"
typedValidator :: AssetClass -> Scripts.TypedValidator (StateMachine State Input)
typedValidator threadToken =
  let val =
        $$(PlutusTx.compile [||validatorParam||])
          `PlutusTx.applyCode` PlutusTx.liftCode threadToken
      validatorParam c = SM.mkValidator (hydraStateMachine c)
      wrap = Scripts.wrapValidator @State @Input
   in Scripts.mkTypedValidator @(StateMachine State Input)
        val
        $$(PlutusTx.compile [||wrap||])

-- | The machine client of the hydra state machine. It contains both, the script
-- instance with the on-chain code, and the Haskell definition of the state
-- machine for off-chain use.
machineClient ::
  -- | Thread token of the instance
  AssetClass ->
  StateMachineClient State Input
machineClient threadToken =
  let machine = hydraStateMachine threadToken
      inst = typedValidator threadToken
   in SM.mkStateMachineClient (SM.StateMachineInstance machine inst)

participationTokenName :: PubKeyHash -> TokenName
participationTokenName = TokenName . getPubKeyHash

threadTokenName :: TokenName
threadTokenName = "thread token"

mkThreadToken :: CurrencySymbol -> AssetClass
mkThreadToken symbol =
  assetClass symbol threadTokenName

-- | Parameters for starting a head.
-- NOTE: kinda makes sense to have them separate because it couuld be the
-- case that the parties (hdyra nodes taking part in the head consensus) and th
-- participants (people commiting UTxOs) and posting transaction in the head
-- are different
data InitParams = InitParams
  { cardanoPubKeys :: [PubKeyHash]
  , hydraParties :: [Party]
  }
  deriving (Generic, FromJSON, ToJSON)

setup :: Contract () (Endpoint "init" InitParams) HydraPlutusError ()
setup = do
  -- NOTE: These are the cardano/chain keys to send PTs to
  InitParams{cardanoPubKeys, hydraParties} <- endpoint @"init" @InitParams

  let stateThreadToken = (threadTokenName, 1) -- XXX: dry with above
      participationTokens = map ((,1) . participationTokenName) cardanoPubKeys
      tokens = stateThreadToken : participationTokens

  -- TODO(SN): replace with SM.getThreadToken
  logInfo $ "Forging tokens: " <> show @String tokens
  ownPK <- pubKeyHash <$> ownPubKey
  symbol <- Currency.currencySymbol <$> Currency.mintContract ownPK tokens
  let threadToken = mkThreadToken symbol
      tokenValues = map (uncurry (singleton symbol)) participationTokens

  logInfo $ "Done, our currency symbol: " <> show @String symbol

  let client = machineClient threadToken
  void $ SM.runInitialise client Setup mempty

  void $ SM.runStep client (Init (zip cardanoPubKeys tokenValues) hydraParties)
  logInfo $ "Triggered Init " <> show @String cardanoPubKeys

-- | Watch 'initialAddress' (with hard-coded parameters) and report all datums
-- seen on each run.
watchInit :: Contract (Last [Party]) Empty ContractError ()
watchInit = do
  logInfo @String $ "watchInit: Looking for an init tx and it's parties"
  pubKey <- ownPubKey
  let address = pubKeyAddress pubKey
      pkh = pubKeyHash pubKey
  forever $ do
    txs <- nextTransactionsAt address
    let foundTokens = txs >>= mapMaybe (findToken pkh) . Map.elems . outputsMapFromTxForAddress address
    logInfo $ "found tokens: " <> show @String foundTokens
    case foundTokens of
      [token] -> do
        let datums = txs >>= rights . fmap (lookupDatum token) . Map.elems . outputsMapFromTxForAddress (scriptAddress token)
        logInfo @String $ "found init tx(s) with datums: " <> show datums
        case datums of
          [Initial parties] -> tell $ Last $ Just parties
          _ -> pure ()
      _ -> pure ()
 where
  -- Find candidates for a Hydra Head threadToken 'AssetClass', that is if the
  -- 'TokenName' matches our public key
  findToken :: PubKeyHash -> TxOutTx -> Maybe AssetClass
  findToken pkh txout =
    let value = txOutValue $ txOutTxOut txout
        flat = flattenValue value
        mres = find (\(_, tokenName, amount) -> amount == 1 && tokenName == participationTokenName pkh) flat
     in case mres of
          Just (symbol, _, _) -> Just $ mkThreadToken symbol
          Nothing -> Nothing

  scriptAddress = Scripts.validatorAddress . typedValidator

  lookupDatum token txOutTx = tyTxOutData <$> typeScriptTxOut (typedValidator token) txOutTx

-- | Wait for 'Init' transaction to appear on chain and return the observed state of the state machine
watchStateMachine :: AssetClass -> Contract () Empty HydraPlutusError State
watchStateMachine threadToken = do
  logInfo @String $ "watchStateMachine: Looking for transitions of SM: " <> show threadToken
  let client = machineClient threadToken
  sl <- currentSlot
  SM.waitForUpdateUntilSlot client (sl + 10) >>= \case
    (Timeout _s) -> throwError $ HydraError "Timed out waiting for transaction"
    ContractEnded -> throwError $ HydraError "Contract ended"
    (WaitingResult s) -> pure s
