{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Contract.PAB where

import Hydra.Prelude

import Control.Lens (makeClassyPrisms)
import qualified Data.Map as Map
import Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import Hydra.Contract.ContestationPeriod (ContestationPeriod)
import Hydra.Contract.Party (Party)
import qualified Hydra.ContractSM as ContractSM
import Ledger (CurrencySymbol, PubKeyHash (..), TxOut (txOutValue), TxOutTx (txOutTxOut), pubKeyAddress, pubKeyHash)
import Ledger.AddressMap (UtxoMap, outputsMapFromTxForAddress)
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Typed.Tx (tyTxOutData, typeScriptTxOut)
import Ledger.Value (AssetClass, TokenName (..), assetClass, flattenValue)
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
  utxoAt,
  waitNSlots,
 )
import Plutus.Contract.StateMachine (StateMachineClient, WaitingResult (..))
import qualified Plutus.Contract.StateMachine as SM
import qualified Plutus.Contracts.Currency as Currency
import Plutus.PAB.Effects.Contract.Builtin (HasDefinitions (..), SomeBuiltin (..))

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

--
-- Setup
--

-- | Parameters for starting a head.
-- NOTE: kinda makes sense to have them separate because it couuld be the
-- case that the parties (hdyra nodes taking part in the head consensus) and th
-- participants (people commiting UTxOs) and posting transaction in the head
-- are different
data InitParams = InitParams
  { contestationPeriod :: ContestationPeriod
  , cardanoPubKeys :: [PubKeyHash]
  , hydraParties :: [Party]
  }
  deriving (Generic, Show, FromJSON, ToJSON)

setup :: Contract () (Endpoint "init" InitParams) HydraPlutusError ()
setup = do
  -- NOTE: These are the cardano/chain keys to send PTs to
  InitParams{contestationPeriod, cardanoPubKeys, hydraParties} <-
    endpoint @"init" @InitParams

  let stateThreadToken = (threadTokenName, 1)
      participationTokens = map ((,1) . participationTokenName) cardanoPubKeys
      tokens = stateThreadToken : participationTokens

  -- TODO(SN): replace with SM.getThreadToken
  logInfo $ "Forging tokens: " <> show @String tokens
  ownPK <- pubKeyHash <$> ownPubKey
  symbol <- Currency.currencySymbol <$> Currency.mintContract ownPK tokens
  let threadToken = mkThreadToken symbol
  -- tokenValues = map (uncurry (singleton symbol)) participationTokens

  logInfo $ "Done, our currency symbol: " <> show @String symbol

  let client = machineClient threadToken
  void $ SM.runInitialise client (ContractSM.Initial contestationPeriod hydraParties) mempty

  logInfo $ "Triggered Init " <> show @String cardanoPubKeys

--
-- GetUtxos
--

getUtxo :: Contract (Last UtxoMap) Empty ContractError ()
getUtxo = do
  logInfo @Text $ "getUtxo: Starting to get and report utxo map every slot"
  address <- pubKeyAddress <$> ownPubKey
  loop address
 where
  loop address = do
    utxos <- utxoAt address
    tell . Last $ Just utxos
    void $ waitNSlots 1
    loop address

--
-- WatchInit
--

-- | Parameters as they are available in the 'Initial' state.
data InitialParams = InitialParams
  { contestationPeriod :: ContestationPeriod
  , parties :: [Party]
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance Arbitrary InitialParams where
  shrink = genericShrink
  arbitrary = genericArbitrary

-- | Watch 'initialAddress' (with hard-coded parameters) and report all datums
-- seen on each run.
watchInit :: Contract (Last InitialParams) Empty ContractError ()
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
          [ContractSM.Initial contestationPeriod parties] ->
            tell . Last . Just $ InitialParams{contestationPeriod, parties}
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

  scriptAddress =
    Scripts.validatorAddress . ContractSM.typedValidator

  lookupDatum token txOutTx =
    tyTxOutData <$> typeScriptTxOut (ContractSM.typedValidator token) txOutTx

-- | Wait for 'Init' transaction to appear on chain and return the observed state of the state machine
--
--
-- TODO: This code is unused?
watchStateMachine :: AssetClass -> Contract () Empty HydraPlutusError ContractSM.State
watchStateMachine threadToken = do
  logInfo @String $ "watchStateMachine: Looking for transitions of SM: " <> show threadToken
  let client = machineClient threadToken
  sl <- currentSlot
  SM.waitForUpdateUntilSlot client (sl + 10) >>= \case
    (Timeout _s) -> throwError $ HydraError "Timed out waiting for transaction"
    ContractEnded -> throwError $ HydraError "Contract ended"
    (WaitingResult s) -> pure s

-- | The machine client of the hydra state machine. It contains both, the script
-- instance with the on-chain code, and the Haskell definition of the state
-- machine for off-chain use.
machineClient ::
  -- | Thread token of the instance
  AssetClass ->
  StateMachineClient ContractSM.State ContractSM.Input
machineClient threadToken =
  let machine = ContractSM.hydraStateMachine threadToken
      inst = ContractSM.typedValidator threadToken
   in SM.mkStateMachineClient (SM.StateMachineInstance machine inst)

--
-- PAB Contract
--

-- | Enumeration of contracts available in the PAB.
data PABContract
  = Setup
  | GetUtxos
  | WatchInit
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Pretty PABContract where
  pretty = viaShow

instance HasDefinitions PABContract where
  getDefinitions =
    [ Setup
    , GetUtxos
    , WatchInit
    ]

  -- REVIEW(SN): There are actual endpoints defined in contracts code but they
  -- are not exposed here -> Is this a problem?
  getSchema = const []

  getContract = \case
    Setup -> SomeBuiltin setup
    GetUtxos -> SomeBuiltin getUtxo
    WatchInit -> SomeBuiltin watchInit

--
-- Helpers
--

mkThreadToken :: CurrencySymbol -> AssetClass
mkThreadToken symbol =
  assetClass symbol threadTokenName

threadTokenName :: TokenName
threadTokenName = "thread token"

participationTokenName :: PubKeyHash -> TokenName
participationTokenName = TokenName . getPubKeyHash
