{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Contract.PAB where

import Hydra.Prelude hiding (init)

import Control.Lens (makeClassyPrisms)
import qualified Data.Map as Map
import Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import Hydra.Contract.ContestationPeriod (ContestationPeriod)
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.Initial as Initial
import Hydra.Contract.Party (Party)
import Ledger (CurrencySymbol, PubKeyHash (..), TxOut (txOutValue), TxOutTx (txOutTxOut), pubKeyAddress, pubKeyHash)
import Ledger.AddressMap (UtxoMap, outputsMapFromTxForAddress)
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Typed.Tx (tyTxOutData, typeScriptTxOut)
import Ledger.Value (AssetClass, TokenName (..), flattenValue)
import qualified Ledger.Value as Value
import Plutus.Contract (
  AsContractError (..),
  Contract,
  ContractError (..),
  Empty,
  Endpoint,
  Promise,
  endpoint,
  logInfo,
  nextTransactionsAt,
  ownPubKey,
  selectList,
  tell,
  utxoAt,
  waitNSlots,
 )
import qualified Plutus.Contract.StateMachine as SM
import qualified Plutus.Contracts.Currency as Currency
import Plutus.PAB.Effects.Contract.Builtin (HasDefinitions (..), SomeBuiltin (..))

-- | Hard-coded port used between hydra-node and the PAB server.
pabPort :: Int
pabPort = 8888

-- | Enumeration of contracts available in the PAB.
data PABContract
  = GetUtxos
  | Init
  | Watch
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Pretty PABContract where
  pretty = viaShow

instance HasDefinitions PABContract where
  getDefinitions =
    [ GetUtxos
    , Init
    , Watch
    ]

  -- REVIEW(SN): There are actual endpoints defined in contracts code but they
  -- are not exposed here -> Is this a problem?
  getSchema = const []

  getContract = \case
    GetUtxos -> SomeBuiltin getUtxo
    Init -> SomeBuiltin init
    Watch -> SomeBuiltin watch

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

threadTokenName :: TokenName
threadTokenName = "thread token"

participationTokenName :: PubKeyHash -> TokenName
participationTokenName = TokenName . getPubKeyHash

mkThreadToken :: CurrencySymbol -> AssetClass
mkThreadToken symbol =
  Value.assetClass symbol threadTokenName

init :: Promise () (Endpoint "init" InitParams) HydraPlutusError ()
init = endpoint @"init" $ \InitParams{contestationPeriod, cardanoPubKeys, hydraParties} -> do
  let stateThreadToken = (threadTokenName, 1)
      participationTokens = map ((,1) . participationTokenName) cardanoPubKeys
      tokens = stateThreadToken : participationTokens

  -- TODO(SN): replace with SM.getThreadToken
  logInfo $ "Forging tokens: " <> show @String tokens
  ownPK <- pubKeyHash <$> ownPubKey
  symbol <- Currency.currencySymbol <$> Currency.mintContract ownPK tokens
  let threadToken = mkThreadToken symbol
      tokenValues = map (uncurry (Value.singleton symbol)) participationTokens
  logInfo $ "Done, our currency symbol: " <> show @String symbol

  let client = Head.machineClient threadToken
  let constraints = foldMap (uncurry Initial.mustPayToScript) $ zip cardanoPubKeys tokenValues
  void $ SM.runInitialiseWith mempty constraints client (Head.Initial contestationPeriod hydraParties) mempty
  logInfo $ "Triggered Init " <> show @String cardanoPubKeys

-- | Parameters as they are available in the 'Initial' state.
data InitialParams = InitialParams
  { contestationPeriod :: ContestationPeriod
  , parties :: [Party]
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance Arbitrary InitialParams where
  shrink = genericShrink
  arbitrary = genericArbitrary

watch :: Contract (Last InitialParams) (Endpoint "abort" ()) HydraPlutusError ()
watch = do
  (params, token) <- watchInit
  tell $ Last $ Just params
  selectList [abort token]

-- | Watch Initial script address to extract head's initial parameters and thread token.
-- Relies on the fact that a participation token (payed to the address) uses the
-- same 'CurrencySymbol' as the thread token.
watchInit :: Contract w s HydraPlutusError (InitialParams, AssetClass)
watchInit = do
  logInfo @String $ "watchInit: Looking for an init tx and it's parties"
  pubKey <- ownPubKey
  let address = Initial.address
      pkh = pubKeyHash pubKey
  loop $ do
    txs <- nextTransactionsAt address
    let foundTokens = txs >>= mapMaybe (findToken pkh) . Map.elems . outputsMapFromTxForAddress address
    logInfo $ "found tokens: " <> show @String foundTokens
    case foundTokens of
      [token] -> do
        let datums = txs >>= rights . fmap (lookupDatum token) . Map.elems . outputsMapFromTxForAddress (scriptAddress token)
        logInfo @String $ "found init tx(s) with datums: " <> show datums
        case datums of
          [Head.Initial contestationPeriod parties] ->
            pure $ Just (InitialParams{contestationPeriod, parties}, token)
          _ -> pure Nothing
      _ -> pure Nothing
 where
  loop action =
    action >>= \case
      Nothing -> loop action
      Just result -> pure result

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

  scriptAddress = Scripts.validatorAddress . Head.typedValidator

  lookupDatum token txOutTx = tyTxOutData <$> typeScriptTxOut (Head.typedValidator token) txOutTx

-- TODO(SN): use this in a greate contract which 'watchInit' first and then does this
abort :: AssetClass -> Promise w (Endpoint "abort" ()) HydraPlutusError ()
abort threadToken = endpoint @"abort" $ \_ -> do
  logInfo @String $ "abort: which contract now?"
  let client = Head.machineClient threadToken
  void $ SM.runStep client Head.Abort

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
