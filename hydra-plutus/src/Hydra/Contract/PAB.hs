{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Contract.PAB where

import Hydra.Prelude hiding (init)

import Control.Lens (makeClassyPrisms)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import Hydra.Contract.ContestationPeriod (ContestationPeriod)
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.Initial as Initial
import Hydra.Contract.Party (Party)
import Ledger (
  ChainIndexTxOut,
  CurrencySymbol,
  PubKeyHash (..),
  TxOut (txOutValue),
  TxOutRef,
  pubKeyAddress,
  pubKeyHash,
  toTxOut,
 )
import Ledger.Typed.Tx (tyTxOutData, typeScriptTxOut)
import Ledger.Value (AssetClass, TokenName (..), flattenValue)
import qualified Ledger.Value as Value
import Plutus.ChainIndex (ChainIndexTx, txOutRefMapForAddr)
import Plutus.Contract (
  AsContractError (..),
  Contract,
  ContractError (..),
  Empty,
  Endpoint,
  awaitUtxoProduced,
  endpoint,
  logInfo,
  ownPubKey,
  selectList,
  tell,
  utxosAt,
  utxosTxOutTxFromTx,
  waitNSlots,
 )
import qualified Plutus.Contract.StateMachine as SM
import Plutus.Contract.Types (Promise (..))
import qualified Plutus.Contracts.Currency as Currency
import Plutus.PAB.Effects.Contract.Builtin (HasDefinitions (..), SomeBuiltin (..))

-- | Hard-coded port used between hydra-node and the PAB server.
pabPort :: Int
pabPort = 8888

-- | Enumeration of contracts available in the PAB.
data PabContract
  = GetUtxos
  | Init
  | Watch
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Pretty PabContract where
  pretty = viaShow

instance HasDefinitions PabContract where
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

getUtxo :: Contract (Last (Map TxOutRef TxOut)) Empty ContractError ()
getUtxo = do
  logInfo @Text $ "getUtxo: Starting to get and report utxo map every slot"
  address <- pubKeyAddress <$> ownPubKey
  loop address
 where
  loop address = do
    utxos <- Map.map toTxOut <$> utxosAt address
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

-- | Transactions as they are observed by the PAB. We use a distinct type from
-- the 'Hydra.Chain' as we want to keep hydra-node not tied to hydra-plutus.
-- Tests are ensuring that this type as the same wire format as 'OnChainTx'.
data ObservedTx
  = OnInitTx {contestationPeriod :: ContestationPeriod, parties :: [Party]}
  | OnAbortTx
  -- TODO(SN): incomplete (obviously)
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Arbitrary ObservedTx where
  shrink = genericShrink
  arbitrary = genericArbitrary

watch :: Contract (Last ObservedTx) (Endpoint "abort" ()) HydraPlutusError ()
watch = do
  (contestationPeriod, parties, threadToken) <- watchInit
  tell $ Last $ Just $ OnInitTx contestationPeriod parties
  selectList
    [ Promise (watchForSM threadToken)
    , abort threadToken
    ]

watchForSM :: AssetClass -> Contract (Last ObservedTx) s HydraPlutusError ()
watchForSM threadToken = do
  logInfo @String $ "watchForSM"
  let client = Head.machineClient threadToken
  smState <- SM.waitForUpdate client
  let mstate = tyTxOutData . SM.ocsTxOut <$> smState
  case mstate of
    Just Head.Final -> tell . Last $ Just OnAbortTx
    _ -> logInfo @String $ "uninferrable state"

-- | Watch Initial script address to extract head's initial parameters and thread token.
-- Relies on the fact that a participation token (payed to the address) uses the
-- same 'CurrencySymbol' as the thread token.
watchInit :: Contract w s HydraPlutusError (ContestationPeriod, [Party], AssetClass)
watchInit = do
  logInfo @String $ "watchInit: Looking for an init tx and it's parties"
  pubKey <- ownPubKey
  let address = Initial.address
      pkh = pubKeyHash pubKey
  loop $ do
    -- REVIEW(SN): we are given the same ChainIndexTx multiple times here, why?
    txs <- NonEmpty.nub <$> awaitUtxoProduced address
    forM_ txs $ \tx -> logInfo @String $ "watchInit: considering tx " <> show tx
    let foundTokens = toList txs >>= mapMaybe (findToken pkh) . Map.elems . txOutRefMapForAddr address
    logInfo @String $ "watchInit: found tokens " <> show foundTokens
    -- TODO(SN): in theory we could see multiple init txs, so fold and return first
    case foundTokens of
      [token] -> do
        -- NOTE(SN): it's important to not use 'txOutRefMapForAddr' here as it throws away the tx reference for 'TxOut'
        allUtxos <- fmap concat . mapM utxosTxOutTxFromTx $ toList txs
        let datums = mapMaybe (lookupDatum token) allUtxos
        logInfo @String $ "watchInit: found init tx(s) with datums: " <> show datums
        case datums of
          [Head.Initial contestationPeriod parties] ->
            pure $ Just (contestationPeriod, parties, token)
          _ -> pure Nothing
      _ -> pure Nothing
 where
  loop action =
    action >>= \case
      Nothing -> loop action
      Just result -> pure result

  -- Find candidates for a Hydra Head threadToken 'AssetClass', that is if the
  -- 'TokenName' matches our public key
  findToken :: PubKeyHash -> (TxOut, ChainIndexTx) -> Maybe AssetClass
  findToken pkh (txout, _tx) =
    let value = txOutValue txout
        flat = flattenValue value
        mres = find (\(_, tokenName, amount) -> amount == 1 && tokenName == participationTokenName pkh) flat
     in case mres of
          Just (symbol, _, _) -> Just $ mkThreadToken symbol
          Nothing -> Nothing

  -- XXX(SN): Maybe is hard to debug
  lookupDatum :: AssetClass -> (TxOutRef, (ChainIndexTxOut, ChainIndexTx)) -> Maybe Head.State
  lookupDatum token (txOutRef, (txOut, _tx)) = do
    typedTxOut <- rightToMaybe $ typeScriptTxOut (Head.typedValidator token) txOutRef txOut
    pure $ tyTxOutData typedTxOut

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
