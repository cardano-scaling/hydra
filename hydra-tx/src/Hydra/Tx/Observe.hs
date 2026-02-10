{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Observe hydra transactions
module Hydra.Tx.Observe (
  module Hydra.Tx.Observe,
  module Hydra.Tx.Init,
  module Hydra.Tx.Abort,
  module Hydra.Tx.Commit,
  module Hydra.Tx.CollectCom,
  module Hydra.Tx.Decrement,
  module Hydra.Tx.Deposit,
  module Hydra.Tx.Increment,
  module Hydra.Tx.Recover,
  module Hydra.Tx.Close,
  module Hydra.Tx.Contest,
  module Hydra.Tx.Fanout,
) where

import "hydra-cardano-api" Hydra.Cardano.Api
import "hydra-prelude" Hydra.Prelude hiding (toList)
import "aeson" Data.Aeson (Value (Object, String), defaultOptions, genericToJSON, withObject, (.:))
import "aeson" Data.Aeson qualified as Aeson (Value)
import "aeson" Data.Aeson.KeyMap qualified as KeyMap
import "cardano-ledger-api" Cardano.Ledger.Api (IsValid (..), isValidTxL)
import "lens" Control.Lens ((^.))
import "lens-aeson" Data.Aeson.Lens (key, _Object, _String)

import Hydra.Tx.Abort (AbortObservation (..), observeAbortTx)
import Hydra.Tx.Close (CloseObservation (..), observeCloseTx)
import Hydra.Tx.CollectCom (CollectComObservation (..), observeCollectComTx)
import Hydra.Tx.Commit (CommitObservation (..), observeCommitTx)
import Hydra.Tx.Contest (ContestObservation (..), observeContestTx)
import Hydra.Tx.Decrement (DecrementObservation (..), observeDecrementTx)
import Hydra.Tx.Deposit (DepositObservation (..), observeDepositTx)
import Hydra.Tx.Fanout (FanoutObservation (..), observeFanoutTx)
import Hydra.Tx.Increment (IncrementObservation (..), observeIncrementTx)
import Hydra.Tx.Init (InitObservation (..), NotAnInitReason (..), observeInitTx)
import Hydra.Tx.Recover (RecoverObservation (..), observeRecoverTx)

-- * Observe Hydra Head transactions

-- | Generalised type for arbitrary Head observations on-chain.
data HeadObservation
  = NoHeadTx
  | Init InitObservation
  | Abort AbortObservation
  | Commit CommitObservation
  | CollectCom CollectComObservation
  | Deposit DepositObservation
  | Recover RecoverObservation
  | Increment IncrementObservation
  | Decrement DecrementObservation
  | Close CloseObservation
  | Contest ContestObservation
  | Fanout FanoutObservation
  deriving stock (Eq, Show, Generic)

-- NOTE: Custom To/FromJSON instances to create a "flat" encoding. The default
-- generic implementation would use 'TaggedObject' with a "contents" field, but
-- we want it flat so it resembles what we (used to) have for 'OnChainTx'
-- without removing the sub-types.

instance ToJSON HeadObservation where
  toJSON = mergeContents . genericToJSON defaultOptions
   where
    mergeContents :: Aeson.Value -> Aeson.Value
    mergeContents v = do
      let tag = v ^. key "tag" . _String
      let km = v ^. key "contents" . _Object
      Object $ KeyMap.singleton "tag" (String tag) <> km

instance FromJSON HeadObservation where
  parseJSON = withObject "HeadObservation" $ \o -> do
    tag <- o .: "tag"
    case tag :: Text of
      "NoHeadTx" -> pure NoHeadTx
      "Init" -> Init <$> parseJSON (Object o)
      "Abort" -> Abort <$> parseJSON (Object o)
      "Commit" -> Commit <$> parseJSON (Object o)
      "CollectCom" -> CollectCom <$> parseJSON (Object o)
      "Deposit" -> Deposit <$> parseJSON (Object o)
      "Recover" -> Recover <$> parseJSON (Object o)
      "Increment" -> Increment <$> parseJSON (Object o)
      "Decrement" -> Decrement <$> parseJSON (Object o)
      "Close" -> Close <$> parseJSON (Object o)
      "Contest" -> Contest <$> parseJSON (Object o)
      "Fanout" -> Fanout <$> parseJSON (Object o)
      _ -> fail $ "Unknown tag: " <> show tag

-- | Observe any Hydra head transaction.
observeHeadTx :: NetworkId -> UTxO -> Tx -> HeadObservation
observeHeadTx networkId utxo tx =
  -- XXX: This is throwing away valuable information! We should be collecting
  -- all "not an XX" reasons here in case we fall through and want that
  -- diagnostic information in the call site of this function. Collecting errors
  -- could be done with 'validation' or a similar package.
  fromMaybe NoHeadTx $ do
    -- NOTE: Never make an observation on invalid transactions.
    guard txIsValid
    either (const Nothing) (Just . Init) (observeInitTx tx)
      <|> Abort <$> observeAbortTx utxo tx
      <|> Commit <$> observeCommitTx networkId utxo tx
      <|> CollectCom <$> observeCollectComTx utxo tx
      <|> Deposit <$> observeDepositTx networkId tx
      <|> Recover <$> observeRecoverTx networkId utxo tx
      <|> Increment <$> observeIncrementTx utxo tx
      <|> Decrement <$> observeDecrementTx utxo tx
      <|> Close <$> observeCloseTx utxo tx
      <|> Contest <$> observeContestTx utxo tx
      <|> Fanout <$> observeFanoutTx utxo tx
 where
  txIsValid = toLedgerTx tx ^. isValidTxL == IsValid True
