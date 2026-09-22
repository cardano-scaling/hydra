{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Hydra.HeadLogic.StateSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Binary (decodeFull', serialize')
import Data.Aeson (Result (Success), fromJSON)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Hydra.Chain.ChainState (ChainSlot (..))
import Hydra.HeadLogic.State
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Tx.Snapshot (SnapshotVersion)
import Test.Hydra.HeadLogic.State ()
import Test.Hydra.Ledger.Simple ()
import Test.QuickCheck (Property, conjoin, counterexample, elements, forAll, listOf, oneof, resize, (===))

data SettlementAction
  = Retain SnapshotVersion (Settlement SimpleTx)
  | Land SnapshotVersion ChainSlot
  | RollBack ChainSlot
  | Prune ChainSlot
  deriving stock (Show)

spec :: Spec
spec = describe "settlement caches" $ do
  prop "match a plain map after every retention, re-landing, rollback and prune" $
    forAll (resize 30 $ listOf genAction) $ \actions ->
      conjoin $ check <$> scanl apply (mempty, mempty) actions

  prop "preserve the map's CBOR encoding and rebuild caches on decode" $ \(entries :: Map.Map SnapshotVersion (Settlement SimpleTx)) ->
    let settlements = settlementsFromMap entries
     in conjoin
          [ serialize' settlements === serialize' entries
          , decodeFull' (serialize' entries) === Right settlements
          ]

  prop "preserve the map's JSON encoding and rebuild caches on decode" $ \(entries :: Map.Map SnapshotVersion (Settlement SimpleTx)) ->
    let settlements = settlementsFromMap entries
     in conjoin
          [ toJSON settlements === toJSON entries
          , fromJSON (toJSON entries) === Success settlements
          ]

  prop "keep a shared deposit blocked until its last retained claim expires" $ \(entry :: Settlement SimpleTx) ->
    let retained = insertSettlement 0 entry{status = Landed 1} $ insertSettlement 1 entry{status = Erased} mempty
        pruned = filterSettlements reachable retained
        released = filterSettlements reachable $ adjustSettlementStatus 1 (Landed 1) pruned
     in conjoin
          [ settlementDeposits pruned === Set.fromList (maybeToList $ settlementDeposit entry)
          , settlementDeposits released === mempty
          ]
 where
  reachable = \case
    Landed slot -> slot > 1
    Erased -> True

  genAction =
    oneof
      [ Retain <$> elements [0 .. 5] <*> arbitrary
      , Land <$> elements [0 .. 5] <*> (ChainSlot <$> elements [0 .. 20])
      , RollBack . ChainSlot <$> elements [0 .. 20]
      , Prune . ChainSlot <$> elements [0 .. 20]
      ]

  apply (settlements, entries) = \case
    Retain version entry ->
      (insertSettlement version entry settlements, Map.insertWith (\_ old -> old) version entry entries)
    Land version slot ->
      (adjustSettlementStatus version (Landed slot) settlements, Map.adjust (\entry -> entry{status = Landed slot}) version entries)
    RollBack slot ->
      let erase = \case
            Landed observed | observed > slot -> Erased
            status -> status
       in (mapSettlementStatuses erase settlements, Map.map (\entry -> entry{status = erase entry.status}) entries)
    Prune cutoff ->
      let keep = \case
            Landed observed -> observed > cutoff
            Erased -> True
       in (filterSettlements keep settlements, Map.filter (keep . (.status)) entries)

  check :: (Settlements SimpleTx, Map.Map SnapshotVersion (Settlement SimpleTx)) -> Property
  check (settlements, entries) =
    counterexample (show entries) $
      conjoin
        [ settlementEntries settlements === entries
        , settlements === settlementsFromMap entries
        , settlementDeposits settlements === Set.fromList (mapMaybe settlementDeposit $ Map.elems entries)
        ]
