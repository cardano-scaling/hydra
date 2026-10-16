{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Hydra.HeadLogic.StateSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Binary (decodeFull', serialize')
import Data.Aeson (Result (Error, Success), fromJSON)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Hydra.Chain.ChainState (ChainSlot (..))
import Hydra.HeadLogic.State (
  Settlement (..),
  SettlementConflict (..),
  SettlementStatus (..),
  Settlements,
  adjustSettlementStatus,
  emptySettlements,
  filterSettlements,
  mapSettlementStatuses,
  settlementDeposit,
  settlementDeposits,
  settlementEntries,
  tryInsertSettlement,
  trySettlementsFromMap,
 )
import Hydra.Ledger.Cardano (Tx)
import Hydra.Tx.IsTx (TxIdType)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (depositTxId, number, version), SnapshotVersion, getSnapshot)
import Test.Hydra.HeadLogic.State ()
import Test.Hydra.Tx.Gen ()
import Test.QuickCheck (Property, conjoin, counterexample, elements, forAll, listOf, oneof, property, resize, suchThat, vectorOf, (===))

data SettlementAction
  = Retain (Settlement Tx)
  | Land SnapshotVersion ChainSlot
  | RollBack ChainSlot
  | Prune ChainSlot
  deriving stock (Show)

spec :: Spec
spec = describe "settlement caches" $ do
  prop "match a plain map after every retention, re-landing, rollback and prune" $
    forAll (resize 30 genActions) $
      checkActions emptySettlements Map.empty

  prop "preserve the map's CBOR encoding and rebuild caches on decode" $
    forAll genEntries $ \(entries :: Map.Map SnapshotVersion (Settlement Tx)) ->
      case trySettlementsFromMap entries of
        Left err -> counterexample (show err) False
        Right settlements ->
          conjoin
            [ serialize' settlements === serialize' entries
            , decodeFull' (serialize' entries) === Right settlements
            ]

  prop "preserve the map's JSON encoding and rebuild caches on decode" $
    forAll genEntries $ \(entries :: Map.Map SnapshotVersion (Settlement Tx)) ->
      case trySettlementsFromMap entries of
        Left err -> counterexample (show err) False
        Right settlements ->
          conjoin
            [ toJSON settlements === toJSON entries
            , fromJSON (toJSON entries) === Success settlements
            ]

  prop "keep erased deposits blocked while pruning landed settlements" $ \depositId ->
    forAll (arbitrary `suchThat` (/= depositId)) $ \otherDepositId ->
      forAll genSmallSnapshot $ \snap ->
        let erased = settlementAt snap 0 (Just depositId) Erased
            landed = settlementAt snap 1 (Just otherDepositId) (Landed 1)
            conflicting = settlementAt snap 2 (Just depositId) Erased
         in case trySettlementsFromMap (Map.fromList [(0, erased), (1, landed)]) of
              Left err -> counterexample (show err) False
              Right stored ->
                let pruned = filterSettlements (reachable 1) stored
                 in conjoin
                      [ settlementEntries pruned === Map.singleton 0 erased
                      , settlementDeposits pruned === Set.singleton depositId
                      , tryInsertSettlement conflicting pruned
                          === Left (DepositAlreadyClaimed (Set.singleton depositId))
                      ]

  prop "release a deposit only when its re-landed settlement is pruned" $ \depositId ->
    forAll genSmallSnapshot $ \snap ->
      let erased = settlementAt snap 0 (Just depositId) Erased
          next = settlementAt snap 1 (Just depositId) (Landed 3)
       in case tryInsertSettlement erased emptySettlements of
            Left err -> counterexample (show err) False
            Right stored ->
              let landed = adjustSettlementStatus 0 (Landed 2) stored
                  retained = filterSettlements (reachable 1) landed
                  released = filterSettlements (reachable 2) retained
               in conjoin
                    [ settlementEntries retained === Map.singleton 0 erased{status = Landed 2}
                    , settlementDeposits retained === Set.singleton depositId
                    , tryInsertSettlement next retained
                        === Left (DepositAlreadyClaimed (Set.singleton depositId))
                    , released === emptySettlements
                    , case tryInsertSettlement next released of
                        Left err -> counterexample (show err) False
                        Right storedAgain ->
                          conjoin
                            [ settlementEntries storedAgain === Map.singleton 1 next
                            , settlementDeposits storedAgain === Set.singleton depositId
                            ]
                    ]

  describe "checked insertion" $ do
    prop "retains a settlement in an empty store" $
      forAll genSmallSettlement $ \entry ->
        case tryInsertSettlement entry emptySettlements of
          Left err ->
            counterexample (show err) False
          Right stored ->
            conjoin
              [ settlementEntries stored
                  === Map.singleton (getSnapshot entry.snapshot).version entry
              , settlementDeposits stored
                  === Set.fromList (maybeToList $ settlementDeposit entry)
              ]
    prop "rejects a deposit already claimed by another version" $ \depositId ->
      forAll genSmallSnapshot $ \snap ->
        let atVersion v = settlementAt snap v (Just depositId) (Landed 1)
         in case tryInsertSettlement (atVersion 0) emptySettlements of
              Left err ->
                counterexample
                  ("First insertion failed: " <> show err)
                  False
              Right stored ->
                tryInsertSettlement (atVersion 1) stored
                  === Left (DepositAlreadyClaimed (Set.singleton depositId))

    prop "preserves the original settlement for an existing version" $ \depositId ->
      forAll genSmallSnapshot $ \snap ->
        let original = settlementAt snap snap.version (Just depositId) Erased
            replacement = settlementAt (snap{number = snap.number + 1}) snap.version (Just depositId) (Landed 1)
         in case tryInsertSettlement original emptySettlements of
              Left err ->
                counterexample ("First insertion failed: " <> show err) False
              Right stored ->
                tryInsertSettlement replacement stored === Right stored

    prop "ignores new deposit claims when a version is already retained" $ \depositId ->
      forAll genSmallSnapshot $ \snap ->
        let original = settlementAt snap snap.version Nothing Erased
            replacement = settlementAt snap snap.version (Just depositId) Erased
         in case tryInsertSettlement original emptySettlements of
              Left err -> counterexample (show err) False
              Right stored ->
                tryInsertSettlement replacement stored === Right stored

    prop "retains a settlement without deposits and preserves existing claims" $ \depositId ->
      forAll genSmallSnapshot $ \snap ->
        let original = settlementAt snap 0 (Just depositId) (Landed 1)
            withoutDeposit = settlementAt snap 1 Nothing (Landed 1)
            result = do
              stored <- tryInsertSettlement original emptySettlements
              tryInsertSettlement withoutDeposit stored
         in case result of
              Left err ->
                counterexample (show err) False
              Right stored ->
                conjoin
                  [ settlementEntries stored
                      === Map.fromList [(0, original), (1, withoutDeposit)]
                  , settlementDeposits stored
                      === Set.singleton depositId
                  ]
  describe "checked map construction" $ do
    it "accepts an empty map" $
      trySettlementsFromMap Map.empty
        `shouldBe` Right (emptySettlements :: Settlements Tx)

    prop "rejects a key that differs from the snapshot version" $
      forAll genSmallSettlement $ \entry ->
        let snapshotVersion = (getSnapshot entry.snapshot).version
            settlementVersion = snapshotVersion + 1
            entries = Map.singleton settlementVersion entry
         in conjoin
              [ trySettlementsFromMap entries
                  === Left SettlementVersionMismatch{settlementVersion, snapshotVersion}
              , rejectsOnDecode entries
              ]

    prop "rejects a deposit claimed by two versions" $ \depositId ->
      forAll genSmallSnapshot $ \snap ->
        let entryAt v = settlementAt snap v (Just depositId) (Landed 1)
            entries =
              Map.fromList [(0, entryAt 0), (1, entryAt 1)]
         in conjoin
              [ trySettlementsFromMap entries
                  === Left (DepositAlreadyClaimed (Set.singleton depositId))
              , rejectsOnDecode entries
              ]

    prop "preserves entries and rebuilds the deposit cache" $ \depositId ->
      forAll genSmallSnapshot $ \snap ->
        let entries =
              Map.fromList
                [ (0, settlementAt snap 0 (Just depositId) Erased)
                , (1, settlementAt snap 1 Nothing (Landed 1))
                ]
         in case trySettlementsFromMap entries of
              Left err ->
                counterexample (show err) False
              Right stored ->
                conjoin
                  [ settlementEntries stored === entries
                  , settlementDeposits stored === Set.singleton depositId
                  ]
 where
  reachable cutoff = \case
    Landed slot -> slot > cutoff
    Erased -> True

  genActions = do
    depositIds <- vectorOf 3 arbitrary
    listOf $ genAction depositIds

  genAction depositIds =
    oneof
      [ do
          snap <- genSmallSnapshot
          version <- elements [0 .. 5]
          depositTxId <- elements (Nothing : map Just depositIds)
          status <- oneof [pure Erased, Landed . ChainSlot <$> elements [0 .. 20]]
          pure $ Retain (settlementAt snap version depositTxId status)
      , Land <$> elements [0 .. 5] <*> (ChainSlot <$> elements [0 .. 20])
      , RollBack . ChainSlot <$> elements [0 .. 20]
      , Prune . ChainSlot <$> elements [0 .. 20]
      ]

  checkActions :: Settlements Tx -> Map.Map SnapshotVersion (Settlement Tx) -> [SettlementAction] -> Property
  checkActions settlements entries actions =
    counterexample (show entries) $
      conjoin
        [ settlementEntries settlements === entries
        , trySettlementsFromMap entries === Right settlements
        , settlementDeposits settlements === Set.fromList (mapMaybe settlementDeposit $ Map.elems entries)
        , case actions of
            [] -> property True
            action : rest -> counterexample (show action) $ case action of
              Retain entry ->
                let actual = tryInsertSettlement entry settlements
                    expected = retainInModel entry entries
                 in conjoin
                      [ fmap settlementEntries actual === expected
                      , checkActions
                          (fromRight settlements actual)
                          (fromRight entries expected)
                          rest
                      ]
              Land version slot ->
                checkActions
                  (adjustSettlementStatus version (Landed slot) settlements)
                  (Map.adjust (\entry -> entry{status = Landed slot}) version entries)
                  rest
              RollBack slot ->
                let erase = \case
                      Landed observed | observed > slot -> Erased
                      status -> status
                 in checkActions
                      (mapSettlementStatuses erase settlements)
                      (Map.map (\entry -> entry{status = erase entry.status}) entries)
                      rest
              Prune cutoff ->
                checkActions
                  (filterSettlements (reachable cutoff) settlements)
                  (Map.filter (reachable cutoff . (.status)) entries)
                  rest
        ]

  retainInModel ::
    Settlement Tx ->
    Map.Map SnapshotVersion (Settlement Tx) ->
    Either (SettlementConflict Tx) (Map.Map SnapshotVersion (Settlement Tx))
  retainInModel entry entries
    | Map.member version entries = Right entries
    | Just depositId <- settlementDeposit entry
    , depositId `elem` mapMaybe settlementDeposit (Map.elems entries) =
        Left (DepositAlreadyClaimed (Set.singleton depositId))
    | otherwise = Right (Map.insert version entry entries)
   where
    version = (getSnapshot entry.snapshot).version

rejectsOnDecode ::
  Map.Map SnapshotVersion (Settlement Tx) ->
  Property
rejectsOnDecode entries =
  conjoin
    [ counterexample "JSON accepted an invalid settlement map" $
        case fromJSON @(Settlements Tx) (toJSON entries) of
          Error _ -> True
          Success _ -> False
    , counterexample "CBOR accepted an invalid settlement map" $
        isLeft (decodeFull' @(Settlements Tx) (serialize' entries))
    ]

settlementAt :: Snapshot Tx -> SnapshotVersion -> Maybe (TxIdType Tx) -> SettlementStatus -> Settlement Tx
settlementAt snap version depositTxId status =
  Settlement{snapshot = ConfirmedSnapshot (snap{version, depositTxId}) mempty, status}

genSmallSnapshot :: Gen (Snapshot Tx)
genSmallSnapshot = resize 2 arbitrary

genSmallSettlement :: Gen (Settlement Tx)
genSmallSettlement = do
  snap <- genSmallSnapshot
  settlementAt snap snap.version snap.depositTxId <$> arbitrary

genEntries :: Gen (Map.Map SnapshotVersion (Settlement Tx))
genEntries =
  settlementEntries <$> resize 5 arbitrary
