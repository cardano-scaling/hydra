module Main where

import Hydra.Prelude

import Data.Text.Lazy qualified as TL
import Hydra.Chain.ChainState (ChainSlot (..))
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.State (FanoutMode (..))
import Hydra.HeadLogic.StateEvent (StateEvent (..))
import Hydra.Ledger.Simple (SimpleChainState (..), SimpleTx)
import Hydra.Tx.HeadId (HeadId (..))
import HydraVis.Compare (mkCompareApp, mkCompareModel)
import HydraVis.History (HistoryStep (..), buildHistory)
import HydraVis.Sample (sampleInitialState, sampleStepTime)
import HydraVis.SampleDb (sampleStateEvents)
import HydraVis.UI (mkModel, viewModel)
import Lucid (renderText, toHtml)
import Miso (App (..))
import Test.Hydra.TastyMain (defaultMainHydra)
import Test.Hydra.Tx.Fixture (testHeadId)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Text.HTML.TagSoup (Tag (..), parseTags, partitions)

main :: IO ()
main =
  defaultMainHydra "head-state-viewer" $
    map
      pure
      [ testCase "sample script opens a Head" $ do
          assertBool "emits HeadOpened" $ any ((\case HeadOpened{} -> True; _ -> False) . stateChanged) sampleStateEvents
          highlighted "Open" (renderSingle (replay []))
      , testCase "renders every step through full and partial fanout" $
          forM_ [[closed, fannedOut, NetworkConnected], [closed, partial, NetworkConnected, fannedOut]] $ \changes ->
            forM_ (inits (replay changes)) $ \steps -> do
              let single = renderSingle steps
                  app = mkCompareApp (mkCompareModel [("alice", steps)])
                  comparison = renderText (toHtml (view app (model app)))
              assertBool "single-node view renders completely" (TL.length single > 0)
              assertBool "comparison renders completely" (TL.length comparison > 0)
              noUnexpected single
      , testCase "shows finalized details in both views" $ do
          let steps = replay [closed, fannedOut]
              single = renderSingle steps
              app = mkCompareApp (mkCompareModel [("alice", steps)])
          assertBool "snapshot is finalized" ("head is Finalized; no current snapshot." `TL.isInfixOf` single)
          assertBool "comparison is finalized" ("finalized" `TL.isInfixOf` renderText (toHtml (view app (model app))))
          highlighted "Final" single
      , testCase "pending and cancelled fanout stay Closed" $
          forM_ [[closed, pending], [closed, pending, HeadFanoutReverted testHeadId]] $ \changes -> do
            let html = renderSingle (replay changes)
            highlighted "Closed" html
            noUnexpected html
      , testCase "restores partial fanout from a checkpoint" $ do
          let checkpoint = checkpointAfter [closed, partial]
              resumed = historyFrom [checkpoint, NetworkConnected]
              finalized = historyFrom [checkpoint, fannedOut, NetworkConnected]
          highlighted "fanoutProgress" (renderSingle resumed)
          highlighted "Final" (renderSingle finalized)
          noUnexpected (renderSingle resumed)
          noUnexpected (renderSingle finalized)
          let atFanout = renderSingle (historyFrom [checkpoint, fannedOut])
          assertBool "highlights final partial fanout" $
            groupHasAttribute "final partial fanout" ("stroke", "#06c") atFanout
      , testCase "restores a finalized checkpoint and accepts later network events" $ do
          let html = renderSingle (historyFrom [checkpointAfter [closed, fannedOut], NetworkConnected])
          highlighted "Final" html
          noUnexpected html
      , testCase "initializing a different Head after finalization is expected" $ do
          let newHead =
                [ HeadOpened parameters chainState (UnsafeHeadId "next-head") headSeed parties
                | StateEvent{stateChanged = HeadOpened parameters chainState _ headSeed parties} <- sampleStateEvents
                ]
              html = renderSingle (replay ([closed, fannedOut] <> newHead))
          highlighted "Open" html
          noUnexpected html
      ]

closed, pending, partial, fannedOut :: StateChanged SimpleTx
closed = HeadClosed testHeadId 0 (SimpleChainState (ChainSlot 3)) (sampleStepTime 10)
pending = HeadFanoutInitiated testHeadId mempty
partial = HeadPartialFannedOut testHeadId mempty mempty (SimpleChainState (ChainSlot 4)) AwaitingSelection
fannedOut = HeadFannedOut testHeadId mempty (SimpleChainState (ChainSlot 5))

replay :: [StateChanged SimpleTx] -> [HistoryStep SimpleTx]
replay changes = historyFrom (map stateChanged sampleStateEvents <> changes)

historyFrom :: [StateChanged SimpleTx] -> [HistoryStep SimpleTx]
historyFrom changes =
  buildHistory sampleInitialState $
    zipWith (\n change -> StateEvent (fromIntegral n) change (sampleStepTime n)) [1 ..] changes

checkpointAfter :: [StateChanged SimpleTx] -> StateChanged SimpleTx
checkpointAfter = Checkpoint . foldl' (\_ step -> stateAfter step) sampleInitialState . replay

renderSingle :: [HistoryStep SimpleTx] -> TL.Text
renderSingle steps =
  renderText . toHtml . viewModel $
    mkModel Nothing Nothing False sampleInitialState (length steps - 1) steps

noUnexpected :: TL.Text -> Assertion
noUnexpected html = assertBool "no unexpected protocol transitions" (not ("unexpected transitions" `TL.isInfixOf` html))

highlighted :: TL.Text -> TL.Text -> Assertion
highlighted caption html =
  assertBool ("highlights " <> toString caption) $
    groupHasAttribute caption ("fill", "#cce5ff") html

-- Inspect labelled SVG groups without depending on diagram coordinates.
groupHasAttribute :: TL.Text -> (TL.Text, TL.Text) -> TL.Text -> Bool
groupHasAttribute caption attribute = any matches . partitions (\case TagOpen "g" _ -> True; _ -> False) . parseTags
 where
  matches tags = TagText caption `elem` tags && any (\case TagOpen _ attrs -> attribute `elem` attrs; _ -> False) tags
