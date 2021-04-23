import Cardano.Prelude
import Hydra.Ledger (cardanoLedger)
import qualified Hydra.Ledger.MaryTest as MaryTest
import Hydra.Logic (HeadParameters (..), SnapshotStrategy (..), createHeadState)
import Hydra.Node

main :: IO ()
main = do
  eq <- createEventQueue
  hh <- createHydraHead headState ledger
  oc <- createChainClient eq
  hn <- createHydraNetwork eq
  cs <- createClientSideRepl oc hh hn loadTx

  -- NOTE(SN): here we would introduce concurrent head processing, e.g. with
  -- something like 'forM_ [0..1] $ async'
  forever $ do
    e <- nextEvent eq
    handleNextEvent hn oc cs hh e
 where
  headState = createHeadState [] HeadParameters SnapshotStrategy
  ledger = cardanoLedger defaultEnv
  defaultEnv = MaryTest.mkLedgerEnv

  loadTx fp = panic $ "should load and decode a tx from " <> show fp
