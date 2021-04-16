import Cardano.Prelude
import Hydra.Logic (HeadParameters (..), SnapshotStrategy (..), createHeadState)
import Hydra.Node

main :: IO ()
main = do
  eq <- createEventQueue
  hh <- createHydraHead headState
  oc <- createChainClient eq
  hn <- createHydraNetwork eq

  -- NOTE(SN): here we would introduce concurrent head processing, e.g. with
  -- something like 'forM_ [0..1] $ async'
  runHydra eq hn oc hh
 where
  headState = createHeadState [] HeadParameters SnapshotStrategy
