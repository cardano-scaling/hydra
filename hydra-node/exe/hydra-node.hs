import Cardano.Prelude
import Hydra.Logic (HeadParameters (..), SnapshotStrategy (..), createHeadState)
import Hydra.Node

main :: IO ()
main = do
  eq <- createEventQueue
  hh <- createHydraHead headState
  oc <- createChainClient eq
  hn <- createHydraNetwork eq
  cs <- createClientSideRepl eq

  -- NOTE(SN): here we would introduce concurrent head processing, e.g. with
  -- something like 'forM_ [0..1] $ async'
  forever $ runHydra eq hn oc cs hh
 where
  headState = createHeadState [] HeadParameters SnapshotStrategy
