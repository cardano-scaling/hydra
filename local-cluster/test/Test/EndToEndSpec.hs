module Test.EndToEndSpec where

import Cardano.Prelude
import HydraNode
import Test.Hspec (
  Spec,
  describe,
  it,
  shouldReturn,
 )
import Prelude (error)

spec :: Spec
spec = describe "End-to-end test using a mocked chain though" $ do
  it "can init a head between three hydra nodes" $ do
    withHydraNode 1 $ \n1 ->
      withHydraNode 2 $ \n2 ->
        withHydraNode 3 $ \n3 -> do
          sendRequest n1 (Init [1, 2, 3])
          wait1sForResponse n1 `shouldReturn` Just ReadyToCommit
          wait1sForResponse n2 `shouldReturn` Just ReadyToCommit
          wait1sForResponse n3 `shouldReturn` Just ReadyToCommit

wait1sForResponse :: HydraNode -> IO (Maybe Response)
wait1sForResponse = error "not implemented"
