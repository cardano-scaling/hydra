{-# LANGUAGE DerivingStrategies #-}

module Hydra.NodeSpec where

import Cardano.Prelude
import qualified Data.Map.Strict as Map
import Hydra.Node
import Test.Hspec (Spec, around, describe, it, shouldBe, shouldReturn)

spec :: Spec
spec = around startStopNode $ do
  describe "Live Hydra Node" $ do
    it "returns Ready when asked status" $ \node -> do
      status node `shouldReturn` Ready

  describe "On-Chain Transactions" $ do
    it "Build Init transaction with outputs to be consumed by peers when given Init command" $ \node -> do
      let initParameters =
            HeadParameters{verificationKeys = [key1, key2, key3]}
          key1 = VerificationKey "key1"
          key2 = VerificationKey "key2"
          key3 = VerificationKey "key3"
          numberOfParticipants = length (verificationKeys initParameters)

      (tx, assetId) <- buildInitialTransaction node initParameters

      length (outputs tx)
        `shouldBe` 1 + numberOfParticipants
      length (filter (hasParticipationToken assetId 1) (outputs tx))
        `shouldBe` numberOfParticipants

hasParticipationToken :: AssetId -> Quantity -> TransactionOutput -> Bool
hasParticipationToken (policyId, assetName) numberOfTokens TransactionOutput{value = Value _ tokens} =
  (Map.lookup policyId tokens >>= Map.lookup assetName) == Just numberOfTokens
