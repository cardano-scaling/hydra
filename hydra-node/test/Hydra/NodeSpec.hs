{-# LANGUAGE DerivingStrategies #-}

module Hydra.NodeSpec where

import Cardano.Prelude
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

      (tx, cid) <- buildInitialTransaction node initParameters

      let numberOfParticipants = length (verificationKeys initParameters)

      length (outputs tx)
        `shouldBe` 1 + numberOfParticipants
      length (filter (hasParticipationToken cid 1) (outputs tx))
        `shouldBe` numberOfParticipants

hasParticipationToken :: AssetId -> Natural -> TransactionOutput -> Bool
hasParticipationToken = panic "TODO"
