{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Hydra.Chain.ExternalPABSpec where

import Hydra.Prelude

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (deriveVerKeyDSIGN), MockDSIGN, SignKeyDSIGN, VerKeyDSIGN)
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import qualified Data.Aeson as Aeson
import Hydra.Chain (Chain (..), HeadParameters (HeadParameters, contestationPeriod), OnChainTx (InitTx))
import Hydra.Chain.ExternalPAB (PostInitParams, withExternalPAB)
import qualified Hydra.ContractSM as OnChain
import Hydra.Ledger (Party (UnsafeParty))
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Logging (nullTracer)
import System.Process (CreateProcess (std_in, std_out), StdStream (CreatePipe), proc, withCreateProcess)
import Test.Hspec (shouldReturn)
import Test.Hspec.Core.Spec (Spec, describe, it)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (counterexample, property)
import Test.Util (failAfter)

spec :: Spec
spec = do
  -- We use slightly different types in off-chain and on-chain code, BUT, they
  -- have identical wire formats. We use (JSON) serialization as a mean to turn
  -- one into the other.
  describe "OnChain / OffChain Serialization Roundtrips" $
    prop "PostInitParams -> InitParams" $ \(params :: PostInitParams) ->
      let bytes = Aeson.encode params
       in counterexample (decodeUtf8 bytes) $ case Aeson.eitherDecode bytes of
            Left e ->
              counterexample ("Failed to decode: " <> show e) $ property False
            Right (_ :: OnChain.InitParams) ->
              property True

  describe "ExternalPAB" $ do
    it "publishes init tx using wallet 1 and observes it also" $ do
      failAfter 40 $
        withHydraPAB $ do
          calledBack <- newEmptyMVar
          -- NOTE(SN): The cardano pubkeys and which wallet is used, is
          -- hard-coded in 'withExternalPAB'!
          withExternalPAB @SimpleTx 1 nullTracer (putMVar calledBack) $ \_ ->
            withExternalPAB 2 nullTracer (const $ pure ()) $ \Chain{postTx} -> do
              let parameters = HeadParameters 100 [alice, bob, carol]
              postTx $ InitTx @SimpleTx parameters
              takeMVar calledBack `shouldReturn` InitTx parameters

alice, bob, carol :: Party
alice = UnsafeParty aliceVk
bob = UnsafeParty bobVk
carol = UnsafeParty carolVk

aliceVk, bobVk, carolVk :: VerKeyDSIGN MockDSIGN
aliceVk = deriveVerKeyDSIGN aliceSk
bobVk = deriveVerKeyDSIGN bobSk
carolVk = deriveVerKeyDSIGN carolSk

aliceSk, bobSk, carolSk :: SignKeyDSIGN MockDSIGN
aliceSk = 10
bobSk = 20
carolSk = 30

withHydraPAB :: IO a -> IO a
withHydraPAB action =
  withCreateProcess pab $ \_ _ _ _ -> action
 where
  -- Open a stdin, as pab tries to read from it and a std_out to silence output
  pab =
    (proc "hydra-pab" [])
      { std_in = CreatePipe
      , std_out = CreatePipe
      }
