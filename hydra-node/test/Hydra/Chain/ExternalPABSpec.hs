{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Hydra.Chain.ExternalPABSpec where

import Hydra.Prelude

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (deriveVerKeyDSIGN), MockDSIGN, SignKeyDSIGN, VerKeyDSIGN)
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Hydra.Chain (Chain (..), OnChainTx (InitTx), HeadParameters (HeadParameters, contestationPeriod))
import Hydra.Chain.ExternalPAB (withExternalPAB)
import Hydra.Ledger (Party (UnsafeParty))
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Logging (nullTracer)
import System.Process (CreateProcess (std_in, std_out), StdStream (CreatePipe), proc, withCreateProcess)
import Test.Hspec (shouldReturn)
import Test.Hspec.Core.Spec (Spec, describe, it)
import Test.Util (failAfter)

spec :: Spec
spec =
  describe "ExternalPAB" $ do
    it "publishes init tx using wallet 1 and observes it also" $ do
      failAfter 40 $
        withHydraPAB $ do
          calledBack <- newEmptyMVar
          -- NOTE(SN): The cardano pubkeys and which wallet is used, is
          -- hard-coded in 'withExternalPAB'!
          withExternalPAB @SimpleTx 1 nullTracer (putMVar calledBack) $ \_ ->
            withExternalPAB 2 nullTracer (const $ pure ()) $ \Chain{postTx} -> do
              let parameters = HeadParameters 42 [alice, bob, carol]
              -- HACK(SN): contestationPeriod = 42 is currently hard-coded!
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
