{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.ExternalPABSpec where

import Hydra.Prelude

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Hydra.Chain (Chain (..))
import Hydra.Chain.ExternalPAB (withExternalPAB)
import Hydra.HeadLogic (OnChainTx (..))
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Logging (nullTracer)
import System.Process (CreateProcess (std_in, std_out), StdStream (CreatePipe), proc, withCreateProcess)
import Test.Hspec (shouldReturn)
import Test.Hspec.Core.Spec (Spec, describe, it)
import Test.Util (failAfter)
import Cardano.Crypto.DSIGN (VerKeyDSIGN, MockDSIGN, DSIGNAlgorithm (deriveVerKeyDSIGN), SignKeyDSIGN)
import Hydra.Ledger (Party (UnsafeParty))

spec :: Spec
spec =
  describe "ExternalPAB" $ do
    it "publishes init tx using wallet 1 and observes it also" $ do
      failAfter 10 $
        withHydraPAB $ do
          calledBack <- newEmptyMVar
          withExternalPAB nullTracer (putMVar calledBack) $ \Chain{postTx} -> do
            let parties = [alice, bob, carol]
            postTx $ InitTx @SimpleTx parties
            failAfter 3 $
              takeMVar calledBack `shouldReturn` InitTx parties

aliceSk, bobSk, carolSk :: SignKeyDSIGN MockDSIGN
aliceSk = 10
bobSk = 20
carolSk = 30

aliceVk, bobVk, carolVk :: VerKeyDSIGN MockDSIGN
aliceVk = deriveVerKeyDSIGN aliceSk
bobVk = deriveVerKeyDSIGN bobSk
carolVk = deriveVerKeyDSIGN carolSk

alice, bob, carol :: Party
alice = UnsafeParty aliceVk
bob = UnsafeParty bobVk
carol = UnsafeParty carolVk

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
