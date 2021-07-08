{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Hydra.Chain.ExternalPABSpec where

import Hydra.Prelude

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (deriveVerKeyDSIGN), MockDSIGN, SignKeyDSIGN, VerKeyDSIGN)
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Hydra.Chain (Chain (..))
import Hydra.Chain.ExternalPAB (withExternalPAB)
import Hydra.HeadLogic (OnChainTx (..))
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
      failAfter 20 $
        withHydraPAB $ do
          calledBack <- newEmptyMVar
          -- NOTE(SN): The cardano pubkeys and which wallet is used, is
          -- hard-coded in 'withExternalPAB'!
          withExternalPAB nullTracer (putMVar calledBack) $ \Chain{postTx} -> do
            let parties = [alice, bob, carol]
            postTx $ InitTx @SimpleTx parties
            takeMVar calledBack `shouldReturn` InitTx parties

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
  pab = proc "hydra-pab" []
