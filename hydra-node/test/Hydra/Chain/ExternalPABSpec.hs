{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.ExternalPABSpec where

import Hydra.Prelude

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (deriveVerKeyDSIGN), MockDSIGN, SignKeyDSIGN, VerKeyDSIGN)
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import qualified Data.Aeson as Aeson
import Hydra.Chain (Chain (..), HeadParameters (..), OnChainTx (..))
import Hydra.Chain.ExternalPAB (PostInitParams, withExternalPab)
import Hydra.Contract.PAB (InitParams, InitialParams)
import Hydra.Ledger (Party (UnsafeParty))
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Logging (nullTracer)
import System.IO.Temp (withSystemTempFile)
import System.Process (CreateProcess (std_in, std_out), StdStream (CreatePipe, UseHandle), proc, withCreateProcess)
import Test.Hspec (pendingWith, shouldReturn)
import Test.Hspec.Core.Spec (Spec, describe, it)
import Test.Hspec.QuickCheck (prop)
import Test.Hydra.Prelude (failAfter)
import Test.QuickCheck (counterexample, property)

spec :: Spec
spec = do
  -- We use slightly different types in off-chain and on-chain code, BUT, they
  -- have identical wire formats. We use (JSON) serialization as a mean to turn
  -- one into the other.
  describe "OffChain <-> OnChain Serialization" $ do
    prop "PostInitParams -> Onchain.InitParams" $ \(params :: PostInitParams) ->
      let bytes = Aeson.encode params
       in counterexample (decodeUtf8 bytes) $ case Aeson.eitherDecode bytes of
            Left e ->
              counterexample ("Failed to decode: " <> show e) $ property False
            Right (_ :: InitParams) ->
              property True

    prop "HeadParameters <- Onchain.InitialParams" $ \(params :: InitialParams) ->
      let bytes = Aeson.encode params
       in counterexample (decodeUtf8 bytes) $ case Aeson.eitherDecode bytes of
            Left e ->
              counterexample ("Failed to decode: " <> show e) $ property False
            Right (_ :: HeadParameters) ->
              property True

  describe "ExternalPAB" $ do
    it "publishes init tx using wallet 1 and observes it also" $ do
      pendingWith "currently failing"
      failAfter 40 $
        withHydraPab $ do
          calledBack1 <- newEmptyMVar
          calledBack2 <- newEmptyMVar

          -- NOTE(SN): The cardano pubkeys and which wallet is used, is
          -- hard-coded in 'withExternalPAB'!
          withExternalPab @SimpleTx 1 nullTracer (putMVar calledBack1) $ \_ ->
            withExternalPab 2 nullTracer (putMVar calledBack2) $ \Chain{postTx} -> do
              let parameters = HeadParameters 100 [alice, bob, carol]
              postTx $ InitTx @SimpleTx parameters
              takeMVar calledBack1 `shouldReturn` InitTx parameters
              takeMVar calledBack2 `shouldReturn` InitTx parameters

    it "publishes init tx, observes it and abort" $ do
      pendingWith "currently failing"
      failAfter 40 $
        withHydraPab $ do
          calledBack1 <- newEmptyMVar
          calledBack2 <- newEmptyMVar
          -- NOTE(SN): The cardano pubkeys and which wallet is used, is
          -- hard-coded in 'withExternalPab'!
          withExternalPab @SimpleTx 1 nullTracer (putMVar calledBack1) $ \client1 ->
            withExternalPab 2 nullTracer (putMVar calledBack2) $ \client2 -> do
              let parameters = HeadParameters 100 [alice, bob, carol]
              postTx client1 $ InitTx @SimpleTx parameters
              takeMVar calledBack1 `shouldReturn` InitTx parameters
              takeMVar calledBack2 `shouldReturn` InitTx parameters
              postTx client2 $ AbortTx @SimpleTx mempty
              takeMVar calledBack1 `shouldReturn` AbortTx @SimpleTx mempty

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

-- TODO(SN): This is not printing the full stdout on failure
withHydraPab :: IO a -> IO a
withHydraPab action =
  withSystemTempFile "hydra-pab" $ \fn h -> do
    withCreateProcess (pab h) $ \_ _ _ _ ->
      action `onException` printStdout fn
 where
  pab h =
    (proc "hydra-pab" [])
      { std_in = CreatePipe -- Open a stdin as pab tries to read from it
      , std_out = UseHandle h -- Gets closed by withCreateProcess
      }

  printStdout fn = do
    putTextLn "This was the stdout of hydra-pab:"
    readFileText fn >>= putText
