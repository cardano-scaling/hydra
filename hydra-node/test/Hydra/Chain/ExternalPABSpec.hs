{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.ExternalPABSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import qualified Data.Aeson as Aeson
import Hydra.Chain (Chain (..), HeadParameters (..), OnChainTx (..), PostChainTx (..))
import Hydra.Chain.ExternalPAB (PostInitParams, withExternalPab)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Logging (nullTracer)
import Hydra.PAB (InitParams, ObservedTx)
import Hydra.Party (Party, deriveParty, generateKey)
import System.IO.Temp (withSystemTempFile)
import System.Process (CreateProcess (std_in, std_out), StdStream (CreatePipe, UseHandle), proc, withCreateProcess)
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

    prop "OnChainTx <- Onchain.ObservedTx" $ \(tx :: ObservedTx) ->
      let bytes = Aeson.encode tx
       in counterexample (decodeUtf8 bytes) $ case Aeson.eitherDecode bytes of
            Left e ->
              counterexample ("Failed to decode: " <> show e) $ property False
            Right (_ :: OnChainTx SimpleTx) ->
              property True

  it "publishes init tx using wallet 1 and observes it also" $ do
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
            takeMVar calledBack1 `shouldReturn` OnInitTx 100 [alice, bob, carol]
            takeMVar calledBack2 `shouldReturn` OnInitTx 100 [alice, bob, carol]

  it "publishes init tx, observes it and abort" $ do
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
            takeMVar calledBack1 `shouldReturn` OnInitTx 100 [alice, bob, carol]
            takeMVar calledBack2 `shouldReturn` OnInitTx 100 [alice, bob, carol]
            postTx client2 $ AbortTx @SimpleTx mempty
            takeMVar calledBack1 `shouldReturn` OnAbortTx

  it "can start over the life cycle" $ do
    failAfter 40 $
      withHydraPab $ do
        calledBack1 <- newEmptyMVar
        -- NOTE(SN): The cardano pubkeys and which wallet is used, is
        -- hard-coded in 'withExternalPab'!
        withExternalPab @SimpleTx 1 nullTracer (putMVar calledBack1) $ \client1 -> do
          postTx client1 $ InitTx @SimpleTx $ HeadParameters 100 [alice, bob, carol]
          takeMVar calledBack1 `shouldReturn` OnInitTx 100 [alice, bob, carol]
          postTx client1 $ AbortTx @SimpleTx mempty
          takeMVar calledBack1 `shouldReturn` OnAbortTx
          postTx client1 $ InitTx @SimpleTx $ HeadParameters 100 [alice, bob]
          takeMVar calledBack1 `shouldReturn` OnInitTx 100 [alice, bob]

alice, bob, carol :: Party
alice = deriveParty $ generateKey 10
bob = deriveParty $ generateKey 20
carol = deriveParty $ generateKey 30

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
