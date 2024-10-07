-- withCreateProcess interface is annoying
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Hydra.Cluster.Observations where

import Hydra.Prelude

import Test.Hydra.Prelude

import CardanoClient (RunningNode (..))
import Control.Concurrent.Class.MonadSTM (modifyTVar', newTVarIO, readTVarIO)
import Control.Lens ((^?))
import Data.Aeson as Aeson
import Data.Aeson.Lens (key)
import Data.ByteString (hGetLine)
import Data.Text qualified as T
import Hydra.Cardano.Api (ChainPoint (..), SlotNo (..), serialiseToRawBytesHexText)
import Hydra.Options qualified as Options
import Hydra.Tx (HeadId (..))
import System.IO.Error (isEOFError, isIllegalOperation)
import System.Process (CreateProcess (std_out), StdStream (..), proc, withCreateProcess)

runCheckObservations ::
  RunningNode ->
  ChainPoint ->
  HeadId ->
  IO ()
runCheckObservations cardanoNode chainPoint headId = do
  withChainObserver cardanoNode (Just chainPoint) $ \observer -> do
    chainObserverSees observer "HeadInitTx" headId
    chainObserverSees observer "HeadCommitTx" headId
    chainObserverSees observer "HeadCollectComTx" headId

chainObserverSees :: HasCallStack => ChainObserverHandle -> Value -> HeadId -> IO ()
chainObserverSees observer txType headId =
  awaitMatch observer 5 $ \v -> do
    guard $ v ^? key "message" . key "tag" == Just txType
    guard $ v ^? key "message" . key "headId" == Just (toJSON headId)

awaitMatch :: HasCallStack => ChainObserverHandle -> DiffTime -> (Aeson.Value -> Maybe a) -> IO a
awaitMatch chainObserverHandle delay f = do
  seenMsgs <- newTVarIO []
  timeout delay (go seenMsgs) >>= \case
    Just x -> pure x
    Nothing -> do
      msgs <- readTVarIO seenMsgs
      failure $
        toString $
          unlines
            [ "awaitMatch did not match a message within " <> show delay
            , padRight ' ' 20 "  seen messages:"
                <> unlines (align 20 (decodeUtf8 . Aeson.encode <$> msgs))
            ]
 where
  go seenMsgs = do
    msg <- awaitNext chainObserverHandle
    atomically (modifyTVar' seenMsgs (msg :))
    maybe (go seenMsgs) pure (f msg)

  align _ [] = []
  align n (h : q) = h : fmap (T.replicate n " " <>) q

newtype ChainObserverHandle = ChainObserverHandle {awaitNext :: IO Value}

-- | Starts a 'hydra-chain-observer' on some Cardano network.
withChainObserver :: RunningNode -> Maybe ChainPoint -> (ChainObserverHandle -> IO ()) -> IO ()
withChainObserver cardanoNode startChainFrom action =
  withCreateProcess process{std_out = CreatePipe} $ \_in (Just out) _err _ph ->
    action
      ChainObserverHandle
        { awaitNext = awaitNext out
        }
 where
  awaitNext :: Handle -> IO Aeson.Value
  awaitNext out = do
    x <- try (hGetLine out)
    case x of
      Left e | isEOFError e || isIllegalOperation e -> do
        threadDelay 1
        awaitNext out
      Left e -> failure $ "awaitNext failed with exception " <> show e
      Right d -> do
        case Aeson.eitherDecode (fromStrict d) of
          Left _err -> do
            putBSLn $ "awaitNext failed to decode msg: " <> d
            threadDelay 1
            awaitNext out
          Right value -> pure value

  process =
    proc
      "hydra-chain-observer"
      $ ["direct"]
        <> Options.toArgNodeSocket nodeSocket
        <> Options.toArgNetworkId networkId
        <> toArgStartChainFrom startChainFrom

  RunningNode{nodeSocket, networkId} = cardanoNode

-- TODO! DRY
toArgStartChainFrom :: Maybe ChainPoint -> [String]
toArgStartChainFrom = \case
  Just ChainPointAtGenesis ->
    ["--start-chain-from", "0"]
  Just (ChainPoint (SlotNo slotNo) headerHash) ->
    let headerHashBase16 = toString (serialiseToRawBytesHexText headerHash)
     in ["--start-chain-from", show slotNo <> "." <> headerHashBase16]
  Nothing ->
    []
