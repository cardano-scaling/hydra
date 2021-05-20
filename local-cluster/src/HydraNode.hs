{-# LANGUAGE TypeApplications #-}

module HydraNode where

import Cardano.Prelude
import System.Process (
  CreateProcess (..),
  StdStream (..),
  proc,
  withCreateProcess,
 )

data HydraNode = HydraNode {hydraNodeId :: Int, inputStream :: Maybe Handle}

data Request
  = Init [Int]
  deriving (Eq, Show)

data Response
  = ReadyToCommit
  deriving (Eq, Show)

sendRequest :: HydraNode -> Request -> IO ()
sendRequest HydraNode{hydraNodeId, inputStream} request =
  case inputStream of
    Just input ->
      hPutStrLn input (show @_ @Text request)
    Nothing ->
      panic $ "Can't connect to node: " <> show hydraNodeId <> ", request: " <> show request

withHydraNode :: Int -> (HydraNode -> IO ()) -> IO ()
withHydraNode hydraId action = do
  let process = hydraNodeProcess hydraId
  withCreateProcess process $
    \stdin_ _stdout _stderr _ ->
      action (HydraNode hydraId stdin_)

hydraNodeProcess :: Int -> CreateProcess
hydraNodeProcess nodeId =
  (proc "hydra-node" ["--node-id", show nodeId])
    { std_in = CreatePipe
    }
