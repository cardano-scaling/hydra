module HydraNode where

import Cardano.Prelude
import System.Process (CreateProcess, withCreateProcess)

data HydraNode = HydraNode {hydraNodeId :: Int}

data Request
  = Init [Int]
  deriving (Eq, Show)

data Response
  = ReadyToCommit
  deriving (Eq, Show)

sendRequest :: HydraNode -> Request -> IO ()
sendRequest = panic "not implemented"

withHydraNode :: Int -> (HydraNode -> IO ()) -> IO ()
withHydraNode hydraId action = do
  let process = hydraNodeProcess hydraId
  withCreateProcess process $
    \_stdin _stdout _stderr _ ->
      action (HydraNode hydraId)

hydraNodeProcess :: Int -> CreateProcess
hydraNodeProcess = panic "not implemented"
