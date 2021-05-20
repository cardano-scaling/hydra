{-# LANGUAGE TypeApplications #-}

module HydraNode where

import Cardano.Prelude
import System.Process (
  CreateProcess (..),
  StdStream (..),
  proc,
  withCreateProcess,
 )

data HydraNode = HydraNode
  { hydraNodeId :: Int
  , inputStream :: Handle
  , outputStream :: Handle
  }

data Request
  = Init [Int]
  deriving (Eq, Show, Read)

data Response
  = ReadyToCommit
  deriving (Eq, Show, Read)

sendRequest :: HydraNode -> Request -> IO ()
sendRequest HydraNode{inputStream} request =
  hPutStrLn inputStream (show @_ @Text request)

withHydraNode :: Int -> (HydraNode -> IO ()) -> IO ()
withHydraNode hydraNodeId action = do
  let process = hydraNodeProcess hydraNodeId
  withCreateProcess process $
    \mstdin mstdout _stderr _ ->
      case (mstdin, mstdout) of
        (Just inputStream, Just outputStream) ->
          action (HydraNode{hydraNodeId, inputStream, outputStream})
        _ ->
          throwIO $ CannotStartHydraNode hydraNodeId

data CannotStartHydraNode = CannotStartHydraNode Int deriving (Show)
instance Exception CannotStartHydraNode

hydraNodeProcess :: Int -> CreateProcess
hydraNodeProcess nodeId =
  (proc "hydra-node" ["--node-id", show nodeId])
    { std_in = CreatePipe
    , std_out = CreatePipe
    }
