module Hydra.TUISpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Monad.Class.MonadTimer (timeout)
import Graphics.Vty (Config (outputFd), Event, Vty (shutdown, update), defaultConfig, mkVty)
import Hydra.Network (Host (..))
import Hydra.TUI (runWithVty)
import Hydra.TUI.Options (Options (..))
import System.Posix (closeFd, createFile, stdFileMode)
import System.Posix.Files (removeLink)

spec :: Spec
spec =
  context "end-to-end smoke test" $
    it "starts & renders" $
      withBrickTest $ \BrickTest{buildVty, getPicture} -> do
        race_ (runWithVty buildVty Options{nodeHost = Host{hostname = "127.0.0.1", port = 4001}}) $ do
          threadDelay 1
          putStrLn $ replicate 100 '-'
          getPicture >>= putBSLn
          putStrLn $ replicate 100 '-'

data BrickTest = BrickTest
  { buildVty :: IO Vty
  , getPicture :: IO ByteString
  , sendInputEvent :: Event -> IO ()
  }

withBrickTest :: (BrickTest -> Expectation) -> Expectation
withBrickTest action = do
  action $
    BrickTest
      { buildVty
      , getPicture
      , sendInputEvent
      }
 where
  buildVty = do
    tempFd <- createFile tempFp stdFileMode
    vty <- mkVty defaultConfig{outputFd = Just tempFd}
    pure $
      vty
        { update = onUpdate vty
        , shutdown = onShutdown vty tempFd
        }

  -- TODO(SN): drop screen clearing escape code bytes
  getPicture = readFileBS tempFp

  sendInputEvent = undefined

  -- TODO(SN): not hard-code this
  tempFp = "/tmp/hydra-tui-tests-vtyout"

  onUpdate vty p = do
    putStrLn "update called"
    update vty p

  onShutdown vty tempFd = do
    closeFd tempFd
    removeLink tempFp
    shutdown vty
