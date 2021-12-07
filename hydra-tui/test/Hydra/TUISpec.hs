module Hydra.TUISpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Monad.Class.MonadTimer (timeout)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (takeWhileEnd)
import Graphics.Vty (Config (outputFd), Event, Vty (shutdown, update), defaultConfig, mkVty)
import Hydra.Network (Host (..))
import Hydra.TUI (runWithVty)
import Hydra.TUI.Options (Options (..))
import System.IO (SeekMode (AbsoluteSeek))
import System.IO.Temp (withSystemTempFile)
import System.Posix (closeFd, createFile, fdSeek, fdWrite, handleToFd, stdFileMode)
import System.Posix.Files (removeLink)

spec :: Spec
spec =
  context "end-to-end smoke test" $
    it "starts & renders" $
      withBrickTest $ \BrickTest{buildVty, getPicture} -> do
        race_ (runWithVty buildVty Options{nodeHost = Host{hostname = "127.0.0.1", port = 4001}}) $ do
          threadDelay 1
          getPicture >>= putBSLn

data BrickTest = BrickTest
  { buildVty :: IO Vty
  , getPicture :: IO ByteString
  , sendInputEvent :: Event -> IO ()
  }

withBrickTest :: (BrickTest -> Expectation) -> Expectation
withBrickTest action = do
  withSystemTempFile "brick-test" $ \fp h -> do
    fd <- handleToFd h
    action $
      BrickTest
        { buildVty = buildVty fd
        , getPicture = getPicture fp
        , sendInputEvent
        }
 where
  buildVty fd = do
    vty <- mkVty defaultConfig{outputFd = Just fd}
    pure $
      vty
        { update = onUpdate vty fd
        , shutdown = onShutdown vty fd
        }

  getPicture fp = readFileBS fp

  sendInputEvent = undefined

  onUpdate vty _fd p = do
    putStrLn "update called"
    update vty p

  onShutdown vty _fd = do
    shutdown vty
