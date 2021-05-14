{-# LANGUAGE OverloadedStrings #-}

import Cardano.Prelude
import Data.String
import Data.Text (pack)
import qualified Data.Text.Encoding as Enc
import System.IO (hFlush)
import System.ZMQ4.Monadic

main :: IO ()
main = do
  args <- getArgs
  case args of
    "prompt" : moreArgs -> prompt moreArgs
    "display" : moreArgs -> display moreArgs
    _ -> hPutStrLn stderr $ Data.String.unlines [promptUsage, displayUsage]

promptUsage :: String
promptUsage = "usage: prompt <address> <username>"

prompt :: [String] -> IO ()
prompt [addr, username] = do
  let name = username <> ": "
  runZMQ $ do
    pub <- socket Pub
    bind pub addr
    forever $ do
      line <- liftIO getLine
      send pub [] (Enc.encodeUtf8 $ pack name <> line)
prompt _ = do
  hPutStrLn stderr promptUsage
  exitFailure

displayUsage :: String
displayUsage = "usage: display <address> [<address>, ...]"

display :: [String] -> IO ()
display args = do
  when (null args) $ do
    hPutStrLn stderr displayUsage
    exitFailure
  runZMQ $ do
    sub <- socket Sub
    subscribe sub ""
    mapM_ (connect sub) args
    forever $ do
      receive sub >>= putStrLn . Enc.decodeUtf8
      liftIO $ hFlush stdout
