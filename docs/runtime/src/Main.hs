{-# LANGUAGE CPP #-}

module Main where

import Prelude

#if defined(javascript_HOST_ARCH)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.JS.Foreign.Callback (Callback, syncCallback1')
import GHC.JS.Prim (JSVal, fromJSString, toJSString)
import Playground.Runtime (newEvaluator)

foreign import javascript "((f) => { globalThis.hydraCremEvaluate = f; if (globalThis.hydraCremReady) globalThis.hydraCremReady(); })"
  publish :: Callback (JSVal -> IO JSVal) -> IO ()

main :: IO ()
main = do
  evaluate <- newEvaluator
  publish =<< syncCallback1' (\input -> do
    let request = LBS.fromStrict $ Text.encodeUtf8 $ Text.pack $ fromJSString input
    result <- evaluate request
    pure $ toJSString $ Text.unpack $ Text.decodeUtf8 $ LBS.toStrict result)
#else
import Data.ByteString.Lazy.Char8 qualified as LBS
import Playground.Runtime (evaluateJSON)

main :: IO ()
main = LBS.interact $ LBS.unlines . map evaluateJSON . LBS.lines
#endif
