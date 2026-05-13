module Hydra.LoggingSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Hydra.Logging (LogFormat (..), traceWith, withTracerOutputTo)
import System.IO.Silently (capture_)

spec :: Spec
spec = do
  it "dumps logs to stdout in JSON with timestamp" $ do
    captured <- capture_ $ do
      withTracerOutputTo LogJSON LineBuffering stdout "test" $ \tracer -> do
        traceWith tracer (object ["foo" .= (42 :: Int)])

    -- This test is flakey in CI. Suspected race condition.
    liftIO $ threadDelay 5

    captured `shouldContain` "{\"foo\":42}"

  it "renders a pretty event with the original-cased tag and an indented kv line" $ do
    captured <- capture_ $ do
      withTracerOutputTo LogPretty LineBuffering stdout "test-pretty" $ \tracer -> do
        traceWith tracer (object ["tag" .= ("FooBar" :: Text), "to" .= ("world" :: Text)])

    liftIO $ threadDelay 5

    captured `shouldContain` "\ESC[" -- ANSI escape
    captured `shouldContain` "FooBar" -- original-cased tag
    captured `shouldContain` "\n  " -- indented field on its own line
    -- key and value sit in different ANSI groups, so they're not contiguous
    captured `shouldContain` "to="
    -- strings render bare, with no surrounding JSON quotes
    captured `shouldContain` "world"
    captured `shouldNotContain` "\"world\""

  it "drills through wrapper layers and prints the dotted, original-cased path" $ do
    captured <- capture_ $ do
      withTracerOutputTo LogPretty LineBuffering stdout "test-pretty" $ \tracer -> do
        traceWith tracer $
          object
            [ "tag" .= ("DirectChain" :: Text)
            , "directChain"
                .= object
                  [ "tag" .= ("PostedTx" :: Text)
                  , "txId" .= ("0a1b2c3d4e5f6789abcdef0123456789" :: Text)
                  ]
            ]

    liftIO $ threadDelay 5

    captured `shouldContain` "DirectChain.PostedTx"
    captured `shouldContain` "txId="
    -- full, untruncated hex value rendered bare (no JSON quoting)
    captured `shouldContain` "0a1b2c3d4e5f6789abcdef0123456789"
    captured `shouldNotContain` "\"0a1b2c3d4e5f6789abcdef0123456789\""

  it "flattens nested objects and drops null fields" $ do
    captured <- capture_ $ do
      withTracerOutputTo LogPretty LineBuffering stdout "test-pretty" $ \tracer -> do
        traceWith tracer $
          object
            [ "tag" .= ("Frob" :: Text)
            , "by" .= object ["vkey" .= ("abcd1234" :: Text)]
            , "skipMe" .= Aeson.Null
            , "kept" .= ("yes" :: Text)
            ]

    liftIO $ threadDelay 5

    -- top-level key with no value on its own line; ANSI dim brackets the key
    captured `shouldContain` "\n  \ESC[2mby="
    -- child indented one level deeper (4 spaces)
    captured `shouldContain` "\n    \ESC[2mvkey="
    captured `shouldContain` "abcd1234"
    captured `shouldNotContain` "\"abcd1234\""
    -- null-valued field is dropped entirely
    captured `shouldNotContain` "skipMe"
    -- sibling string field is preserved
    captured `shouldContain` "kept="
    captured `shouldContain` "yes"
    captured `shouldNotContain` "\"yes\""

  it "colours error-ish tags (Failed/Error/Invalid/Failure) in bold red" $ do
    captured <- capture_ $ do
      withTracerOutputTo LogPretty LineBuffering stdout "test-pretty" $ \tracer -> do
        traceWith tracer $
          object
            [ "tag" .= ("Network" :: Text)
            , "contents"
                .= object
                  [ "tag" .= ("Etcd" :: Text)
                  , "contents"
                      .= object
                        [ "tag" .= ("BroadcastFailed" :: Text)
                        , "reason" .= ("nope" :: Text)
                        ]
                  ]
            ]
        traceWith tracer $
          object
            [ "tag" .= ("APIServer" :: Text)
            , "api" .= object ["tag" .= ("APIServerStarted" :: Text), "listeningPort" .= (4001 :: Int)]
            ]

    liftIO $ threadDelay 5

    -- the error path uses bold-red (1;31)
    captured `shouldContain` "\ESC[1;31m"
    captured `shouldContain` "Network.Etcd.BroadcastFailed"
    -- the non-error path keeps its normal (green) namespace colour
    captured `shouldContain` "\ESC[32m"
    captured `shouldContain` "APIServer.APIServerStarted"

  it "flattens arrays element-by-element with [N] synthetic keys" $ do
    captured <- capture_ $ do
      withTracerOutputTo LogPretty LineBuffering stdout "test-pretty" $ \tracer -> do
        traceWith tracer $
          object
            [ "tag" .= ("Effects" :: Text)
            , "effects"
                .= [ object ["tag" .= ("NetEff" :: Text)]
                   , object ["tag" .= ("CliEff" :: Text)]
                   ]
            ]

    liftIO $ threadDelay 5

    captured `shouldContain` "effects="
    -- each element becomes its own flattened sub-block under [N]=
    captured `shouldContain` "[0]="
    captured `shouldContain` "[1]="
    captured `shouldContain` "tag="
    captured `shouldContain` "NetEff"
    captured `shouldContain` "CliEff"
    -- nothing should be rendered as inline JSON
    captured `shouldNotContain` "[{"
