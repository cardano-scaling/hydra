module Hydra.LoggingSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Hydra.Logging (LogFormat (..), traceWith, withTracerOutputTo)
import Hydra.Logging.PrettyError (PrettyError (..))
import Hydra.Logging.PrettyError qualified as Severity
import System.IO.Silently (capture_)

-- A toy log type used to exercise the 'PrettyError' class without depending
-- on any production log type. Severity dispatches off the type; 'Boom' is
-- the only constructor that opts into a hand-rolled 'showPretty'.
data Frob = Ok | Wobble | Boom
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance PrettyError Frob where
  severity = \case
    Boom -> Severity.Error
    Wobble -> Severity.Warning
    Ok -> Severity.Info
  showPretty = \case
    Boom -> ["  detail=oops", "  count=3"]
    Wobble -> ["  reason=maybe"]
    Ok -> []

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

  it "picks bold-red for typed Severity.Error and a normal colour for Info" $ do
    captured <- capture_ $ do
      withTracerOutputTo LogPretty LineBuffering stdout "test-pretty" $ \tracer -> do
        traceWith tracer Boom
        traceWith tracer Wobble
        traceWith tracer Ok

    liftIO $ threadDelay 5

    -- Boom is Error → bold-red path + an explicit [error] tag in front
    captured `shouldContain` "\ESC[1;31m"
    captured `shouldContain` "[error]"
    -- Wobble is Warning → yellow path + an explicit [warning] tag
    captured `shouldContain` "\ESC[33m"
    captured `shouldContain` "[warning]"
    -- Ok is Info → the catch-all white colour and an [info] tag
    captured `shouldContain` "\ESC[37m"
    captured `shouldContain` "[info]"

  it "uses showPretty lines verbatim in place of the generic JSON-flatten" $ do
    captured <- capture_ $ do
      withTracerOutputTo LogPretty LineBuffering stdout "test-pretty" $ \tracer -> do
        traceWith tracer Boom

    liftIO $ threadDelay 5

    -- the hand-rolled lines appear verbatim under the path
    captured `shouldContain` "detail=oops"
    captured `shouldContain` "count=3"

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
