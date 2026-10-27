module Hydra.LoggingSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Exception (AsyncException (StackOverflow), IOException, throw)
import Control.Tracer.JSON (defaultLogBuffering, defaultQueueSize, traceWith, withTracerOutputTo)
import Data.Aeson (object, (.=))
import Data.Text.IO qualified as Text.IO
import System.FilePath ((</>))
import System.IO (hClose)
import System.Process (createPipe)

spec :: Spec
spec = do
  it "dumps logs to the given handle in JSON" $ do
    -- Write to a file handle instead of capturing the process-global stdout:
    -- capturing swaps the stdout file descriptor under every concurrently
    -- running test and raced the tracer's own shutdown flush.
    withTempDir "logging-spec" $ \dir -> do
      let logFile = dir </> "log.jsonl"
      withFile logFile WriteMode $ \h ->
        withTracerOutputTo LineBuffering h "test" $ \tracer ->
          traceWith tracer (object ["foo" .= (42 :: Int)])
      captured <- readFileBS logFile
      toString (decodeUtf8 @Text captured) `shouldContain` "{\"foo\":42}"

  -- A pipe is what a log reader actually gets, and unlike a file it cannot be
  -- satisfied after the fact by the flush on tracer shutdown.
  it "flushes entries without waiting for the buffer to fill" $
    withPipe $ \(readEnd, writeEnd) ->
      withTracerOutputTo defaultLogBuffering writeEnd "test" $ \tracer -> do
        traceWith tracer (object ["foo" .= (42 :: Int)])
        -- The writer thread is asynchronous, so this blocks until it has both
        -- written and flushed the entry.
        line <- failAfter 5 $ Text.IO.hGetLine readEnd
        toString line `shouldContain` "{\"foo\":42}"

  it "keeps logging after the reader of its output has gone away" $
    withPipe $ \(readEnd, writeEnd) ->
      withTracerOutputTo defaultLogBuffering writeEnd "test" $ \tracer -> do
        hClose readEnd
        -- Writing to a pipe nobody reads raises an IOException, as GHC ignores
        -- SIGPIPE. The writer has to survive that: were it to die, the queue
        -- would fill and every subsequent 'traceWith' would block forever.
        failAfter 5 $
          forM_ [1 .. 2 * fromIntegral defaultQueueSize :: Int] $ \i ->
            traceWith tracer (object ["foo" .= i])

  -- A 'ToJSON' reachable from a traced type can be
  -- partial: the node traces client inputs, and a side-loaded snapshot's
  -- accumulator hash is a lazy thunk that calls 'error' above the trusted
  -- setup's capacity. The encode runs here, on the writer thread, not at the
  -- 'traceWith' call site -- and this thread is unlinked, so its death is
  -- invisible until the queue fills and the whole process wedges.
  it "keeps logging when a message's ToJSON throws" $
    withPipe $ \(readEnd, writeEnd) ->
      withTracerOutputTo defaultLogBuffering writeEnd "test" $ \tracer -> do
        forM_ [Unencodable, Overflowing, Undescribable] $ \poison -> do
          traceWith tracer poison
          -- Loudly, not silently: the offending entry is replaced by a
          -- diagnostic one rather than dropped.
          line <- failAfter 5 $ Text.IO.hGetLine readEnd
          toString line `shouldContain` "UnencodableLogEntry"
        -- And the writer is still draining, so nothing blocks.
        failAfter 5 $
          forM_ [1 .. 2 * fromIntegral defaultQueueSize :: Int] $ \i -> do
            traceWith tracer (Encodable i)
            void $ Text.IO.hGetLine readEnd
 where
  withPipe :: ((Handle, Handle) -> IO a) -> IO a
  withPipe = bracket createPipe $ \(readEnd, writeEnd) ->
    forM_ [readEnd, writeEnd] $ \h ->
      hClose h `catch` \(_ :: IOException) -> pure ()

-- | A traced message whose 'ToJSON' is partial, standing in for the node's real
-- ones (an 'Input' carrying a snapshot whose accumulator cannot be committed
-- to).
data Loggable = Unencodable | Overflowing | Undescribable | Encodable Int

instance ToJSON Loggable where
  toJSON = \case
    Unencodable -> error "ToJSON Loggable: deliberately partial"
    -- 'StackOverflow' is an 'AsyncException' but nobody cancelled us: it comes
    -- out of the encoding work itself, so it has to be survived like any other
    -- synchronous failure rather than rethrown.
    Overflowing -> throw StackOverflow
    -- Fails twice: encoding the entry throws, and describing /that/ exception
    -- for the diagnostic entry throws as well. The writer has to survive both.
    Undescribable -> throw Undescribed
    Encodable i -> object ["foo" .= i]

-- | An exception that cannot be rendered, so the diagnostic entry substituted
-- for it cannot be encoded either. 'unencodable' reaches this through
-- 'SomeException''s own 'displayException', which delegates to the wrapped
-- exception's.
data Undescribed = Undescribed
  deriving stock (Show)

instance Exception Undescribed where
  displayException _ = error "displayException Undescribed: deliberately partial"
