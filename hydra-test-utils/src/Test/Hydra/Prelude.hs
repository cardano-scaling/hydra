{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Hydra.Prelude (
  createSystemTempDirectory,
  failure,
  location,
  failAfter,
  dualFormatter,
  reasonablySized,
  ReasonablySized (..),
  genericCoverTable,
  GUniformCoverage,
  GCoverTableRequirements,
  CoverTableDistribution (..),

  -- * HSpec re-exports
  module Test.Hspec,
  module Test.Hspec.QuickCheck,
  withTempDir,
  withFile',
  checkProcessHasNotDied,
) where

import Hydra.Prelude
import Test.Hspec
import Test.Hspec.QuickCheck

import Control.Monad.Class.MonadTimer (timeout)
import Data.Ratio ((%))
import GHC.Exception (SrcLoc (..))
import GHC.Generics
import System.Directory (removePathForcibly)
import System.Exit (ExitCode (..))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Process (ProcessHandle, waitForProcess)
import Test.HSpec.JUnit (junitFormat)
import Test.HUnit.Lang (FailureReason (Reason), HUnitFailure (HUnitFailure))
import Test.Hspec.Core.Format (Format, FormatConfig (..))
import Test.Hspec.Core.Formatters (formatterToFormat, specdoc)
import Test.QuickCheck (Property, Testable, coverTable, scale, tabulate)
import qualified Prelude

-- | Create a unique temporary directory.
createSystemTempDirectory :: String -> IO FilePath
createSystemTempDirectory template =
  getCanonicalTemporaryDirectory >>= \tmpDir ->
    createTempDirectory tmpDir template

-- | Create a temporary directory for the given 'action' to use.
-- The directory is removed if and only if the action completes successfuly.
withTempDir :: String -> (FilePath -> IO r) -> IO r
withTempDir baseName action = do
  tmpDir <- createSystemTempDirectory baseName
  res <- action tmpDir
  removePathForcibly tmpDir
  pure res

-- | Print a message with filepath to @stderr@ on exceptions.
withFile' :: FilePath -> (Handle -> IO a) -> IO a
withFile' filepath io =
  withFile filepath ReadWriteMode io
    `onException` putStrLn ("Logfile written to: " <> filepath)

-- | Fails a test with given error message.
-- This function improves over existing 'expectationFailure' by throwing a
-- 'HUnitFailure' exception containig the location of the error and providing
-- better callstack context.
failure :: (HasCallStack, MonadThrow m) => String -> m a
failure msg =
  throwIO (HUnitFailure location $ Reason msg)

-- | Fail some monadic action if it does not complete within given timeout.
-- A 'DiffTime' can be represented as a decimal number of seconds.
failAfter :: (HasCallStack, MonadTimer m, MonadThrow m) => DiffTime -> m a -> m a
failAfter seconds action =
  timeout seconds action >>= \case
    Nothing -> failure $ "Test timed out after " <> show seconds <> " seconds"
    Just a -> pure a

-- | Provides the source code location where this function is called.
-- This relies on the <https://hackage.haskell.org/package/base-4.15.0.0/docs/GHC-Exception.html#t:CallStack CallStack>
-- information provided by GHC and to be useful requires all functions to be properly
-- annotated.
location :: HasCallStack => Maybe SrcLoc
location = case reverse $ getCallStack callStack of
  (_, loc) : _ -> Just loc
  _ -> Nothing

-- | An HSpec test formatter that outputs __both__ a JUnit formatted file and stdout test results.
dualFormatter ::
  -- | The name of the test suite run, for reporting purpose.
  String ->
  -- | Configuration, will be passed by the HSpec test runner.
  FormatConfig ->
  IO Format
dualFormatter suiteName config = do
  junit <- junitFormat "test-results.xml" suiteName config
  docSpec <- formatterToFormat specdoc config
  pure $ \e -> junit e >> docSpec e

-- | Wait for process termination and do 'failure' on non-zero exit code.
-- This function is useful for end-to-end testing of external processes esp. in
-- conjunction with 'race' combinator:
--
-- @@
-- withCreateProcess p $
--   \_stdin _stdout _stderr processHandle -> do
--       race_
--         (checkProcessHasNotDied "my-process" processHandle)
--         doStuff
-- @@
checkProcessHasNotDied :: Text -> ProcessHandle -> IO ()
checkProcessHasNotDied name processHandle =
  waitForProcess processHandle >>= \case
    ExitSuccess -> pure ()
    ExitFailure exit -> failure $ "Process " <> show name <> " exited with failure code: " <> show exit

-- | Resize a generator to grow with the size parameter, but remains reasonably
-- sized. That is handy when testing on data-structures that can be arbitrarily
-- large and, when large entities don't really bring any value to the test
-- itself.
--
-- It uses a square root function which makes the size parameter grows
-- quadratically slower than normal. That is,
--
--     +-------------+------------------+
--     | Normal Size | Reasonable Size  |
--     | ----------- + ---------------- +
--     | 0           | 0                |
--     | 1           | 1                |
--     | 10          | 3                |
--     | 100         | 10               |
--     | 1000        | 31               |
--     +-------------+------------------+
reasonablySized :: Gen a -> Gen a
reasonablySized = scale (ceiling . sqrt @Double . fromIntegral)

-- | A QuickCheck modifier to make use of `reasonablySized` on existing types.
newtype ReasonablySized a = ReasonablySized a
  deriving newtype (Show)

instance Arbitrary a => Arbitrary (ReasonablySized a) where
  arbitrary = ReasonablySized <$> reasonablySized arbitrary

-- Like 'coverTable', but construct the weight requirements generically from the
-- label type-representation.
--
--     data MyCoverLabel = Case1 | Case2 deriving (Generic, Data, Show)
--
--     forAll arbitrary $ \(lbl :: MyCoverLabel, arg) ->
--         myProp arg
--         & genericCoverTable @UniformCoverage [lbl]
--         & checkCoverage
--
--     ===
--
--     forAll arbitrary $ (lbl :: MyCoverLabel, arg) ->
--        myProp arg
--        & coverTable "MyCoverTable" [(Case1, 50), (Case2, 50)]
--        & tabulate "MyCoverTable" [show lbl]
--        & checkCoverage
--
genericCoverTable ::
  forall distr a prop.
  ( GCoverTableRequirements distr (Rep a)
  , GLabelName (Rep a)
  , Generic a
  , Show a
  , Testable prop
  ) =>
  [a] ->
  prop ->
  Property
genericCoverTable xs =
  coverTable tableName requirements . tabulate tableName (gLabelName . from <$> xs)
 where
  (tableName, requirements) = gCoverTableRequirements @distr (Proxy @(Rep a))

-- | A Constraint alias to ease signatures on the consumer-side.
type GUniformCoverage a =
  ( GCoverTableRequirements 'UniformCoverage (Rep a)
  , GLabelName (Rep a)
  , Generic a
  )

-- | Type of distribution requirements.
data CoverTableDistribution
  = -- | Equal chance for all branches.
    UniformCoverage

-- | A type-class for generically creating a QuickCheck's 'coverTable'
-- weight requirement. It is parameterized by the type of distribution wanted.
--
-- Any basic sum of constructors is an instance of `GCoverTableRequirements`.
class GCoverTableRequirements (k :: CoverTableDistribution) (f :: Type -> Type) where
  gCoverTableRequirements :: Proxy f -> (String, [(String, Double)])

-- | A simple class to get a label name from a generic representation. Instances
-- exist for sum-types and uses the constructor's name.
class GLabelName (f :: Type -> Type) where
  gLabelName :: f a -> String

instance
  ( GCoverTableRequirements 'UniformCoverage f
  , Datatype c
  ) =>
  GCoverTableRequirements 'UniformCoverage (D1 c f)
  where
  gCoverTableRequirements _ =
    -- NOTE: A careful reader may notice the `3 * lengthI cs` instead of
    -- `lengthI cs` as the weights denominator.
    --
    -- This is because `checkCoverage` in QuickCheck will do a statistical
    -- verification with a quite high level of confidence. For types with
    -- many (i.e. more than 2) constructors, it may require a quite significant
    -- amount of tests cases before all values statistically converge towards
    -- the expected weights. In practice, we really want to make sure that
    -- all branches are covered. That a branch is covered 2 times as much
    -- than another isn't of significant importance.
    let lengthI = toInteger . length
        cs = snd (gCoverTableRequirements @ 'UniformCoverage (Proxy @f))
     in ( datatypeName (Prelude.undefined :: D1 c f a)
        , [(c, fromRational (100 % (3 * lengthI cs))) | (c, _) <- cs]
        )

instance (GLabelName f) => GLabelName (D1 c f) where
  gLabelName = gLabelName . unM1

instance
  ( GCoverTableRequirements k l
  , GCoverTableRequirements k r
  ) =>
  GCoverTableRequirements k (l :+: r)
  where
  gCoverTableRequirements _ =
    ( ""
    , mconcat
        [ snd (gCoverTableRequirements @k (Proxy @l))
        , snd (gCoverTableRequirements @k (Proxy @r))
        ]
    )

instance (GLabelName l, GLabelName r) => GLabelName (l :+: r) where
  gLabelName = \case
    L1 l -> gLabelName l
    R1 r -> gLabelName r

instance (Constructor c) => GCoverTableRequirements k (C1 c f) where
  gCoverTableRequirements _ =
    ("", [(conName (Prelude.undefined :: C1 c f a), 0)])

instance (Constructor c) => GLabelName (C1 c f) where
  gLabelName = conName
