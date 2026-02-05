module Test.Hydra.Prelude (
  failure,
  HUnitFailure (..),
  location,
  failAfter,
  reasonablySized,
  ReasonablySized (..),
  genericCoverTable,
  pickBlind,
  module Test.Hspec,
  module Test.Hspec.QuickCheck,
  createTempDir,
  withTempDir,
  withLogFile,
  checkProcessHasNotDied,
  exceptionContaining,
  withClearedPATH,
  onlyNightly,
  requiresBlockfrost,
  Gen,
  Arbitrary (..),
  genericArbitrary,
  genericShrink,
  generateWith,
  shrinkListAggressively,
  MinimumSized (..),
  HydraTestnet (..),
  getHydraTestnet,
) where

import Hydra.Prelude
import Test.Hspec
import Test.Hspec.QuickCheck

import Data.List (isInfixOf)
import Data.Ratio ((%))
import Data.Text qualified as Text
import Data.Text.IO (hGetContents)
import Data.Typeable (typeRep)
import GHC.Exception (SrcLoc (..))
import GHC.Generics (Rep)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import Generic.Random qualified as Random
import Generic.Random.Internal.Generic qualified as Random
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.Environment (getEnv, setEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Info (os)
import System.Process (ProcessHandle, waitForProcess)
import Test.HUnit.Lang (FailureReason (Reason), HUnitFailure (HUnitFailure))
import Test.QuickCheck (Arbitrary (..), Property, Testable, coverTable, forAllBlind, genericShrink, scale, tabulate)
import Test.QuickCheck.Arbitrary.ADT (ADTArbitrary (..), ADTArbitrarySingleton (..), ConstructorArbitraryPair (..), ToADTArbitrary (..))
import Test.QuickCheck.Gen (Gen (..))
import Test.QuickCheck.Monadic (PropertyM (MkPropertyM))
import Test.QuickCheck.Random (mkQCGen)

-- | Create a unique directory in the caonical, system-specific temporary path,
-- e.g. in /tmp.
createTempDir :: MonadIO m => String -> m FilePath
createTempDir template = liftIO $ do
  tmpDir <- case os of
    "darwin" -> pure "/tmp" -- https://github.com/cardano-scaling/hydra/issues/158.
    _ -> getCanonicalTemporaryDirectory
  createTempDirectory tmpDir template

-- | Create a temporary directory for the given 'action' to use. The directory
-- is removed if and only if the action completes successfully.
withTempDir :: MonadIO m => String -> (FilePath -> m r) -> m r
withTempDir baseName action = do
  tmpDir <- createTempDir baseName
  res <- action tmpDir
  liftIO $ cleanup 0 tmpDir
  pure res
 where
  -- NOTE: Somehow, since 1.35.0, cleaning-up cardano-node database directory
  -- _sometimes_ generates an empty 'clean' file which prevents the 'db' folder
  -- to be fully removed and triggers an 'UnsatisfiedConstraints' IOException.
  cleanup (maxAttempts :: Word) dir =
    removePathForcibly dir
      `catch` ( \e -> case ioe_type e of
                  UnsatisfiedConstraints ->
                    if maxAttempts < 3 then cleanup (succ maxAttempts) dir else throwIO e
                  _ ->
                    throwIO e
              )

-- | Open given log file non-buffered in append mode and print a message with
-- filepath to @stderr@ on exceptions.
withLogFile :: FilePath -> (Handle -> IO a) -> IO a
withLogFile filepath io = do
  createDirectoryIfMissing True (takeDirectory filepath)
  withFile filepath AppendMode (\out -> hSetBuffering out NoBuffering >> io out)
    `onException` putStrLn ("Logfile written to: " <> filepath)

-- | Fails a test with given error message.
-- This function improves over existing 'expectationFailure' by throwing a
-- 'HUnitFailure' exception containing the location of the error and providing
-- better callstack context.
failure :: (HasCallStack, MonadThrow m) => String -> m a
failure msg =
  throwIO (HUnitFailure location $ Reason msg)

-- | Fail some monadic action if it does not complete within given timeout.
-- A 'NominalDiffTime' can be represented as a decimal number of seconds.
failAfter :: (HasCallStack, MonadTimer m, MonadThrow m) => NominalDiffTime -> m a -> m a
failAfter seconds action =
  timeout (realToFrac seconds) action >>= \case
    Nothing -> failure $ "Test timed out after " <> show seconds
    Just a -> pure a

-- | Provides the source code location where this function is called.
-- This relies on the <https://hackage.haskell.org/package/base-4.15.0.0/docs/GHC-Exception.html#t:CallStack CallStack>
-- information provided by GHC and to be useful requires all functions to be properly
-- annotated.
location :: HasCallStack => Maybe SrcLoc
location = case reverse $ getCallStack callStack of
  (_, loc) : _ -> Just loc
  _ -> Nothing

-- | Wait for process termination and do 'failure' on non-zero exit code.
-- This function is useful for end-to-end testing of external processes esp. in
-- conjunction with 'race' combinator:
--
-- @@
-- withCreateProcess p $
--   \_stdin _stdout mStdErr processHandle -> do
--       race_
--         (checkProcessHasNotDied "my-process" processHandle mStdErr)
--         doStuff
-- @@
-- Note: make sure you do not use an Inherit handle for stderr, as it will NOT work.
checkProcessHasNotDied :: Text -> ProcessHandle -> Maybe Handle -> IO Void
checkProcessHasNotDied name processHandle mStdErr =
  waitForProcess processHandle >>= \case
    ExitSuccess -> failure "Process has stopped"
    ExitFailure exit -> do
      mErrorOutput <- traverse hGetContents mStdErr
      let mErrorMsg = ("Process stderr: " <>) <$> mErrorOutput
      failure . toString $
        unlines
          ( "Process " <> show name <> " exited with failure code: " <> show exit
              : maybeToList mErrorMsg
          )

-- | Like 'coverTable', but construct the weight requirements generically from
-- the provided label.
--
--     data MyCoverLabel = Case1 | Case2 deriving (Enum, Bounded, Show)
--
--     forAll arbitrary $ \a ->
--         myProp a
--         & genericCoverTable [a]
--         & checkCoverage
--
--     ===
--
--     forAll arbitrary $ \a ->
--        myProp a
--        & coverTable "MyCoverTable" [("Case1", 50), ("Case2", 50)]
--        & tabulate "MyCoverTable" [head $ words $ show a]
--        & checkCoverage
--
-- If 'myProp' should take some data, use a product type around 'MyCoverLabel',
-- for example:
--
--     type WithLabel a = (MyCoverLabel, a)
--
--     forAll arbitrary $ \(label, input) ->
--         myProp input
--         & genericCoverTable [label]
--         & checkCoverage
genericCoverTable ::
  forall a prop.
  ( Show a
  , Enum a
  , Bounded a
  , Typeable a
  , Testable prop
  ) =>
  [a] ->
  prop ->
  Property
genericCoverTable xs =
  coverTable tableName requirements . tabulate tableName (show <$> xs)
 where
  tableName = show $ typeRep (Proxy :: Proxy a)
  requirements = [(show lbl, percent) | lbl <- allLabels]
  -- NOTE: We lower the requirement of minimum coverage depending on the number
  -- of labels, e.g. 3 labels would have an ideal, uniform distribution of 33%
  -- each, but we only require 33% / 3 = 11%. With 10 labels evenly distributed
  -- means 10%, but getting 10% / 3 = 3% would be much harder to achieve with
  -- this bigger set to draw from. Hence we only expect 10% / 10 = 1% coverage
  -- in that case and consequently should reasonable numbers of tests required.
  percent = fromRational (100 % (numberOfLabels * numberOfLabels))
  allLabels = enumerate :: [a]
  enumerate = [minBound .. maxBound]
  numberOfLabels = toInteger $ length allLabels

-- | Like 'pick' but using 'forAllBlind' under the hood.
pickBlind :: Monad m => Gen a -> PropertyM m a
pickBlind gen = MkPropertyM $ \k -> do
  a <- gen
  mp <- k a
  pure (forAllBlind (return a) . const <$> mp)

-- | Selector for use with 'shouldThrow' to select exceptions containing some
-- string. Use with TypeApplications, e.g.
--
-- @@
-- exceptionContaining @IOException "foo"
-- @@
exceptionContaining :: Exception e => String -> Selector e
exceptionContaining msg e =
  msg `isInfixOf` displayException e

-- | Clear PATH environment variable while executing given action.
withClearedPATH :: IO () -> IO ()
withClearedPATH act =
  bracket capture (setEnv "PATH") (const act)
 where
  capture = do
    env <- getEnv "PATH"
    setEnv "PATH" ""
    pure env

-- | Only run this task when the CI_NIGHTLY environment variable is set (to
-- anything).
--
-- If you're using this, you want to tag the test with `@nightly` as well;
-- like:
--
--  spec = around_ onlyNightly $ describe "... @nightly" $ do
--    ...
onlyNightly :: IO () -> IO ()
onlyNightly action = do
  lookupEnv "CI_NIGHTLY" >>= \case
    Nothing -> pendingWith "Only runs nightly"
    Just _ -> action

-- | Only run this task when the HYDRA_BACKEND environment variable is set (to
-- anything).
--
-- If you're using this, you want to tag the test with `@requiresBlockfrost` as well;
-- like:
--
--  spec = around_ requiresBlockfrost $ describe "... @requiresBlockfrost" $ do
--    ...
requiresBlockfrost :: IO () -> IO ()
requiresBlockfrost action = do
  getHydraTestnet >>= \case
    BlockfrostTesting -> action
    _ -> pendingWith "Only runs requiresBlockfrost"

data HydraTestnet
  = LocalDevnet
  | PreviewTestnet
  | PreproductionTestnet
  | MainnetTesting
  | BlockfrostTesting

getHydraTestnet :: IO HydraTestnet
getHydraTestnet = do
  backend <- lookupEnv "HYDRA_BACKEND"
  pure $ case backend of
    Nothing -> LocalDevnet
    Just "devnet" -> LocalDevnet
    Just "preview" -> PreviewTestnet
    Just "preproduction" -> PreproductionTestnet
    Just "mainnet" -> MainnetTesting
    Just "blockfrost" -> BlockfrostTesting
    Just other -> error $ "Unknown HYDRA_BACKEND: " <> Text.pack other

-- | Provides a sensible way of automatically deriving generic 'Arbitrary'
-- instances for data-types. In the case where more advanced or tailored
-- generators are needed, custom hand-written generators should be used with
-- functions such as `forAll` or `forAllShrink`.
genericArbitrary ::
  ( Generic a
  , Random.GA Random.UnsizedOpts (Rep a)
  , Random.UniformWeight (Random.Weights_ (Rep a))
  ) =>
  Gen a
genericArbitrary =
  Random.genericArbitrary Random.uniform

-- | A seeded, deterministic, generator
generateWith :: Gen a -> Int -> a
generateWith (MkGen runGen) seed =
  runGen (mkQCGen seed) 30

-- | Like 'shrinkList', but more aggressive :)
--
-- Useful for shrinking large nested Map or Lists where the shrinker "don't have
-- time" to go through many cases.
shrinkListAggressively :: [a] -> [[a]]
shrinkListAggressively = \case
  [] -> []
  xs -> [[], take (length xs `div` 2) xs, drop 1 xs]

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
  deriving newtype (Show, ToJSON, FromJSON)

instance Arbitrary a => Arbitrary (ReasonablySized a) where
  arbitrary = ReasonablySized <$> reasonablySized arbitrary

-- | Reszie gneratator to size = 1.
minimumSized :: Gen a -> Gen a
minimumSized = scale (const 1)

-- | A QuickCheck modifier that only generates values with size = 1.
newtype MinimumSized a = MinimumSized a
  deriving newtype (Show, Eq, ToJSON, FromJSON, Generic)

instance Arbitrary a => Arbitrary (MinimumSized a) where
  arbitrary = MinimumSized <$> minimumSized arbitrary

instance ToADTArbitrary a => ToADTArbitrary (MinimumSized a) where
  toADTArbitrarySingleton _ = do
    adt <- minimumSized $ toADTArbitrarySingleton (Proxy @a)
    let mappedCAP = adtasCAP adt & \cap -> cap{capArbitrary = MinimumSized $ capArbitrary cap}
    pure adt{adtasCAP = mappedCAP}

  toADTArbitrary _ = do
    adt <- minimumSized $ toADTArbitrary (Proxy @a)
    let mappedCAPs = adtCAPs adt <&> \adtPair -> adtPair{capArbitrary = MinimumSized $ capArbitrary adtPair}
    pure adt{adtCAPs = mappedCAPs}
