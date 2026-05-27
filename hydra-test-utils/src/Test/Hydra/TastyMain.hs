-- | Shared test driver for Hydra test-suites. Each package's @test/Main.hs@
-- builds a list of 'TestTree's and hands them to 'defaultMainHydra', which
-- wraps everything in the project-wide options.
--
-- For Hspec specs use the re-exported 'testSpec'; for native tasty test trees
-- use 'pure tree'.
--
-- Use 'hydraTestTree' if you need to wrap the tree in extra
-- 'localOption' / 'adjustOption' layers (e.g. forcing 'NumThreads 1' for an
-- integration suite) before handing it to 'Test.Tasty.defaultMain'.
module Test.Hydra.TastyMain (
  defaultMainHydra,
  runHydraTests,
  hydraTestTree,
  hydraIngredients,
  testSpec,
) where

import Hydra.Prelude

import System.Environment qualified as Env
import Test.Tasty (TestName, TestTree, defaultMainWithIngredients, localOption, testGroup)
import Test.Tasty.Hspec (TreatPendingAs (TreatPendingAsSuccess), testSpec)
import Test.Tasty.Ingredients (Ingredient, composeReporters)
import Test.Tasty.Ingredients.Rerun (rerunningTests)
import Test.Tasty.Runners (consoleTestReporter, listingTests)
import Test.Tasty.Runners.AntXML (antXMLRunner)
import Test.Tasty.Runners.Html (htmlRunner)

-- | Resolve a list of @IO TestTree@ into one Hydra-style 'TestTree' wrapped in
-- the project-wide options. Suitable for further wrapping with 'localOption'
-- before handing to 'Test.Tasty.defaultMain'.
hydraTestTree :: TestName -> [IO TestTree] -> IO TestTree
hydraTestTree groupName trees = do
  resolved <- sequence trees
  pure $ localOption TreatPendingAsSuccess (testGroup groupName resolved)

-- | The tasty ingredients used by every Hydra test-suite. Bundles
-- @tasty-rerun@ (pass @--rerun@ to re-run only the previously-failed
-- tests), @tasty-html@ (pass @--html=PATH@ to also write an HTML
-- report) and @tasty-ant-xml@ (pass @--xml=PATH@ to write a JUnit-style
-- XML report, which CI consumes to surface per-test failures in the
-- GitHub UI) on top of the default console runner.
--
-- The XML / HTML / console reporters are composed with 'composeReporters'
-- so they all run together: passing @--xml=…@ or @--html=…@ still prints
-- the per-test pass\/fail list to stdout, rather than suppressing it.
hydraIngredients :: [Ingredient]
hydraIngredients =
  [ rerunningTests
      [ listingTests
      , antXMLRunner `composeReporters` htmlRunner `composeReporters` consoleTestReporter
      ]
  ]

-- | Run a tasty 'TestTree' with 'hydraIngredients'. Before tasty parses the
-- command line, any literal @{suite}@ in argv is replaced with the given
-- suite name. This lets a single @cabal test all
-- --test-options=\"--html=test-reports\/{suite}.html\"@ write one report
-- per package (e.g. @test-reports\/hydra-node.html@,
-- @test-reports\/hydra-tx.html@, ...) instead of every suite overwriting
-- the same file. Args without @{suite}@ pass through unchanged.
runHydraTests :: TestName -> TestTree -> IO ()
runHydraTests suite tree = do
  args <- map (substituteSuite suite) <$> Env.getArgs
  Env.withArgs args $ defaultMainWithIngredients hydraIngredients tree

substituteSuite :: TestName -> String -> String
substituteSuite suite = go
 where
  placeholder = "{suite}"
  plen = length placeholder
  go [] = []
  go xs@(c : cs)
    | placeholder `isPrefixOf` xs = suite <> go (drop plen xs)
    | otherwise = c : go cs

-- | Build a tasty test runner from a list of @IO TestTree@. Pending hspec
-- tests (e.g. those guarded by 'onlyNightly' / 'onlyLocal') are treated as
-- successes by default to match the project's hspec-era behaviour.
--
-- Substitutes the @{suite}@ placeholder in argv (see 'runHydraTests') and
-- runs with 'hydraIngredients'.
defaultMainHydra :: TestName -> [IO TestTree] -> IO ()
defaultMainHydra groupName trees =
  hydraTestTree groupName trees >>= runHydraTests groupName
