{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-dodgy-imports #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
-- 'fdefer-type-errors' makes GHC's usage analysis miss references that
-- live inside ill-typed expressions, so suppress the false-positive
-- "unused import" / "import instances only" warnings.
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

-- | Compile-fail checks for 'Hydra.Tx.Secret' and the @SigningKey HydraKey@
-- wrapper in 'Hydra.Tx.Crypto'. Each spec passes a deliberately ill-typed
-- expression to 'shouldFailToTypecheck'; GHC's '-fdefer-type-errors' lifts
-- the would-be compile error into a runtime exception, which we catch.
--
-- We use a local helper (not the 'should-not-typecheck' package) because
-- our 'Forbid'-bearing instances throw 'Control.Exception.TypeError'
-- explicitly from their unreachable bodies. The helper catches both
-- 'TypeError' (raw "no instance" cases under deferral) and any other
-- exception thrown from those bodies.
--
-- This module is isolated so the defer flag stays scoped to expressions
-- that intentionally fail to type-check. Do not add positive tests here.
module Hydra.Tx.SecretNegativeSpec where

import Hydra.Prelude

import Cardano.Binary (ToCBOR (toCBOR))
import Codec.Serialise (serialise)
import Control.DeepSeq (NFData, force)
import Control.Exception qualified as E
import Data.Aeson (encode, toJSON)
import Hydra.Tx.Crypto (HydraKey, SigningKey, generateSigningKey)
import Hydra.Tx.Secret (Secret, mkSecret, withSecret)
import Test.Hydra.Prelude

-- | Assert that evaluating an expression raises an exception. Used to
-- detect both deferred 'TypeError's (from '-fdefer-type-errors') and the
-- explicit throws inside 'Forbid'-bearing instance bodies in
-- 'Hydra.Tx.Secret'.
shouldFailToTypecheck :: NFData a => a -> Expectation
shouldFailToTypecheck a = do
  result <- E.try @E.SomeException (E.evaluate (force a))
  case result of
    Left _ -> pure ()
    Right _ ->
      expectationFailure "Expected the expression to throw (deferred TypeError or Forbid-instance body), but it returned normally."

spec :: Spec
spec = describe "Secret (compile-fail checks)" $ do
  it "Secret refuses ToJSON" $
    shouldFailToTypecheck (toJSON (mkSecret (42 :: Int)))

  it "Secret refuses aeson 'encode'" $
    shouldFailToTypecheck (encode (mkSecret (42 :: Int)))

  it "Secret refuses ToCBOR" $
    shouldFailToTypecheck (toCBOR (mkSecret (42 :: Int)))

  it "Secret refuses Codec.Serialise" $
    shouldFailToTypecheck (serialise (mkSecret (42 :: Int)))

  it "Raw SigningKey HydraKey has no Show instance" $
    shouldFailToTypecheck (withSecret (generateSigningKey "alice" :: Secret (SigningKey HydraKey)) show)

  it "Raw SigningKey HydraKey refuses ToJSON" $
    shouldFailToTypecheck (withSecret (generateSigningKey "alice" :: Secret (SigningKey HydraKey)) toJSON)

  it "Raw SigningKey HydraKey refuses ToCBOR" $
    shouldFailToTypecheck (withSecret (generateSigningKey "alice" :: Secret (SigningKey HydraKey)) toCBOR)
