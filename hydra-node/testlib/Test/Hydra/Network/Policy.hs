{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Network.Policy where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Text qualified as Text
import Hydra.Network.Policy (PolicyFingerprint (..))
import Test.QuickCheck (elements, vectorOf)

instance Arbitrary PolicyFingerprint where
  arbitrary =
    PolicyFingerprint . Text.pack
      <$> vectorOf 64 (elements "0123456789abcdef")
