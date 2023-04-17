{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.BlockHeader where

import Hydra.Cardano.Api.Prelude

import qualified Data.ByteString as BS
import Test.QuickCheck (vectorOf)

unsafeBlockHeaderHashFromBytes :: HasCallStack => ByteString -> Hash BlockHeader
unsafeBlockHeaderHashFromBytes bytes =
  case deserialiseFromRawBytes (proxyToAsType Proxy) bytes of
    Left e ->
      error $
        "unsafeBlockHeaderHashFromBytes: failed on bytes "
          <> show bytes
          <> " with error "
          <> show e
    Right h -> h

-- * Arbitrary values

-- | Fully arbitrary block header with completely random hash.
genBlockHeader :: Gen BlockHeader
genBlockHeader = do
  slotNo <- SlotNo <$> arbitrary
  headerHash <- unsafeBlockHeaderHashFromBytes . BS.pack <$> vectorOf 32 arbitrary
  blockNo <- BlockNo <$> arbitrary
  pure $ BlockHeader slotNo headerHash blockNo

instance Arbitrary BlockHeader where
  arbitrary = genBlockHeader
