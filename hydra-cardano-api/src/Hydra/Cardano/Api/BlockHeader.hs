{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.BlockHeader where

import Hydra.Cardano.Api.Prelude

import Cardano.Api (BlockHeader (..), BlockNo (..), Hash, SlotNo (..), deserialiseFromRawBytes, proxyToAsType)
import Data.ByteString qualified as BS
import Test.QuickCheck (vectorOf)

-- * Generators

-- | Fully arbitrary block header with completely random hash.
genBlockHeader :: Gen BlockHeader
genBlockHeader = do
  slotNo <- SlotNo <$> arbitrary
  genBlockHeaderAt slotNo

-- | Generate a random block header with completely random hash, but at a
-- certain slot.
genBlockHeaderAt :: SlotNo -> Gen BlockHeader
genBlockHeaderAt slotNo = do
  headerHash <- genBlockHeaderHash
  blockNo <- BlockNo <$> arbitrary
  pure $ BlockHeader slotNo headerHash blockNo

-- | Generate a random block header hash.
genBlockHeaderHash :: Gen (Hash BlockHeader)
genBlockHeaderHash =
  unsafeBlockHeaderHashFromBytes . BS.pack <$> vectorOf 32 arbitrary
 where
  unsafeBlockHeaderHashFromBytes :: ByteString -> Hash BlockHeader
  unsafeBlockHeaderHashFromBytes bytes =
    case deserialiseFromRawBytes (proxyToAsType Proxy) bytes of
      Left e ->
        error $
          "unsafeBlockHeaderHashFromBytes: failed on bytes "
            <> show bytes
            <> " with error "
            <> show e
      Right h -> h

-- * Orphans

instance Arbitrary BlockHeader where
  arbitrary = genBlockHeader
