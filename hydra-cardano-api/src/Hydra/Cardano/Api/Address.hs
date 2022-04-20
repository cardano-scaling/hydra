{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hydra.Cardano.Api.Address where

import Hydra.Cardano.Api.Prelude

import Cardano.Api.Byron (Address (..))
import Cardano.Binary (unsafeDeserialize')
import qualified Cardano.Chain.Common as Ledger
import qualified Data.ByteString as BS
import Test.QuickCheck (frequency, oneof, vector)

-- * Orphans

instance ToJSON (Address ByronAddr) where
  toJSON = error "toJSON"

instance FromJSON (Address ByronAddr) where
  parseJSON = error "parseJSON"

instance Arbitrary (Address ByronAddr) where
  arbitrary = do
    address <- Ledger.makeAddress <$> genSpendingData <*> genAttributes
    pure $ ByronAddress address
   where
    genSpendingData :: Gen Ledger.AddrSpendingData
    genSpendingData =
      let keyLen = 32
          majorType02 = 88
          cborPrefix = BS.pack [majorType02, fromIntegral keyLen]
       in frequency
            [
              ( 5
              , Ledger.VerKeyASD . unsafeDeserialize' . (cborPrefix <>)
                  <$> genBytes keyLen
              )
            ,
              ( 1
              , Ledger.RedeemASD . unsafeDeserialize' . (cborPrefix <>)
                  <$> genBytes keyLen
              )
            ]

    genAttributes :: Gen Ledger.AddrAttributes
    genAttributes =
      let payloadLen = 32
       in Ledger.AddrAttributes
            <$> oneof
              [ pure Nothing
              , Just . Ledger.HDAddressPayload <$> genBytes payloadLen
              ]
            <*> genNetworkMagic

    genNetworkMagic :: Gen Ledger.NetworkMagic
    genNetworkMagic =
      oneof
        [ pure Ledger.NetworkMainOrStage
        , Ledger.NetworkTestnet <$> arbitrary
        ]

    genBytes :: Int -> Gen ByteString
    genBytes = fmap BS.pack . vector
