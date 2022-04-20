{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hydra.Cardano.Api.Address where

import Hydra.Cardano.Api.Prelude

import Cardano.Api.Byron (Address (..))
import Cardano.Binary (unsafeDeserialize')
import qualified Cardano.Chain.Common as Ledger
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Test.QuickCheck (frequency, oneof, vector)

-- * Orphans

instance ToJSON (Address ByronAddr) where
  toJSON = toJSON . AddressInEra (ByronAddressInAnyEra @Era)

instance FromJSON (Address ByronAddr) where
  parseJSON =
    Aeson.withText "Address Byron" $
      maybe empty pure . deserialiseAddress AsByronAddress

instance Arbitrary (Address ByronAddr) where
  arbitrary = do
    address <- Ledger.makeAddress <$> genSpendingData <*> genAttributes
    pure $ ByronAddress address
   where
    genSpendingData :: Gen Ledger.AddrSpendingData
    genSpendingData =
      let keyLen = 32
          chainCodeLen = 32
          majorType02 = 88
          cborPrefix n = BS.pack [majorType02, fromIntegral n]
       in frequency
            [
              ( 5
              , Ledger.VerKeyASD
                  . unsafeDeserialize'
                  . (cborPrefix (keyLen + chainCodeLen) <>)
                  <$> genBytes (keyLen + chainCodeLen)
              )
            ,
              ( 1
              , Ledger.RedeemASD
                  . unsafeDeserialize'
                  . (cborPrefix keyLen <>)
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
