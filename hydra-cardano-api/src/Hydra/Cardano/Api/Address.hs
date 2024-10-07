{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hydra.Cardano.Api.Address where

import Hydra.Cardano.Api.Prelude

import Cardano.Binary (unsafeDeserialize')
import Cardano.Chain.Common qualified as Ledger
import Data.ByteString qualified as BS
import Test.QuickCheck (frequency, oneof, vector)

-- * Orphans

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
