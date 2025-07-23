module Hydra.CBORSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import Hydra.Ledger.Simple (SimpleTx, SimpleTxOut)
import Hydra.Network (Host)
import Hydra.Network.Authenticate (Signed)
import Hydra.Network.Message (Message)
import Hydra.Tx.Party (Party)
import Test.QuickCheck (Property, (===))
import Test.QuickCheck.Instances ()

spec :: Spec
spec =
  describe "CBOR" $ do
    describe "Network.Host" $
      prop "can be round-tripped" $
        prop_canRoundtripCBOREncoding @Host
    describe "Network.Message" $
      prop "can be round-tripped" $
        prop_canRoundtripCBOREncoding @(Message SimpleTx)
    describe "SimpleTx" $
      prop "can be round-tripped" $
        prop_canRoundtripCBOREncoding @SimpleTx
    describe "SimpleTxOut" $
      prop "can be round-tripped" $
        prop_canRoundtripCBOREncoding @SimpleTxOut
    describe "Party" $
      prop "can be round-tripped" $
        prop_canRoundtripCBOREncoding @Party
    describe "Signed" $
      prop "can be round-tripped" $
        prop_canRoundtripCBOREncoding @(Signed (Message SimpleTx))

prop_canRoundtripCBOREncoding ::
  (ToCBOR a, FromCBOR a, Eq a, Show a) => a -> Property
prop_canRoundtripCBOREncoding a =
  let encoded = toLazyByteString $ toCBOR a
   in (snd <$> deserialiseFromBytes fromCBOR encoded) === Right a
