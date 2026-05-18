module Hydra.Tx.ParameterUpdateSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Binary (decodeFull, serialize)
import Cardano.Crypto.Util (SignableRepresentation (..))
import Data.ByteString qualified as BS
import Hydra.Cardano.Api (Tx)
import Hydra.Tx (ParameterUpdate (..), Snapshot (..))
import Test.Hydra.Tx.Gen ()
import Test.QuickCheck ((===))

spec :: Spec
spec = parallel $ do
  describe "ParameterUpdate" $ do
    prop "round-trip via ToCBOR/FromCBOR" $ \(up :: ParameterUpdate) ->
      let encoded = serialize up
       in case decodeFull encoded of
            Right (decoded :: ParameterUpdate) -> decoded === up
            Left err -> error ("decode failed: " <> show err)

  describe "Snapshot signable representation" $ do
    prop "differs when a parameter update is attached" $ \(snapshot :: Snapshot Tx) (up :: ParameterUpdate) ->
      let withoutUp = snapshot{parameterUpdate = Nothing}
          withUp = snapshot{parameterUpdate = Just up}
       in getSignableRepresentation withoutUp /= getSignableRepresentation withUp

    prop "is stable when parameter update remains the same" $ \(snapshot :: Snapshot Tx) ->
      let bytes1 = getSignableRepresentation snapshot
          bytes2 = getSignableRepresentation snapshot
       in bytes1 === bytes2

    prop "non-empty for a snapshot with a pending update" $ \(snapshot :: Snapshot Tx) (up :: ParameterUpdate) ->
      let withUp = snapshot{parameterUpdate = Just up}
       in not (BS.null (getSignableRepresentation withUp))
