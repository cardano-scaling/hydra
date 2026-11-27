module Cardano.Crypto.Util where

import Data.ByteString (ByteString)

class SignableRepresentation a where
  getSignableRepresentation :: a -> ByteString
