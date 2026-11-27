module Cardano.Crypto.Hash.Class where

import Data.ByteString (ByteString)

digest :: proxy algorithm -> ByteString -> ByteString
digest _ bytes = bytes
