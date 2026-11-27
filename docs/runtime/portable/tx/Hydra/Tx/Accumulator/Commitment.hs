module Hydra.Tx.Accumulator.Commitment where

import Hydra.Prelude

import Accumulator (Accumulator)
import Data.Aeson (encode)
import Data.Map.Strict qualified as Map

commit :: Int -> Accumulator -> Either Text ByteString
commit _ = Right . toStrict . encode . map (\(element, (_, count)) -> (encodeBase16 element, count)) . Map.toAscList

hashCommitment :: ByteString -> ByteString
hashCommitment = id
