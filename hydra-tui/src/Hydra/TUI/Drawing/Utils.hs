module Hydra.TUI.Drawing.Utils where

import Brick (Widget, txt)
import Hydra.Cardano.Api (SerialiseAsRawBytes, TxId, serialiseToRawBytesHexText)
import Hydra.Prelude
import Hydra.Tx (HeadId)

-- | Render a 'HeadId' as its hex-encoded bytes (matches the Main tab display).
prettyHeadId :: HeadId -> Text
prettyHeadId = serialiseToRawBytesHexText

-- | Render a 'TxId' as its hex-encoded bytes (no quotes, no wrapper type).
prettyTxId :: TxId -> Text
prettyTxId = serialiseToRawBytesHexText

drawHex :: SerialiseAsRawBytes a => a -> Widget n
drawHex = txt . ("⚬ " <>) . serialiseToRawBytesHexText

drawShow :: forall a n. Show a => a -> Widget n
drawShow = txt . show

